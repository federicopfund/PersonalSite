(* ::Package:: *)

(* PersonalSite`Controller`  (parte: flow)
   --------------------------------------------------------------------------
   Pagina /flow: define un DAG de tareas de ejemplo (inspirado en
   NestGraph[{2#+1, #+14, #-18}&, ...]), lo ejecuta con PersonalSite`Flow`
   (cada tarea = un TaskObject) y muestra capas, dependencias y resultados. *)

BeginPackage["PersonalSite`Controller`"];

flow::usage =
  "flow[request] renderiza /flow: ejecuta el DAG de ejemplo y muestra el flujo.";

Begin["`Private`"];

esc[x_] := PersonalSite`View`escape[ToString[x]];

(* Helper distribuible: vive en PersonalSite`Flow`Lib`, contexto que el motor
   envia a los subkernels con DistributeDefinitions antes de correr en paralelo.
   Una accion que lo usa demuestra que la definicion viaja a los subkernels. *)
PersonalSite`Flow`Lib`boost[x_] := x^2 + 1;

(* DAG de ejemplo. Cada tarea: <|deps, action, label|>.
   action[<|dep -> resultado|>] -> resultado de la tarea. *)
flowSpec[] := <|
  "seed"    -> <|"deps" -> {},                           "label" -> "semilla = 1",            "action" -> (1 &)|>,
  "f1"      -> <|"deps" -> {"seed"},                      "label" -> "2x + 1",                 "action" -> (2 #["seed"] + 1 &)|>,
  "f2"      -> <|"deps" -> {"seed"},                      "label" -> "x + 14",                 "action" -> (#["seed"] + 14 &)|>,
  "f3"      -> <|"deps" -> {"seed"},                      "label" -> "x - 18",                 "action" -> (#["seed"] - 18 &)|>,
  "boosted" -> <|"deps" -> {"f1"},                        "label" -> "Lib`boost(f1) = f1^2+1", "action" -> (PersonalSite`Flow`Lib`boost[#["f1"]] &)|>,
  "merge"   -> <|"deps" -> {"f1", "f2", "f3", "boosted"}, "label" -> "f1+f2+f3+boosted",       "action" -> (#["f1"] + #["f2"] + #["f3"] + #["boosted"] &)|>,
  "final"   -> <|"deps" -> {"merge"},                     "label" -> "merge ^ 2",              "action" -> (#["merge"]^2 &)|>
|>;

flow[request_] :=
  Module[{spec = flowSpec[], backendReq, r, ls, results, es, maxPar, eff, distChk},
    backendReq = flowBackend[request];
    r       = PersonalSite`Flow`run[spec, backendReq];
    ls      = Lookup[r, "layers", {}];
    results = Lookup[r, "results", <||>];
    es      = Lookup[r, "edges", {}];
    maxPar  = If[ls === {}, 0, Max[Length /@ ls]];
    eff     = Lookup[r, "backend", "session"];

    (* Prueba en vivo de DistributeDefinitions: corre el helper en TODOS los
       subkernels. Si la definicion viajo, devuelve {5, 5, ...}; si no, queda
       sin evaluar. Solo aplica en backend parallel. *)
    distChk = If[eff === "parallel",
      esc @ Quiet @ ParallelEvaluate[PersonalSite`Flow`Lib`boost[2]],
      "n/a (solo en parallel)"];

    PersonalSite`View`render["flow", <|
      "diagram"     -> flowDiagram[spec, ls, results],
      "edges"       -> flowEdges[es],
      "rows"        -> flowRows[spec, ls, results],
      "tasks"       -> ToString[Length[spec]],
      "layersN"     -> ToString[Length[ls]],
      "maxPar"      -> ToString[maxPar],
      "elapsed"     -> ToString[Round[1000. Lookup[r, "elapsed", 0.]]] <> " ms",
      "final"       -> esc[Lookup[results, "final", "?"]],
      "backend"     -> eff,
      "kernels"     -> ToString[Lookup[r, "kernels", 0]],
      "maxKernels"  -> ToString[Lookup[r, "maxKernels", 0]],
      "distcheck"   -> distChk,
      "degraded"    -> If[eff =!= backendReq,
                        " &middot; \"" <> esc[backendReq] <> "\" no disponible (free engine), usando " <> eff,
                        ""],
      "sessionSel"  -> If[eff === "session", " is-on", ""],
      "parallelSel" -> If[eff === "parallel", " is-on", ""],
      "syncSel"     -> If[eff === "sync", " is-on", ""]
    |>]
  ];

(* Backend pedido por query (?backend=session|parallel|sync); default session. *)
flowBackend[request_] :=
  Module[{q},
    q = Quiet @ Lookup[
      If[ListQ[request["Query"]], Association[request["Query"]], <||>],
      "backend", "session"];
    If[MemberQ[{"session", "parallel", "sync"}, q], q, "session"]
  ];

(* Capas como columnas; cada nodo muestra nombre, accion y resultado. *)
flowDiagram[spec_, ls_, results_] :=
  StringRiffle[
    MapIndexed[
      Function[{layer, idx},
        "<div class=\"flow-layer\">" <>
          "<div class=\"flow-layer__head\">Capa " <> ToString[First @ idx] <>
            " &middot; " <> ToString[Length[layer]] <> " hilo(s)</div>" <>
          StringRiffle[flowNode[#, spec, results] & /@ layer, "\n"] <>
        "</div>"
      ],
      ls
    ],
    "\n"
  ];

flowNode[name_, spec_, results_] :=
  "<div class=\"flow-node\">" <>
    "<span class=\"flow-node__name\">" <> esc[name] <> "</span>" <>
    "<span class=\"flow-node__label\">" <> esc[Lookup[spec[name], "label", ""]] <> "</span>" <>
    "<span class=\"flow-node__res\">= " <> esc[Lookup[results, name, "?"]] <> "</span>" <>
  "</div>";

flowEdges[es_] :=
  StringRiffle[
    Function[e,
      "<li><code>" <> esc[First[e]] <> "</code> <span>&rarr;</span> <code>" <>
        esc[Last[e]] <> "</code></li>"] /@ es,
    "\n"];

flowRows[spec_, ls_, results_] :=
  StringRiffle[
    Flatten @ MapIndexed[
      Function[{layer, idx},
        Function[name,
          "<tr><td>" <> ToString[First @ idx] <> "</td>" <>
            "<td><code>" <> esc[name] <> "</code></td>" <>
            "<td>" <> esc[Lookup[spec[name], "label", ""]] <> "</td>" <>
            "<td>" <> esc[StringRiffle[Lookup[spec[name], "deps", {}], ", "]] <> "</td>" <>
            "<td><strong>" <> esc[Lookup[results, name, "?"]] <> "</strong></td></tr>"
        ] /@ layer
      ],
      ls
    ],
    "\n"];

End[];
EndPackage[];
