(* ::Package:: *)

(* PersonalSite`TaskManager`
   --------------------------------------------------------------------------
   Registry central de TaskObjects de runtime.

   Cada tarea tiene un "spec" (descripcion estatica) y un "state" (estado
   dinamico que se actualiza en cada disparo). El manager envuelve la accion
   con un monitor de timing que registra duracion, resultado y errores.

   Spec de una tarea:
     <| "label"    -> "Heartbeat",    (* nombre legible              *)
        "group"    -> "system",       (* system | flow | nest | user *)
        "action"   -> fn,             (* funcion () -> resultado     *)
        "interval" -> 30,             (* segundos                    *)
        "enabled"  -> True            (* auto-start en start[]       *)
     |>

   State dinamico:
     <| "task"     -> TaskObject,  (* RunScheduledTask handle        *)
        "running"  -> True/False,
        "runs"     -> n,           (* total de disparos              *)
        "errors"   -> n,
        "lastRun"  -> DateObject,
        "lastMs"   -> 12.3,        (* ultimo tiempo de ejecucion ms  *)
        "avgMs"    -> 8.5,         (* promedio movil (20 muestras)   *)
        "lastErr"  -> "msg"/None,
        "history"  -> { <|"at","ms","ok","err"|>, ... }  (* ring 20 *)
     |>
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`TaskManager`"];

register::usage =
  "register[name, spec] registra la tarea con su spec. No la inicia.";

start::usage =
  "start[name] arranca la ScheduledTask de la tarea registrada. \
start[] arranca todas las que tienen enabled->True.";

stop::usage =
  "stop[name] detiene la ScheduledTask. stop[] detiene todas.";

restart::usage =
  "restart[name] detiene e inicia de nuevo la tarea.";

configure::usage =
  "configure[name, key, value] actualiza una clave del spec (p.ej. intervalo). \
Reinicia la tarea si estaba corriendo.";

allTasks::usage =
  "allTasks[] devuelve la Association completa de specs + states.";

info::usage =
  "info[name] devuelve el estado actual de una tarea.";

history::usage =
  "history[name] devuelve el historial de ejecucion (hasta 20 registros).";

summary::usage =
  "summary[] devuelve un JSON-serializable snapshot del runtime.";

dagData::usage =
  "dagData[] devuelve el grafo DAG de dependencias entre tareas como JSON.";

depGraph::usage =
  "depGraph[] devuelve un Graph WL nativo con el DAG de dependencias. \n" <>
  "V\u00e9rtices coloreados por grupo, tama\u00f1o por estado running/stopped, \n" <>
  "layout LayeredDigraphEmbedding (izq \u2192 der). \n" <>
  "Se puede usar como In[n]:= PersonalSite`TaskManager`depGraph[].";

unregister::usage =
  "unregister[name] detiene y elimina la tarea del registro. Devuelve name o $Failed.";

Begin["`Private`"];

(* ── Almacenamiento ─────────────────────────────────────────────────── *)
$specs  = <||>;   (* name -> spec Association            *)
$states = <||>;   (* name -> state Association           *)

$histLen = 20;    (* largo del ring buffer de historial  *)

initState[] := <|
  "task"    -> None,
  "running" -> False,
  "runs"    -> 0,
  "errors"  -> 0,
  "lastRun" -> None,
  "lastMs"  -> 0.,
  "avgMs"   -> 0.,
  "lastErr" -> Null,
  "history" -> {}
|>;

(* ── Registro ───────────────────────────────────────────────────────── *)
register[name_String, spec_Association] :=
  ($specs[name]  = spec;
   If[! KeyExistsQ[$states, name],
     $states[name] = initState[]];
   name);

(* ── Monitor wrapper ────────────────────────────────────────────────── *)
(* Envuelve la accion original: mide tiempo, captura errores, actualiza state. *)
monitoredRun[name_String] :=
  Module[{t0 = AbsoluteTime[], result, ms, ok = True, errMsg = Null},
    result = Quiet @ Check[
      $specs[name]["action"][],
      (ok = False; $Failed)];
    If[result === $Failed, errMsg = "execution returned $Failed"; ok = False];
    ms = 1000. * (AbsoluteTime[] - t0);

    (* Actualizar state *)
    With[{s = $states[name], prev = $states[name]["avgMs"],
          atStr = DateString[Now, {"Hour24", ":", "Minute", ":", "Second"}],
          errStr = If[ok, "", errMsg]},
      $states[name] = <|s,
        "runs"    -> s["runs"] + 1,
        "errors"  -> s["errors"] + If[ok, 0, 1],
        "lastRun" -> Now,
        "lastMs"  -> ms,
        "avgMs"   -> If[s["runs"] === 0, ms, .85 * prev + .15 * ms],
        "lastErr" -> If[ok, s["lastErr"], errMsg],
        "history" -> Take[
          Prepend[s["history"],
            <|"at" -> atStr, "ms" -> Round[ms, .1],
              "ok" -> ok, "err" -> errStr|>],
          Min[$histLen, Length[s["history"]] + 1]]
      |>];
    result
  ];

(* ── Iniciar / detener ─────────────────────────────────────────────── *)
start[name_String] :=
  Module[{spec, iv, task},
    If[! KeyExistsQ[$specs, name], Return[$Failed]];
    spec = $specs[name];
    If[TrueQ[$states[name]["running"]], Return[$states[name]["task"]]];
    iv   = Lookup[spec, "interval", 60];
    task = RunScheduledTask[monitoredRun[name], iv];
    $states[name] = <|$states[name],
      "task" -> task, "running" -> True|>;
    task
  ];

start[] :=
  Association @ KeyValueMap[
    Function[{name, spec},
      name -> If[TrueQ[Lookup[spec, "enabled", True]],
                 start[name], "disabled"]],
    $specs];

stop[name_String] :=
  (If[TrueQ[$states[name]["running"]],
     Quiet @ RemoveScheduledTask[$states[name]["task"]]];
   $states[name] = <|$states[name], "task" -> None, "running" -> False|>;
   name);

stop[] := (stop /@ Keys[$specs]; $specs);

restart[name_String] := (stop[name]; start[name]);

unregister[name_String] :=
  If[! KeyExistsQ[$specs, name], $Failed,
    (stop[name];
     KeyDropFrom[$specs,  name];
     KeyDropFrom[$states, name];
     name)];

(* ── Reconfigurar en caliente ────────────────────────────────────────── *)
configure[name_String, key_String, value_] :=
  Module[{wasRunning},
    If[! KeyExistsQ[$specs, name], Return[$Failed]];
    wasRunning = TrueQ[$states[name]["running"]];
    If[wasRunning, stop[name]];
    $specs[name] = <|$specs[name], key -> value|>;
    If[wasRunning, start[name]];
    $specs[name]
  ];

(* ── Consultas ────────────────────────────────────────────────────────── *)
(* Estado se fusiona al primer nivel: s["running"], s["runs"], s["avgMs"]   *)
(* La clave "state" se mantiene como alias para compatibilidad.             *)
allTasks[] :=
  Association @ KeyValueMap[
    Function[{name, spec},
      With[{st = If[KeyExistsQ[$states, name], $states[name], initState[]]},
        name -> <|spec, st, "state" -> st|>]],
    $specs];

info[name_String] :=
  If[KeyExistsQ[$specs, name],
    With[{st = If[KeyExistsQ[$states, name], $states[name], initState[]]},
      <|$specs[name], st, "state" -> st|>],
    $Failed];

history[name_String] :=
  If[KeyExistsQ[$states, name],
    Map[
      Function[h, <|
        "at"  -> ToString @ Lookup[h, "at",  ""],
        "ms"  -> N @ Lookup[h, "ms", 0],
        "ok"  -> TrueQ @ Lookup[h, "ok", False],
        "err" -> If[Lookup[h, "err", ""] === None || Lookup[h, "err", ""] === Null,
                   "", ToString @ Lookup[h, "err", ""]]
      |>],
      $states[name]["history"]],
    {}];

(* ── Snapshot JSON-serializable ─────────────────────────────────────── *)
toJ[x_] := Which[
  x === True || x === False, x,
  IntegerQ[x],               x,
  NumberQ[x],                N[x],
  StringQ[x],                x,
  x === None || x === Null,  "",
  True,                      ToString[x]];

summary[] :=
  Module[{tsk},
    tsk = Association @ KeyValueMap[
      Function[{name, spec},
        name -> <|
          "label"    -> toJ @ Lookup[spec, "label", name],
          "group"    -> toJ @ Lookup[spec, "group", "user"],
          "interval" -> toJ @ Lookup[spec, "interval", 60],
          "enabled"  -> TrueQ @ Lookup[spec, "enabled", True],
          "running"  -> TrueQ[$states[name]["running"]],
          "runs"     -> toJ[$states[name]["runs"]],
          "errors"   -> toJ[$states[name]["errors"]],
          "lastRun"  -> If[$states[name]["lastRun"] === None, "",
                          DateString[$states[name]["lastRun"],
                            {"Hour24", ":", "Minute", ":", "Second"}]],
          "lastMs"   -> N @ $states[name]["lastMs"],
          "avgMs"    -> N @ $states[name]["avgMs"],
          "lastErr"  -> toJ @ $states[name]["lastErr"],
          "history"  -> Map[
            Function[h, <|
              "at"  -> toJ @ Lookup[h, "at",  ""],
              "ms"  -> N @ Lookup[h, "ms", 0],
              "ok"  -> TrueQ @ Lookup[h, "ok", False],
              "err" -> toJ @ Lookup[h, "err", ""]
            |>],
            $states[name]["history"]]
        |>],
      $specs];
    <|"kernel"    -> $KernelID,
      "taskCount" -> Length[$specs],
      "running"   -> Length @ Select[$states, TrueQ[#["running"]] &],
      "tasks"     -> tsk
    |>
  ];

(* ── DAG de dependencias ──────────────────────────────────────────────── *)
dagData[] :=
  Module[{ids, links, intervals, depEdges, g, topo, depthOf, dist, cp, best},
    ids = Keys[$specs];
    If[Length[ids] == 0,
      Return[<|"nodes"->{}, "links"->{}, "topoOrder"->{},
               "critPath"->{}, "nodeCount"->0, "edgeCount"->0|>]];

    intervals = Association @ KeyValueMap[
      Function[{name, spec}, name -> Lookup[spec, "interval", 60]], $specs];

    links = Flatten @ KeyValueMap[
      Function[{name, spec},
        Map[Function[dep, <|"source"->dep, "target"->name|>],
            Lookup[spec, "deps", {}]]],
      $specs];

    (* Topological sort via WL built-in graph *)
    depEdges = Map[Function[e, DirectedEdge[e["source"], e["target"]]], links];
    g    = Graph[ids, depEdges];
    topo = Quiet @ Check[TopologicalSort[g], ids];

    (* Depth = longest path from roots (recursive, memoized inline) *)
    depthOf[name_String] :=
      With[{pars = Select[links, #["target"] === name &][[All, "source"]]},
        If[pars === {}, 0, 1 + Max[depthOf /@ pars]]];

    (* Critical path: max cumulative interval by topo order *)
    dist = AssociationThread[ids, ConstantArray[0, Length[ids]]];
    Scan[Function[u,
      Scan[Function[e,
        If[e["source"] === u,
           With[{w = dist[u] + Lookup[intervals, e["target"], 0]},
             If[w > dist[e["target"]], dist[e["target"]] = w]]]],
        links]],
      topo];
    best = First @ MaximalBy[ids, Lookup[dist, #, 0] &];
    (* Trace back path *)
    cp = Module[{path = {best}, cur = best, pars},
      While[True,
        pars = Select[links, #["target"] === cur &][[All, "source"]];
        If[pars === {}, Break[]];
        cur = First @ MaximalBy[pars, Lookup[dist, #, 0] &];
        PrependTo[path, cur]];
      path];

    <|
      "nodes" -> Map[Function[name,
        <|"id"       -> name,
          "label"    -> toJ @ Lookup[$specs[name], "label", name],
          "group"    -> toJ @ Lookup[$specs[name], "group", "user"],
          "interval" -> toJ @ Lookup[$specs[name], "interval", 60],
          "enabled"  -> TrueQ @ Lookup[$specs[name], "enabled", True],
          "running"  -> TrueQ[$states[name]["running"]],
          "runs"     -> toJ[$states[name]["runs"]],
          "depth"    -> Quiet @ Check[depthOf[name], 0],
          "cp"       -> TrueQ[MemberQ[cp, name]]
        |>], ids],
      "links"    -> links,
      "topoOrder"-> topo,
      "critPath" -> cp,
      "nodeCount"-> Length[ids],
      "edgeCount"-> Length[links]
    |>
  ];

(* ── depGraph: WL Graph nativo del DAG de tareas ──────────────────
   Vértices coloreados por grupo, aristas directed dep→tarea,
   layout LayeredDigraphEmbedding izquierda→derecha.         *)

$groupPalette = <|
  "system" -> RGBColor[0.35, 0.69, 1.00],   (* azul          *)
  "dev"    -> RGBColor[0.66, 0.33, 0.97],   (* violeta       *)
  "cache"  -> RGBColor[0.00, 0.90, 1.00],   (* cyan          *)
  "flow"   -> RGBColor[0.66, 0.85, 0.66],   (* verde         *)
  "theme"  -> RGBColor[1.00, 0.72, 0.40],   (* naranja       *)
  "kernel" -> RGBColor[0.92, 0.48, 0.10],   (* WL orange     *)
  (* ── DevOps pipeline ── *)
  "test"   -> RGBColor[0.92, 0.86, 0.18],   (* amarillo lima *)
  "git"    -> RGBColor[0.06, 0.78, 0.52],   (* esmeralda     *)
  "build"  -> RGBColor[0.42, 0.28, 0.94],   (* indigo        *)
  "ops"    -> RGBColor[0.96, 0.28, 0.37]    (* rosa/rojo     *)
|>;

PersonalSite`TaskManager`depGraph[] :=
  Module[{ids, edges, roots, vLabels, vStyles, vSizes, running,
          hbRuns, hbRunning, hbDeps, entangled, hbEdges, edgeStyles},
    ids = Keys[$specs];
    If[Length[ids] == 0,
      Return[Graph[{}, {}, ImageSize -> 500, Background -> RGBColor[0.05, 0.05, 0.07]]]];

    (* dep → task *)
    edges = Flatten @ KeyValueMap[
      Function[{name, spec},
        Map[Function[dep, DirectedEdge[dep, name]],
            Lookup[spec, "deps", {}]]],
      $specs];

    roots   = Select[ids, Lookup[$specs[#], "deps", {}] === {} &];
    running = Select[ids, TrueQ[$states[#]["running"]] &];

    (* ── Heartbeat entanglement ──────────────────────────────────── *)
    hbRuns    = Quiet @ Check[$states["heartbeat"]["runs"], 0];
    hbRunning = TrueQ[$states["heartbeat"]["running"]];
    hbDeps    = Select[ids,
                  MemberQ[Lookup[$specs[#], "deps", {}], "heartbeat"] &];
    entangled = Join[{"heartbeat"}, hbDeps];

    (* Aristas desde heartbeat → doradas punteadas (entanglement bond) *)
    hbEdges = Select[edges, Function[e, First[e] === "heartbeat"]];
    edgeStyles = Join[
      (* entanglement bonds: dorado, grueso, punteado *)
      Map[Function[e,
        e -> Directive[
          RGBColor[1.00, 0.82, 0.12],
          Arrowheads[{{0.028, 1}}],
          AbsoluteThickness[2.0],
          Dashing[{0.025, 0.012}]]],
        hbEdges],
      (* aristas normales *)
      {_ -> Directive[
        GrayLevel[0.40], Arrowheads[{{0.018, 1}}], AbsoluteThickness[0.9]]}
    ];

    (* ── Vertex labels ───────────────────────────────────────────── *)
    vLabels = Map[
      Function[name,
        With[{iv = ToString[Lookup[$specs[name], "interval", 0]] <> "s"},
          If[name === "heartbeat",
            (* ♩ latiendo: nota musical + beat count + intervalo *)
            name -> Placed[
              Column[{
                Style["\[MusicNote] \[MusicNote]",
                      FontSize -> 12,
                      FontColor -> RGBColor[1.00, 0.85, 0.15],
                      FontWeight -> Bold],
                Style["heartbeat",
                      FontSize -> 7, FontFamily -> "Courier New",
                      FontColor -> GrayLevel[0.95], FontWeight -> Bold],
                Style[ToString[hbRuns] <> "\[CenterDot]" <> iv,
                      FontSize -> 6, FontFamily -> "Courier New",
                      FontColor -> RGBColor[1.00, 0.82, 0.12]]
              }, Alignment -> Center],
              Below],
            (* label normal: id + interval *)
            name -> Placed[
              Column[{
                Style[name, FontSize -> 7, FontFamily -> "Courier New",
                      FontColor -> GrayLevel[0.85], FontWeight -> Bold],
                Style[iv, FontSize -> 6, FontFamily -> "Courier New",
                      FontColor -> GrayLevel[0.50]]
              }, Alignment -> Center],
              Below]
          ]]],
      ids];

    (* ── Vertex styles ───────────────────────────────────────────── *)
    vStyles = Map[
      Function[name,
        With[{grp  = Lookup[$specs[name], "group", "system"],
              enab = TrueQ[Lookup[$specs[name], "enabled", True]],
              run  = MemberQ[running, name]},
          name -> Which[
            (* heartbeat: dorado brillante con borde blanco grueso *)
            name === "heartbeat",
              Directive[
                RGBColor[1.00, 0.80, 0.10],
                Opacity[1.0],
                EdgeForm[Directive[White, AbsoluteThickness[2.5]]]],
            (* nodos entangled (deps directas de heartbeat): borde dorado *)
            MemberQ[hbDeps, name],
              Directive[
                Lookup[$groupPalette, grp, GrayLevel[0.45]],
                Opacity[If[enab, 1.0, 0.45]],
                EdgeForm[Directive[
                  RGBColor[1.00, 0.82, 0.12],
                  AbsoluteThickness[If[run, 1.8, 1.2]]]]],
            (* running: borde blanco *)
            run,
              Directive[
                Lookup[$groupPalette, grp, GrayLevel[0.45]],
                Opacity[1.0],
                EdgeForm[Directive[White, AbsoluteThickness[1.5]]]],
            (* default *)
            True,
              Directive[
                Lookup[$groupPalette, grp, GrayLevel[0.45]],
                Opacity[If[enab, 1.0, 0.45]],
                EdgeForm[Directive[GrayLevel[0.35], AbsoluteThickness[0.8]]]]
          ]]],
      ids];

    (* ── Vertex sizes ────────────────────────────────────────────── *)
    vSizes = Map[
      Function[name,
        name -> Which[
          name === "heartbeat",           If[hbRunning, 0.80, 0.65],
          MemberQ[running, name],         0.55,
          MemberQ[entangled, name],       0.42,
          True,                           0.38]],
      ids];

    Graph[
      ids, edges,
      VertexLabels -> vLabels,
      VertexStyle  -> vStyles,
      VertexSize   -> vSizes,
      EdgeStyle    -> edgeStyles,
      GraphLayout  -> {
        "LayeredDigraphEmbedding",
        "Orientation" -> Left,
        If[roots =!= {}, "RootVertex" -> First[roots], Nothing]},
      Background   -> RGBColor[0.05, 0.05, 0.07],
      ImageSize    -> {1100, 520},
      ImagePadding -> {{45, 45}, {70, 20}},
      PlotTheme    -> "Monochrome",
      GraphHighlight      -> Join[running, entangled],
      GraphHighlightStyle -> Directive[
        RGBColor[1.00, 0.82, 0.12], Opacity[0.20], AbsoluteThickness[3]]
    ]
  ];

End[];
EndPackage[];
