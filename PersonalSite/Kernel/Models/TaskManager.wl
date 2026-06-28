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
          atStr = TimeString[Now], errStr = If[ok, "", errMsg]},
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
allTasks[] :=
  Association @ KeyValueMap[
    Function[{name, spec},
      name -> <|spec, "state" -> $states[name]|>],
    $specs];

info[name_String] :=
  If[KeyExistsQ[$specs, name],
    <|$specs[name], "state" -> $states[name]|>,
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

End[];
EndPackage[];
