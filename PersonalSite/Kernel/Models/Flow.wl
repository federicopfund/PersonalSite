(* ::Package:: *)

(* PersonalSite`Flow`
   --------------------------------------------------------------------------
   Orquestador de tareas como DAG (grafo dirigido aciclico). Cada tarea declara
   sus dependencias; el motor las ordena topologicamente y ejecuta cada tarea
   lista como un TaskObject asincrono (un "hilo" de flujo) via SessionSubmit.
   Las tareas de una misma capa (sin dependencias entre si) se reparten y corren
   a la vez.

   Una tarea se define como:
       name -> <| "deps" -> {dep1, ...}, "action" -> fn |>
   donde fn[<|dep1 -> r1, ...|>] recibe los resultados de sus dependencias y
   devuelve el resultado de la tarea. (Claves extra como "label" se ignoran.) *)

BeginPackage["PersonalSite`Flow`"];

graph::usage =
  "graph[spec] devuelve el Graph dirigido del DAG (dependencia -> dependiente).";

edges::usage =
  "edges[spec] devuelve la lista de aristas dep -> tarea.";

acyclicQ::usage =
  "acyclicQ[spec] indica si el DAG es valido (sin ciclos ni dependencias inexistentes).";

layers::usage =
  "layers[spec] devuelve las capas topologicas: listas de tareas que pueden correr en paralelo, o $Failed si hay ciclo.";

run::usage =
  "run[spec] o run[spec, backend] ejecuta el DAG (cada tarea como TaskObject) y \
devuelve <|\"ok\",\"results\",\"layers\",\"edges\",\"log\",\"elapsed\",\"backend\",\"kernels\"|>. \
backend: \"session\" (cooperativo, 1 kernel), \"parallel\" (subkernels reales via \
ParallelSubmit) o \"sync\". \"parallel\" degrada a \"session\" si no hay kernels.";

kernelInfo::usage =
  "kernelInfo[] devuelve <|\"max\", \"running\"|> del pool de subkernels.";

setMaxKernels::usage =
  "setMaxKernels[n] fija el tope de subkernels por kernel del pool.";

shutdownKernels::usage =
  "shutdownKernels[] cierra los subkernels lanzados.";

distribute::usage =
  "distribute[] envia a los subkernels las definiciones de los contextos en \
$distributeContexts (p.ej. los helpers de PersonalSite`Flow`Lib`).";

Begin["`Private`"];

deps[spec_, n_]   := Lookup[spec[n], "deps", {}];
action[spec_, n_] := Lookup[spec[n], "action", (Null &)];

edges[spec_Association] :=
  Flatten @ KeyValueMap[
    Function[{n, def}, (# -> n) & /@ Lookup[def, "deps", {}]],
    spec];

graph[spec_Association] :=
  Graph[Keys[spec], edges[spec], VertexLabels -> "Name"];

(* Valido: todas las dependencias existen y el grafo es aciclico. *)
acyclicQ[spec_Association] :=
  Module[{names = Keys[spec], es = edges[spec]},
    AllTrue[es, MemberQ[names, First[#]] &] && AcyclicGraphQ[Graph[names, es]]
  ];

(* Capas topologicas (Kahn por niveles): en cada paso entran las tareas cuyas
   dependencias ya estan resueltas. $Failed si queda un ciclo. *)
layers[spec_Association] :=
  Module[{names = Keys[spec], done = {}, out = {}, ready},
    While[Length[done] < Length[names],
      ready = Select[names,
        Function[n, ! MemberQ[done, n] && SubsetQ[done, deps[spec, n]]]];
      If[ready === {}, Return[$Failed]];
      AppendTo[out, ready];
      done = Join[done, ready]
    ];
    out
  ];

(* --- Pool de subkernels (centralizado y con tope) --------------------
   Cada kernel del pool web mantiene su propio pool de subkernels, lanzado una
   sola vez y limitado por $maxKernels (env FLOW_MAX_KERNELS). El total de
   kernels del sistema es, como maximo, POOLSIZE * $maxKernels. *)

$maxKernels =
  With[{n = Quiet @ ToExpression @ PersonalSite`Config`value["FLOW_MAX_KERNELS", "2"]},
    If[IntegerQ[n] && n > 0, n, 2]];

(* Lanza (una vez) hasta $maxKernels subkernels; idempotente y con tope. *)
ensureKernels[] :=
  (If[$KernelCount < $maxKernels,
     Quiet @ Check[LaunchKernels[$maxKernels - $KernelCount], Null]];
   $KernelCount);

setMaxKernels[n_Integer /; n > 0] := ($maxKernels = n);
kernelInfo[]      := <|"max" -> $maxKernels, "running" -> $KernelCount|>;
shutdownKernels[] := (Quiet @ CloseKernels[]; $KernelCount);

(* --- Distribucion de definiciones a los subkernels -------------------
   Las acciones que usan helpers fuera de System`/Global` necesitan que sus
   definiciones viajen a los subkernels. Se distribuyen los contextos de
   $distributeContexts; Global se auto-distribuye via $DistributedContexts. *)

$distributeContexts = {"PersonalSite`Flow`Lib`"};

distributeContext[ctx_String] :=
  With[{names = Names[ctx <> "*"]},
    If[names =!= {},
      Quiet @ Check[
        ToExpression["DistributeDefinitions[" <> StringRiffle[names, ", "] <> "]"],
        Null]]];

distribute[] :=
  ($DistributedContexts = DeleteDuplicates @ Join[
     If[ListQ[$DistributedContexts], $DistributedContexts, {"Global`"}],
     $distributeContexts];
   Scan[distributeContext, $distributeContexts];);

(* --- Backends de ejecucion -------------------------------------------
   "sync"     : evaluacion directa (sin tareas).
   "session"  : un TaskObject por tarea (SessionSubmit), cooperativo en 1 kernel.
   "parallel" : un ParallelSubmit por tarea -> subkernels reales (con
                DistributeDefinitions); degrada a "session" si no hay kernels. *)

resolveBackend["parallel"] :=
  With[{n = ensureKernels[]},
    If[IntegerQ[n] && n > 0, {"parallel", n}, {"session", 0}]];
resolveBackend["sync"] := {"sync", 0};
resolveBackend[_]      := {"session", 0};

(* Ejecuta UNA capa con el backend dado; devuelve <|tarea -> resultado|>.
   Lee `prev` (resultados de capas anteriores) para alimentar cada accion. *)
runLayer["sync", spec_, layer_, prev_] :=
  Association @ Map[
    Function[name, name -> action[spec, name][KeyTake[prev, deps[spec, name]]]],
    layer];

runLayer["session", spec_, layer_, prev_] :=
  Module[{out = <||>, tasks},
    tasks = Association @ Map[
      Function[name,
        With[{fn = action[spec, name], dr = KeyTake[prev, deps[spec, name]]},
          name -> SessionSubmit[out[name] = fn[dr]]]],
      layer];
    TimeConstrained[TaskWait[Values[tasks]], 20];
    (* respaldo sincrono si algun hilo no dejo resultado *)
    Scan[
      Function[name,
        If[! KeyExistsQ[out, name],
          out[name] = action[spec, name][KeyTake[prev, deps[spec, name]]]]],
      layer];
    out
  ];

runLayer["parallel", spec_, layer_, prev_] :=
  Module[{evs, vals},
    (* Cada tarea de la capa se reparte a un subkernel real. *)
    evs = Map[
      Function[name,
        With[{fn = action[spec, name], dr = KeyTake[prev, deps[spec, name]]},
          ParallelSubmit[fn[dr]]]],
      layer];
    vals = TimeConstrained[WaitAll[evs], 30, $Failed];
    If[ListQ[vals] && Length[vals] === Length[layer],
      AssociationThread[layer, vals],
      runLayer["sync", spec, layer, prev]]   (* degradacion segura *)
  ];

run[spec_Association] := run[spec, "session"];

run[spec_Association, backendReq_String] :=
  Module[{ls, results = <||>, t0 = AbsoluteTime[], log = {}, backend, nk},
    If[! acyclicQ[spec],
      Return[<|"ok" -> False, "error" -> "DAG invalido: ciclo o dependencia inexistente."|>]];
    ls = layers[spec];
    If[ls === $Failed,
      Return[<|"ok" -> False, "error" -> "No se pudo ordenar el DAG (ciclo)."|>]];

    {backend, nk} = resolveBackend[backendReq];
    If[backend === "parallel", distribute[]];

    Do[
      AssociateTo[results, runLayer[backend, spec, layer, results]];
      AppendTo[log, <|"layer" -> layer, "threads" -> Length[layer]|>],
      {layer, ls}
    ];

    <|
      "ok"         -> True,
      "results"    -> results,
      "layers"     -> ls,
      "edges"      -> edges[spec],
      "log"        -> log,
      "elapsed"    -> AbsoluteTime[] - t0,
      "requested"  -> backendReq,
      "backend"    -> backend,
      "kernels"    -> nk,
      "maxKernels" -> $maxKernels
    |>
  ];

End[];
EndPackage[];
