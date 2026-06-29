(* ::Package:: *)
(* Source/TaskGraph.wl : the DAG layer. This is where the NestGraph from the *)
(* image becomes a task dependency graph.                                    *)

(* ---------- raw graph from dependencies ---------- *)

taskGraph[mid_] := Module[{ids, edges},
  ids = Keys[tasksOf[mid]];
  edges = Flatten@KeyValueMap[
    Function[{id, t}, DirectedEdge[#, id] & /@ t["Dependencies"]],
    tasksOf[mid]];
  Graph[ids, edges]
];

TopologicalTaskOrder[TaskManagerObject[mid_]] := Module[{g = taskGraph[mid]},
  If[! AcyclicGraphQ[g],
    Failure["CyclicGraph", <|
      "MessageTemplate" -> "The task graph contains a cycle; cannot order it.",
      "MessageParameters" -> {}, "Cycles" -> FindCycle[g, Infinity, All]|>],
    TopologicalSort[g]
  ]
];

(* longest-path depth of each node -> execution "levels" (no intra-level deps). *)
(* Walking ids in topological order guarantees parents are scored first.        *)
topoLevels[mid_] := Module[{order = TopologicalSort[taskGraph[mid]], depth = <||>},
  Scan[Function[id,
      With[{parents = taskOf[mid, id]["Dependencies"]},
        depth[id] = If[parents === {}, 0, 1 + Max[Lookup[depth, parents]]]]],
    order];
  Values@KeySort@GroupBy[order, depth]  (* {level0Ids, level1Ids, ...} *)
];

ReadyTasks[TaskManagerObject[mid_]] :=
  Select[Keys[tasksOf[mid]],
    Function[id,
      taskOf[mid, id]["State"] === "Pending" &&
      AllTrue[taskOf[mid, id]["Dependencies"],
        taskOf[mid, #]["State"] === "Completed" &]]];

(* ---------- NestGraph-style generator -> task DAG ----------

   gen     : a value -> {childValue, ...} function, e.g. {2 #+1, #+14, #-18} &
   init    : list of seed values, e.g. {1}
   levels  : expansion depth (the "3" in your NestGraph)

   Mirrors NestGraph[gen, init, levels, "IncludeStepNumber"->True]: a node is the
   pair (value, step), so the same value at different steps is a distinct task,
   and a value reached at the same step from two parents becomes ONE task with
   two dependencies (the merging edges visible in your picture).               *)

Options[TaskManagerFromGenerator] = {"Name" -> "Generated", "NodeFunction" -> Automatic};

TaskManagerFromGenerator[gen_, init_List, levels_Integer?NonNegative, opts : OptionsPattern[]] :=
  Module[{tm, mid, nodeFn, frontier, nextFrontier, nid},
    nodeFn = OptionValue["NodeFunction"] /. Automatic -> (#["Input"] &);
    tm = CreateTaskManager[OptionValue["Name"]];
    mid = First@tm;
    nid[v_, s_] := "n_" <> ToString[v] <> "_" <> ToString[s];

    (* roots: step 0 *)
    frontier = Map[
      Function[v,
        With[{id = nid[v, 0]},
          AddTask[tm, id, nodeFn, "Input" -> v, "Dependencies" -> {},
            "Metadata" -> <|"Value" -> v, "Step" -> 0|>];
          {id, v}]],
      DeleteDuplicates@init];

    Do[
      nextFrontier = {};
      Do[
        With[{pid = fp[[1]], pv = fp[[2]]},
          Function[cv,
            With[{cid = nid[cv, s]},
              If[! taskExistsQ[mid, cid],
                AddTask[tm, cid, nodeFn, "Input" -> cv, "Dependencies" -> {pid},
                  "Metadata" -> <|"Value" -> cv, "Step" -> s|>],
                addDependency[mid, cid, pid]];
              AppendTo[nextFrontier, {cid, cv}]]] /@ gen[pv]],
        {fp, frontier}];
      frontier = DeleteDuplicates@nextFrontier,
      {s, 1, levels}];
    tm
  ];

(* ---------- styled visualization (echoes the image, colored by state) ---------- *)

$StateColor = <|
  "Pending"   -> GrayLevel[0.7],
  "Running"   -> RGBColor[0.95, 0.75, 0.2],
  "Completed" -> RGBColor[0.30, 0.69, 0.40],
  "Failed"    -> RGBColor[0.85, 0.30, 0.30],
  "Skipped"   -> RGBColor[0.60, 0.60, 0.85]
|>;

TaskManagerGraph[TaskManagerObject[mid_]] := Module[{g = taskGraph[mid]},
  Graph[g,
    VertexStyle -> Map[# -> Lookup[$StateColor, taskOf[mid, #]["State"], Gray] &, VertexList[g]],
    VertexLabels -> Placed["Name", Tooltip],
    VertexSize -> 0.6,
    GraphLayout -> "LayeredDigraphEmbedding",
    EdgeStyle -> Directive[Opacity[0.4], RGBColor[0.36, 0.51, 0.83]],
    PerformanceGoal -> "Quality"]
];
