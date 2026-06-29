(* ::Package:: *)
(* Source/Scheduler.wl : the execution / scheduling engine.                  *)
(* This is the layer that touches the native Wolfram task primitives:        *)
(* SessionSubmit, ScheduledTask, TaskObject, TaskWait.                       *)

(* ---------- single-task execution (shared by every run mode) ----------    *)
(* Honors dependency results, retries, failure propagation and timing.       *)

runOneTask[mid_, id_] := Module[{t, depStates, ctx, depRes, res, maxr, attempts, ok},
  t = taskOf[mid, id];

  (* if any dependency failed/was skipped, skip (propagate failure forward) *)
  depStates = taskOf[mid, #]["State"] & /@ t["Dependencies"];
  If[AnyTrue[depStates, MatchQ[#, "Failed" | "Skipped"] &],
    $Managers[mid, "Tasks", id, "State"]  = "Skipped";
    $Managers[mid, "Tasks", id, "Result"] = Missing["DependencyFailed"];
    Return[taskOf[mid, id]]];

  depRes = AssociationMap[taskOf[mid, #]["Result"] &, t["Dependencies"]];
  ctx = <|"Input" -> t["Input"], "Dependencies" -> depRes,
          "Id" -> id, "Metadata" -> t["Metadata"]|>;

  maxr = t["MaxRetries"]; attempts = 0; ok = False; res = $Failed;
  $Managers[mid, "Tasks", id, "State"]     = "Running";
  $Managers[mid, "Tasks", id, "StartTime"] = Now;

  While[! ok && attempts <= maxr,
    attempts++;
    res = CheckAbort[Check[t["Body"][ctx], $Failed], $Failed];
    If[! MatchQ[res, $Failed] && ! FailureQ[res], ok = True]];

  $Managers[mid, "Tasks", id, "Attempts"] = attempts;
  $Managers[mid, "Tasks", id, "EndTime"]  = Now;
  If[ok,
    $Managers[mid, "Tasks", id, "Result"] = res;
    $Managers[mid, "Tasks", id, "State"]  = "Completed",
    (* else *)
    $Managers[mid, "Tasks", id, "Result"] = Missing["Failed"];
    $Managers[mid, "Tasks", id, "State"]  = "Failed";
    $Managers[mid, "Tasks", id, "Error"]  = res];
  taskOf[mid, id]
];

(* ---------- synchronous run ---------- *)

Options[TaskManagerRun] = {"Method" -> "Sequential"};

TaskManagerRun[obj : TaskManagerObject[mid_], OptionsPattern[]] :=
  Module[{order, levels},
    order = TopologicalTaskOrder[obj];
    If[FailureQ[order], Return[order]];

    Switch[OptionValue["Method"],
      "Sequential",
        Scan[runOneTask[mid, #] &, order],

      "Parallel",
        (* level by level: tasks in a level are independent, so run them in    *)
        (* parallel. Contexts are resolved in the master kernel, the pure body *)
        (* is mapped across subkernels, results written back here.             *)
        levels = topoLevels[mid];
        Scan[Function[level,
            Module[{jobs, results},
              jobs = Select[level, taskOf[mid, #]["State"] === "Pending" &];
              results = ParallelMap[runPure[mid, #] &, jobs];
              MapThread[writeResult[mid, #1, #2] &, {jobs, results}]]],
          levels],

      _, Return[Failure["BadMethod",
          <|"MessageTemplate" -> "Unknown \"Method\". Use \"Sequential\" or \"Parallel\"."|>]]
    ];
    obj
  ];

(* pure compute used by Parallel: no global mutation, returns the result     *)
runPure[mid_, id_] := Module[{t = taskOf[mid, id], ctx},
  ctx = <|"Input" -> t["Input"],
          "Dependencies" -> AssociationMap[taskOf[mid, #]["Result"] &, t["Dependencies"]],
          "Id" -> id, "Metadata" -> t["Metadata"]|>;
  CheckAbort[Check[t["Body"][ctx], $Failed], $Failed]
];

writeResult[mid_, id_, res_] := If[MatchQ[res, $Failed] || FailureQ[res],
  ($Managers[mid, "Tasks", id, "State"] = "Failed";
   $Managers[mid, "Tasks", id, "Result"] = Missing["Failed"];
   $Managers[mid, "Tasks", id, "Error"] = res),
  ($Managers[mid, "Tasks", id, "State"] = "Completed";
   $Managers[mid, "Tasks", id, "Result"] = res)];

(* ---------- asynchronous, dependency-aware run via SessionSubmit ----------
   Every task is submitted as a background task that first TaskWaits on the
   TaskObjects of its dependencies, then computes. This delegates the actual
   fan-out / ordering to Wolfram's own scheduler.                            *)

TaskManagerRunAsync[obj : TaskManagerObject[mid_]] := Module[{order, tobjs = <||>},
  order = TopologicalTaskOrder[obj];
  If[FailureQ[order], Return[order]];
  Scan[Function[id,
      With[{deps = taskOf[mid, id]["Dependencies"]},
        tobjs[id] = SessionSubmit[
          If[deps =!= {}, TaskWait[Lookup[tobjs, deps]]];
          runOneTask[mid, id]]]],
    order];
  $Managers[mid, "AsyncTasks"] = tobjs;
  obj
];

TaskManagerWait[obj : TaskManagerObject[mid_]] := (
  TaskWait[Values[$Managers[mid, "AsyncTasks"]]];
  obj);

(* ---------- recurring / time-based scheduling ---------- *)

ScheduleTask[obj : TaskManagerObject[mid_], id_, schedule_] :=
  If[! taskExistsQ[mid, id], taskFailure[id],
    $Managers[mid, "Tasks", id, "Schedule"] = schedule;
    $Managers[mid, "ScheduledTasks", id] =
      SessionSubmit[ScheduledTask[runOneTask[mid, id], schedule]];
    obj
  ];

(* ---------- inspection ---------- *)

TaskManagerStatus[TaskManagerObject[mid_]] := <|
  "Counts" -> Counts[#["State"] & /@ Values[tasksOf[mid]]],
  "Tasks"  -> (#["State"] & /@ tasksOf[mid])
|>;

TaskManagerResults[TaskManagerObject[mid_]] :=
  (#["Result"] &) /@ Select[tasksOf[mid], #["State"] === "Completed" &];
