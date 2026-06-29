#!/usr/bin/env wolframscript
(* ::Package:: *)
(* Examples/example.wls : end-to-end walkthrough.                            *)
(* Run:  wolframscript -file Examples/example.wls                            *)

PacletDirectoryLoad[ParentDirectory@DirectoryName[$InputFileName]];
Needs["TaskManager`"];

(* ------------------------------------------------------------------ *)
(* 1. Same generator / init / depth as the picture, but each node now   *)
(*    carries real work: its value plus the sum of its parents' results.*)
(*    NestGraph[{2#+1, #+14, #-18}&, {1}, 3, "IncludeStepNumber"->True]  *)
(* ------------------------------------------------------------------ *)

gen = {2 # + 1, # + 14, # - 18} &;

nodeWork = Function[ctx,
  ctx["Metadata"]["Value"] + Total[Values@ctx["Dependencies"]]];

tm = TaskManagerFromGenerator[gen, {1}, 3,
  "Name" -> "PermutationPipeline",
  "NodeFunction" -> nodeWork];

Print["Tasks: ", Length@TaskManagerTasks[tm]];
Print["Acyclic / orderable? ", ! FailureQ@TopologicalTaskOrder[tm]];

(* ------------------------------------------------------------------ *)
(* 2. Run synchronously in topological order.                          *)
(* ------------------------------------------------------------------ *)

TaskManagerRun[tm];
Print["Sync counts: ", TaskManagerStatus[tm]["Counts"]];

(* ------------------------------------------------------------------ *)
(* 3. Run again, async, letting Wolfram's scheduler do the fan-out.    *)
(* ------------------------------------------------------------------ *)

TaskManagerReset[tm];
TaskManagerRunAsync[tm];
TaskManagerWait[tm];
Print["Async counts: ", TaskManagerStatus[tm]["Counts"]];

(* ------------------------------------------------------------------ *)
(* 4. Add an independent heartbeat task on a recurring schedule.       *)
(* ------------------------------------------------------------------ *)

AddTask[tm, "heartbeat",
  Function[ctx, Print["tick @ ", Now]; Now],
  "Schedule" -> Quantity[5, "Seconds"]];
ScheduleTask[tm, "heartbeat", Quantity[5, "Seconds"]];

(* ------------------------------------------------------------------ *)
(* 5. Persist + export for Power BI.                                   *)
(* ------------------------------------------------------------------ *)

ExportTaskManager[tm, FileNameJoin[{$TemporaryDirectory, "pipeline.wxf"}]];
paths = ExportToPowerBI[tm, FileNameJoin[{$TemporaryDirectory, "taskmanager_bi"}]];
Print["Power BI files: ", paths];
