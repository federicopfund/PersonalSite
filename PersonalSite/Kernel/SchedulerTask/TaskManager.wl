(* ::Package:: *)

(* ::Title:: *)
(* TaskManager` : DAG-based task orchestration on top of Wolfram task primitives *)

BeginPackage["TaskManager`"];

(* ---- Public API (usage declarations create the public symbols up front, *)
(*       so the Source/*.wl files attach definitions to THESE symbols)     *)

CreateTaskManager::usage =
  "CreateTaskManager[name] returns an empty TaskManagerObject handle.";

AddTask::usage =
  "AddTask[tm, id, body] registers a task whose body is invoked as body[ctx], where ctx is an association \
<|\"Input\"->..., \"Dependencies\"-><|depId->result,...|>, \"Id\"->id, \"Metadata\"->...|>. \
Options: \"Dependencies\", \"Input\", \"Priority\", \"MaxRetries\", \"Schedule\", \"Metadata\".";

RemoveTask::usage = "RemoveTask[tm, id] removes a task from the manager.";

TaskManagerTasks::usage = "TaskManagerTasks[tm] returns the list of task ids.";

TaskManagerFromGenerator::usage =
  "TaskManagerFromGenerator[gen, init, levels] builds a task DAG by expanding gen over init for the given \
number of levels, exactly mirroring NestGraph[gen, init, levels, \"IncludeStepNumber\"->True]: each (value, step) \
pair becomes a task that depends on the task that produced it. Option \"NodeFunction\" sets each task's body \
(default returns the node value).";

TaskManagerGraph::usage =
  "TaskManagerGraph[tm] returns a Graph of the dependency DAG, colored by current task state.";

ReadyTasks::usage =
  "ReadyTasks[tm] returns the ids of Pending tasks whose dependencies are all Completed.";

TopologicalTaskOrder::usage =
  "TopologicalTaskOrder[tm] returns a topological ordering of the task ids, or a Failure if the graph is cyclic.";

TaskManagerRun::usage =
  "TaskManagerRun[tm] executes all tasks synchronously in topological order. \
Option \"Method\" -> \"Sequential\" (default) | \"Parallel\" (level-by-level).";

TaskManagerRunAsync::usage =
  "TaskManagerRunAsync[tm] submits every task with SessionSubmit so that each task TaskWaits on its dependency \
tasks; returns immediately. Use TaskManagerWait[tm] to block until completion.";

TaskManagerWait::usage = "TaskManagerWait[tm] blocks until all async tasks submitted by TaskManagerRunAsync finish.";

ScheduleTask::usage =
  "ScheduleTask[tm, id, schedule] runs task id on a recurring schedule via SessionSubmit[ScheduledTask[...]]. \
schedule is any spec accepted by ScheduledTask (e.g. Quantity[5,\"Seconds\"], or {start, \"Hourly\"}).";

TaskManagerStatus::usage =
  "TaskManagerStatus[tm] returns <|\"Counts\"-><|state->n|>, \"Tasks\"-><|id->state|>|>.";

TaskManagerResults::usage = "TaskManagerResults[tm] returns <|id->result|> for completed tasks.";

TaskManagerReset::usage = "TaskManagerReset[tm] resets every task to the Pending state and clears results.";

ExportTaskManager::usage = "ExportTaskManager[tm, file] serializes the manager state to WXF.";
ImportTaskManager::usage = "ImportTaskManager[file] reconstructs a TaskManagerObject from a WXF file.";

ExportToPowerBI::usage =
  "ExportToPowerBI[tm, dir] writes TaskNodes.csv, TaskEdges.csv and TaskNodes.json to dir, shaped as a \
node table + edge table that Power BI can load directly (relationship Source->Target) for decomposition / \
network visuals. Returns an association of the written file paths.";

TaskManagerObject::usage = "TaskManagerObject[uuid] is a handle to a manager's mutable state.";

Begin["`Private`"];

(* Central, in-kernel registry of mutable manager states, keyed by UUID.    *)
(* TaskManagerObject is only a lightweight handle, like a TaskObject.        *)
$Managers = <||>;

(* Load implementation modules. They run inside TaskManager`Private` with    *)
(* TaskManager` on $ContextPath, so the public symbols above are resolved    *)
(* and extended, while local helpers stay private.                           *)
$sourceDir = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "Source"}];
Scan[
  Get[FileNameJoin[{$sourceDir, #}]] &,
  {"TaskObject.wl", "TaskGraph.wl", "Scheduler.wl", "Persistence.wl", "PowerBI.wl"}
];

End[];

EndPackage[];
