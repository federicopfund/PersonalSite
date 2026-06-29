# TaskManager

DAG-based task orchestration for Wolfram Language, built directly on the native
task primitives (`SessionSubmit`, `ScheduledTask`, `TaskObject`, `TaskWait`).

The model is the one in the source notebook: a `NestGraph`-style expansion such as

```wolfram
NestGraph[{2#+1, #+14, #-18} &, {1}, 3, "IncludeStepNumber" -> True]
```

is read as a **task dependency DAG** — every `(value, step)` node is a task, every
edge is a dependency. The same primitive that draws the picture also defines the
execution plan.

## Module map

| Layer | File | Responsibility |
|-------|------|----------------|
| Public API + loader | `Kernel/TaskManager.wl` | declares the public symbols, loads the modules |
| Task model | `Source/TaskObject.wl` | task record, manager lifecycle, `AddTask`, summary box |
| DAG | `Source/TaskGraph.wl` | dependency graph, topological order, levels, `ReadyTasks`, `TaskManagerFromGenerator`, styled `TaskManagerGraph` |
| Scheduler / executor | `Source/Scheduler.wl` | `runOneTask`, sync (sequential/parallel), async (`SessionSubmit`+`TaskWait`), `ScheduleTask`, status/results |
| Persistence | `Source/Persistence.wl` | WXF export/import of full state |
| BI export | `Source/PowerBI.wl` | node table + edge table as CSV/JSON for Power BI |

## Task contract

A task body is always called as `body[ctx]` where

```wolfram
ctx = <|
  "Input"        -> <seed value, if any>,
  "Dependencies" -> <| parentId -> parentResult, ... |>,
  "Id"           -> <this task id>,
  "Metadata"     -> <| ... |>
|>
```

State machine: `Pending -> Running -> Completed | Failed`, with `Skipped` when an
upstream dependency failed (failure propagates forward instead of silently
producing wrong results). `MaxRetries` re-runs the body on message/`$Failed`.

## Quick start

```wolfram
PacletDirectoryLoad["/path/to/TaskManager"];
Needs["TaskManager`"];

tm = TaskManagerFromGenerator[{2#+1, #+14, #-18} &, {1}, 3,
       "NodeFunction" -> Function[ctx,
          ctx["Metadata"]["Value"] + Total[Values@ctx["Dependencies"]]]];

TaskManagerRun[tm];                 (* sequential, topological            *)
TaskManagerRunAsync[tm];            (* async fan-out on Wolfram scheduler *)
TaskManagerWait[tm];

TaskManagerGraph[tm]                (* colored by state                   *)
TaskManagerStatus[tm]
ExportToPowerBI[tm, "out/bi"]
```

## Power BI flow

`ExportToPowerBI` writes `TaskNodes.csv` (one row per task: id, state, value,
step, result, attempts, start/end, duration) and `TaskEdges.csv`
(`Source`,`Target`). In Power BI:

1. Get Data → Text/CSV → load both tables.
2. Model view → relationship `TaskEdges[Source] -> TaskNodes[Id]`.
3. Visualize with a Decomposition Tree, Force-Directed Graph or Sankey, colored
   by `State`, sized by `DurationSeconds`.

For a live push instead of files, the same node/edge tables can be sent to a
Power BI streaming dataset via `URLExecute` against the Power BI REST API.

## Note

This is a reference implementation written against Wolfram Language 13+
semantics; it has not been executed in this environment (no kernel available
here). Validate with `Examples/example.wls`.
