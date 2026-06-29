(* ::Package:: *)

PacletObject[<|
  "Name" -> "TaskManager",
  "Version" -> "1.0.0",
  "WolframVersion" -> "13.0+",
  "Description" -> "DAG-based task orchestration and scheduling built on Wolfram's native task primitives (SessionSubmit / ScheduledTask / TaskObject). Models pipelines as a directed acyclic graph in the spirit of NestGraph, with topological execution, async fan-out, retries, failure propagation and Power BI export.",
  "Creator" -> "Architecture reference",
  "License" -> "MIT",
  "Extensions" -> {
    {"Kernel",
      "Root" -> "Kernel",
      "Context" -> "TaskManager`",
      "Symbols" -> {
        "TaskManager`CreateTaskManager",
        "TaskManager`AddTask",
        "TaskManager`RemoveTask",
        "TaskManager`TaskManagerTasks",
        "TaskManager`TaskManagerFromGenerator",
        "TaskManager`TaskManagerGraph",
        "TaskManager`ReadyTasks",
        "TaskManager`TopologicalTaskOrder",
        "TaskManager`TaskManagerRun",
        "TaskManager`TaskManagerRunAsync",
        "TaskManager`TaskManagerWait",
        "TaskManager`ScheduleTask",
        "TaskManager`TaskManagerStatus",
        "TaskManager`TaskManagerResults",
        "TaskManager`TaskManagerReset",
        "TaskManager`ExportTaskManager",
        "TaskManager`ImportTaskManager",
        "TaskManager`ExportToPowerBI",
        "TaskManager`TaskManagerObject"
      }
    },
    {"Documentation", "Language" -> "English"}
  }
|>]
