(* ::Package:: *)
(* Source/TaskObject.wl : task records, manager lifecycle, pretty printing *)
(* Loaded inside TaskManager`Private`. Public symbols resolve via $ContextPath. *)

(* ---------- failures & guards ---------- *)

managerFailure[mid_] := Failure["UnknownManager",
  <|"MessageTemplate" -> "No task manager registered for handle `1`.",
    "MessageParameters" -> {mid}|>];

taskFailure[id_] := Failure["UnknownTask",
  <|"MessageTemplate" -> "No task with id `1`.", "MessageParameters" -> {id}|>];

managerStateQ[mid_] := KeyExistsQ[$Managers, mid];

(* ---------- default task record ---------- *)

$DefaultTask = <|
  "Id"           -> Null,
  "Body"         -> (#["Input"] &),       (* default body: pass the input through *)
  "Dependencies" -> {},
  "Input"        -> Missing["NoInput"],
  "State"        -> "Pending",            (* Pending|Running|Completed|Failed|Skipped *)
  "Result"       -> Missing["NotRun"],
  "Priority"     -> 0,
  "MaxRetries"   -> 0,
  "Attempts"     -> 0,
  "Schedule"     -> None,
  "StartTime"    -> Missing["NotStarted"],
  "EndTime"      -> Missing["NotFinished"],
  "Error"        -> None,
  "Metadata"     -> <||>
|>;

(* ---------- accessors (private) ---------- *)

tasksOf[mid_] := $Managers[mid, "Tasks"];
taskOf[mid_, id_] := $Managers[mid, "Tasks", id];
taskExistsQ[mid_, id_] := managerStateQ[mid] && KeyExistsQ[$Managers[mid, "Tasks"], id];

(* ---------- lifecycle ---------- *)

CreateTaskManager[name_String : "Untitled"] := Module[{mid = CreateUUID[]},
  $Managers[mid] = <|
    "Name"           -> name,
    "Tasks"          -> <||>,
    "CreatedAt"      -> Now,
    "AsyncTasks"     -> <||>,   (* id -> TaskObject from SessionSubmit          *)
    "ScheduledTasks" -> <||>    (* id -> TaskObject from ScheduledTask submit   *)
  |>;
  TaskManagerObject[mid]
];

Options[AddTask] = {
  "Dependencies" -> {}, "Input" -> Missing["NoInput"],
  "Priority" -> 0, "MaxRetries" -> 0, "Schedule" -> None, "Metadata" -> <||>
};

AddTask[obj : TaskManagerObject[mid_], id_, body_, opts : OptionsPattern[]] :=
  If[! managerStateQ[mid], managerFailure[mid],
    $Managers[mid, "Tasks", id] = Join[$DefaultTask, <|
      "Id"           -> id,
      "Body"         -> body,
      "Dependencies" -> DeleteDuplicates@Flatten[{OptionValue["Dependencies"]}],
      "Input"        -> OptionValue["Input"],
      "Priority"     -> OptionValue["Priority"],
      "MaxRetries"   -> OptionValue["MaxRetries"],
      "Schedule"     -> OptionValue["Schedule"],
      "Metadata"     -> OptionValue["Metadata"]
    |>];
    obj
  ];

(* used by the generator builder when a node is reached from several parents *)
addDependency[mid_, id_, dep_] := $Managers[mid, "Tasks", id, "Dependencies"] =
  DeleteDuplicates@Append[taskOf[mid, id]["Dependencies"], dep];

RemoveTask[obj : TaskManagerObject[mid_], id_] :=
  If[! taskExistsQ[mid, id], taskFailure[id],
    $Managers[mid, "Tasks"] = KeyDrop[$Managers[mid, "Tasks"], id];
    (* also drop it from anybody depending on it *)
    KeyValueMap[
      Function[{tid, t},
        $Managers[mid, "Tasks", tid, "Dependencies"] = DeleteCases[t["Dependencies"], id]],
      tasksOf[mid]];
    obj
  ];

TaskManagerTasks[TaskManagerObject[mid_]] :=
  If[managerStateQ[mid], Keys[tasksOf[mid]], managerFailure[mid]];

TaskManagerReset[obj : TaskManagerObject[mid_]] := (
  $Managers[mid, "Tasks"] = Association@KeyValueMap[
    Function[{id, t}, id -> Join[t, <|
      "State" -> "Pending", "Result" -> Missing["NotRun"], "Attempts" -> 0,
      "StartTime" -> Missing["NotStarted"], "EndTime" -> Missing["NotFinished"],
      "Error" -> None|>]],
    tasksOf[mid]];
  obj
);

(* ---------- summary box ---------- *)

iconGraphics[] := Graphics[{
    RGBColor[0.36, 0.51, 0.83],
    Line[{{0, 2}, {-1, 1}}], Line[{{0, 2}, {0, 1}}], Line[{{0, 2}, {1, 1}}],
    Line[{{-1, 1}, {-1, 0}}], Line[{{1, 1}, {1, 0}}],
    PointSize[0.16],
    Point[{{0, 2}, {-1, 1}, {0, 1}, {1, 1}, {-1, 0}, {1, 0}}]},
  ImageSize -> Dynamic[{Automatic, 3.5 CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}],
  PlotRangePadding -> 0.4];

TaskManagerObject /: MakeBoxes[obj : TaskManagerObject[mid_String], form : (StandardForm | TraditionalForm)] /;
    managerStateQ[mid] :=
  Module[{st = $Managers[mid], counts},
    counts = Counts[#["State"] & /@ Values[st["Tasks"]]];
    BoxForm`ArrangeSummaryBox[
      TaskManagerObject, obj, iconGraphics[],
      {BoxForm`SummaryItem[{"Name: ", st["Name"]}],
       BoxForm`SummaryItem[{"Tasks: ", Length[st["Tasks"]]}]},
      {BoxForm`SummaryItem[{"Completed: ", Lookup[counts, "Completed", 0]}],
       BoxForm`SummaryItem[{"Failed: ", Lookup[counts, "Failed", 0]}],
       BoxForm`SummaryItem[{"States: ", counts}]},
      form]
  ];
