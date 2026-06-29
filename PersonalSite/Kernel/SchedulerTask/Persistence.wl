(* ::Package:: *)
(* Source/Persistence.wl : serialize / restore a manager's full state.       *)
(* WXF preserves pure-function bodies, associations and missings faithfully. *)

ExportTaskManager[TaskManagerObject[mid_], file_String] :=
  If[! managerStateQ[mid], managerFailure[mid],
    Export[file,
      <|"Handle" -> mid, "State" -> KeyDrop[$Managers[mid], {"AsyncTasks", "ScheduledTasks"}]|>,
      "WXF"];
    file
  ];

ImportTaskManager[file_String] := Module[{data, mid},
  data = Import[file, "WXF"];
  If[! AssociationQ[data] || ! KeyExistsQ[data, "State"],
    Return[Failure["BadFile",
      <|"MessageTemplate" -> "`1` is not a TaskManager export.", "MessageParameters" -> {file}|>]]];
  mid = Lookup[data, "Handle", CreateUUID[]];
  $Managers[mid] = Join[
    <|"AsyncTasks" -> <||>, "ScheduledTasks" -> <||>|>,
    data["State"]];
  TaskManagerObject[mid]
];
