(* ::Package:: *)
(* Source/PowerBI.wl : flatten the manager into a node table + edge table.   *)
(* Power BI loads TaskNodes.csv and TaskEdges.csv and you wire a relationship *)
(* Source -> Target for a decomposition tree / network / Sankey visual.       *)

safeStr[x_] := Which[
  MissingQ[x], "",
  StringQ[x], x,
  True, StringTake[ToString[x, InputForm], UpTo[2000]]];

durationSeconds[t_] := If[MissingQ[t["StartTime"]] || MissingQ[t["EndTime"]],
  Missing["NA"],
  QuantityMagnitude@DateDifference[t["StartTime"], t["EndTime"], "Second"]];

taskNodeTable[mid_] := KeyValueMap[
  Function[{id, t},
    <|
      "Id"              -> id,
      "State"           -> t["State"],
      "Value"           -> Lookup[t["Metadata"], "Value", Missing["NA"]],
      "Step"            -> Lookup[t["Metadata"], "Step", Missing["NA"]],
      "Input"           -> safeStr[t["Input"]],
      "Result"          -> safeStr[t["Result"]],
      "Priority"        -> t["Priority"],
      "Attempts"        -> t["Attempts"],
      "MaxRetries"      -> t["MaxRetries"],
      "NumDependencies" -> Length[t["Dependencies"]],
      "StartTime"       -> t["StartTime"],
      "EndTime"         -> t["EndTime"],
      "DurationSeconds" -> durationSeconds[t]
    |>],
  tasksOf[mid]];

taskEdgeTable[mid_] := Flatten@KeyValueMap[
  Function[{id, t}, <|"Source" -> #, "Target" -> id|> & /@ t["Dependencies"]],
  tasksOf[mid]];

ExportToPowerBI[TaskManagerObject[mid_], dir_String] :=
  If[! managerStateQ[mid], managerFailure[mid],
    Module[{nodes, edges, nf, ef, jf},
      nodes = taskNodeTable[mid];
      edges = taskEdgeTable[mid];
      If[! DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
      nf = FileNameJoin[{dir, "TaskNodes.csv"}];
      ef = FileNameJoin[{dir, "TaskEdges.csv"}];
      jf = FileNameJoin[{dir, "TaskNodes.json"}];
      Export[nf, Dataset[nodes], "CSV"];
      Export[ef, Dataset[If[edges === {}, {<|"Source" -> "", "Target" -> ""|>}, edges]], "CSV"];
      Export[jf, nodes, "JSON"];
      <|"Nodes" -> nf, "Edges" -> ef, "JSON" -> jf|>
    ]
  ];
