(* ::Package:: *)

(* PersonalSite`Controller` (parte: kernel)
   --------------------------------------------------------------------------
   Wolfram Language REPL web — evaluacion nativa del kernel en tiempo real.

   Rutas:
     GET  /kernel            →  pagina HTML del notebook REPL
     POST /kernel/eval       →  evalua expresion WL, devuelve JSON con
                                OutputForm + TeXForm + tipo
     GET  /kernel/cells      →  historial de celdas evaluadas
     POST /kernel/schedule   →  registra re-evaluacion periodica via
                                ScheduledTask (TaskManager)
     POST /kernel/history    →  historial de una celda programada
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`Controller`"];

kernelPage::usage     = "kernelPage[req] renderiza /kernel: REPL nativo de WL.";
kernelEval::usage     = "kernelEval[req] evalua expresion WL, devuelve JSON.";
kernelCells::usage    = "kernelCells[req] devuelve historial de evaluaciones.";
kernelSchedule::usage = "kernelSchedule[req] registra re-evaluacion periodica.";
kernelHistory::usage  = "kernelHistory[req] devuelve historial de celda programada.";

Begin["`Private`"];

(* ── Almacenamiento en memoria del kernel ─────────────────────── *)
$cells        = {};   (* ring buffer: celdas evaluadas            *)
$cellN        = 0;    (* contador monotono                        *)
$maxCells     = 80;
$schedHistory = <||>; (* taskId -> lista de entradas {ts,ms,out} *)
$maxHistory   = 40;

(* ── Formatear salida WL a estructura JSON-serializable ─────── *)
wlFmt[expr_] :=
  Module[{outStr, texStr, htmlStr, typ},
    outStr  = Quiet @ Check[ToString[expr, OutputForm], "?"];
    texStr  = Quiet @ Check[ToString[expr, TeXForm],    ""];
    htmlStr = Quiet @ Check[PersonalSite`FrontEnd`Output`toHtml[expr], ""];
    typ = Which[
      expr === $Failed,                                           "error",
      Head[expr] === String,                                      "string",
      AssociationQ[expr] || (ListQ[expr] && Length[expr] <= 20), "list",
      NumericQ[expr],                                             "number",
      True,                                                       "output"
    ];
    <|"type"    -> typ,
      "out"     -> StringTake[outStr, Min[800, StringLength[outStr]]],
      "tex"     -> texStr,
      "full"    -> outStr,
      "html"    -> htmlStr
    |>];

(* ── POST /kernel/eval ────────────────────────────────────────── *)
kernelEval[req_] :=
  Module[{input, result, t0, ms, fout, cell},
    input = StringTrim @ Lookup[req["FormRules"], "input", ""];
    If[input === "",
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"error" -> "empty input"|>],
        <|"StatusCode" -> 400, "Content-Type" -> "application/json"|>]];

    t0     = AbsoluteTime[];
    result = Quiet @ Check[
      ToExpression[input, InputForm],
      $Failed];
    ms     = Round[1000. * (AbsoluteTime[] - t0), 0.01];

    $cellN++;
    fout = wlFmt[result];

    cell = <|
      "id"    -> $cellN,
      "input" -> input,
      "ms"    -> ms,
      "ts"    -> UnixTime[],
      "tsStr" -> DateString[Now, {"Hour24",":", "Minute",":", "Second"}],
      "out"   -> fout
    |>;

    $cells = Take[
      Append[$cells, cell],
      -Min[$maxCells, Length[$cells] + 1]];

    HTTPResponse[
      Developer`WriteRawJSONString[cell],
      <|"Content-Type" -> "application/json"|>]
  ];

(* ── GET /kernel/cells ───────────────────────────────────────── *)
kernelCells[req_] :=
  HTTPResponse[
    Developer`WriteRawJSONString[<|
      "cells"   -> Reverse[$cells],
      "total"   -> Length[$cells],
      "cellN"   -> $cellN,
      "kernel"  -> $KernelID,
      "ts"      -> UnixTime[]
    |>],
    <|"Content-Type" -> "application/json"|>];

(* ── POST /kernel/schedule ───────────────────────────────────── *)
kernelSchedule[req_] :=
  Module[{input, iv, taskId, spec, action},
    input = StringTrim @ Lookup[req["FormRules"], "input",    ""];
    iv    = Quiet @ Check[
              ToExpression @ Lookup[req["FormRules"], "interval", "30"],
              30];
    If[input === "" || !IntegerQ[iv] || iv < 5,
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"error" -> "invalid params"|>],
        <|"StatusCode" -> 400, "Content-Type" -> "application/json"|>]];

    $cellN++;
    taskId = "k-" <> ToString[$cellN] <> "-" <> ToString[UnixTime[]];
    $schedHistory[taskId] = {};

    (* Capturar input y taskId en el closure con With *)
    action = With[{inp = input, tid = taskId},
      Function[
        Module[{t0 = AbsoluteTime[], r, ms, entry},
          r  = Quiet @ Check[ToExpression[StringTrim[inp], InputForm], $Failed];
          ms = Round[1000. * (AbsoluteTime[] - t0), 0.01];
          entry = <|"ts" -> UnixTime[], "ms" -> ms, "out" -> wlFmt[r]|>;
          $schedHistory[tid] = Take[
            Append[$schedHistory[tid], entry],
            -Min[$maxHistory, Length[$schedHistory[tid]] + 1]];
          r]]];

    spec = <|
      "label"    -> "Kernel: " <> StringTake[input, Min[50, StringLength[input]]],
      "group"    -> "kernel",
      "interval" -> iv,
      "enabled"  -> True,
      "action"   -> action
    |>;

    PersonalSite`TaskManager`register[taskId, spec];
    PersonalSite`TaskManager`start[taskId];

    HTTPResponse[
      Developer`WriteRawJSONString[<|
        "taskId"   -> taskId,
        "input"    -> input,
        "interval" -> iv,
        "label"    -> spec["label"]
      |>],
      <|"Content-Type" -> "application/json"|>]
  ];

(* ── POST /kernel/history ────────────────────────────────────── *)
(* FormRules: "id" -> taskId *)
kernelHistory[req_] :=
  Module[{taskId, hist},
    taskId = Lookup[req["FormRules"], "id", ""];
    hist   = Lookup[$schedHistory, taskId, {}];
    HTTPResponse[
      Developer`WriteRawJSONString[<|
        "taskId"  -> taskId,
        "history" -> hist,
        "total"   -> Length[hist]
      |>],
      <|"Content-Type" -> "application/json"|>]
  ];

(* ── GET /kernel ────────────────────────────────────────────────── *)
kernelPage[req_] :=
  PersonalSite`View`render["kernel", <||>];

End[];
EndPackage[];
