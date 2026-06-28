(* ::Package:: *)

(* PersonalSite`Controller`TaskController
   --------------------------------------------------------------------------
   HTTP API para gestionar las tareas de runtime via TaskManager.

   GET  /tasks              → Dashboard UI
   GET  /tasks/summary      → JSON snapshot de todas las tareas (polling)
   GET  /tasks/history/:id  → historial de ejecucion de una tarea
   POST /tasks/start/:id    → iniciar tarea
   POST /tasks/stop/:id     → detener tarea
   POST /tasks/restart/:id  → reiniciar tarea
   POST /tasks/configure    → reconfigurar (interval, enabled, label)
   POST /tasks/register     → registrar tarea nueva en caliente
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`Controller`",
  {"PersonalSite`TaskManager`"}];

tasks::usage         = "tasks[req] sirve el dashboard de TaskObjects.";
tasksSummary::usage  = "tasksSummary[req] devuelve JSON snapshot.";
tasksHistory::usage  = "tasksHistory[id, req] devuelve historial JSON.";
tasksStart::usage    = "tasksStart[id, req] inicia la tarea.";
tasksStop::usage     = "tasksStop[id, req] detiene la tarea.";
tasksRestart::usage  = "tasksRestart[id, req] reinicia la tarea.";
tasksConfigure::usage = "tasksConfigure[req] reconfigura una tarea.";
tasksRegister::usage   = "tasksRegister[req] registra una tarea nueva.";
tasksUnregister::usage = "tasksUnregister[id, req] elimina una tarea del registro.";
tasksDag::usage        = "tasksDag[req] devuelve el DAG de dependencias como JSON.";

Begin["`Private`"];

(* WolframWebEngine no expone JSON bodies (Body->None).
   Leer desde FormRules (application/x-www-form-urlencoded)
   o desde Query (query string), en ese orden. *)
parseBody[req_] :=
  Module[{fd = req["FormRules"], qp = req["Query"]},
    If[ListQ[fd] && Length[fd] > 0, Return[Association[fd]]];
    If[ListQ[qp] && Length[qp] > 0, Return[Association[qp]]];
    <||>];

jsonResp[data_, code_: 200] :=
  HTTPResponse[
    Quiet @ Check[
      Developer`WriteRawJSONString[data],
      Quiet @ Check[ExportString[data, "JSON"], "{\"ok\":false}"]],
    <|"StatusCode" -> code,
      "Headers" -> <|"Content-Type" -> "application/json"|>|>];

(* ── GET /tasks ────────────────────────────────────────────────────── *)
tasks[req_] :=
  Module[{snap = PersonalSite`TaskManager`summary[]},
    PersonalSite`View`render["tasks",
      <|"kernelID"   -> ToString[snap["kernel"]],
        "taskCount"  -> ToString[snap["taskCount"]],
        "running"    -> ToString[snap["running"]]
      |>]
  ];

(* ── GET /tasks/summary ────────────────────────────────────────────── *)
tasksSummary[req_] :=
  jsonResp[PersonalSite`TaskManager`summary[]];

(* ── GET /tasks/dag ────────────────────────────────────────────────── *)
tasksDag[req_] :=
  jsonResp[PersonalSite`TaskManager`dagData[]];

(* ── GET /tasks/history/:id ────────────────────────────────────────── *)
tasksHistory[id_String, req_] :=
  If[PersonalSite`TaskManager`info[id] === $Failed,
    jsonResp[<|"ok" -> False, "error" -> "task not found"|>, 404],
    jsonResp[PersonalSite`TaskManager`history[id]]];

(* ── POST /tasks/start/:id ─────────────────────────────────────────── *)
tasksStart[id_String, req_] :=
  Module[{res = Quiet @ Check[PersonalSite`TaskManager`start[id], $Failed]},
    jsonResp[<|"ok" -> (res =!= $Failed), "id" -> id|>]];

(* ── POST /tasks/stop/:id ──────────────────────────────────────────── *)
tasksStop[id_String, req_] :=
  (PersonalSite`TaskManager`stop[id];
   jsonResp[<|"ok" -> True, "id" -> id|>]);

(* ── POST /tasks/restart/:id ───────────────────────────────────────── *)
tasksRestart[id_String, req_] :=
  Module[{res = Quiet @ Check[PersonalSite`TaskManager`restart[id], $Failed]},
    jsonResp[<|"ok" -> (res =!= $Failed), "id" -> id|>]];

(* ── POST /tasks/configure ─────────────────────────────────────────── *)
tasksConfigure[req_] :=
  Module[{body = parseBody[req], id, key, value, res},
    id    = Lookup[body, "id", ""];
    key   = Lookup[body, "key", ""];
    value = Lookup[body, "value", Null];
    If[id === "" || key === "" || value === Null,
      Return[jsonResp[<|"ok" -> False, "error" -> "id, key and value required"|>, 400]]];
    (* Convierte el intervalo a numero si aplica *)
    If[key === "interval",
      value = Quiet @ Check[If[NumberQ[value], value, ToExpression[ToString[value]]], 60]];
    res = Quiet @ Check[
      PersonalSite`TaskManager`configure[id, key, value], $Failed];
    jsonResp[<|"ok" -> (res =!= $Failed), "id" -> id, "key" -> key|>]
  ];

(* ── POST /tasks/register ──────────────────────────────────────────── *)
(* Permite registrar tareas ad-hoc desde la UI (sin reiniciar el kernel).
   El "action" se especifica como codigo Wolfram en "actionCode" (String). *)
tasksRegister[req_] :=
  Module[{body = parseBody[req], id, spec, fn, res},
    id = Lookup[body, "id", ""];
    If[id === "",
      Return[jsonResp[<|"ok" -> False, "error" -> "id is required"|>, 400]]];
    (* Evalua el actionCode de forma segura en un contexto acotado *)
    fn = Quiet @ Check[
      ToExpression[Lookup[body, "actionCode", "Function[True]"]],
      Function[True]];
    spec = <|
      "label"    -> Lookup[body, "label", id],
      "group"    -> Lookup[body, "group", "user"],
      "interval" -> Quiet @ Check[
                     With[{iv = Lookup[body, "interval", "60"]},
                       If[IntegerQ[iv], iv, ToExpression[ToString[iv]]]],
                     60],
      "enabled"  -> ! MemberQ[{"false", "False", "0"}, Lookup[body, "enabled", "true"]],
      "action"   -> fn
    |>;
    res = Quiet @ Check[
      (PersonalSite`TaskManager`register[id, spec];
       If[TrueQ[spec["enabled"]],
         PersonalSite`TaskManager`start[id]]; id),
      $Failed];
    jsonResp[<|"ok" -> (res =!= $Failed), "id" -> id|>]
  ];

(* ── POST /tasks/unregister/:id ───────────────────────────────────── *)
tasksUnregister[id_String, req_] :=
  Module[{res = Quiet @ Check[PersonalSite`TaskManager`unregister[id], $Failed]},
    jsonResp[<|"ok" -> (res =!= $Failed), "id" -> id|>]];

End[];
EndPackage[];
