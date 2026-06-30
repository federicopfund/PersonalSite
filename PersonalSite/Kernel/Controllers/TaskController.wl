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
  {"PersonalSite`TaskManager`", "PersonalSite`TaskConfig`", "PersonalSite`DevOps`",
   "PersonalSite`Database`"}];

tasks::usage         = "tasks[req] sirve el dashboard de TaskObjects.";
dagDashboard::usage  = "dagDashboard[req] sirve el DAG Engineer Dashboard.";
tasksSummary::usage  = "tasksSummary[req] devuelve JSON snapshot.";
tasksHistory::usage  = "tasksHistory[id, req] devuelve historial JSON.";
tasksStart::usage    = "tasksStart[id, req] inicia la tarea.";
tasksStop::usage     = "tasksStop[id, req] detiene la tarea.";
tasksRestart::usage  = "tasksRestart[id, req] reinicia la tarea.";
tasksConfigure::usage  = "tasksConfigure[req] reconfigura una tarea.";
tasksRegister::usage   = "tasksRegister[req] registra una tarea nueva.";
tasksUnregister::usage = "tasksUnregister[id, req] elimina una tarea del registro.";
tasksDag::usage        = "tasksDag[req] devuelve el DAG de dependencias como JSON.";
tasksConfigList::usage   = "tasksConfigList[req] lista configs de la DB.";
tasksConfigCreate::usage = "tasksConfigCreate[req] crea un config en la DB.";
tasksConfigUpdate::usage = "tasksConfigUpdate[req] actualiza un config en la DB.";
tasksConfigDelete::usage = "tasksConfigDelete[id, req] elimina un config de la DB.";
tasksConfigApply::usage  = "tasksConfigApply[req] aplica todos los configs de DB al runtime.";
tasksConfigSeed::usage   = "tasksConfigSeed[req] siembra los defaults en la DB.";
tasksConfigById::usage   = "tasksConfigById[id, req] devuelve un config por task_id.";
uxContactState::usage    = "uxContactState[req] devuelve el estado UX del boton Contacto (JSON).";
devopsDag::usage         = "devopsDag[req] devuelve el DAG del pipeline DevOps (18 stages).";
devopsStatus::usage      = "devopsStatus[req] devuelve el estado del bridge y resultados recientes.";
devopsRunStage::usage    = "devopsRunStage[stage, req] ejecuta una etapa del pipeline DevOps.";
devopsPipelineRun::usage     = "devopsPipelineRun[req] ejecuta runPipeline() via Flow.run y persiste en Warehouse.";
devopsPipelineHistory::usage = "devopsPipelineHistory[req] retorna los ultimos 50 runs del Warehouse.";
devopsTrajectory::usage      = "devopsTrajectory[n, req] ejecuta NestList[runPipeline, state0, n] y guarda cada step.";
devopsTestsRun::usage        = "devopsTestsRun[req] ejecuta runTests[layer] — layer via query ?layer=... o FormRules.";
devopsTestsRunLayer::usage   = "devopsTestsRunLayer[layer, req] ejecuta runTests[layer] para la capa dada.";
kpiPage::usage               = "kpiPage[req] sirve la estación de trabajo KPI (health, tasks en runtime, test runner).";  

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
(* ── GET /dag ─────────────────────────────────────────────────── *)
dagDashboard[req_] :=
  Module[{snap = PersonalSite`TaskManager`summary[]},
    PersonalSite`View`render["dag",
      <|"kernelID"   -> ToString[snap["kernel"]],
        "taskCount"  -> ToString[snap["taskCount"]],
        "running"    -> ToString[snap["running"]]
      |>]
  ];
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

(* ── GET  /tasks/config         → lista configs DB ──────────────── *)
tasksConfigList[req_] :=
  Module[{rows = Quiet @ Check[PersonalSite`TaskConfig`all[], {}]},
    jsonResp[rows]];

(* ── GET  /tasks/config/:id     → config individual ─────────────── *)
tasksConfigById[id_String, req_] :=
  Module[{r = Quiet @ Check[PersonalSite`TaskConfig`byId[id], $Failed]},
    If[r === $Failed,
      jsonResp[<|"ok"->False,"error"->"not found"|>, 404],
      jsonResp[r]]];

(* ── POST /tasks/config/create  → crea config en DB ─────────────── *)
tasksConfigCreate[req_] :=
  Module[{body = parseBody[req], spec, res},
    spec = <|
      "task_id"     -> Lookup[body, "task_id",    ""],
      "label"       -> Lookup[body, "label",      ""],
      "group_name"  -> Lookup[body, "group_name",  "user"],
      "interval_s"  -> Quiet @ Check[
                         ToExpression[ToString @ Lookup[body, "interval_s", "60"]], 60],
      "enabled"     -> ! MemberQ[{"false","False","0"}, Lookup[body, "enabled", "true"]],
      "deps"        -> StringSplit[Lookup[body, "deps", ""], ","],
      "dag_order"   -> Quiet @ Check[
                         ToExpression[ToString @ Lookup[body, "dag_order", "0"]], 0],
      "action_code" -> Lookup[body, "action_code", "Function[True]"]
    |>;
    If[spec["task_id"] === "",
      Return[jsonResp[<|"ok"->False,"error"->"task_id required"|>, 400]]];
    res = Quiet @ Check[PersonalSite`TaskConfig`create[spec], $Failed];
    jsonResp[<|"ok"->(res =!= $Failed), "task_id"->spec["task_id"]|>]];

(* ── POST /tasks/config/update  → actualiza campo en DB ───────────── *)
tasksConfigUpdate[req_] :=
  Module[{body = parseBody[req], id, key, value, res},
    id    = Lookup[body, "task_id", ""];
    key   = Lookup[body, "key",     ""];
    value = Lookup[body, "value",   Null];
    If[id === "" || key === "" || value === Null,
      Return[jsonResp[<|"ok"->False,"error"->"task_id, key and value required"|>, 400]]];
    (* Conversiones de tipo *)
    value = Which[
      key === "interval_s",  Quiet @ Check[ToExpression[ToString[value]], 60],
      key === "dag_order",   Quiet @ Check[ToExpression[ToString[value]], 0],
      key === "enabled",     If[MemberQ[{"false","False","0"}, value], 0, 1],
      key === "deps",        StringSplit[value, ","],
      True,                  value];
    res = Quiet @ Check[PersonalSite`TaskConfig`update[id, key, value], $Failed];
    jsonResp[<|"ok"->(res =!= $Failed), "task_id"->id, "key"->key|>]];

(* ── POST /tasks/config/delete/:id → elimina config de DB ───────── *)
tasksConfigDelete[id_String, req_] :=
  Module[{res = Quiet @ Check[PersonalSite`TaskConfig`delete[id], $Failed]},
    jsonResp[<|"ok"->(res =!= $Failed), "task_id"->id|>]];

(* ── POST /tasks/config/apply   → aplica DB al runtime TaskManager ── *)
tasksConfigApply[req_] :=
  Module[{rows, applied = {}, failed = {}},
    rows = Quiet @ Check[PersonalSite`TaskConfig`all[], {}];
    Scan[Function[row,
      Module[{id = row["task_id"], spec, fn, res},
        fn  = Quiet @ Check[ToExpression[row["action_code"]], Function[True]];
        spec = <|
          "label"    -> row["label"],
          "group"    -> row["group_name"],
          "interval" -> row["interval_s"],
          "enabled"  -> row["enabled"],
          "deps"     -> row["deps"],
          "dag_order"-> row["dag_order"],
          "action"   -> fn
        |>;
        res = Quiet @ Check[
          (PersonalSite`TaskManager`register[id, spec];
           If[TrueQ[row["enabled"]],
             PersonalSite`TaskManager`start[id]]; id),
          $Failed];
        If[res === $Failed, AppendTo[failed, id], AppendTo[applied, id]]
      ]],
      rows];
    jsonResp[<|"ok"->True, "applied"->applied, "failed"->failed,
               "total"->Length[rows]|>]];

(* ── POST /tasks/config/seed    → siembra defaults en DB ───────── *)
tasksConfigSeed[req_] :=
  Module[{res = Quiet @ Check[PersonalSite`TaskConfig`seedDefaults[], $Failed]},
    jsonResp[<|"ok"->(res =!= $Failed), "result"->ToString[res]|>]];

(* ── GET /ux/contact  → estado UX del anillo del boton Contacto ─── *)
(* Retorna {active: bool, runs: n, lastMs: n}.
   `active` refleja si la tarea `contact-ux` esta en su ventana activa
   (Mod[Floor[UnixTime[]/20], 2] === 1). El JS del cliente agrega la
   clase .is-running al boton para disparar la animacion CSS. *)
uxContactState[req_] :=
  Module[{active, taskInfo},
    active   = PersonalSite`Settings`get["ux.contact.active", "0"];
    taskInfo = Quiet @ Check[PersonalSite`TaskManager`info["contact-ux"], <||>];
    jsonResp[<|
      "active" -> (active === "1"),
      "runs"   -> Lookup[taskInfo, "runs",   0],
      "lastMs" -> Lookup[taskInfo, "lastMs", 0.]
    |>]
  ];

(* ══ DevOps Pipeline API ══════════════════════════════════════════════
   GET  /devops/dag         → estructura del pipeline (18 stages)
   GET  /devops/status      → bridge health + resultados recientes
   POST /devops/run/:stage  → ejecuta una etapa puntual
   ══════════════════════════════════════════════════════════════════ *)

(* ── GET /devops/dag ─────────────────────────────────────────────── *)
devopsDag[req_] :=
  jsonResp[PersonalSite`DevOps`dag[]];

(* ── GET /devops/status ─────────────────────────────────────────── *)
devopsStatus[req_] :=
  Module[{bh, results},
    bh      = Quiet @ Check[PersonalSite`DevOps`bridgeHealth[], <|"ok"->False|>];
    results = Quiet @ Check[PersonalSite`DevOps`stageResults[], <||>];
    jsonResp[<|
      "bridge_ok"   -> TrueQ[bh["ok"]],
      "bridge_ms"   -> Lookup[bh, "ms", 0],
      "bridge_host" -> Lookup[bh, "host", "172.18.0.1:8091"],
      "bridge_err"  -> If[TrueQ[bh["ok"]], "", Lookup[bh, "err", ""]],
      "stageResults"-> results,
      "ts"          -> DateString["ISODateTime"]
    |>]];

(* ── POST /devops/run/:stage ─────────────────────────────────────── *)
devopsRunStage[stage_String, req_] :=
  Module[{result = Quiet @ Check[
    PersonalSite`DevOps`runStage[stage],
    <|"ok"->False, "stage"->stage, "err"->"exception",
      "ms"->0, "ts"->DateString["ISODateTime"]|>]},
    jsonResp[result]];

(* ── POST /devops/pipeline/run ────────────────────────────────── *)
(*  Ejecuta runPipeline() via Flow.run (paralelo topologico),        *)
(*  persiste en Warehouse y retorna el estado final.                 *)
devopsPipelineRun[req_] :=
  Module[{res, saved},
    res   = Quiet @ Check[
      TimeConstrained[
        PersonalSite`DevOps`runPipeline[],
        90,
        <|"ok"->False, "err"->"pipeline timeout (90s)",
          "ts"->DateString["ISODateTime"]|>],
      <|"ok"->False, "err"->"runPipeline exception",
        "ts"->DateString["ISODateTime"]|>];
    saved = Quiet @ Check[
      PersonalSite`DevOps`saveRun[1,
        <|"ok"  -> TrueQ[res["ok"]],
          "sha" -> Lookup[res, "sha", ""],
          "ts"  -> Lookup[res, "ts", DateString["ISODateTime"]],
          "runLog" -> {<|"step"->1, "ok"->TrueQ[res["ok"]],
                         "ms"->Lookup[res,"elapsedMs",0],
                         "ts"->Lookup[res,"ts",""]|>}|>],
      $Failed];
    jsonResp[<|res, "warehouse" -> (saved =!= $Failed)|>]];

(* ── GET /devops/pipeline/history ───────────────────────────── *)
(*  Retorna los ultimos 50 runs del Warehouse (SQLite pipeline_runs) *)
devopsPipelineHistory[req_] :=
  Module[{rows = Quiet @ Check[
    PersonalSite`DevOps`pipelineHistory[50], {}]},
    jsonResp[<|"runs" -> rows, "count" -> Length[rows],
               "ts"  -> DateString["ISODateTime"]|>]];

(* ── POST /devops/trajectory/:n ─────────────────────────────── *)
(*  Ejecuta NestList[runPipeline, state0, n] y guarda cada step.     *)
devopsTrajectory[nStr_String, req_] :=
  Module[{n, traj},
    n = Quiet @ ToExpression[nStr];
    If[!IntegerQ[n] || n < 1 || n > 5,
      Return[jsonResp[<|"ok"->False,
        "err"->"n must be integer 1-5"|>]]];
    traj = Quiet @ Check[
      PersonalSite`DevOps`trajectory[n],
      {}];
    (* Persiste cada step (excepto el seed state0) *)
    Scan[Function[state,
      PersonalSite`DevOps`saveRun[state["step"], state]],
      Rest[traj]];
    jsonResp[<|"ok"    -> True,
              "steps" -> Length[traj],
              "trajectory" -> Map[KeyDrop[#, {"stageResults"}]&, traj],
              "ts"   -> DateString["ISODateTime"]|>]];

(* ── GET /kpi ───────────────────────────────────────────────────────── *)
(*  Estación de trabajo KPI: health %, tareas en runtime, test runner.  *)
kpiPage[req_] :=
  Module[{snap},
    snap = Quiet @ Check[PersonalSite`TaskManager`summary[], <||>];
    PersonalSite`View`render["kpi",
      <|"kernelID"  -> ToString[Lookup[snap, "kernel", "?"]],
        "taskCount" -> ToString[Lookup[snap, "taskCount", 0]],
        "running"   -> ToString[Lookup[snap, "running",   0]]|>]];

(* ── POST /devops/tests/run  ────────────────────────────────────── *)
(*  Acepta capa como:                                                      *)
(*    - query string  ?layer=session                                        *)
(*    - form body     layer=session                                         *)
(*    - sin parametro → "all"                                               *)
devopsTestsRun[req_] :=
  Module[{qmap, layer},
    qmap  = If[ListQ[req["Query"]], Association[req["Query"]], <||>];
    layer = StringTrim @ Lookup[
              Join[qmap, If[AssociationQ[req["FormRules"]], req["FormRules"], <||>]],
              "layer", "all"];
    devopsTestsRunLayer[layer, req]];

(* ── POST /devops/tests/run/:layer ────────────────────────────── *)
devopsTestsRunLayer[layer_String, req_] :=
  Module[{t0, result, ms},
    t0     = AbsoluteTime[];
    result = Quiet @ Check[
      TimeConstrained[
        PersonalSite`DevOps`runTests[layer],
        300,
        <|"ok"->False, "layer"->layer, "err"->"test timeout (300s)",
          "ts"->DateString["ISODateTime"]|>],
      <|"ok"->False, "layer"->layer, "err"->"exception in runTests",
        "ts"->DateString["ISODateTime"]|>];
    ms = Round[(AbsoluteTime[] - t0) * 1000, 1];
    jsonResp[<|result, "elapsedMs" -> ms|>]];

End[];
EndPackage[];
