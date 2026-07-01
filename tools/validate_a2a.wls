#!/usr/bin/env wolframscript
(* ::Package:: *)

(* tools/validate_a2a.wls
   --------------------------------------------------------------------------
   Validacion END-TO-END del modulo A2A (Agent2Agent) sobre la Ruliad.

   Ejecuta, dentro del kernel, exactamente el mismo flujo que la "celda"
   JSON-RPC de la pagina /a2a (curl -X POST /a2a  message/send), sin necesidad
   de levantar el servidor HTTP. Cubre:

     1. Carga del paclet.
     2. Agent Card (/.well-known/agent-card.json) + campos obligatorios A2A.
     3. JSON-RPC message/send  -> Task completada (Ruliad expandida).
     4. Estructura de la Task: artifacts (trayectoria + stacks + summary).
     5. Grafo de comunicacion: 40 nodos / 39 mensajes / 27 stacks.
     6. tasks/get (round-trip por id).
     7. tasks/cancel sobre task terminal  -> TaskNotCancelable.
     8. metodo desconocido -> MethodNotFound ; message/stream -> UnsupportedOperation.
     9. Suite de tests Tests/A2A.wlt (TestReport).

   Uso:
     wolframscript -f tools/validate_a2a.wls
     make a2a-validate

   Salida: reporte PASS/FAIL por chequeo y codigo de salida 0 (ok) / 1 (fallo).
   -------------------------------------------------------------------------- *)

(* ── Carga del paclet ──────────────────────────────────────────────────── *)
$pacletDir = FileNameJoin[{DirectoryName[$InputFileName], "..", "PersonalSite"}];
Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["PersonalSite`"];

(* ── Mini-framework de asserts ─────────────────────────────────────────── *)
$pass = 0; $fail = 0; $failures = {};

check[label_String, actual_, expected_] :=
  If[actual === expected,
    $pass++; Print["  \[Checkmark] ", label],
    $fail++; AppendTo[$failures, label];
    Print["  \[Times] ", label,
      "  (esperado: ", ToString[expected, InputForm],
      " | obtenido: ", ToString[actual, InputForm], ")"]];

checkTrue[label_String, cond_] := check[label, TrueQ[cond], True];

section[title_String] := (Print[""]; Print["== ", title, " =="]);

(* ── 1. Paclet cargado ─────────────────────────────────────────────────── *)
section["1. Carga del paclet"];
checkTrue["PersonalSite`A2A` disponible",
  StringQ[PersonalSite`A2A`$protocolVersion]];
checkTrue["PersonalSite`AgentMesh` disponible",
  ListQ[PersonalSite`AgentMesh`agents[]]];

(* ── 2. Agent Card ─────────────────────────────────────────────────────── *)
section["2. Agent Card (discovery)"];
card = PersonalSite`AgentMesh`agentCard["https://example.com"];
check["url",                 card["url"], "https://example.com/a2a"];
check["preferredTransport",  card["preferredTransport"], "JSONRPC"];
check["protocolVersion",     card["protocolVersion"], PersonalSite`A2A`$protocolVersion];
checkTrue["capabilities.streaming = False",  card["capabilities"]["streaming"] === False];
check["skills (3 reglas + 1 expand)", Length[card["skills"]], 4];
checkTrue["Agent Card serializa a JSON",
  StringQ[Quiet @ Developer`WriteRawJSONString[card]]];

(* ── 3. JSON-RPC message/send (la celda) ───────────────────────────────── *)
section["3. JSON-RPC message/send (celda /a2a)"];
$rpcJson = "{
  \"jsonrpc\": \"2.0\",
  \"id\": \"1\",
  \"method\": \"message/send\",
  \"params\": {
    \"message\": {
      \"role\": \"user\",
      \"parts\": [{ \"kind\": \"data\", \"data\": { \"seed\": 1, \"depth\": 3 } }],
      \"messageId\": \"msg-1\"
    }
  }
}";
$rpcBody = Quiet @ Check[ImportString[$rpcJson, "RawJSON"], $Failed];
checkTrue["body JSON-RPC parseado", AssociationQ[$rpcBody]];

resp = PersonalSite`A2A`dispatch[$rpcBody, PersonalSite`AgentMesh`handlers[]];
check["jsonrpc",       resp["jsonrpc"], "2.0"];
check["id preservado", resp["id"], "1"];
checkTrue["sin error",  ! KeyExistsQ[resp, "error"]];
task = Lookup[resp, "result", <||>];
check["result.kind",         Lookup[task, "kind", ""], "task"];
check["result.status.state", task["status"]["state"], "completed"];
checkTrue["result.id es UUID", StringQ[task["id"]] && StringLength[task["id"]] > 10];
checkTrue["respuesta serializa a JSON",
  StringQ[Quiet @ Developer`WriteRawJSONString[resp]]];

(* ── 4. Artifacts de la Task ───────────────────────────────────────────── *)
section["4. Artifacts (trayectoria + stacks + summary)"];
arts = Lookup[task, "artifacts", {}];
check["3 artifacts", Length[arts], 3];
artNames = Lookup[#, "name", ""] & /@ arts;
checkTrue["artifact ruliad-trajectory", MemberQ[artNames, "ruliad-trajectory"]];
checkTrue["artifact function-stacks",   MemberQ[artNames, "function-stacks"]];
checkTrue["artifact summary",           MemberQ[artNames, "summary"]];

(* ── 5. Grafo de comunicacion (via AgentMesh`run directo) ──────────────── *)
section["5. Grafo de comunicacion de la Ruliad"];
res = PersonalSite`AgentMesh`run[1, 3, "sync"];
graph = res["graph"];
check["40 nodos (1+3+9+27)", Length[graph["nodes"]], 40];
check["39 mensajes A2A",     Length[graph["edges"]], 39];
check["27 stacks (3^3)",     Length[graph["stacks"]], 27];
check["stats.messages",      res["stats"]["messages"], 39];
checkTrue["cada arista es a2a.message",
  AllTrue[graph["edges"], #["kind"] === "a2a.message" &]];
checkTrue["stack raiz->hoja de 4 pasos",
  Length[First[graph["stacks"]]["stack"]] === 4];
checkTrue["grafo serializa a JSON",
  StringQ[Quiet @ Developer`WriteRawJSONString[graph]]];

(* ── 6. tasks/get round-trip ───────────────────────────────────────────── *)
section["6. tasks/get (round-trip)"];
getResp = PersonalSite`A2A`dispatch[
  <|"jsonrpc" -> "2.0", "id" -> "2", "method" -> "tasks/get",
    "params" -> <|"id" -> task["id"]|>|>,
  PersonalSite`AgentMesh`handlers[]];
check["tasks/get devuelve la misma task",
  Lookup[getResp, "result", <||>]["id"], task["id"]];

(* ── 7. tasks/cancel sobre task terminal ───────────────────────────────── *)
section["7. tasks/cancel (task terminal)"];
cancelResp = PersonalSite`A2A`dispatch[
  <|"jsonrpc" -> "2.0", "id" -> "3", "method" -> "tasks/cancel",
    "params" -> <|"id" -> task["id"]|>|>,
  PersonalSite`AgentMesh`handlers[]];
check["TaskNotCancelable (-32002)",
  Lookup[cancelResp, "error", <||>]["code"], -32002];

(* ── 8. Errores JSON-RPC ───────────────────────────────────────────────── *)
section["8. Errores JSON-RPC"];
mnf = PersonalSite`A2A`dispatch[
  <|"jsonrpc" -> "2.0", "id" -> "4", "method" -> "does/notExist", "params" -> <||>|>,
  PersonalSite`AgentMesh`handlers[]];
check["MethodNotFound (-32601)", mnf["error"]["code"], -32601];
strm = PersonalSite`A2A`dispatch[
  <|"jsonrpc" -> "2.0", "id" -> "5", "method" -> "message/stream", "params" -> <||>|>,
  PersonalSite`AgentMesh`handlers[]];
check["UnsupportedOperation (-32004)", strm["error"]["code"], -32004];

(* ── 9. Suite Tests/A2A.wlt ─────────────────────────────────────────────── *)
section["9. Suite Tests/A2A.wlt"];
$wlt = FileNameJoin[{$pacletDir, "Tests", "A2A.wlt"}];
If[FileExistsQ[$wlt],
  Module[{report = Quiet @ TestReport[$wlt], nAll, nOk},
    nAll = report["TestsSucceededCount"] + report["TestsFailedCount"];
    nOk  = report["TestsSucceededCount"];
    check["TestReport: todos los tests OK", nOk, nAll];
    Print["  (", nOk, "/", nAll, " verificaciones)"]],
  Print["  \[Times] no se encontro ", $wlt]; $fail++];

(* ── Resumen ───────────────────────────────────────────────────────────── *)
Print[""];
Print["────────────────────────────────────────────────────────"];
Print["A2A end-to-end: ", $pass, " PASS, ", $fail, " FAIL"];
If[$fail > 0,
  Print["FALLOS: ", $failures];
  Print["RESULTADO: \[Times] FAIL"];
  Exit[1],
  Print["RESULTADO: \[Checkmark] TODO OK"];
  Exit[0]];
