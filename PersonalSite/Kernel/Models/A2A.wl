(* ::Package:: *)

(* PersonalSite`A2A`
   --------------------------------------------------------------------------
   Implementacion nativa del protocolo A2A (Agent2Agent) — https://a2a-protocol.org

   Este modulo cubre la *capa de protocolo*: los objetos de datos (Message,
   Part, Task, Artifact), la maquina de estados del ciclo de vida de una Task,
   un almacen de tareas (en memoria + persistencia best-effort en la DB) y el
   framing/dispatch JSON-RPC 2.0 sobre el que viaja A2A.

   La *capa de dominio* (que agentes existen y que hacen) vive en
   PersonalSite`AgentMesh`, que mapea la Ruliad generada por NestList a una
   malla de agentes A2A.

   Objetos (todos Associations JSON-serializables):

     Part      : <|"kind" -> "text"|"data"|"file", ...|>
     Message   : <|"role", "parts", "messageId", "kind" -> "message",
                   "taskId"?, "contextId"?|>
     Artifact  : <|"artifactId", "name", "parts", "metadata"?|>
     Task      : <|"id", "contextId", "kind" -> "task",
                   "status" -> <|"state", "timestamp", "message"?|>,
                   "artifacts", "history", "metadata"|>

   Ciclo de vida de una Task (A2A TaskState):
     submitted -> working -> {completed | failed | canceled | input-required}
     input-required / auth-required -> working -> ...
     Estados terminales: completed, canceled, failed, rejected.
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`A2A`"];

$protocolVersion::usage =
  "$protocolVersion es la version del protocolo A2A implementada.";

textPart::usage =
  "textPart[s] construye un TextPart A2A.";
dataPart::usage =
  "dataPart[assoc] construye un DataPart A2A (payload estructurado).";
filePart::usage =
  "filePart[name, mime, uri] construye un FilePart A2A por referencia.";

message::usage =
  "message[role, parts] o message[role, parts, opts] construye un Message A2A \
con messageId nuevo. opts puede incluir \"taskId\" y \"contextId\".";

artifact::usage =
  "artifact[name, parts] construye un Artifact A2A con artifactId nuevo.";

newTask::usage =
  "newTask[] o newTask[contextId] crea una Task (estado submitted), la almacena \
y la devuelve.";

getTask::usage =
  "getTask[taskId] devuelve la Task almacenada o $Failed si no existe.";

setState::usage =
  "setState[taskId, state] o setState[taskId, state, statusMessage] hace la \
transicion de estado (validada por la FSM). Devuelve la Task o $Failed.";

cancelTask::usage =
  "cancelTask[taskId] cancela una Task no-terminal. Devuelve la Task o $Failed.";

addArtifact::usage =
  "addArtifact[taskId, artifact] adjunta un Artifact a la Task.";

pushHistory::usage =
  "pushHistory[taskId, message] agrega un Message al historial de la Task.";

tasksList::usage =
  "tasksList[] devuelve la lista de Tasks almacenadas (mas recientes primero).";

clearTasks::usage =
  "clearTasks[] vacia el almacen de tareas en memoria.";

taskStates::usage =
  "taskStates[] devuelve la lista de estados A2A validos.";

terminalStateQ::usage =
  "terminalStateQ[state] indica si el estado es terminal.";

canTransition::usage =
  "canTransition[from, to] indica si la transicion de estado esta permitida.";

$errorCodes::usage =
  "$errorCodes mapea nombres de error A2A/JSON-RPC a sus codigos numericos.";

rpcSuccess::usage =
  "rpcSuccess[id, result] arma una respuesta JSON-RPC 2.0 exitosa.";
rpcError::usage =
  "rpcError[id, code, msg] o rpcError[id, code, msg, data] arma un error JSON-RPC 2.0.";

dispatch::usage =
  "dispatch[body, handlers] enruta un request JSON-RPC 2.0 (Association) al \
handler del metodo (handlers[method][params, id]) y captura fallos como \
InternalError. Devuelve la Association de respuesta JSON-RPC.";

Begin["`Private`"];

$protocolVersion = "0.3.0";

(* ── Estado del modulo ─────────────────────────────────────────────────── *)
$tasks = <||>;   (* taskId -> Task Association, orden de insercion preservado *)

(* ── Helpers de fecha (ISO-8601 UTC) ──────────────────────────────────── *)
isoNow[] :=
  DateString[TimeZoneConvert[Now, 0],
    {"Year", "-", "Month", "-", "Day", "T",
     "Hour24", ":", "Minute", ":", "Second", "Z"}];

uuid[] := CreateUUID[];

(* ── Parts ────────────────────────────────────────────────────────────── *)
textPart[s_] := <|"kind" -> "text", "text" -> ToString[s]|>;

dataPart[data_Association] := <|"kind" -> "data", "data" -> data|>;
dataPart[data_] := <|"kind" -> "data", "data" -> data|>;

filePart[name_String, mime_String, uri_String] :=
  <|"kind" -> "file",
    "file" -> <|"name" -> name, "mimeType" -> mime, "uri" -> uri|>|>;

(* ── Message ──────────────────────────────────────────────────────────── *)
message[role_String, parts_List] := message[role, parts, <||>];

message[role_String, parts_List, opts_Association] :=
  Join[
    <|"role" -> role,
      "parts" -> parts,
      "messageId" -> uuid[],
      "kind" -> "message"|>,
    KeySelect[opts, MemberQ[{"taskId", "contextId", "referenceTaskIds"}, #] &]
  ];

(* ── Artifact ─────────────────────────────────────────────────────────── *)
artifact[name_String, parts_List] :=
  <|"artifactId" -> uuid[], "name" -> name, "parts" -> parts|>;

artifact[name_String, parts_List, metadata_Association] :=
  <|"artifactId" -> uuid[], "name" -> name, "parts" -> parts,
    "metadata" -> metadata|>;

(* ── Maquina de estados del ciclo de vida ─────────────────────────────── *)
$transitions = <|
  "submitted"      -> {"working", "input-required", "auth-required",
                       "completed", "canceled", "failed", "rejected"},
  "working"        -> {"input-required", "auth-required", "completed",
                       "canceled", "failed"},
  "input-required" -> {"working", "completed", "canceled", "failed"},
  "auth-required"  -> {"working", "completed", "canceled", "failed"},
  "completed"      -> {},
  "canceled"       -> {},
  "failed"         -> {},
  "rejected"       -> {}
|>;

taskStates[] := Keys[$transitions];

terminalStateQ[state_String] :=
  KeyExistsQ[$transitions, state] && $transitions[state] === {};

canTransition[from_String, to_String] :=
  KeyExistsQ[$transitions, from] && MemberQ[$transitions[from], to];

(* ── Almacen de tareas (memoria + DB best-effort) ─────────────────────── *)
statusObj[state_String, msg_ : None] :=
  If[msg === None || msg === {} || msg === <||>,
    <|"state" -> state, "timestamp" -> isoNow[]|>,
    <|"state" -> state, "timestamp" -> isoNow[], "message" -> msg|>];

newTask[] := newTask[uuid[]];

newTask[contextId_String] :=
  Module[{id = uuid[], task},
    task = <|
      "id"        -> id,
      "contextId" -> contextId,
      "kind"      -> "task",
      "status"    -> statusObj["submitted"],
      "artifacts" -> {},
      "history"   -> {},
      "metadata"  -> <|"createdAt" -> isoNow[]|>
    |>;
    $tasks = Append[$tasks, id -> task];
    dbPersist[task];
    task
  ];

getTask[taskId_String] := Lookup[$tasks, taskId, $Failed];

setState[taskId_String, state_String] := setState[taskId, state, None];

setState[taskId_String, state_String, statusMessage_] :=
  Module[{task = getTask[taskId], from},
    If[task === $Failed, Return[$Failed]];
    from = task["status"]["state"];
    If[! canTransition[from, state],
      Return[$Failed]];
    task["status"] = statusObj[state, statusMessage];
    If[AssociationQ[statusMessage],
      task["history"] = Append[task["history"], statusMessage]];
    $tasks[taskId] = task;
    dbPersist[task];
    task
  ];

cancelTask[taskId_String] :=
  Module[{task = getTask[taskId], from},
    If[task === $Failed, Return[$Failed]];
    from = task["status"]["state"];
    If[terminalStateQ[from], Return[$Failed]];
    setState[taskId, "canceled"]
  ];

addArtifact[taskId_String, art_Association] :=
  Module[{task = getTask[taskId]},
    If[task === $Failed, Return[$Failed]];
    task["artifacts"] = Append[task["artifacts"], art];
    $tasks[taskId] = task;
    dbPersist[task];
    task
  ];

pushHistory[taskId_String, msg_Association] :=
  Module[{task = getTask[taskId]},
    If[task === $Failed, Return[$Failed]];
    task["history"] = Append[task["history"], msg];
    $tasks[taskId] = task;
    task
  ];

tasksList[] := Reverse[Values[$tasks]];

clearTasks[] := ($tasks = <||>;);

(* Persistencia best-effort: nunca debe romper un request. La tabla a2a_tasks
   vive en init.sql; si la DB no esta disponible (p.ej. Cloud sin JDBC), el
   Quiet@Check descarta el error y el almacen en memoria sigue siendo la
   fuente de verdad para esta sesion del kernel. *)
dbPersist[task_Association] :=
  Quiet @ Check[
    PersonalSite`Database`execute[
      "INSERT OR REPLACE INTO a2a_tasks \
(task_id, context_id, state, artifacts, history, metadata, updated_at) \
VALUES (?,?,?,?,?,?,datetime('now'))",
      {task["id"], task["contextId"], task["status"]["state"],
       Developer`WriteRawJSONString[task["artifacts"]],
       Developer`WriteRawJSONString[task["history"]],
       Developer`WriteRawJSONString[task["metadata"]]}],
    Null];

(* ── JSON-RPC 2.0 framing ─────────────────────────────────────────────── *)
$errorCodes = <|
  "ParseError"                   -> -32700,
  "InvalidRequest"               -> -32600,
  "MethodNotFound"               -> -32601,
  "InvalidParams"                -> -32602,
  "InternalError"                -> -32603,
  "TaskNotFound"                 -> -32001,
  "TaskNotCancelable"            -> -32002,
  "PushNotificationNotSupported" -> -32003,
  "UnsupportedOperation"         -> -32004,
  "ContentTypeNotSupported"      -> -32005,
  "InvalidAgentResponse"         -> -32006
|>;

rpcSuccess[id_, result_] :=
  <|"jsonrpc" -> "2.0", "id" -> id, "result" -> result|>;

rpcError[id_, code_Integer, msg_String] :=
  <|"jsonrpc" -> "2.0", "id" -> id,
    "error" -> <|"code" -> code, "message" -> msg|>|>;

rpcError[id_, code_Integer, msg_String, data_] :=
  <|"jsonrpc" -> "2.0", "id" -> id,
    "error" -> <|"code" -> code, "message" -> msg, "data" -> data|>|>;

(* Enruta un request JSON-RPC al handler del metodo. Los handlers reciben
   (params, id) y devuelven una respuesta JSON-RPC completa (via rpcSuccess /
   rpcError). Cualquier excepcion se degrada a InternalError. *)
dispatch[body_Association, handlers_Association] :=
  Module[{id, method, params},
    id     = Lookup[body, "id", Null];
    method = Lookup[body, "method", None];
    params = Lookup[body, "params", <||>];
    If[! StringQ[method],
      Return[rpcError[id, $errorCodes["InvalidRequest"],
        "Missing or invalid 'method'."]]];
    If[! KeyExistsQ[handlers, method],
      Return[rpcError[id, $errorCodes["MethodNotFound"],
        "Method not found: " <> method]]];
    Quiet @ Check[
      handlers[method][If[AssociationQ[params], params, <||>], id],
      rpcError[id, $errorCodes["InternalError"], "Internal error handling " <> method]]
  ];

End[];
EndPackage[];
