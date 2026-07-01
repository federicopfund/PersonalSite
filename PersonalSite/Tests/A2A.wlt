(* PersonalSite/Tests/A2A.wlt
   ─────────────────────────────────────────────────────────────────────────
   Tests para el protocolo A2A (PersonalSite`A2A`) y la malla de agentes de la
   Ruliad (PersonalSite`AgentMesh`).
   Cubre: objetos de protocolo, ciclo de vida de Task (FSM), framing JSON-RPC,
   Agent Card, expansion de la Ruliad y handlers A2A.

   Ejecutar:
       TestReport["Tests/A2A.wlt"]
   ─────────────────────────────────────────────────────────────────────────*)

PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 1: Objetos de protocolo A2A
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["protocol-objects"]

VerificationTest[
  StringQ[PersonalSite`A2A`$protocolVersion],
  True,
  TestID -> "A2A::Protocol::VersionIsString"
];

VerificationTest[
  PersonalSite`A2A`textPart["hola"],
  <|"kind" -> "text", "text" -> "hola"|>,
  TestID -> "A2A::Protocol::TextPart"
];

VerificationTest[
  PersonalSite`A2A`dataPart[<|"seed" -> 1|>],
  <|"kind" -> "data", "data" -> <|"seed" -> 1|>|>,
  TestID -> "A2A::Protocol::DataPart"
];

With[{m = PersonalSite`A2A`message["user", {PersonalSite`A2A`textPart["hi"]}]},
  VerificationTest[
    m["role"] === "user" && m["kind"] === "message" && StringQ[m["messageId"]],
    True,
    TestID -> "A2A::Protocol::MessageShape"
  ];
];

With[{a = PersonalSite`A2A`artifact["out", {PersonalSite`A2A`textPart["r"]}]},
  VerificationTest[
    StringQ[a["artifactId"]] && a["name"] === "out" && ListQ[a["parts"]],
    True,
    TestID -> "A2A::Protocol::ArtifactShape"
  ];
];

EndTestSection[]

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 2: Ciclo de vida de Task (FSM)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["task-fsm"]

VerificationTest[
  PersonalSite`A2A`canTransition["submitted", "working"],
  True,
  TestID -> "A2A::FSM::SubmittedToWorking"
];

VerificationTest[
  PersonalSite`A2A`canTransition["completed", "working"],
  False,
  TestID -> "A2A::FSM::CompletedIsTerminal"
];

VerificationTest[
  PersonalSite`A2A`terminalStateQ["completed"] &&
    PersonalSite`A2A`terminalStateQ["canceled"] &&
    PersonalSite`A2A`terminalStateQ["failed"],
  True,
  TestID -> "A2A::FSM::TerminalStates"
];

With[{t = PersonalSite`A2A`newTask[]},
  VerificationTest[
    t["status"]["state"],
    "submitted",
    TestID -> "A2A::FSM::NewTaskSubmitted"
  ];

  VerificationTest[
    PersonalSite`A2A`setState[t["id"], "working"]["status"]["state"],
    "working",
    TestID -> "A2A::FSM::TransitionToWorking"
  ];

  VerificationTest[
    (* transicion invalida: working -> submitted no existe *)
    PersonalSite`A2A`setState[t["id"], "submitted"],
    $Failed,
    TestID -> "A2A::FSM::InvalidTransitionFails"
  ];

  VerificationTest[
    PersonalSite`A2A`setState[t["id"], "completed"]["status"]["state"],
    "completed",
    TestID -> "A2A::FSM::TransitionToCompleted"
  ];

  VerificationTest[
    (* no se puede cancelar una task terminal *)
    PersonalSite`A2A`cancelTask[t["id"]],
    $Failed,
    TestID -> "A2A::FSM::CancelTerminalFails"
  ];
];

VerificationTest[
  PersonalSite`A2A`getTask["no-existe-uuid"],
  $Failed,
  TestID -> "A2A::FSM::GetMissingTask"
];

EndTestSection[]

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 3: JSON-RPC 2.0 framing y dispatch
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["jsonrpc"]

VerificationTest[
  PersonalSite`A2A`rpcSuccess["1", <|"ok" -> True|>],
  <|"jsonrpc" -> "2.0", "id" -> "1", "result" -> <|"ok" -> True|>|>,
  TestID -> "A2A::RPC::SuccessShape"
];

With[{e = PersonalSite`A2A`rpcError["1", -32601, "Method not found"]},
  VerificationTest[
    e["error"]["code"] === -32601 && e["jsonrpc"] === "2.0",
    True,
    TestID -> "A2A::RPC::ErrorShape"
  ];
];

VerificationTest[
  (* metodo desconocido -> MethodNotFound *)
  PersonalSite`A2A`dispatch[
    <|"jsonrpc" -> "2.0", "id" -> "9", "method" -> "does/notExist", "params" -> <||>|>,
    PersonalSite`AgentMesh`handlers[]]["error"]["code"],
  -32601,
  TestID -> "A2A::RPC::MethodNotFound"
];

VerificationTest[
  (* streaming no soportado -> UnsupportedOperation *)
  PersonalSite`A2A`dispatch[
    <|"jsonrpc" -> "2.0", "id" -> "9", "method" -> "message/stream", "params" -> <||>|>,
    PersonalSite`AgentMesh`handlers[]]["error"]["code"],
  -32004,
  TestID -> "A2A::RPC::StreamUnsupported"
];

VerificationTest[
  (* tasks/get sin id -> InvalidParams *)
  PersonalSite`A2A`dispatch[
    <|"jsonrpc" -> "2.0", "id" -> "9", "method" -> "tasks/get", "params" -> <||>|>,
    PersonalSite`AgentMesh`handlers[]]["error"]["code"],
  -32602,
  TestID -> "A2A::RPC::TasksGetInvalidParams"
];

EndTestSection[]

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 4: Agent Card y agentes de la malla
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["agent-mesh"]

VerificationTest[
  (* orquestador + 3 agentes-regla *)
  Length[PersonalSite`AgentMesh`agents[]],
  4,
  TestID -> "AgentMesh::AgentCount"
];

VerificationTest[
  First[PersonalSite`AgentMesh`agents[]]["kind"],
  "orchestrator",
  TestID -> "AgentMesh::OrchestratorFirst"
];

With[{card = PersonalSite`AgentMesh`agentCard["https://example.com"]},
  VerificationTest[
    card["url"],
    "https://example.com/a2a",
    TestID -> "AgentMesh::CardUrl"
  ];

  VerificationTest[
    card["preferredTransport"],
    "JSONRPC",
    TestID -> "AgentMesh::CardTransport"
  ];

  VerificationTest[
    (* 3 skills de regla + 1 skill de expansion *)
    Length[card["skills"]],
    4,
    TestID -> "AgentMesh::CardSkillCount"
  ];

  VerificationTest[
    (* la Agent Card debe serializar a JSON sin funciones crudas *)
    StringQ[Developer`WriteRawJSONString[card]],
    True,
    TestID -> "AgentMesh::CardJsonSerializable"
  ];
];

EndTestSection[]

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 5: Expansion de la Ruliad (run) y stacks de funciones
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["ruliad-run"]

With[{res = PersonalSite`AgentMesh`run[1, 3, "sync"]},

  VerificationTest[
    AssociationQ[res] && KeyExistsQ[res, "task"] && KeyExistsQ[res, "graph"],
    True,
    TestID -> "AgentMesh::Run::Shape"
  ];

  VerificationTest[
    res["task"]["status"]["state"],
    "completed",
    TestID -> "AgentMesh::Run::TaskCompleted"
  ];

  VerificationTest[
    (* 40 nodos: 1 + 3 + 9 + 27 *)
    Length[res["graph"]["nodes"]],
    40,
    TestID -> "AgentMesh::Run::NodeCount40"
  ];

  VerificationTest[
    (* 39 aristas = mensajes A2A *)
    Length[res["graph"]["edges"]],
    39,
    TestID -> "AgentMesh::Run::EdgeCount39"
  ];

  VerificationTest[
    (* 27 hojas = 27 stacks de funciones (3^3) *)
    Length[res["graph"]["stacks"]],
    27,
    TestID -> "AgentMesh::Run::LeafStacks27"
  ];

  VerificationTest[
    (* cada stack tiene 4 pasos: seed + 3 reglas *)
    Length[First[res["graph"]["stacks"]]["stack"]],
    4,
    TestID -> "AgentMesh::Run::StackDepth"
  ];

  VerificationTest[
    (* el resultado completo debe serializar a JSON *)
    StringQ[Developer`WriteRawJSONString[res["graph"]]],
    True,
    TestID -> "AgentMesh::Run::GraphJsonSerializable"
  ];

  VerificationTest[
    (* la Task tiene 3 artifacts: trayectoria, stacks, resumen *)
    Length[res["task"]["artifacts"]],
    3,
    TestID -> "AgentMesh::Run::ArtifactCount"
  ];
];

(* Handler message/send end-to-end via JSON-RPC *)
With[{resp = PersonalSite`A2A`dispatch[
    <|"jsonrpc" -> "2.0", "id" -> "42", "method" -> "message/send",
      "params" -> <|"message" -> <|"role" -> "user",
        "parts" -> {<|"kind" -> "data", "data" -> <|"seed" -> 1, "depth" -> 2, "backend" -> "sync"|>|>},
        "messageId" -> "m1"|>|>|>,
    PersonalSite`AgentMesh`handlers[]]},

  VerificationTest[
    resp["result"]["kind"],
    "task",
    TestID -> "AgentMesh::MessageSend::ReturnsTask"
  ];

  VerificationTest[
    (* seguimos pudiendo recuperar la task por id (tasks/get) *)
    PersonalSite`A2A`dispatch[
      <|"jsonrpc" -> "2.0", "id" -> "43", "method" -> "tasks/get",
        "params" -> <|"id" -> resp["result"]["id"]|>|>,
      PersonalSite`AgentMesh`handlers[]]["result"]["id"],
    resp["result"]["id"],
    TestID -> "AgentMesh::TasksGet::RoundTrip"
  ];
];

EndTestSection[]
