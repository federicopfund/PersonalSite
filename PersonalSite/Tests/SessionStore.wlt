(* PersonalSite/Tests/SessionStore.wlt
   ─────────────────────────────────────────────────────────────────────────
   Tests de integracion para PersonalSite`SessionStore`.
   Requieren la base de datos SQLite inicializada (tabla sessions).

   Ejecutar:
       TestReport["Tests/SessionStore.wlt"]
   ─────────────────────────────────────────────────────────────────────────*)

PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* ── Setup global: asegurar que la DB este disponible ────────────────── *)
PersonalSite`Database`setup[];

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 1: createSession — generacion de token y persistencia
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["createSession"]

(* Creamos una sesion de referencia para este bloque *)
Module[{result},
  result = PersonalSite`SessionStore`createSession["test-user", 1, <|"source"->"wlt"|>];

  VerificationTest[
    AssociationQ[result],
    True,
    TestID -> "SessionStore::Create::ReturnsAssoc"
  ];

  VerificationTest[
    Keys[result],
    {"sessionId", "token", "expiresAt"},
    SameTest -> (Sort[#1] === Sort[#2] &),
    TestID -> "SessionStore::Create::KeysPresent"
  ];

  VerificationTest[
    (* sessionId es UUID v4: 36 chars, 4 guiones *)
    StringLength[result["sessionId"]] === 36 &&
    StringCount[result["sessionId"], "-"] === 4,
    True,
    TestID -> "SessionStore::Create::SessionIdIsUUID"
  ];

  VerificationTest[
    (* token tiene exactamente 3 partes separadas por "." *)
    Length @ StringSplit[result["token"], ".", 3],
    3,
    TestID -> "SessionStore::Create::TokenHas3Parts"
  ];

  VerificationTest[
    (* la parte del MAC mide 64 hex chars — SHA-256 *)
    StringLength @ Last[StringSplit[result["token"], ".", 3]],
    64,
    TestID -> "SessionStore::Create::MacIs64Hex"
  ];

  VerificationTest[
    StringQ[result["expiresAt"]],
    True,
    TestID -> "SessionStore::Create::ExpiresAtIsString"
  ];

  (* Cleanup *)
  PersonalSite`SessionStore`destroySession[result["sessionId"]]
];

(* Roles 1, 2, 3 — verificar permisos en sesion guardada *)
Module[{r1, r2, r3},
  r1 = PersonalSite`SessionStore`createSession["u1", 1];
  r2 = PersonalSite`SessionStore`createSession["u2", 2];
  r3 = PersonalSite`SessionStore`createSession["u3", 3];

  VerificationTest[
    Length @ (PersonalSite`SessionStore`getSession[r1["sessionId"]]["permissions"]),
    3,
    TestID -> "SessionStore::Create::Role1Perms3"
  ];

  VerificationTest[
    Length @ (PersonalSite`SessionStore`getSession[r2["sessionId"]]["permissions"]),
    9,
    TestID -> "SessionStore::Create::Role2Perms9"
  ];

  VerificationTest[
    Length @ (PersonalSite`SessionStore`getSession[r3["sessionId"]]["permissions"]),
    16,
    TestID -> "SessionStore::Create::Role3Perms16"
  ];

  (* Cleanup *)
  PersonalSite`SessionStore`destroySession[r1["sessionId"]];
  PersonalSite`SessionStore`destroySession[r2["sessionId"]];
  PersonalSite`SessionStore`destroySession[r3["sessionId"]]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 2: getSession — Cache + DB
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["getSession"]

Module[{result, sid, session},
  result  = PersonalSite`SessionStore`createSession["cache-test", 2];
  sid     = result["sessionId"];

  (* Primer acceso: desde Cache (acaba de ser creada) *)
  session = PersonalSite`SessionStore`getSession[sid];
  VerificationTest[
    AssociationQ[session],
    True,
    TestID -> "SessionStore::Get::ReturnsAssoc"
  ];

  VerificationTest[
    session["userId"],
    "cache-test",
    TestID -> "SessionStore::Get::UserIdCorrect"
  ];

  VerificationTest[
    session["role"],
    2,
    TestID -> "SessionStore::Get::RoleCorrect"
  ];

  VerificationTest[
    session["state"],
    "active",
    TestID -> "SessionStore::Get::StateActive"
  ];

  VerificationTest[
    session["sessionId"],
    sid,
    TestID -> "SessionStore::Get::SessionIdMatches"
  ];

  (* Session inexistente devuelve $Failed *)
  VerificationTest[
    PersonalSite`SessionStore`getSession["00000000-0000-0000-0000-000000000000"],
    $Failed,
    TestID -> "SessionStore::Get::MissingReturnsFailure"
  ];

  PersonalSite`SessionStore`destroySession[sid]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 3: validateToken — HMAC y estado
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["validateToken"]

Module[{result, token, sid, session},
  result  = PersonalSite`SessionStore`createSession["hmac-test", 3];
  token   = result["token"];
  sid     = result["sessionId"];

  session = PersonalSite`SessionStore`validateToken[token];

  VerificationTest[
    AssociationQ[session],
    True,
    TestID -> "SessionStore::Validate::ValidTokenReturnsSession"
  ];

  VerificationTest[
    session["state"],
    "active",
    TestID -> "SessionStore::Validate::StateActive"
  ];

  VerificationTest[
    session["userId"],
    "hmac-test",
    TestID -> "SessionStore::Validate::UserIdCorrect"
  ];

  (* Token con MAC corrupto (ultimo char alterado) *)
  VerificationTest[
    PersonalSite`SessionStore`validateToken[
      StringDrop[token, -1] <>
      If[StringTake[token, -1] === "0", "1", "0"]],
    $Failed,
    TestID -> "SessionStore::Validate::CorruptMacFails"
  ];

  (* Token malformado (sin separadores) *)
  VerificationTest[
    PersonalSite`SessionStore`validateToken["not-a-valid-token"],
    $Failed,
    TestID -> "SessionStore::Validate::MalformedFails"
  ];

  (* Token vacio *)
  VerificationTest[
    PersonalSite`SessionStore`validateToken[""],
    $Failed,
    TestID -> "SessionStore::Validate::EmptyFails"
  ];

  (* Session suspendida no valida por validateToken (requiere active/elevated) *)
  PersonalSite`SessionStore`applyTransition[sid, "suspend"];
  VerificationTest[
    PersonalSite`SessionStore`validateToken[token],
    $Failed,
    TestID -> "SessionStore::Validate::SuspendedFails"
  ];

  (* Pero verifyTokenIdentity SI acepta suspended *)
  VerificationTest[
    AssociationQ @ PersonalSite`SessionStore`verifyTokenIdentity[token],
    True,
    TestID -> "SessionStore::Validate::VerifyIdentitySuspendedOk"
  ];

  PersonalSite`SessionStore`destroySession[sid]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 4: applyTransition — FSM via Store
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["applyTransition"]

Module[{result, sid},
  result = PersonalSite`SessionStore`createSession["fsm-test", 3];
  sid    = result["sessionId"];

  (* active -> elevated *)
  VerificationTest[
    Last @ PersonalSite`SessionStore`applyTransition[sid, "elevate"],
    "ok",
    TestID -> "SessionStore::Transition::ElevateOk"
  ];

  VerificationTest[
    (PersonalSite`SessionStore`getSession[sid])["state"],
    "elevated",
    TestID -> "SessionStore::Transition::ElevateStatePersisted"
  ];

  (* elevated -> active *)
  VerificationTest[
    Last @ PersonalSite`SessionStore`applyTransition[sid, "downgrade"],
    "ok",
    TestID -> "SessionStore::Transition::DowngradeOk"
  ];

  (* active -> suspended *)
  VerificationTest[
    Last @ PersonalSite`SessionStore`applyTransition[sid, "suspend"],
    "ok",
    TestID -> "SessionStore::Transition::SuspendOk"
  ];

  (* suspended -> active *)
  VerificationTest[
    Last @ PersonalSite`SessionStore`applyTransition[sid, "resume"],
    "ok",
    TestID -> "SessionStore::Transition::ResumeOk"
  ];

  (* Transicion invalida devuelve {$Failed, razon} *)
  With[{bad = PersonalSite`SessionStore`applyTransition[sid, "resume"]},
    VerificationTest[
      First[bad],
      $Failed,
      TestID -> "SessionStore::Transition::InvalidReturnsFailure"
    ];
    VerificationTest[
      StringContainsQ[Last[bad], "invalid_transition"],
      True,
      TestID -> "SessionStore::Transition::InvalidReasonString"
    ]
  ];

  (* SID inexistente *)
  VerificationTest[
    First @ PersonalSite`SessionStore`applyTransition[
      "00000000-0000-0000-0000-000000000000", "elevate"],
    $Failed,
    TestID -> "SessionStore::Transition::MissingSidFails"
  ];

  PersonalSite`SessionStore`destroySession[sid]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 5: refreshSession — extension de TTL
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["refreshSession"]

Module[{result, sid, before, after},
  result = PersonalSite`SessionStore`createSession["refresh-test", 1];
  sid    = result["sessionId"];
  before = (PersonalSite`SessionStore`getSession[sid])["expiresAt"];

  Pause[1];  (* asegurar que el timestamp cambie *)
  after  = (PersonalSite`SessionStore`refreshSession[sid])["expiresAt"];

  VerificationTest[
    StringQ[after],
    True,
    TestID -> "SessionStore::Refresh::ReturnsString"
  ];

  VerificationTest[
    (* expiresAt se extiende o iguala — nunca retrocede;
       usar StringOrder porque >= en strings no evalua a True/False en WL;
       StringOrder[a,b]=1 si a<b, 0 si a=b, -1 si a>b *)
    StringQ[before] && StringQ[after] &&
    StringOrder[before, after] =!= -1,
    True,
    TestID -> "SessionStore::Refresh::ExpiresAtNotDecreased"
  ];

  VerificationTest[
    PersonalSite`SessionStore`refreshSession[
      "00000000-0000-0000-0000-000000000000"],
    $Failed,
    TestID -> "SessionStore::Refresh::MissingFails"
  ];

  PersonalSite`SessionStore`destroySession[sid]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 6: destroySession — invalidacion
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["destroySession"]

Module[{result, sid, token},
  result = PersonalSite`SessionStore`createSession["destroy-test", 2];
  sid    = result["sessionId"];
  token  = result["token"];

  (* Antes del destroy: sesion existe *)
  VerificationTest[
    AssociationQ @ PersonalSite`SessionStore`getSession[sid],
    True,
    TestID -> "SessionStore::Destroy::ExistsBefore"
  ];

  PersonalSite`SessionStore`destroySession[sid];

  (* Despues del destroy: getSession falla *)
  VerificationTest[
    PersonalSite`SessionStore`getSession[sid],
    $Failed,
    TestID -> "SessionStore::Destroy::GetFailsAfter"
  ];

  (* Token ya no valida *)
  VerificationTest[
    PersonalSite`SessionStore`validateToken[token],
    $Failed,
    TestID -> "SessionStore::Destroy::TokenInvalidatedAfter"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 7: sessionStats — metricas
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["sessionStats"]

Module[{stats},
  stats = PersonalSite`SessionStore`sessionStats[];

  VerificationTest[
    AssociationQ[stats],
    True,
    TestID -> "SessionStore::Stats::IsAssoc"
  ];

  VerificationTest[
    KeyExistsQ[stats, "byState"],
    True,
    TestID -> "SessionStore::Stats::HasByState"
  ];

  VerificationTest[
    KeyExistsQ[stats, "cacheStats"],
    True,
    TestID -> "SessionStore::Stats::HasCacheStats"
  ];

  VerificationTest[
    AssociationQ[stats["byState"]],
    True,
    TestID -> "SessionStore::Stats::ByStateIsAssoc"
  ];

  VerificationTest[
    (* ratio siempre en [0,1] *)
    0 <= stats["cacheStats"]["ratio"] <= 1,
    True,
    TestID -> "SessionStore::Stats::RatioInRange"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 8: gcSessions — recoleccion de basura
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["gcSessions"]

VerificationTest[
  (* gcSessions no lanza errores y devuelve True *)
  PersonalSite`SessionStore`gcSessions[],
  True,
  TestID -> "SessionStore::GC::RunsClean"
];

VerificationTest[
  (* Segunda llamada consecutiva tambien es segura (idempotente) *)
  PersonalSite`SessionStore`gcSessions[],
  True,
  TestID -> "SessionStore::GC::Idempotent"
];

EndTestSection[]
