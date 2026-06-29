(* PersonalSite/Tests/SessionFSM.wlt
   ─────────────────────────────────────────────────────────────────────────
   Tests de caja blanca para PersonalSite`SessionFSM`.
   No requieren conexion a base de datos ni servidor HTTP.

   Ejecutar:
       TestReport["Tests/SessionFSM.wlt"]
   ─────────────────────────────────────────────────────────────────────────*)

PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 1: derivePermissions — union del NestGraph por role
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["derivePermissions"]

VerificationTest[
  Sort @ PersonalSite`SessionFSM`derivePermissions[1],
  {"arch.view", "blog.read", "public.read"},
  TestID -> "SessionFSM::Perms::Role1::ExactSet"
]

VerificationTest[
  Length @ PersonalSite`SessionFSM`derivePermissions[1],
  3,
  TestID -> "SessionFSM::Perms::Role1::Count"
]

VerificationTest[
  MemberQ[PersonalSite`SessionFSM`derivePermissions[1], "public.read"],
  True,
  TestID -> "SessionFSM::Perms::Role1::HasPublicRead"
]

VerificationTest[
  MemberQ[PersonalSite`SessionFSM`derivePermissions[1], "kernel.eval"],
  False,
  TestID -> "SessionFSM::Perms::Role1::NoKernelEval"
]

VerificationTest[
  (* role 2 incluye todo lo de role 1 *)
  SubsetQ[
    PersonalSite`SessionFSM`derivePermissions[2],
    PersonalSite`SessionFSM`derivePermissions[1]],
  True,
  TestID -> "SessionFSM::Perms::Role2::IncludesRole1"
]

VerificationTest[
  (* role 2 tiene mas permisos que role 1 *)
  Length[PersonalSite`SessionFSM`derivePermissions[2]] >
  Length[PersonalSite`SessionFSM`derivePermissions[1]],
  True,
  TestID -> "SessionFSM::Perms::Role2::MoreThanRole1"
]

VerificationTest[
  MemberQ[PersonalSite`SessionFSM`derivePermissions[2], "content.write"],
  True,
  TestID -> "SessionFSM::Perms::Role2::HasContentWrite"
]

VerificationTest[
  MemberQ[PersonalSite`SessionFSM`derivePermissions[2], "kernel.eval"],
  False,
  TestID -> "SessionFSM::Perms::Role2::NoKernelEval"
]

VerificationTest[
  (* role 3 incluye todo lo de role 2 *)
  SubsetQ[
    PersonalSite`SessionFSM`derivePermissions[3],
    PersonalSite`SessionFSM`derivePermissions[2]],
  True,
  TestID -> "SessionFSM::Perms::Role3::IncludesRole2"
]

VerificationTest[
  Length @ PersonalSite`SessionFSM`derivePermissions[3],
  16,
  TestID -> "SessionFSM::Perms::Role3::Count"
]

VerificationTest[
  MemberQ[PersonalSite`SessionFSM`derivePermissions[3], "kernel.eval"],
  True,
  TestID -> "SessionFSM::Perms::Role3::HasKernelEval"
]

VerificationTest[
  MemberQ[PersonalSite`SessionFSM`derivePermissions[3], "admin.*"],
  True,
  TestID -> "SessionFSM::Perms::Role3::HasAdmin"
]

(* permisos son lista ordenada (Union garantiza esto) *)
VerificationTest[
  OrderedQ @ PersonalSite`SessionFSM`derivePermissions[3],
  True,
  TestID -> "SessionFSM::Perms::Role3::IsSorted"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 2: permissionTree — estructura del NestGraph (40 nodos)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["permissionTree"]

VerificationTest[
  Length @ PersonalSite`SessionFSM`permissionTree[1],
  40,
  TestID -> "SessionFSM::Tree::NodeCount"
]

VerificationTest[
  (* nivel 0: exactamente 1 nodo raiz *)
  Length @ Select[PersonalSite`SessionFSM`permissionTree[1],
    Function[n, n["level"] === 0]],
  1,
  TestID -> "SessionFSM::Tree::L0Count"
]

VerificationTest[
  (* nivel 1: exactamente 3 nodos (una por cada regla) *)
  Length @ Select[PersonalSite`SessionFSM`permissionTree[1],
    Function[n, n["level"] === 1]],
  3,
  TestID -> "SessionFSM::Tree::L1Count"
]

VerificationTest[
  (* nivel 2: 3^2 = 9 nodos *)
  Length @ Select[PersonalSite`SessionFSM`permissionTree[1],
    Function[n, n["level"] === 2]],
  9,
  TestID -> "SessionFSM::Tree::L2Count"
]

VerificationTest[
  (* nivel 3: 3^3 = 27 nodos *)
  Length @ Select[PersonalSite`SessionFSM`permissionTree[1],
    Function[n, n["level"] === 3]],
  27,
  TestID -> "SessionFSM::Tree::L3Count"
]

VerificationTest[
  (* raiz: id == 1, parent == Null, perms vacios *)
  First[PersonalSite`SessionFSM`permissionTree[1]]["parent"],
  Null,
  TestID -> "SessionFSM::Tree::RootParentNull"
]

VerificationTest[
  First[PersonalSite`SessionFSM`permissionTree[1]]["perms"],
  {},
  TestID -> "SessionFSM::Tree::RootPermsEmpty"
]

VerificationTest[
  (* todos los nodos tienen clave "id", "level", "parent", "perms", "role" *)
  AllTrue[PersonalSite`SessionFSM`permissionTree[2],
    Function[n, SubsetQ[Keys[n], {"id","level","parent","perms","role"}]]],
  True,
  TestID -> "SessionFSM::Tree::NodeStructure"
]

VerificationTest[
  (* con role=1 la union de todos los perms del arbol == derivePermissions[1] *)
  Sort @ Union @ Flatten[
    Map[Function[n, n["perms"]],
      PersonalSite`SessionFSM`permissionTree[1]]],
  Sort @ PersonalSite`SessionFSM`derivePermissions[1],
  TestID -> "SessionFSM::Tree::UnionMatchesDerived"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 3: transition — maquina de estados finitos
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["transition"]

(* Session de prueba base *)
With[{baseSession = <|
  "sessionId"   -> "test-sid",
  "userId"      -> "tester",
  "role"        -> 3,
  "state"       -> "active",
  "permissions" -> PersonalSite`SessionFSM`derivePermissions[3]|>},

  VerificationTest[
    Last @ PersonalSite`SessionFSM`transition[baseSession, "elevate"],
    "ok",
    TestID -> "SessionFSM::FSM::ElevateStatus"
  ];

  VerificationTest[
    (First @ PersonalSite`SessionFSM`transition[baseSession, "elevate"])["state"],
    "elevated",
    TestID -> "SessionFSM::FSM::ElevateNewState"
  ];

  VerificationTest[
    Last @ PersonalSite`SessionFSM`transition[baseSession, "suspend"],
    "ok",
    TestID -> "SessionFSM::FSM::SuspendFromActiveStatus"
  ];

  VerificationTest[
    (First @ PersonalSite`SessionFSM`transition[baseSession, "logout"])["state"],
    "expired",
    TestID -> "SessionFSM::FSM::LogoutNewState"
  ];

  (* Transicion invalida desde active *)
  VerificationTest[
    First @ PersonalSite`SessionFSM`transition[baseSession, "resume"],
    $Failed,
    TestID -> "SessionFSM::FSM::ResumeFromActiveFails"
  ];

  VerificationTest[
    First @ PersonalSite`SessionFSM`transition[baseSession, "downgrade"],
    $Failed,
    TestID -> "SessionFSM::FSM::DowngradeFromActiveFails"
  ];

  VerificationTest[
    First @ PersonalSite`SessionFSM`transition[baseSession, "relogin"],
    $Failed,
    TestID -> "SessionFSM::FSM::ReloginFromActiveFails"
  ];

  (* Evento inexistente siempre falla *)
  VerificationTest[
    First @ PersonalSite`SessionFSM`transition[baseSession, "nonexistent-event"],
    $Failed,
    TestID -> "SessionFSM::FSM::UnknownEventFails"
  ]
];

(* Transition desde elevated *)
With[{elevated = <|
  "sessionId" -> "test-sid",
  "state"     -> "elevated",
  "role"      -> 3,
  "permissions" -> PersonalSite`SessionFSM`derivePermissions[3]|>},

  VerificationTest[
    (First @ PersonalSite`SessionFSM`transition[elevated, "downgrade"])["state"],
    "active",
    TestID -> "SessionFSM::FSM::DowngradeFromElevated"
  ];

  VerificationTest[
    (First @ PersonalSite`SessionFSM`transition[elevated, "suspend"])["state"],
    "suspended",
    TestID -> "SessionFSM::FSM::SuspendFromElevated"
  ];

  VerificationTest[
    First @ PersonalSite`SessionFSM`transition[elevated, "resume"],
    $Failed,
    TestID -> "SessionFSM::FSM::ResumeFromElevatedFails"
  ]
];

(* Transition desde suspended *)
With[{suspended = <|
  "sessionId" -> "test-sid",
  "state"     -> "suspended",
  "role"      -> 1,
  "permissions" -> PersonalSite`SessionFSM`derivePermissions[1]|>},

  VerificationTest[
    (First @ PersonalSite`SessionFSM`transition[suspended, "resume"])["state"],
    "active",
    TestID -> "SessionFSM::FSM::ResumeFromSuspended"
  ];

  VerificationTest[
    (First @ PersonalSite`SessionFSM`transition[suspended, "logout"])["state"],
    "expired",
    TestID -> "SessionFSM::FSM::LogoutFromSuspended"
  ];

  VerificationTest[
    First @ PersonalSite`SessionFSM`transition[suspended, "elevate"],
    $Failed,
    TestID -> "SessionFSM::FSM::ElevateFromSuspendedFails"
  ]
];

(* Transition actualiza permissions tras cambio de estado *)
With[{sess = <|"sessionId"->"x", "state"->"active", "role"->3,
               "permissions"->{}|>},
  VerificationTest[
    Length[(First @ PersonalSite`SessionFSM`transition[sess, "elevate"])["permissions"]],
    16,
    TestID -> "SessionFSM::FSM::TransitionReDerivePerms"
  ]
];

(* lastTransition se registra *)
With[{sess = <|"sessionId"->"x", "state"->"active", "role"->1,
               "permissions"->PersonalSite`SessionFSM`derivePermissions[1]|>},
  VerificationTest[
    KeyExistsQ[
      First @ PersonalSite`SessionFSM`transition[sess, "suspend"],
      "lastTransition"],
    True,
    TestID -> "SessionFSM::FSM::LastTransitionRecorded"
  ];

  VerificationTest[
    (First @ PersonalSite`SessionFSM`transition[sess, "suspend"])["lastTransition"]["event"],
    "suspend",
    TestID -> "SessionFSM::FSM::LastTransitionEvent"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 4: can — verificacion de permisos
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["can"]

With[{s1 = <|"state"->"active",   "permissions"->PersonalSite`SessionFSM`derivePermissions[1]|>,
      s2 = <|"state"->"active",   "permissions"->PersonalSite`SessionFSM`derivePermissions[2]|>,
      s3 = <|"state"->"active",   "permissions"->PersonalSite`SessionFSM`derivePermissions[3]|>,
      se = <|"state"->"elevated", "permissions"->PersonalSite`SessionFSM`derivePermissions[3]|>,
      sx = <|"state"->"suspended","permissions"->PersonalSite`SessionFSM`derivePermissions[3]|>,
      sf = <|"state"->"expired",  "permissions"->PersonalSite`SessionFSM`derivePermissions[3]|>},

  (* role 1 *)
  VerificationTest[PersonalSite`SessionFSM`can[s1, "public.read"],  True,  TestID->"SessionFSM::Can::R1PublicRead"];
  VerificationTest[PersonalSite`SessionFSM`can[s1, "kernel.eval"],  False, TestID->"SessionFSM::Can::R1NoKernel"];
  VerificationTest[PersonalSite`SessionFSM`can[s1, "admin.*"],      False, TestID->"SessionFSM::Can::R1NoAdmin"];

  (* role 2 *)
  VerificationTest[PersonalSite`SessionFSM`can[s2, "content.write"],True,  TestID->"SessionFSM::Can::R2ContentWrite"];
  VerificationTest[PersonalSite`SessionFSM`can[s2, "kernel.eval"],  False, TestID->"SessionFSM::Can::R2NoKernel"];

  (* role 3 *)
  VerificationTest[PersonalSite`SessionFSM`can[s3, "kernel.eval"],  True,  TestID->"SessionFSM::Can::R3Kernel"];
  VerificationTest[PersonalSite`SessionFSM`can[s3, "admin.*"],      True,  TestID->"SessionFSM::Can::R3Admin"];

  (* admin.* otorga todo *)
  VerificationTest[PersonalSite`SessionFSM`can[s3, "cualquier.cosa"], True, TestID->"SessionFSM::Can::AdminWildcard"];

  (* estado elevated tambien permite *)
  VerificationTest[PersonalSite`SessionFSM`can[se, "kernel.eval"],  True,  TestID->"SessionFSM::Can::ElevatedKernel"];

  (* suspended y expired deniegan independientemente de permisos *)
  VerificationTest[PersonalSite`SessionFSM`can[sx, "admin.*"],      False, TestID->"SessionFSM::Can::SuspendedDenied"];
  VerificationTest[PersonalSite`SessionFSM`can[sf, "admin.*"],      False, TestID->"SessionFSM::Can::ExpiredDenied"]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 5: fsmGraph — estructura del grafo de transiciones
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["fsmGraph"]

With[{g = PersonalSite`SessionFSM`fsmGraph[]},

  VerificationTest[AssociationQ[g],                     True,  TestID->"SessionFSM::Graph::IsAssoc"];
  VerificationTest[Length[g["states"]],                 6,     TestID->"SessionFSM::Graph::StateCount"];
  VerificationTest[Length[g["events"]],                 8,     TestID->"SessionFSM::Graph::EventCount"];
  VerificationTest[g["edgeCount"],                      13,    TestID->"SessionFSM::Graph::EdgeCount"];
  VerificationTest[Length[g["edges"]],                  13,    TestID->"SessionFSM::Graph::EdgesListLen"];

  VerificationTest[
    MemberQ[g["states"], "unauthenticated"],           True,  TestID->"SessionFSM::Graph::HasUnauthState"];
  VerificationTest[
    MemberQ[g["states"], "elevated"],                  True,  TestID->"SessionFSM::Graph::HasElevatedState"];
  VerificationTest[
    MemberQ[g["events"], "elevate"],                   True,  TestID->"SessionFSM::Graph::HasElevateEvent"];
  VerificationTest[
    MemberQ[g["events"], "relogin"],                   True,  TestID->"SessionFSM::Graph::HasReloginEvent"];

  (* Cada arista tiene clave from, to, event *)
  VerificationTest[
    AllTrue[g["edges"],
      Function[e, SubsetQ[Keys[e], {"from","to","event"}]]],
    True, TestID->"SessionFSM::Graph::EdgeStructure"];

  (* Verificar arista especifica: active -[elevate]-> elevated *)
  VerificationTest[
    AnyTrue[g["edges"],
      Function[e, e["from"]==="active" && e["event"]==="elevate" && e["to"]==="elevated"]],
    True, TestID->"SessionFSM::Graph::ElevateEdge"];

  (* Solo 1 arista de relogin *)
  VerificationTest[
    Length @ Select[g["edges"], Function[e, e["event"] === "relogin"]],
    1, TestID->"SessionFSM::Graph::OneReloginEdge"]
];

EndTestSection[]
