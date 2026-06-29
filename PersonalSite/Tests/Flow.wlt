(* PersonalSite/Tests/Flow.wlt
   ─────────────────────────────────────────────────────────────────────────
   Tests de caja blanca para PersonalSite`Flow`.
   Cubren: construccion de DAG, ordenamiento topologico, deteccion de
   ciclos y ejecucion por capas (backend "sync").

   Ejecutar:
       TestReport["Tests/Flow.wlt"]
   ─────────────────────────────────────────────────────────────────────────*)

PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* ── Spec de referencia: DAG lineal A -> B -> C ──────────────────────── *)
$specLinear = <|
  "A" -> <|"deps" -> {},    "action" -> (42 &)|>,
  "B" -> <|"deps" -> {"A"}, "action" -> Function[p, p["A"] + 1]|>,
  "C" -> <|"deps" -> {"B"}, "action" -> Function[p, p["B"] * 2]|>
|>;

(* ── Spec de referencia: DAG en abanico (A, B independientes -> C) ───── *)
$specFan = <|
  "A" -> <|"deps" -> {},    "action" -> (10 &)|>,
  "B" -> <|"deps" -> {},    "action" -> (20 &)|>,
  "C" -> <|"deps" -> {"A","B"},
           "action" -> Function[p, p["A"] + p["B"]]|>
|>;

(* ── Spec ciclico (invalido) ─────────────────────────────────────────── *)
$specCyclic = <|
  "X" -> <|"deps" -> {"Y"}, "action" -> (1 &)|>,
  "Y" -> <|"deps" -> {"X"}, "action" -> (2 &)|>
|>;


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 1: edges y graph
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["edges"]

VerificationTest[
  Sort @ PersonalSite`Flow`edges[$specLinear],
  Sort[{"A" -> "B", "B" -> "C"}],
  TestID -> "Flow::Edges::LinearSpec"
]

VerificationTest[
  Sort @ PersonalSite`Flow`edges[$specFan],
  Sort[{"A" -> "C", "B" -> "C"}],
  TestID -> "Flow::Edges::FanSpec"
]

VerificationTest[
  PersonalSite`Flow`edges[<|"solo" -> <|"deps"->{}|>|>],
  {},
  TestID -> "Flow::Edges::NoEdgesForNoDeps"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 2: acyclicQ
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["acyclicQ"]

VerificationTest[
  PersonalSite`Flow`acyclicQ[$specLinear],
  True,
  TestID -> "Flow::Acyclic::LinearIsAcyclic"
]

VerificationTest[
  PersonalSite`Flow`acyclicQ[$specFan],
  True,
  TestID -> "Flow::Acyclic::FanIsAcyclic"
]

VerificationTest[
  PersonalSite`Flow`acyclicQ[$specCyclic],
  False,
  TestID -> "Flow::Acyclic::CyclicIsNotAcyclic"
]

VerificationTest[
  PersonalSite`Flow`acyclicQ[<|"solo" -> <|"deps"->{}|>|>],
  True,
  TestID -> "Flow::Acyclic::SingleNodeIsAcyclic"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 3: layers — ordenamiento topologico por capas
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["layers"]

VerificationTest[
  PersonalSite`Flow`layers[$specLinear],
  {{"A"}, {"B"}, {"C"}},
  TestID -> "Flow::Layers::Linear3Capas"
]

With[{ls = PersonalSite`Flow`layers[$specFan]},
  VerificationTest[
    Length[ls],
    2,
    TestID -> "Flow::Layers::Fan2Capas"
  ];
  (* Primera capa: A y B en cualquier orden *)
  VerificationTest[
    Sort[First[ls]],
    {"A","B"},
    TestID -> "Flow::Layers::FanCapa1"
  ];
  (* Segunda capa: solo C *)
  VerificationTest[
    Last[ls],
    {"C"},
    TestID -> "Flow::Layers::FanCapa2"
  ]
];

(* Grafo ciclico devuelve $Failed *)
VerificationTest[
  PersonalSite`Flow`layers[$specCyclic],
  $Failed,
  TestID -> "Flow::Layers::CyclicFails"
]

(* Nodo sin dependencias: una sola capa *)
VerificationTest[
  PersonalSite`Flow`layers[<|"X" -> <|"deps"->{}|>|>],
  {{"X"}},
  TestID -> "Flow::Layers::SingleNode"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 4: run — ejecucion correcta del DAG
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["run"]

(* Backend "sync": determinista, sin TaskObjects *)
With[{r = PersonalSite`Flow`run[$specLinear, "sync"]},
  VerificationTest[
    r["ok"],
    True,
    TestID -> "Flow::Run::LinearOk"
  ];
  VerificationTest[
    r["results"]["A"],
    42,
    TestID -> "Flow::Run::LinearA"
  ];
  VerificationTest[
    r["results"]["B"],
    43,                   (* 42 + 1 *)
    TestID -> "Flow::Run::LinearB"
  ];
  VerificationTest[
    r["results"]["C"],
    86,                   (* 43 * 2 *)
    TestID -> "Flow::Run::LinearC"
  ]
];

With[{r = PersonalSite`Flow`run[$specFan, "sync"]},
  VerificationTest[
    r["ok"],
    True,
    TestID -> "Flow::Run::FanOk"
  ];
  VerificationTest[
    r["results"]["C"],
    30,                   (* 10 + 20 *)
    TestID -> "Flow::Run::FanC"
  ]
];

(* DAG ciclico -> ok=False con error legible *)
With[{r = PersonalSite`Flow`run[$specCyclic, "sync"]},
  VerificationTest[
    r["ok"],
    False,
    TestID -> "Flow::Run::CyclicFails"
  ];
  VerificationTest[
    StringQ[r["error"]],
    True,
    TestID -> "Flow::Run::CyclicHasError"
  ]
];

(* Nodo unico sin dependencias *)
With[{r = PersonalSite`Flow`run[
    <|"solo" -> <|"deps"->{}, "action" -> ("hello" &)|>|>, "sync"]},
  VerificationTest[
    r["results"]["solo"],
    "hello",
    TestID -> "Flow::Run::SingleNode"
  ]
];

(* run devuelve el resumen correcto *)
With[{r = PersonalSite`Flow`run[$specLinear, "sync"]},
  VerificationTest[
    KeyExistsQ[r, "results"],
    True,
    TestID -> "Flow::Run::HasResults"
  ];
  VerificationTest[
    KeyExistsQ[r, "elapsed"],
    True,
    TestID -> "Flow::Run::HasElapsed"
  ];
  VerificationTest[
    KeyExistsQ[r, "layers"],
    True,
    TestID -> "Flow::Run::HasLayers"
  ];
  VerificationTest[
    r["elapsed"] >= 0,
    True,
    TestID -> "Flow::Run::ElapsedNonNegative"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 5: kernelInfo / setMaxKernels
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["kernelInfo"]

VerificationTest[
  AssociationQ @ PersonalSite`Flow`kernelInfo[],
  True,
  TestID -> "Flow::KernelInfo::IsAssoc"
]

VerificationTest[
  KeyExistsQ[PersonalSite`Flow`kernelInfo[], "max"],
  True,
  TestID -> "Flow::KernelInfo::HasMax"
]

VerificationTest[
  IntegerQ[PersonalSite`Flow`kernelInfo[]["max"]],
  True,
  TestID -> "Flow::KernelInfo::MaxIsInt"
]

VerificationTest[
  (PersonalSite`Flow`setMaxKernels[4];
   PersonalSite`Flow`kernelInfo[]["max"]),
  4,
  TestID -> "Flow::KernelInfo::SetMax"
]

(* Restaurar *)
PersonalSite`Flow`setMaxKernels[2];

EndTestSection[]
