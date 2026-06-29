(* PersonalSite/Tests/NestScheduler.wlt
   ─────────────────────────────────────────────────────────────────────────
   Tests para PersonalSite`NestScheduler`.
   Cubre: build, estructura de nodos, ejecucion y exportacion.

   Ejecutar:
       TestReport["Tests/NestScheduler.wlt"]
   ─────────────────────────────────────────────────────────────────────────*)

PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* Reglas del diagrama del UI: NestGraph[{2#+1, #+14, #-18}&, {1}, 3] *)
$rules = {2 # + 1 &, # + 14 &, # - 18 &};
$seeds = {1};
$depth = 3;

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 1: build — construccion del arbol de nodos
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["build"]

With[{built = PersonalSite`NestScheduler`build[$rules, $seeds, $depth]},

  VerificationTest[
    AssociationQ[built],
    True,
    TestID -> "NestScheduler::Build::IsAssoc"
  ];

  VerificationTest[
    (* 1 seed * (1 + 3 + 9 + 27) = 40 nodos *)
    built["nodeCount"],
    40,
    TestID -> "NestScheduler::Build::NodeCount40"
  ];

  VerificationTest[
    built["depth"],
    3,
    TestID -> "NestScheduler::Build::DepthCorrect"
  ];

  VerificationTest[
    built["ruleCount"],
    3,
    TestID -> "NestScheduler::Build::RuleCount3"
  ];

  VerificationTest[
    built["seedCount"],
    1,
    TestID -> "NestScheduler::Build::SeedCount1"
  ];

  VerificationTest[
    Length[built["records"]],
    40,
    TestID -> "NestScheduler::Build::RecordsLen"
  ];

  (* Spec compatible con Flow`run *)
  VerificationTest[
    AssociationQ[built["spec"]],
    True,
    TestID -> "NestScheduler::Build::SpecIsAssoc"
  ];

  VerificationTest[
    Length[built["spec"]],
    40,
    TestID -> "NestScheduler::Build::SpecLen"
  ];

  (* Cada nodo del spec tiene "deps" y "action" *)
  VerificationTest[
    AllTrue[Values[built["spec"]],
      Function[node, KeyExistsQ[node, "deps"] && KeyExistsQ[node, "action"]]],
    True,
    TestID -> "NestScheduler::Build::SpecNodeStructure"
  ];

  (* Raiz: nodo n1 sin dependencias *)
  VerificationTest[
    built["spec"]["n1"]["deps"],
    {},
    TestID -> "NestScheduler::Build::RootNoDeps"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 2: records — estructura de los nodos
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["records"]

With[{recs = PersonalSite`NestScheduler`build[$rules, $seeds, $depth]["records"]},

  (* Niveles correctos *)
  With[{byLevel = GroupBy[recs, Function[r, r["level"]]]},

    VerificationTest[
      Length[byLevel[0]],
      1,
      TestID -> "NestScheduler::Records::Level0Count"
    ];

    VerificationTest[
      Length[byLevel[1]],
      3,
      TestID -> "NestScheduler::Records::Level1Count"
    ];

    VerificationTest[
      Length[byLevel[2]],
      9,
      TestID -> "NestScheduler::Records::Level2Count"
    ];

    VerificationTest[
      Length[byLevel[3]],
      27,
      TestID -> "NestScheduler::Records::Level3Count"
    ]
  ];

  (* Raiz: level=0, parent=None, value=1 (el seed) *)
  With[{root = SelectFirst[recs, Function[r, r["level"] === 0]]},
    VerificationTest[
      root["parent"],
      None,
      TestID -> "NestScheduler::Records::RootParentNone"
    ];
    VerificationTest[
      root["value"],
      1,
      TestID -> "NestScheduler::Records::RootValueIsSeed"
    ]
  ];

  (* Nodos de nivel 1: valores correctos segun las 3 reglas *)
  With[{l1vals = Sort @ Map[Function[r, r["value"]],
                   Select[recs, Function[r, r["level"] === 1]]]},
    VerificationTest[
      l1vals,
      Sort[{2*1+1, 1+14, 1-18}],   (* {3, 15, -17} *)
      TestID -> "NestScheduler::Records::Level1Values"
    ]
  ];

  (* IDs son unicos *)
  VerificationTest[
    Length[DeleteDuplicates[Map[Function[r, r["id"]], recs]]],
    40,
    TestID -> "NestScheduler::Records::IdsUnique"
  ];

  (* Cada nodo (salvo raiz) tiene parent existente *)
  VerificationTest[
    AllTrue[
      Select[recs, Function[r, r["parent"] =!= None]],
      Function[r, AnyTrue[recs, Function[p, p["id"] === r["parent"]]]]],
    True,
    TestID -> "NestScheduler::Records::ParentExists"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 3: run — ejecucion completa
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["run"]

With[{res = PersonalSite`NestScheduler`run[$rules, $seeds, $depth, "sync"]},

  VerificationTest[
    AssociationQ[res],
    True,
    TestID -> "NestScheduler::Run::ReturnsAssoc"
  ];

  VerificationTest[
    KeyExistsQ[res, "built"],
    True,
    TestID -> "NestScheduler::Run::HasBuilt"
  ];

  VerificationTest[
    KeyExistsQ[res, "flow"],
    True,
    TestID -> "NestScheduler::Run::HasFlow"
  ];

  VerificationTest[
    res["flow"]["ok"],
    True,
    TestID -> "NestScheduler::Run::FlowOk"
  ];

  (* El resultado del nodo raiz debe ser el seed *)
  VerificationTest[
    res["flow"]["results"]["n1"],
    1,   (* seed = 1 *)
    TestID -> "NestScheduler::Run::RootResultIsSeed"
  ];

  (* Nodos de nivel 1: resultados = reglas aplicadas al seed *)
  With[{results = res["flow"]["results"]},
    VerificationTest[
      Sort @ Map[results[#] &, {"n2","n3","n4"}],
      Sort[{2*1+1, 1+14, 1-18}],
      TestID -> "NestScheduler::Run::Level1Results"
    ]
  ];

  (* elapsed registrado *)
  VerificationTest[
    res["elapsed"] >= 0,
    True,
    TestID -> "NestScheduler::Run::ElapsedNonNegative"
  ];

  (* runCount incrementa (accedido via taskInfo) *)
  VerificationTest[
    PersonalSite`NestScheduler`taskInfo[]["runCount"] >= 1,
    True,
    TestID -> "NestScheduler::Run::RunCountIncremented"
  ]
];

(* Multiples seeds: nodeCount = seeds * (1 + 3 + 9 + 27) = 80 para 2 seeds *)
With[{built2 = PersonalSite`NestScheduler`build[$rules, {1, 5}, 3]},
  VerificationTest[
    built2["nodeCount"],
    80,
    TestID -> "NestScheduler::Run::TwoSeedsCount"
  ]
];

(* depth=0: solo el seed, 1 nodo *)
With[{built0 = PersonalSite`NestScheduler`build[$rules, $seeds, 0]},
  VerificationTest[
    built0["nodeCount"],
    1,
    TestID -> "NestScheduler::Run::Depth0Count"
  ]
];

(* depth=1: 1 + 3 = 4 nodos *)
With[{built1 = PersonalSite`NestScheduler`build[$rules, $seeds, 1]},
  VerificationTest[
    built1["nodeCount"],
    4,
    TestID -> "NestScheduler::Run::Depth1Count"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 4: export — serializacion CSV
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["export"]

(* Ejecutar primero para tener $lastResults *)
PersonalSite`NestScheduler`run[$rules, $seeds, 2, "sync"];

(* Default export es JSON *)
With[{json = PersonalSite`NestScheduler`export[]},
  VerificationTest[
    StringQ[json],
    True,
    TestID -> "NestScheduler::Export::JsonIsString"
  ];

  VerificationTest[
    (* JSON array no vacio *)
    StringContainsQ[json, "["],
    True,
    TestID -> "NestScheduler::Export::JsonIsArray"
  ]
];

(* Export CSV explícito *)
With[{csv = PersonalSite`NestScheduler`export["csv"]},
  VerificationTest[
    StringQ[csv],
    True,
    TestID -> "NestScheduler::Export::CsvIsString"
  ];

  VerificationTest[
    (* CSV contiene cabecera *)
    StringContainsQ[csv, "id,level"],
    True,
    TestID -> "NestScheduler::Export::CsvHasHeader"
  ];

  VerificationTest[
    (* Al menos 1 linea de datos mas la cabecera *)
    Length[StringSplit[csv, "\n"]] > 1,
    True,
    TestID -> "NestScheduler::Export::CsvHasRows"
  ]
];

EndTestSection[]
