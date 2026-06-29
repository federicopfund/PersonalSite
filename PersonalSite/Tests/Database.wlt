(* PersonalSite/Tests/Database.wlt
   ─────────────────────────────────────────────────────────────────────────
   Tests de integracion para PersonalSite`Database`.
   Verifican conectividad, setup, diag y operaciones CRUD basicas.
   Requieren la base de datos SQLite inicializada.

   Ejecutar:
       TestReport["Tests/Database.wlt"]
   ─────────────────────────────────────────────────────────────────────────*)

PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* ── Setup: asegurar que la DB este configurada ──────────────────────── *)
PersonalSite`Database`setup[];

(* ══════════════════════════════════════════════════════════════════════════
   SECCION 1: setup y diag
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["setup"]

VerificationTest[
  StringQ[PersonalSite`Config`$databasePath],
  True,
  TestID -> "Database::Setup::PathIsString"
]

VerificationTest[
  FileExistsQ[PersonalSite`Config`$databasePath],
  True,
  TestID -> "Database::Setup::FileExists"
]

With[{d = PersonalSite`Database`diag[]},
  VerificationTest[
    AssociationQ[d],
    True,
    TestID -> "Database::Diag::IsAssoc"
  ];

  VerificationTest[
    d["fileExists"],
    True,
    TestID -> "Database::Diag::FileExists"
  ];

  VerificationTest[
    d["ping"],
    "OK",
    TestID -> "Database::Diag::PingOk"
  ]
];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 2: execute — consultas basicas
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["execute"]

VerificationTest[
  PersonalSite`Database`execute["SELECT 1"],
  {{1}},
  TestID -> "Database::Execute::Select1"
]

VerificationTest[
  PersonalSite`Database`execute["SELECT 1+1"],
  {{2}},
  TestID -> "Database::Execute::Select1Plus1"
]

VerificationTest[
  PersonalSite`Database`execute["SELECT 'hello'"],
  {{"hello"}},
  TestID -> "Database::Execute::SelectString"
]

(* Consulta con parametro *)
VerificationTest[
  PersonalSite`Database`execute["SELECT ?", {42}],
  {{42}},
  TestID -> "Database::Execute::SelectParam"
]

(* Tabla posts existe *)
VerificationTest[
  ListQ @ PersonalSite`Database`execute[
    "SELECT COUNT(*) FROM posts"],
  True,
  TestID -> "Database::Execute::PostsTableExists"
]

(* Tabla scheduler_tasks existe *)
VerificationTest[
  ListQ @ PersonalSite`Database`execute[
    "SELECT COUNT(*) FROM scheduler_tasks"],
  True,
  TestID -> "Database::Execute::SchedulerTasksTableExists"
]

(* Tabla sessions existe *)
VerificationTest[
  ListQ @ PersonalSite`Database`execute[
    "SELECT COUNT(*) FROM sessions"],
  True,
  TestID -> "Database::Execute::SessionsTableExists"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 3: CRUD sobre tabla temporal de prueba
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["crud"]

(* Crear tabla temporal *)
PersonalSite`Database`execute[
  "CREATE TABLE IF NOT EXISTS _wlt_test (id INTEGER PRIMARY KEY, val TEXT)"];

VerificationTest[
  (* INSERT devuelve {} (sin filas de resultado) o no falla *)
  ! MatchQ[
    PersonalSite`Database`execute[
      "INSERT INTO _wlt_test (val) VALUES (?)", {"hello"}],
    $Failed],
  True,
  TestID -> "Database::CRUD::Insert"
]

VerificationTest[
  (PersonalSite`Database`execute[
    "SELECT val FROM _wlt_test WHERE val=?", {"hello"}]),
  {{"hello"}},
  TestID -> "Database::CRUD::SelectAfterInsert"
]

VerificationTest[
  (* UPDATE *)
  ! MatchQ[
    PersonalSite`Database`execute[
      "UPDATE _wlt_test SET val=? WHERE val=?", {"world", "hello"}],
    $Failed],
  True,
  TestID -> "Database::CRUD::Update"
]

VerificationTest[
  (PersonalSite`Database`execute[
    "SELECT val FROM _wlt_test WHERE val=?", {"world"}]),
  {{"world"}},
  TestID -> "Database::CRUD::SelectAfterUpdate"
]

VerificationTest[
  (* DELETE *)
  ! MatchQ[
    PersonalSite`Database`execute[
      "DELETE FROM _wlt_test WHERE val=?", {"world"}],
    $Failed],
  True,
  TestID -> "Database::CRUD::Delete"
]

VerificationTest[
  (* Tabla vacia tras delete *)
  PersonalSite`Database`execute["SELECT COUNT(*) FROM _wlt_test"],
  {{0}},
  TestID -> "Database::CRUD::EmptyAfterDelete"
]

(* Cleanup: eliminar tabla temporal *)
PersonalSite`Database`execute["DROP TABLE IF EXISTS _wlt_test"];

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 4: datos de referencia (posts de ejemplo)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["seed-data"]

VerificationTest[
  (* Al menos 1 post de ejemplo cargado por init.sql *)
  First[First @ PersonalSite`Database`execute["SELECT COUNT(*) FROM posts"]] >= 1,
  True,
  TestID -> "Database::Seed::PostsExist"
]

VerificationTest[
  (* Schema correcto: columnas slug, title, body *)
  ListQ @ PersonalSite`Database`execute[
    "SELECT slug, title, body FROM posts LIMIT 1"],
  True,
  TestID -> "Database::Seed::PostsSchema"
]

VerificationTest[
  (* Al menos 6 tareas del sistema cargadas *)
  First[First @ PersonalSite`Database`execute[
    "SELECT COUNT(*) FROM scheduler_tasks"]] >= 6,
  True,
  TestID -> "Database::Seed::TasksExist"
]

VerificationTest[
  (* La tarea 'heartbeat' existe *)
  PersonalSite`Database`execute[
    "SELECT task_id FROM scheduler_tasks WHERE task_id=?",
    {"heartbeat"}],
  {{"heartbeat"}},
  TestID -> "Database::Seed::HeartbeatTask"
]

EndTestSection[]
