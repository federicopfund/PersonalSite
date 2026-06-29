(* ::Package:: *)

(* PersonalSite`DevStyle`
   --------------------------------------------------------------------------
   Pipeline de desarrollo SCSS: detecta cambios, compila, hashea, e invalida
   el cache WL para hot-reload sin reiniciar el servidor.

   Etapas (1 → 5):
     1. detect[]     — compara fechas de .scss vs styles.css
     2. compile[]    — invoca sass/npx si disponible; actualiza styles.css
     3. hashCss[]    — CRC32 del CSS compilado; escribe css-version.json
     4. cacheBust[]  — limpia el cache WL de fragmentos HTML
     5. report[]     — snapshot del estado del pipeline

   Habilitar en caliente (dev):
       PersonalSite`TaskManager`start["scss-watch"]
       PersonalSite`TaskManager`start["scss-compile"]
       PersonalSite`TaskManager`start["css-version"]
       PersonalSite`TaskManager`start["css-cache-bust"]
       PersonalSite`TaskManager`start["scss-report"]

   Todas las tareas arrancan con enabled=False; no afectan produccion.
   -------------------------------------------------------------------------- *)

Begin["PersonalSite`DevStyle`Private`"];

(* ── Rutas ────────────────────────────────────────────────────────────── *)
$cssOut   := FileNameJoin[{PersonalSite`$Root,"Resources","Static","styles.css"}];
$scssDir  := FileNameJoin[{PersonalSite`$Root,"Resources","Scss"}];
$scssMain := FileNameJoin[{$scssDir,"styles.scss"}];
$verFile  := FileNameJoin[{PersonalSite`$Root,"Resources","Static","css-version.json"}];

(* ── Estado del pipeline ──────────────────────────────────────────────── *)
$lastCssT      = None;   (* AbsoluteTime de la ultima mod vista        *)
$changed       = False;  (* True cuando CSS cambia entre dos detect[]  *)
$lastHash      = "";     (* hash CRC32 del CSS en hex                  *)
$compileAvail  = None;   (* True/False/None=no-probado                 *)
$lastCompileMs = 0;
$lastErr       = None;
$bustCount     = 0;

(* ── 1. detect[] ─────────────────────────────────────────────────────── *)
(*   Compara la fecha de modificacion del CSS compilado con la del run
    anterior. En el primer disparo inicializa $lastCssT sin marcar
    cambio (evita bust innecesario al arrancar el servidor).             *)
PersonalSite`DevStyle`detect[] :=
  Module[{cssT},
    If[! FileExistsQ[$cssOut],
      Return[<|"changed"->False,"reason"->"no-css-output"|>]];
    cssT = AbsoluteTime[FileDate[$cssOut, "Modification"]];
    If[$lastCssT === None,
      (* primer disparo: solo marcar baseline *)
      $lastCssT = cssT;
      $changed  = False,
      (* disparos subsiguientes: comparar *)
      $changed  = cssT > $lastCssT;
      $lastCssT = cssT
    ];
    <|"changed" -> $changed, "cssModTime" -> cssT|>
  ];

(* ── 2. compile[] ─────────────────────────────────────────────────────── *)
(*   Intenta compilar SCSS en este orden:
       1. sass --version       (instalacion global)
       2. npx --yes sass       (via npm en PATH)
    Si ninguno esta disponible, retorna status="skip" para no bloquear
    el pipeline (el usuario puede compilar con `make scss` en el host).  *)
PersonalSite`DevStyle`compile[] :=
  Module[{t0 = AbsoluteTime[], res, exitOk},
    If[! FileExistsQ[$scssMain],
      Return[<|"status"->"skip","reason"->"no-scss-source"|>]];

    (* Probar disponibilidad una sola vez *)
    If[$compileAvail === None,
      $compileAvail =
        Quiet[RunProcess[{"sass","--version"},"ExitCode"]] === 0 ||
        Quiet[RunProcess[{"npx","sass","--version"},"ExitCode"]] === 0
    ];

    If[! TrueQ[$compileAvail],
      Return[<|"status"->"skip","reason"->"sass-not-in-path"|>]];

    (* sass directo *)
    res = Quiet @ RunProcess[
      {"sass","--no-source-map","--style=compressed",$scssMain,$cssOut}];
    exitOk = AssociationQ[res] && res["ExitCode"] === 0;

    (* npx fallback *)
    If[! exitOk,
      res = Quiet @ RunProcess[
        {"npx","--yes","sass","--no-source-map","--style=compressed",$scssMain,$cssOut}];
      exitOk = AssociationQ[res] && res["ExitCode"] === 0
    ];

    $lastCompileMs = Round[1000 * (AbsoluteTime[] - t0)];

    If[exitOk,
      $lastErr  = None;
      $changed  = True;
      <|"status"->"ok","ms"->$lastCompileMs|>,
      $lastErr  = If[AssociationQ[res],
        StringTake[res["StandardError"], UpTo[200]], "compile-failed"];
      <|"status"->"error","err"->$lastErr,"ms"->$lastCompileMs|>
    ]
  ];

(* ── 3. hashCss[] ────────────────────────────────────────────────────── *)
(*   Calcula CRC32 del CSS compilado y escribe css-version.json cuando
    el hash cambia. El template puede leer este JSON para cache-busting
    del <link rel="stylesheet"> sin fingerprinting de build.             *)
PersonalSite`DevStyle`hashCss[] :=
  Module[{h, prev = $lastHash},
    If[! FileExistsQ[$cssOut],
      Return[<|"hash"->"","reason"->"no-css-output"|>]];
    h = IntegerString[Hash[ReadString[$cssOut], "CRC32"], 16];
    If[h =!= prev,
      $lastHash = h;
      Quiet @ Check[
        Export[$verFile,
          <|"v"->h,"at"->DateString[],"compileMs"->$lastCompileMs|>,
          "JSON"],
        Null]
    ];
    <|"hash"->h,"new"->(h =!= prev)|>
  ];

(* ── 4. cacheBust[] ─────────────────────────────────────────────────── *)
(*   Si hubo cambio, limpia todo el cache WL de fragmentos HTML y CSS.
    Cada request siguiente recalculara su fragmento desde cero.          *)
PersonalSite`DevStyle`cacheBust[] :=
  If[! $changed,
    <|"busted"->0,"reason"->"no-change"|>,
    PersonalSite`Cache`clear[];
    $bustCount++;
    $changed = False;
    <|"busted"->$bustCount,"at"->DateString[]|>
  ];

(* ── 5. report[] ─────────────────────────────────────────────────────── *)
(*   Snapshot inmutable del estado actual del pipeline: util para el
    endpoint /tasks/info y para el panel de admin.                       *)
PersonalSite`DevStyle`report[] :=
  <|
    "hash"          -> $lastHash,
    "cssExists"     -> FileExistsQ[$cssOut],
    "scssExists"    -> FileExistsQ[$scssMain],
    "compileAvail"  -> $compileAvail,
    "lastCompileMs" -> $lastCompileMs,
    "lastErr"       -> $lastErr,
    "bustCount"     -> $bustCount,
    "changed"       -> $changed,
    "cache"         -> PersonalSite`Cache`stats[]
  |>;

End[];
