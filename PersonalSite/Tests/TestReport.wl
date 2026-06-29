(* PersonalSite/Tests/TestReport.wl
   ─────────────────────────────────────────────────────────────────────────
   Runner con soporte de layers para ejecutar grupos de modulos.

   Uso:
       wolframscript -script Tests/TestReport.wl
       wolframscript -script Tests/TestReport.wl -- --layer session
       wolframscript -script Tests/TestReport.wl -- --layer flow
       wolframscript -script Tests/TestReport.wl -- --layer db
       wolframscript -script Tests/TestReport.wl -- --layer models
       wolframscript -script Tests/TestReport.wl -- --layer all

   Layers disponibles:
       all      → SessionFSM, SessionStore, Flow, NestScheduler, Database  (default)
       session  → SessionFSM, SessionStore
       flow     → Flow, NestScheduler
       db       → Database
       models   → Flow, NestScheduler, Database

   Docker:
       docker exec profile-web-1 wolframscript -script /app/PersonalSite/Tests/TestReport.wl -- --layer session
   ─────────────────────────────────────────────────────────────────────────*)

(* ── Cargar el paclet desde el directorio padre ──────────────────────── *)
PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* ── Directorio de tests ─────────────────────────────────────────────── *)
$testDir = DirectoryName[$InputFileName];

(* ── Layers disponibles ──────────────────────────────────────────────── *)
$layers = <|
  "all"     -> {"SessionFSM", "SessionStore", "Flow", "NestScheduler", "Database", "UXColorRules"},
  "session" -> {"SessionFSM", "SessionStore"},
  "flow"    -> {"Flow", "NestScheduler"},
  "db"      -> {"Database"},
  "models"  -> {"Flow", "NestScheduler", "Database"},
  "ux"      -> {"UXColorRules"}
|>;

(* ── Parsear --layer desde la linea de comandos ─────────────────────── *)
(*   Formas aceptadas:
       --layer session       (dos tokens separados)
       --layer=session       (un token con signo igual)            *)
$layerArg = Module[{args = $ScriptCommandLine, idx},
  With[{eq = SelectFirst[args, StringMatchQ[#, "--layer=" ~~ __] &, None]},
    If[eq =!= None,
      StringDrop[eq, 8],
      idx = Position[args, "--layer"];
      If[idx =!= {} && Length[args] >= Last[Flatten[idx]] + 1,
        args[[ Last[Flatten[idx]] + 1 ]],
        None]]]];

$layerName = If[$layerArg === None, "all", ToLowerCase[$layerArg]];

$suites = Lookup[$layers, $layerName,
  (Print["[WARN] Layer desconocida: '" <> $layerName <>
         "'. Disponibles: " <> StringRiffle[Keys[$layers], ", "]];
   $layers["all"])];

(* ── Ejecutar cada suite ─────────────────────────────────────────────── *)
Print[""];
Print["╭" <> StringRepeat["─", 58] <> "╮"];
Print["  PersonalSite — TestReport"];
Print["  Layer : " <> $layerName <> "  " <>
      "(" <> StringRiffle[$suites, ", "] <> ")"];
Print["╰" <> StringRepeat["─", 58] <> "╯"];
Print[""];

$results   = <||>;
$overallOk = True;

Scan[
  Function[suite,
    Module[{file, report, pass, fail, err, total},
      file = FileNameJoin[{$testDir, suite <> ".wlt"}];

      If[! FileExistsQ[file],
        Print["[SKIP] " <> suite <> " — archivo no encontrado"];
        Return[]
      ];

      Print["──── " <> suite <> " ────────────────────────────────────"];

      report = Quiet @ Check[
        TestReport[file],
        $Failed
      ];

      If[report === $Failed,
        Print["  [ERROR] No se pudo cargar el archivo de tests."];
        $overallOk = False;
        AssociateTo[$results, suite -> <|"pass"->0, "fail"->0, "error"->1|>];
        Return[]
      ];

      pass  = report["TestsSucceededCount"] /. _Missing -> 0;
      fail  = report["TestsFailedCount"]     /. _Missing -> 0;
      err   = report["TestsErroredCount"]    /. _Missing -> 0;
      total = report["TestsEvaluatedCount"]  /. _Missing -> (pass + fail + err);

      AssociateTo[$results, suite ->
        <|"pass" -> pass, "fail" -> fail, "error" -> err, "total" -> total|>];

      If[(fail > 0 || err > 0) && !MatchQ[fail, _Missing],
        $overallOk = False];

      (* Detalles de los fallos *)
      If[fail > 0 || err > 0,
        Print["  FAIL: " <> ToString[fail] <> "  ERROR: " <> ToString[err]];
        With[{trs = report["TestResults"]},
          If[AssociationQ[trs] || ListQ[trs],
            Scan[
              Function[tr,
                If[tr["Outcome"] =!= "Success",
                  Print["    [" <> tr["Outcome"] <> "] " <> ToString[tr["TestID"]]];
                  Print["      Esperado: " <> ToString[tr["ExpectedOutput"]]];
                  Print["      Obtenido: " <> ToString[tr["ActualOutput"]]]]],
              If[AssociationQ[trs], Values[trs], trs]]]]
      ];

      Print["  Passed: " <> ToString[pass] <>
            "  /  Total: " <> ToString[total]];
      Print[""]
    ]
  ],
  $suites
];

(* ── Resumen global ───────────────────────────────────────────────────── *)
Print["══════════════════════════════════════════════════════════════"];
With[{
  totalPass  = Total[Values[$results[[All, "pass"  ]]]],
  totalFail  = Total[Values[$results[[All, "fail"  ]]]],
  totalErr   = Total[Values[$results[[All, "error" ]]]],
  totalTests = Total[Values[$results[[All, "total" ]]]]
},
  Print["  TOTAL PASSED : " <> ToString[totalPass]];
  Print["  TOTAL FAILED : " <> ToString[totalFail]];
  Print["  TOTAL ERRORS : " <> ToString[totalErr]];
  Print["  GRAND TOTAL  : " <> ToString[totalTests]];
  Print["══════════════════════════════════════════════════════════════"];
  Print[If[$overallOk, "  ALL TESTS PASSED", "  SOME TESTS FAILED"]];
  Print[""]
];

If[! $overallOk, Exit[1], Exit[0]]
