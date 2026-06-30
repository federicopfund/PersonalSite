
(* PersonalSite`DevOps`
   --------------------------------------------------------------------------
   Pipeline DevOps — 17 etapas en 8 niveles (L0–L7).
   Implementacion RunProcess/URLRead nativa (container).

   L0: code-lint       git-status
   L1: test-run        git-diff
   L2: test-report     paclet-clean
   L3: paclet-build    git-stage
   L4: paclet-verify   docker-build     git-commit
   L5: docker-verify   git-push
   L6: smoke-test      deploy-notify
   L7: perf-check      changelog-gen

   Cada funcion devuelve <|"ok"->True/False, ...|> JSON-serializable.
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`DevOps`"];

(* ── API publica ─────────────────────────────────────────────────────── *)
codeLint::usage      = "codeLint[] SyntaxQ-check de todos los .wl en Kernel/.";
runTests::usage      = "runTests[] | runTests[layer] ejecuta los WLT tests.\nLayers: all (default), session, flow, ux, db, models.";  
testReport::usage    = "testReport[] snapshot del ultimo commit + timestamp.";
gitStatus::usage     = "gitStatus[] RunProcess git status --porcelain.";
gitDiff::usage       = "gitDiff[] RunProcess git diff --stat HEAD.";
pacletClean::usage   = "pacletClean[] elimina build/*.paclet antes de rebuild.";
pacletBuild::usage   = "pacletBuild[] llama tools/build_paclet.py.";
pacletVerify::usage  = "pacletVerify[] verifica que exista un .paclet en build/.";
dockerBuild::usage   = "dockerBuild[] docker build -t personalsite:latest.";
dockerVerify::usage  = "dockerVerify[] docker ps --filter name=profile-web-1.";
gitStage::usage      = "gitStage[] git add -A.";
gitCommit::usage     = "gitCommit[] git commit -m auto:<iso>.";
gitPush::usage       = "gitPush[] git push origin main.";
smokeTest::usage     = "smokeTest[] HTTP GET localhost:18000 health check.";
deployNotify::usage  = "deployNotify[] registra evento de deploy con timestamp.";
perfCheck::usage     = "perfCheck[] mide latencia de GET / en ms.";
changelogGen::usage  = "changelogGen[] git log --oneline -10 como changelog.";
toFlowSpec::usage    = "toFlowSpec[] convierte $devopsStages en un Flow spec (deps+action).";
runPipeline::usage   = "runPipeline[] ejecuta el pipeline completo via Flow.run con paralelismo topologico.";
trajectory::usage    = "trajectory[n] aplica NestList[runPipeline, state0, n] — trayectoria Ruliad.";
saveRun::usage       = "saveRun[stepNum, state] persiste un paso del pipeline en pipeline_runs (Warehouse).";
pipelineHistory::usage = "pipelineHistory[n] retorna los ultimos n runs desde el Warehouse (SQLite).";

Begin["`Private`"];

(* ── Configuracion de rutas ─────────────────────────────────────────── *)
$root    = "/app";
$appRoot = FileNameJoin[{$root, "PersonalSite"}];

(* ── Helper: ejecutar proceso externo ───────────────────────────────── *)
run[args_List] :=
  Module[{r},
    r = TimeConstrained[
          RunProcess[args, All, ProcessDirectory -> $root],
          30,
          <|"ExitCode" -> -1, "StandardOutput" -> "",
            "StandardError" -> "process timeout (30s)"|>];
    <|"exit" -> Lookup[r, "ExitCode", -1],
      "out"  -> StringTake[Lookup[r, "StandardOutput",  ""], UpTo[600]],
      "err"  -> StringTake[Lookup[r, "StandardError",   ""], UpTo[300]]|>];

runQ[args_List] := run[args]["exit"] === 0;

(* ── L0 · code-lint ─────────────────────────────────────────────────── *)
(*  SyntaxQ check de cada archivo .wl en Kernel/                         *)
PersonalSite`DevOps`codeLint[] :=
  Module[{kernelDir, files, bad},
    kernelDir = FileNameJoin[{$appRoot, "Kernel"}];
    files = If[DirectoryQ[kernelDir],
              FileNames["*.wl", kernelDir, Infinity], {}];
    bad = Select[files,
            Function[f,
              Quiet @ Check[
                !SyntaxQ[ReadString[f]],
                True  (* si falla el read, cuenta como error *)
              ]]];
    <|"ok"    -> (bad === {}),
      "total" -> Length[files],
      "bad"   -> Length[bad],
      "files" -> FileNameTake /@ bad|>];

(* ── L0 · git-status ────────────────────────────────────────────────── *)
PersonalSite`DevOps`gitStatus[] :=
  Module[{r, lines},
    r = run[{"git", "-C", $root, "status", "--porcelain"}];
    lines = Select[StringSplit[r["out"], "\n"], StringLength[#] > 0 &];
    <|"ok"      -> (r["exit"] === 0),
      "changed" -> Length[lines],
      "raw"     -> StringTake[r["out"], UpTo[400]]|>];

(* ── L1 · test-run ───────────────────────────────────────────────────── *)
(*  Ejecuta TestReport[file] nativamente en el kernel actual —            *)
(*  sin spawning de subprocesos, compatible con la licencia free.         *)
(*  Layers: "all" (default), "session", "flow", "ux", "db", "models"      *)

$testDir = FileNameJoin[{$appRoot, "Tests"}];

$testLayers = <|
  "all"     -> {"SessionFSM", "SessionStore", "Flow",
                "NestScheduler", "Database", "UXColorRules"},
  "session" -> {"SessionFSM", "SessionStore"},
  "flow"    -> {"Flow", "NestScheduler"},
  "db"      -> {"Database"},
  "models"  -> {"Flow", "NestScheduler", "Database"},
  "ux"      -> {"UXColorRules"}
|>;

PersonalSite`DevOps`runTests[] :=
  PersonalSite`DevOps`runTests["all"];

PersonalSite`DevOps`runTests[layer_String] :=
  Module[{suites, results, totalPass, totalFail, totalErr, totalTests, allOk},
    If[!KeyExistsQ[$testLayers, layer],
      Return[<|"ok"    -> False,
               "layer" -> layer,
               "err"   -> "unknown layer '" <> layer <>
                          "' — valid: " <>
                          StringRiffle[Keys[$testLayers], ", "]|>]];
    suites = $testLayers[layer];
    results = Association @ Map[
      Function[suite,
        Module[{file, report, pass, fail, err, total},
          file = FileNameJoin[{$testDir, suite <> ".wlt"}];
          If[!FileExistsQ[file],
            suite -> <|"ok"->False, "pass"->0, "fail"->0,
                       "error"->1, "total"->0,
                       "err"->"file not found: " <> file|>,
            report = Quiet[TestReport[file]];
            If[Head[report] =!= TestReportObject,
              suite -> <|"ok"->False, "pass"->0, "fail"->0,
                         "error"->1, "total"->0,
                         "err"->"TestReport failed"|>,
              pass  = report["TestsSucceededCount"] /. _Missing -> 0;
              fail  = report["TestsFailedCount"]    /. _Missing -> 0;
              err   = report["TestsErroredCount"]   /. _Missing -> 0;
              total = report["TestsEvaluatedCount"] /. _Missing -> (pass+fail+err);
              suite -> <|"ok"    -> (fail === 0 && err === 0),
                         "pass"  -> pass,
                         "fail"  -> fail,
                         "error" -> err,
                         "total" -> total|>]]]],
      suites];
    totalPass  = Total[Lookup[#, "pass",  0] & /@ Values[results]];
    totalFail  = Total[Lookup[#, "fail",  0] & /@ Values[results]];
    totalErr   = Total[Lookup[#, "error", 0] & /@ Values[results]];
    totalTests = Total[Lookup[#, "total", 0] & /@ Values[results]];
    allOk      = AllTrue[Values[results], Function[r, TrueQ[r["ok"]]]];
    <|"ok"          -> allOk,
      "layer"       -> layer,
      "suites"      -> results,
      "totalPass"   -> totalPass,
      "totalFail"   -> totalFail,
      "totalErrors" -> totalErr,
      "totalTests"  -> totalTests,
      "ts"          -> DateString["ISODateTime"]|>];

(* ── L1 · git-diff ──────────────────────────────────────────────────── *)
PersonalSite`DevOps`gitDiff[] :=
  Module[{r},
    r = run[{"git", "-C", $root, "diff", "--stat", "HEAD"}];
    <|"ok"   -> (r["exit"] === 0),
      "stat" -> StringTake[r["out"] <> r["err"], UpTo[500]]|>];

(* ── L2 · test-report ───────────────────────────────────────────────── *)
PersonalSite`DevOps`testReport[] :=
  Module[{r},
    r = run[{"git", "-C", $root, "log", "--oneline", "-1"}];
    <|"ok"     -> (r["exit"] === 0),
      "commit" -> StringTrim[r["out"]],
      "ts"     -> DateString["ISODateTime"]|>];

(* ── L2 · paclet-clean ──────────────────────────────────────────────── *)
PersonalSite`DevOps`pacletClean[] :=
  Module[{buildDir, files},
    buildDir = FileNameJoin[{$root, "build"}];
    files = If[DirectoryQ[buildDir],
              FileNames["*.paclet", buildDir], {}];
    Quiet @ Scan[DeleteFile, files];
    <|"ok"      -> True,
      "deleted" -> Length[files]|>];

(* ── L3 · paclet-build ─────────────────────────────────────────────── *)
PersonalSite`DevOps`pacletBuild[] :=
  Module[{r, built},
    r = run[{"python3",
             FileNameJoin[{$root, "tools", "build_paclet.py"}]}];
    built = If[DirectoryQ[FileNameJoin[{$root, "build"}]],
              FileNames["*.paclet", FileNameJoin[{$root, "build"}]], {}];
    <|"ok"     -> (r["exit"] === 0),
      "exit"   -> r["exit"],
      "paclet" -> If[built =!= {}, FileNameTake[Last[built]], "none"],
      "out"    -> StringTake[r["out"], UpTo[400]]|>];

(* ── L3 · git-stage ─────────────────────────────────────────────────── *)
PersonalSite`DevOps`gitStage[] :=
  Module[{r},
    r = run[{"git", "-C", $root, "add", "-A"}];
    <|"ok"   -> (r["exit"] === 0),
      "exit" -> r["exit"]|>];

(* ── L4 · paclet-verify ─────────────────────────────────────────────── *)
PersonalSite`DevOps`pacletVerify[] :=
  Module[{buildDir, files, f},
    buildDir = FileNameJoin[{$root, "build"}];
    files = If[DirectoryQ[buildDir],
              FileNames["*.paclet", buildDir], {}];
    If[files === {},
      Return[<|"ok" -> False, "err" -> "no .paclet in build/"|>]];
    f = Last[files];
    <|"ok"    -> True,
      "file"  -> FileNameTake[f],
      "bytes" -> FileByteCount[f]|>];

(* ── L4 · docker-build ──────────────────────────────────────────────── *)
PersonalSite`DevOps`dockerBuild[] :=
  Module[{dockerfile, r},
    dockerfile = FileNameJoin[{$appRoot, "deploy", "Dockerfile"}];
    r = run[{"docker", "build",
             "-t", "personalsite:latest",
             "-f", dockerfile,
             $root}];
    <|"ok"   -> (r["exit"] === 0),
      "exit" -> r["exit"],
      "err"  -> StringTake[r["err"], UpTo[300]]|>];

(* ── L4 · git-commit ────────────────────────────────────────────────── *)
PersonalSite`DevOps`gitCommit[] :=
  Module[{msg, r},
    msg = "auto: devops pipeline deploy " <> DateString["ISODateTime"];
    r = run[{"git", "-C", $root, "commit",
             "-m", msg, "--allow-empty"}];
    <|"ok"   -> (r["exit"] === 0),
      "msg"  -> msg,
      "out"  -> StringTake[r["out"] <> r["err"], UpTo[300]]|>];

(* ── L5 · docker-verify ─────────────────────────────────────────────── *)
PersonalSite`DevOps`dockerVerify[] :=
  Module[{r},
    r = run[{"docker", "ps",
             "--filter", "name=profile-web-1",
             "--format", "{{.Status}}"}];
    <|"ok"     -> StringContainsQ[r["out"], "Up"],
      "status" -> StringTrim[r["out"]]|>];

(* ── L5 · git-push ──────────────────────────────────────────────────── *)
PersonalSite`DevOps`gitPush[] :=
  Module[{r},
    r = run[{"git", "-C", $root, "push", "origin", "main"}];
    <|"ok"   -> (r["exit"] === 0),
      "exit" -> r["exit"],
      "out"  -> StringTake[r["out"] <> r["err"], UpTo[400]]|>];

(* ── L6 · smoke-test ────────────────────────────────────────────────── *)
PersonalSite`DevOps`smokeTest[] :=
  Module[{t0, r, ms, code},
    t0 = AbsoluteTime[];
    r  = Quiet @ Check[
           TimeConstrained[
             URLRead[HTTPRequest["http://127.0.0.1:18000/"], "StatusCode"],
             5, -1],
           -1];
    ms   = Round[(AbsoluteTime[] - t0) * 1000, 0.1];
    code = If[IntegerQ[r], r, -1];
    <|"ok"        -> TrueQ[code === 200],
      "statusCode"-> code,
      "latencyMs" -> ms|>];

(* ── L6 · deploy-notify ─────────────────────────────────────────────── *)
PersonalSite`DevOps`deployNotify[] :=
  Module[{r},
    r = run[{"git", "-C", $root, "log", "--oneline", "-1"}];
    <|"ok"     -> True,
      "event"  -> "deploy",
      "commit" -> StringTrim[r["out"]],
      "ts"     -> DateString["ISODateTime"]|>];

(* ── L7 · perf-check ────────────────────────────────────────────────── *)
PersonalSite`DevOps`perfCheck[] :=
  Module[{times, t0, ms},
    times = Table[
      (t0 = AbsoluteTime[];
       Quiet @ Check[
         TimeConstrained[URLRead[HTTPRequest["http://127.0.0.1:18000/"], "StatusCode"], 5, -1],
         -1];
       Round[(AbsoluteTime[] - t0) * 1000, 0.1]),
      {3}];
    <|"ok"      -> True,
      "samples" -> times,
      "avgMs"   -> Round[Mean[times], 0.1],
      "minMs"   -> Min[times],
      "maxMs"   -> Max[times]|>];

(* ── L7 · changelog-gen ─────────────────────────────────────────────── *)
PersonalSite`DevOps`changelogGen[] :=
  Module[{r, logR, authors},
    r     = run[{"git", "-C", $root, "log", "--oneline", "-10"}];
    logR  = run[{"git", "-C", $root, "log",
                 "--pretty=format:%s", "-10"}];
    <|"ok"     -> (r["exit"] === 0),
      "log"    -> StringTrim[r["out"]],
      "msgs"   -> StringSplit[logR["out"], "\n"],
      "ts"     -> DateString["ISODateTime"]|>];

(* ══════════════════════════════════════════════════════════════════════
   FASE 1 — Flow integration + Ruliad NestList Warehouse
   ══════════════════════════════════════════════════════════════════════

   toFlowSpec[]      → spec para PersonalSite`Flow`run (deps + action)
   runPipeline[]     → ejecuta con paralelismo topologico via Flow.run
   trajectory[n]     → NestList sobre runPipeline → trayectoria Ruliad
   saveRun[n,state]  → persiste step en pipeline_runs (SQLite Warehouse)
   pipelineHistory[] → query los ultimos N runs
   ────────────────────────────────────────────────────────────────────── *)

(* ── toFlowSpec[] ─────────────────────────────────────────────────── *)
(*  Convierte $devopsStages en Association{id -> <|deps, action|>}    *)
(*  compatible con PersonalSite`Flow`run.                             *)
PersonalSite`DevOps`toFlowSpec[] :=
  Association @ Map[
    Function[s,
      With[{id = s[[1]], deps = s[[5]]},
        id -> <|
          "deps"   -> deps,
          "action" -> With[{stageId = id},
            Function[prev, PersonalSite`DevOps`runStage[stageId]]]
        |>]],
    $devopsStages];

(* ── runPipeline[] ────────────────────────────────────────────────── *)
(*  Ejecuta el pipeline completo via Flow.run ("session" backend).    *)
(*  Las etapas independientes de cada nivel topologico corren en      *)
(*  paralelo como TaskObjects (SessionSubmit, timeout 60s por capa).  *)
PersonalSite`DevOps`runPipeline[] :=
  Module[{spec, t0, flowResult, allOk, ms, sha},
    spec       = PersonalSite`DevOps`toFlowSpec[];
    t0         = AbsoluteTime[];
    flowResult = Quiet @ Check[
      PersonalSite`Flow`run[spec, "session"],
      <||>];
    ms     = Round[(AbsoluteTime[] - t0) * 1000, 1];
    allOk  = AllTrue[
      Values[flowResult],
      Function[r, AssociationQ[r] && TrueQ[r["ok"]]]];
    sha    = Quiet @ Check[
      StringTrim[run[{"git", "-C", $root, "log", "--format=%h", "-1"}]["out"]],
      "unknown"];
    $devopsResults = Association @ KeyValueMap[
      Function[{k, v}, k -> If[AssociationQ[v], v, <|"ok"->False|>]],
      flowResult];
    <|"ok"          -> allOk,
      "sha"         -> sha,
      "stageResults"-> flowResult,
      "elapsedMs"   -> ms,
      "ts"          -> DateString["ISODateTime"],
      "layerCount"  -> (Max[#[[4]] & /@ $devopsStages] + 1)|>];

(* ── trajectory[n] ────────────────────────────────────────────────── *)
(*  NestList[step, state0, n]: genera lista de n+1 estados del        *)
(*  pipeline — trayectoria en el Ruliad de Wolfram.                   *)
$pipelineState0 = <|
  "ok"     -> True,
  "sha"    -> None,
  "runLog" -> {},
  "step"   -> 0,
  "ts"     -> None|>;

PersonalSite`DevOps`trajectory[n_Integer : 1] :=
  NestList[
    Function[state,
      Module[{res = PersonalSite`DevOps`runPipeline[]},
        <|state,
          "ok"     -> TrueQ[res["ok"]],
          "sha"    -> res["sha"],
          "runLog" -> Append[state["runLog"],
                       <|"step"  -> state["step"] + 1,
                         "ok"    -> TrueQ[res["ok"]],
                         "ms"    -> res["elapsedMs"],
                         "ts"    -> res["ts"]|>],
          "step"   -> state["step"] + 1,
          "ts"     -> res["ts"]|>]],
    $pipelineState0,
    n];

(* ── saveRun[stepNum, state] ──────────────────────────────────────── *)
(*  Persiste un paso del pipeline en pipeline_runs (Warehouse SQLite).*)
PersonalSite`DevOps`saveRun[stepNum_Integer, state_Association] :=
  Quiet @ Check[
    PersonalSite`Database`execute[
      "INSERT INTO pipeline_runs
         (sha, started_at, elapsed_ms, status, state_json, step_num)
       VALUES (?, ?, ?, ?, ?, ?)",
      {ToString[Lookup[state, "sha", ""]],
       ToString[Lookup[state, "ts", DateString["ISODateTime"]]],
       N @ With[{log = Lookup[state, "runLog", {}]},
             If[log =!= {}, Lookup[Last[log], "ms", 0], 0]],
       If[TrueQ[state["ok"]], "ok", "fail"],
       ExportString[KeyDrop[state, {"stageResults"}], "JSON"],
       stepNum}],
    $Failed];

(* ── pipelineHistory[n] ───────────────────────────────────────────── *)
(*  Retorna los ultimos n runs como lista de Associations.            *)
PersonalSite`DevOps`pipelineHistory[n_Integer : 20] :=
  Module[{rows},
    rows = Quiet @ Check[
      PersonalSite`Database`execute[
        "SELECT id, sha, started_at, elapsed_ms, status, step_num
           FROM pipeline_runs
          ORDER BY id DESC
          LIMIT ?",
        {n}],
      {}];
    Map[Function[r,
      If[ListQ[r] && Length[r] >= 6,
        <|"id"         -> r[[1]],
          "sha"        -> r[[2]],
          "started_at" -> r[[3]],
          "elapsed_ms" -> r[[4]],
          "status"     -> r[[5]],
          "step_num"   -> r[[6]]|>,
        r]],
      rows]];

(* ── bridge-health (nativo — ping al DevOps Bridge HTTP) ─────────── *)
PersonalSite`DevOps`bridgeHealth[] :=
  Module[{body, t0, ms},
    t0   = AbsoluteTime[];
    body = Quiet @ Check[
      TimeConstrained[
        URLRead[HTTPRequest["http://172.18.0.1:8091/health"], "Body"],
        5, $Failed],
      $Failed];
    ms = Round[(AbsoluteTime[] - t0) * 1000, 0.1];
    If[!StringQ[body],
      <|"ok" -> False, "ms" -> ms,
        "err" -> "bridge unavailable — run: python3 tools/devops_bridge.py &"|>,
      <|"ok" -> True, "ms" -> ms, "host" -> "172.18.0.1:8091"|>]];

(* ── DAG estático del pipeline DevOps ───────────────────────────────
   Cada stage: {id, label, group, level, deps}
   Levels siguen el esquema original de 8 capas (L0–L7).           *)
$devopsStages = {
  {"bridge-health", "Bridge Health",  "ops",   0, {}},
  {"code-lint",     "Code Lint",      "build", 0, {}},
  {"git-status",    "Git Status",     "git",   0, {}},
  {"test-run",      "Test Runner",    "test",  1, {"code-lint"}},
  {"git-diff",      "Git Diff",       "git",   1, {"git-status"}},
  {"test-report",   "Test Report",    "test",  2, {"test-run"}},
  {"paclet-clean",  "Paclet Clean",   "build", 2, {"git-diff"}},
  {"paclet-build",  "Paclet Build",   "build", 3, {"paclet-clean", "test-report"}},
  {"git-stage",     "Git Stage",      "git",   3, {"git-status", "git-diff"}},
  {"paclet-verify", "Paclet Verify",  "build", 4, {"paclet-build"}},
  {"docker-build",  "Docker Build",   "ops",   4, {"paclet-verify", "git-stage"}},
  {"git-commit",    "Git Commit",     "git",   4, {"git-stage", "test-report"}},
  {"docker-verify", "Docker Verify",  "ops",   5, {"docker-build"}},
  {"git-push",      "Git Push",       "git",   5, {"git-commit", "docker-verify"}},
  {"smoke-test",    "Smoke Test",     "ops",   6, {"git-push", "docker-verify"}},
  {"deploy-notify", "Deploy Notify",  "ops",   6, {"git-push"}},
  {"perf-check",    "Perf Check",     "ops",   7, {"smoke-test"}},
  {"changelog-gen", "Changelog Gen",  "git",   7, {"deploy-notify"}}};

(* Almacena el ultimo resultado de cada etapa (por kernel del pool). *)
If[! AssociationQ[$devopsResults], $devopsResults = <||>];

(* ── dag[] — estructura JSON del pipeline ───────────────────────── *)
PersonalSite`DevOps`dag[] :=
  Module[{nodes, edges},
    nodes = Map[Function[s,
      With[{id=s[[1]], lbl=s[[2]], grp=s[[3]], lv=s[[4]], deps=s[[5]]},
        <|"id"        -> id,
          "label"     -> lbl,
          "group"     -> grp,
          "level"     -> lv,
          "dag_order" -> lv,
          "deps"      -> deps,
          "lastResult"-> If[KeyExistsQ[$devopsResults, id],
                            $devopsResults[id], <||>]
        |>]],
      $devopsStages];
    edges = Flatten @ Map[Function[s,
      Map[Function[dep, <|"from"->dep, "to"->s[[1]]|>], s[[5]]]],
      $devopsStages];
    <|"nodes"      -> nodes,
      "edges"      -> edges,
      "stageCount" -> Length[$devopsStages]|>];

(* ── runStage[name] — despacha a la funcion correcta ───────────── *)
PersonalSite`DevOps`runStage[name_String] :=
  Module[{fn, t0, result, ms},
    fn = Switch[name,
      "bridge-health", PersonalSite`DevOps`bridgeHealth,
      "code-lint",     PersonalSite`DevOps`codeLint,
      "git-status",    PersonalSite`DevOps`gitStatus,
      "test-run",      PersonalSite`DevOps`runTests,
      "git-diff",      PersonalSite`DevOps`gitDiff,
      "test-report",   PersonalSite`DevOps`testReport,
      "paclet-clean",  PersonalSite`DevOps`pacletClean,
      "paclet-build",  PersonalSite`DevOps`pacletBuild,
      "git-stage",     PersonalSite`DevOps`gitStage,
      "paclet-verify", PersonalSite`DevOps`pacletVerify,
      "docker-build",  PersonalSite`DevOps`dockerBuild,
      "git-commit",    PersonalSite`DevOps`gitCommit,
      "docker-verify", PersonalSite`DevOps`dockerVerify,
      "git-push",      PersonalSite`DevOps`gitPush,
      "smoke-test",    PersonalSite`DevOps`smokeTest,
      "deploy-notify", PersonalSite`DevOps`deployNotify,
      "perf-check",    PersonalSite`DevOps`perfCheck,
      "changelog-gen", PersonalSite`DevOps`changelogGen,
      _, Null];
    If[fn === Null,
      Return[<|"ok"->False, "err"->"unknown stage: "<>name,
               "stage"->name, "ms"->0,
               "ts"->DateString["ISODateTime"]|>]];
    t0     = AbsoluteTime[];
    result = Quiet @ Check[
      TimeConstrained[fn[], 30, <|"ok"->False, "err"->"stage timeout (30s)"|>],
      <|"ok"->False, "err"->"exception in stage"|>];
    ms     = Round[(AbsoluteTime[] - t0) * 1000, 0.1];
    result = <|result, "stage"->name, "ms"->ms, "ts"->DateString["ISODateTime"]|>;
    $devopsResults[name] = result;
    result];

(* ── stageResults[] — snapshot de todos los resultados ─────────── *)
PersonalSite`DevOps`stageResults[] := $devopsResults;
End[];
EndPackage[];
