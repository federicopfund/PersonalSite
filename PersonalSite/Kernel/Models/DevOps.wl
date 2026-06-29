(* PersonalSite`DevOps`
   --------------------------------------------------------------------------
   Pipeline DevOps como SchedulerTasks WL.
   Arquitectura híbrida:
     - Operaciones nativas (SyntaxQ, URLRead, docker ps): corren en el
       kernel WolframWebEngine directamente.
     - Operaciones de host (git, paclet build, test runner): delegadas al
       DevOps Bridge HTTP (tools/devops_bridge.py) escuchando en
       http://172.18.0.1:8091 desde el devcontainer.

   Arrancar el bridge (devcontainer):
     python3 tools/devops_bridge.py &

   17 etapas distribuidas en 8 niveles del DAG:
   L0: code-lint       git-status
   L1: test-run        git-diff
   L2: test-report     paclet-clean
   L3: paclet-build    git-stage
   L4: paclet-verify   docker-build     git-commit
   L5: docker-verify   git-push
   L6: smoke-test      deploy-notify
   L7: perf-check      changelog-gen
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`DevOps`"];

codeLint::usage    = "codeLint[] SyntaxQ-check de todos los .wl en Kernel/.";
runTests::usage    = "runTests[] ejecuta la suite de tests via bridge.";
testReport::usage  = "testReport[] snapshot del ultimo commit + timestamp.";
gitStatus::usage   = "gitStatus[] git status --porcelain via bridge.";
gitDiff::usage     = "gitDiff[] git diff --stat HEAD via bridge.";
pacletClean::usage = "pacletClean[] elimina build/*.paclet via bridge.";
pacletBuild::usage = "pacletBuild[] llama build_paclet.py via bridge.";
pacletVerify::usage= "pacletVerify[] verifica .paclet en build/ via bridge.";
dockerBuild::usage = "dockerBuild[] docker build via bridge.";
dockerVerify::usage= "dockerVerify[] docker ps profile-web-1 (nativo).";
gitStage::usage    = "gitStage[] git add -A via bridge.";
gitCommit::usage   = "gitCommit[] git commit auto via bridge.";
gitPush::usage     = "gitPush[] git push origin main via bridge.";
smokeTest::usage   = "smokeTest[] HTTP GET localhost:18000 (nativo).";
deployNotify::usage= "deployNotify[] registra evento deploy.";
perfCheck::usage   = "perfCheck[] latencia promedio 3 probes (nativo).";
changelogGen::usage= "changelogGen[] git log -10 via bridge.";
bridgeHealth::usage= "bridgeHealth[] verifica que el DevOps Bridge responde.";

Begin["`Private`"];

(* ── Bridge endpoint ─────────────────────────────────────────────── *)
$bridgeBase = "http://172.18.0.1:8091";
$appRoot    = "/app/PersonalSite";

(* ImportString["JSON"] devuelve lista de Rules, no Association:
   convertimos con Association @ ...                               *)
parseJSON[body_String] :=
  Module[{parsed},
    parsed = Quiet @ Check[ImportString[body, "JSON"], $Failed];
    Which[
      AssociationQ[parsed],  parsed,
      ListQ[parsed],         Association[parsed],
      True,                  $Failed]];

(* POST al bridge — falla en 5s si no responde, nunca bloquea el kernel *)
bridge[path_String] :=
  Module[{url, body},
    url  = $bridgeBase <> path;
    body = Quiet @ Check[
      TimeConstrained[
        URLRead[HTTPRequest[url, <|"Method" -> "POST", "Body" -> ""|>], "Body"],
        5,       (* max 5 segundos — no bloquear el kernel pool *)
        $Failed],
      $Failed];
    If[!StringQ[body],
      Return[<|"ok" -> False, "err" -> "bridge unavailable (timeout/down)"|>]];
    With[{p = parseJSON[body]},
      If[AssociationQ[p], p,
        <|"ok" -> False, "err" -> "parse error", "raw" -> StringTake[body, UpTo[200]]|>]]];

(* ── bridgeHealth ────────────────────────────────────────────────── *)
PersonalSite`DevOps`bridgeHealth[] :=
  Module[{body},
    body = Quiet @ Check[
      TimeConstrained[
        URLRead[HTTPRequest[$bridgeBase <> "/health"], "Body"],
        5, $Failed],
      $Failed];
    If[!StringQ[body],
      Return[<|"ok" -> False, "status" -> "bridge down — run: make bridge"|>]];
    With[{p = parseJSON[body]},
      If[AssociationQ[p],
        <|p, "status" -> "bridge ok"|>,
        <|"ok" -> False, "status" -> "parse error"|>]]];

(* ── L0 · code-lint (nativo — SyntaxQ en /app/PersonalSite/Kernel) ── *)
PersonalSite`DevOps`codeLint[] :=
  Module[{kernelDir, files, bad},
    kernelDir = FileNameJoin[{$appRoot, "Kernel"}];
    files = If[DirectoryQ[kernelDir], FileNames["*.wl", kernelDir, Infinity], {}];
    bad = Select[files,
            Function[f, Quiet @ Check[!SyntaxQ[ReadString[f]], True]]];
    <|"ok"    -> (bad === {}),
      "total" -> Length[files],
      "bad"   -> Length[bad],
      "files" -> FileNameTake /@ bad|>];

(* ── L0 · git-status ────────────────────────────────────────────── *)
PersonalSite`DevOps`gitStatus[] := bridge["/git/status"];

(* ── L1 · test-run ─────────────────────────────────────────────── *)
PersonalSite`DevOps`runTests[]  := bridge["/test/run"];

(* ── L1 · git-diff ──────────────────────────────────────────────── *)
PersonalSite`DevOps`gitDiff[]   := bridge["/git/diff"];

(* ── L2 · test-report ───────────────────────────────────────────── *)
PersonalSite`DevOps`testReport[] :=
  Module[{r},
    r = bridge["/git/log"];
    <|"ok"     -> TrueQ[r["ok"]],
      "commit" -> StringTake[ToString[Lookup[r, "log", ""]], UpTo[80]],
      "ts"     -> DateString["ISODateTime"]|>];

(* ── L2 · paclet-clean ──────────────────────────────────────────── *)
PersonalSite`DevOps`pacletClean[] := bridge["/build/clean"];

(* ── L3 · paclet-build ─────────────────────────────────────────── *)
PersonalSite`DevOps`pacletBuild[] := bridge["/build/paclet"];

(* ── L3 · git-stage ─────────────────────────────────────────────── *)
PersonalSite`DevOps`gitStage[]  := bridge["/git/stage"];

(* ── L4 · paclet-verify ─────────────────────────────────────────── *)
PersonalSite`DevOps`pacletVerify[] := bridge["/build/verify"];

(* ── L4 · docker-build (via bridge — docker disponible en host) ─── *)
PersonalSite`DevOps`dockerBuild[] := bridge["/docker/verify"];

(* ── L4 · git-commit ────────────────────────────────────────────── *)
PersonalSite`DevOps`gitCommit[] := bridge["/git/commit"];

(* ── L5 · docker-verify (nativo — detecta container desde dentro) ── *)
PersonalSite`DevOps`dockerVerify[] :=
  Module[{r},
    (* Llama a /docker/verify en bridge — docker CLI en el host *)
    r = bridge["/docker/verify"];
    If[TrueQ[r["ok"]], r,
      (* fallback nativo: HTTP health check propio *)
      Module[{t0, code},
        t0   = AbsoluteTime[];
        code = Quiet @ Check[
          URLRead[HTTPRequest["http://localhost:18000/"], "StatusCode"], -1];
        <|"ok" -> TrueQ[code === 200],
          "status" -> If[TrueQ[code===200], "Up (self-check)", "Down"],
          "latencyMs" -> Round[(AbsoluteTime[]-t0)*1000, 0.1]|>]]];

(* ── L5 · git-push ──────────────────────────────────────────────── *)
PersonalSite`DevOps`gitPush[]   := bridge["/git/push"];

(* ── L6 · smoke-test (nativo — URLRead desde container) ─────────── *)
PersonalSite`DevOps`smokeTest[] :=
  Module[{t0, code, ms},
    t0   = AbsoluteTime[];
    code = Quiet @ Check[
      URLRead[HTTPRequest["http://localhost:18000/"], "StatusCode"], -1];
    ms   = Round[(AbsoluteTime[] - t0) * 1000, 0.1];
    <|"ok" -> TrueQ[code === 200], "statusCode" -> code, "latencyMs" -> ms|>];

(* ── L6 · deploy-notify ─────────────────────────────────────────── *)
PersonalSite`DevOps`deployNotify[] :=
  Module[{r},
    r = bridge["/git/log"];
    <|"ok"    -> True,
      "event" -> "deploy",
      "commit"-> StringTake[ToString[Lookup[r,"log",""]], UpTo[80]],
      "ts"    -> DateString["ISODateTime"]|>];

(* ── L7 · perf-check (nativo — 3 probes de latencia) ───────────── *)
PersonalSite`DevOps`perfCheck[] :=
  Module[{times},
    times = Table[
      Module[{t0 = AbsoluteTime[]},
        Quiet @ URLRead[HTTPRequest["http://localhost:18000/"], "StatusCode"];
        Round[(AbsoluteTime[] - t0) * 1000, 0.1]],
      {3}];
    <|"ok"      -> True,
      "samples" -> times,
      "avgMs"   -> Round[Mean[times], 0.1],
      "minMs"   -> Min[times],
      "maxMs"   -> Max[times]|>];

(* ── L7 · changelog-gen ─────────────────────────────────────────── *)
PersonalSite`DevOps`changelogGen[] :=
  Module[{r},
    r = bridge["/git/log"];
    <|"ok"  -> TrueQ[r["ok"]],
      "log" -> ToString[Lookup[r, "log", ""]],
      "ts"  -> DateString["ISODateTime"]|>];

End[];
EndPackage[];

   17 etapas distribuidas en 8 niveles del DAG:

   L0 (roots) : code-lint          git-status
   L1          : test-run           git-diff
   L2          : test-report        paclet-clean
   L3          : paclet-build       git-stage
   L4          : paclet-verify      docker-build     git-commit
   L5          : docker-verify      git-push
   L6          : smoke-test         deploy-notify
   L7          : perf-check         changelog-gen

   Cada funcion devuelve una Association JSON-serializable con:
     "ok"    -> True/False
     (campos adicionales segun la etapa)
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`DevOps`"];

(* ── API publica ─────────────────────────────────────────────────────── *)
codeLint::usage    = "codeLint[] SyntaxQ-check de todos los .wl en Kernel/.";
runTests::usage    = "runTests[] ejecuta la suite de tests via python tools/test_tasks.py.";
testReport::usage  = "testReport[] snapshot del ultimo commit + timestamp.";
gitStatus::usage   = "gitStatus[] RunProcess git status --porcelain.";
gitDiff::usage     = "gitDiff[] RunProcess git diff --stat HEAD.";
pacletClean::usage = "pacletClean[] elimina build/*.paclet antes de rebuild.";
pacletBuild::usage = "pacletBuild[] llama tools/build_paclet.py.";
pacletVerify::usage= "pacletVerify[] verifica que exista un .paclet en build/.";
dockerBuild::usage = "dockerBuild[] docker build -t personalsite:latest.";
dockerVerify::usage= "dockerVerify[] docker ps --filter name=profile-web-1.";
gitStage::usage    = "gitStage[] git add -A.";
gitCommit::usage   = "gitCommit[] git commit -m auto:<iso>.";
gitPush::usage     = "gitPush[] git push origin main.";
smokeTest::usage   = "smokeTest[] HTTP GET localhost:18000 health check.";
deployNotify::usage= "deployNotify[] registra evento de deploy con timestamp.";
perfCheck::usage   = "perfCheck[] mide latencia de GET / en ms.";
changelogGen::usage= "changelogGen[] git log --oneline -10 como changelog.";

Begin["`Private`"];

(* ── Configuracion de rutas ─────────────────────────────────────────── *)
$root    = "/app";
$appRoot = FileNameJoin[{$root, "PersonalSite"}];

(* ── Helper: ejecutar proceso externo ───────────────────────────────── *)
run[args_List] :=
  Module[{r},
    r = RunProcess[args, All, ProcessDirectory -> $root];
    <|"exit" -> r["ExitCode"],
      "out"  -> StringTake[r["StandardOutput"],  UpTo[600]],
      "err"  -> StringTake[r["StandardError"],   UpTo[300]]|>];

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
PersonalSite`DevOps`runTests[] :=
  Module[{r},
    r = run[{"python3",
             FileNameJoin[{$root, "tools", "test_tasks.py"}]}];
    <|"ok"   -> (r["exit"] === 0),
      "exit" -> r["exit"],
      "out"  -> r["out"]|>];

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
    r  = Quiet @ URLRead[
           HTTPRequest["http://localhost:18000/"],
           {"StatusCode"}];
    ms   = Round[(AbsoluteTime[] - t0) * 1000, 0.1];
    code = Quiet @ Check[r["StatusCode"], -1];
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
  Module[{times, t0, r, ms},
    times = Table[
      (t0 = AbsoluteTime[];
       Quiet @ URLRead[HTTPRequest["http://localhost:18000/"], "StatusCode"];
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
    result = Quiet @ Check[fn[], <|"ok"->False, "err"->"exception in stage"|>];
    ms     = Round[(AbsoluteTime[] - t0) * 1000, 0.1];
    result = <|result, "stage"->name, "ms"->ms, "ts"->DateString["ISODateTime"]|>;
    $devopsResults[name] = result;
    result];

(* ── stageResults[] — snapshot de todos los resultados ─────────── *)
PersonalSite`DevOps`stageResults[] := $devopsResults;
End[];
EndPackage[];
