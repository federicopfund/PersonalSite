(* ::Package:: *)

(* PersonalSite`DevOps`
   --------------------------------------------------------------------------
   Pipeline DevOps como SchedulerTasks WL.

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

End[];
EndPackage[];
