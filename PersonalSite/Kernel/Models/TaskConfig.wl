(* ::Package:: *)

(* PersonalSite`TaskConfig`
   --------------------------------------------------------------------------
   CRUD sobre la tabla `scheduler_tasks`.
   Persistencia de configuraciones de tareas: cada fila es un spec completo
   que el Scheduler puede leer para arrancar TaskObjects en runtime.

   dag_order : nivel en el NestGraph layered digraph
               (cardinalidad de la estructura ruliar)
               0 = raíz (sin deps), 1 = L1, 2 = L2, 3 = L3

   Contrato de la tabla:
     task_id, label, group_name, interval_s, enabled,
     deps (JSON array), dag_order, action_code,
     created_at, updated_at
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`TaskConfig`",
  {"PersonalSite`Database`"}];

all::usage =
  "all[] devuelve lista de Associations con todos los configs de la tabla.";

byId::usage =
  "byId[task_id] devuelve el config como Association, o $Failed.";

create::usage =
  "create[spec] inserta una nueva fila. spec debe tener task_id y label.";

update::usage =
  "update[task_id, key, value] actualiza un campo. Devuelve True o $Failed.";

delete::usage =
  "delete[task_id] elimina la fila. Devuelve True o $Failed.";

seedDefaults::usage =
  "seedDefaults[] inserta las 6 tareas del sistema si la tabla está vacía.";

Begin["`Private`"];

(* ── Serialización deps ──────────────────────────────────────────────── *)
parseDeps[s_String] :=
  Quiet @ Check[
    ToExpression[StringReplace[s, {"["->"{"," ]"->"}","["->"{","]"->"}"}]],
    {}];
parseDeps[_] := {};

(* JSON array de strings → WL List  e.g.  '["a","b"]' → {"a","b"} *)
depsFromJSON[s_String] :=
  Quiet @ Check[
    Cases[StringSplit[StringDelete[s, "["|"]"|"\""|" "], ","], _?(StringLength[#]>0 &)],
    {}];

(* WL List → JSON string  e.g.  {"a","b"} → '["a","b"]' *)
depsToJSON[deps_List] :=
  "[" <> StringRiffle[("\"" <> # <> "\"") & /@ deps, ","] <> "]";

(* ── Row → Association ───────────────────────────────────────────────── *)
rowToSpec[row_List] :=
  If[Length[row] < 10, $Failed,
    <|
      "task_id"     -> row[[1]],
      "label"       -> row[[2]],
      "group_name"  -> row[[3]],
      "interval_s"  -> If[IntegerQ[row[[4]]], row[[4]], 60],
      "enabled"     -> TrueQ[row[[5]] === 1 || row[[5]] === "1" || TrueQ[row[[5]]]],
      "deps"        -> depsFromJSON[ToString @ row[[6]]],
      "dag_order"   -> If[IntegerQ[row[[7]]], row[[7]],
                         Quiet @ Check[ToExpression[ToString @ row[[7]]], 0]],
      "action_code" -> ToString @ row[[8]],
      "created_at"  -> ToString @ row[[9]],
      "updated_at"  -> ToString @ row[[10]]
    |>];

(* ── CRUD ────────────────────────────────────────────────────────────── *)
all[] :=
  Module[{rows},
    rows = PersonalSite`Database`execute[
      "SELECT task_id, label, group_name, interval_s, enabled,
              deps, dag_order, action_code, created_at, updated_at
       FROM scheduler_tasks
       ORDER BY dag_order ASC, task_id ASC", {}];
    If[rows === $Failed || ! ListQ[rows], Return[{}]];
    DeleteCases[rowToSpec /@ rows, $Failed]
  ];

byId[taskId_String] :=
  Module[{rows},
    rows = PersonalSite`Database`execute[
      "SELECT task_id, label, group_name, interval_s, enabled,
              deps, dag_order, action_code, created_at, updated_at
       FROM scheduler_tasks WHERE task_id = ?", {taskId}];
    If[rows === $Failed || ! ListQ[rows] || Length[rows] === 0,
      Return[$Failed]];
    rowToSpec[First[rows]]
  ];

create[spec_Association] :=
  Module[{id, lbl, grp, iv, en, deps, order, code, r},
    id    = Lookup[spec, "task_id",    ""];
    lbl   = Lookup[spec, "label",      id];
    grp   = Lookup[spec, "group_name", "user"];
    iv    = Lookup[spec, "interval_s", 60];
    en    = If[TrueQ[Lookup[spec, "enabled", True]], 1, 0];
    deps  = depsToJSON[Lookup[spec, "deps", {}]];
    order = Lookup[spec, "dag_order",  0];
    code  = Lookup[spec, "action_code","Function[True]"];
    If[id === "", Return[$Failed]];
    (* Reject duplicates explicitly — INSERT OR IGNORE would silently succeed *)
    If[byId[id] =!= $Failed, Return[$Failed]];
    r = PersonalSite`Database`execute[
      "INSERT INTO scheduler_tasks
         (task_id, label, group_name, interval_s, enabled,
          deps, dag_order, action_code)
       VALUES (?,?,?,?,?,?,?,?)",
      {id, lbl, grp, iv, en, deps, order, code}];
    If[r === $Failed, $Failed, id]
  ];

(* Mapa de campo público → columna SQL *)
$colMap = <|
  "label"       -> "label",
  "group_name"  -> "group_name",
  "interval_s"  -> "interval_s",
  "enabled"     -> "enabled",
  "deps"        -> "deps",
  "dag_order"   -> "dag_order",
  "action_code" -> "action_code"
|>;

update[taskId_String, key_String, value_] :=
  Module[{col, val, r},
    col = Lookup[$colMap, key, ""];
    If[col === "", Return[$Failed]];
    (* Serializar deps si aplica *)
    val = If[key === "deps" && ListQ[value],
              depsToJSON[value],
              value];
    r = PersonalSite`Database`execute[
      "UPDATE scheduler_tasks SET " <> col <> " = ? WHERE task_id = ?",
      {val, taskId}];
    If[r === $Failed, $Failed, True]
  ];

delete[taskId_String] :=
  Module[{r},
    r = PersonalSite`Database`execute[
      "DELETE FROM scheduler_tasks WHERE task_id = ?", {taskId}];
    If[r === $Failed, $Failed, True]
  ];

(* ── Seed idempotente ────────────────────────────────────────────────── *)
$defaults = {
  <|"task_id"->"heartbeat",     "label"->"Heartbeat",
    "group_name"->"system","interval_s"->30,  "enabled"->True,
    "deps"->{},                       "dag_order"->0,
    "action_code"->"Function[True]"|>,
  <|"task_id"->"cache-warm",    "label"->"Cache warm-up",
    "group_name"->"system","interval_s"->300, "enabled"->True,
    "deps"->{"heartbeat"},            "dag_order"->1,
    "action_code"->"Function[Quiet@Check[PersonalSite`Post`recent[10];True,False]]"|>,
  <|"task_id"->"theme-rotate",  "label"->"Theme rotation tick",
    "group_name"->"theme", "interval_s"->10,  "enabled"->True,
    "deps"->{"heartbeat"},            "dag_order"->1,
    "action_code"->"Function[PersonalSite`Theme`tick[]]"|>,
  <|"task_id"->"cards-refresh", "label"->"Cards refresh (DB)",
    "group_name"->"cache", "interval_s"->20,  "enabled"->True,
    "deps"->{"cache-warm"},           "dag_order"->2,
    "action_code"->"Function[PersonalSite`Assets`refreshCards[]]"|>,
  <|"task_id"->"nest-refresh",  "label"->"NestGraph {2x+1,x+14,x-18}",
    "group_name"->"flow",  "interval_s"->300, "enabled"->True,
    "deps"->{"cache-warm"},           "dag_order"->2,
    "action_code"->"Function[PersonalSite`NestScheduler`run[{2#+1&,#+14&,#-18&},{1},3,\"session\"]]"|>,
  <|"task_id"->"metric-refresh","label"->"Metric refresh (heavy)",
    "group_name"->"cache", "interval_s"->300, "enabled"->True,
    "deps"->{"cards-refresh"},        "dag_order"->3,
    "action_code"->"Function[PersonalSite`Assets`refreshMetric[]]"|>,

  (* ── UX ──────────────────────────────────────────────────────────────── *)
  (*  contact-ux: pulso del boton CTA (activo/inactivo cada 20 s).
      ux-color-{eval,apply,report}: motor de reglas de color por dominio.
      DAG: heartbeat → {theme-rotate, contact-ux} → ux-color-eval
                       → ux-color-apply → ux-color-report              *)
  <|"task_id"->"contact-ux",     "label"->"Contact UX ring pulse",
    "group_name"->"ux",   "interval_s"->5,   "enabled"->True,
    "deps"->{"heartbeat"},            "dag_order"->1,
    "action_code"->"Function[Module[{active=If[Mod[Floor[UnixTime[]/20],2]===1,\"1\",\"0\"]},PersonalSite`Settings`set[\"ux.contact.active\",active];active===\"1\"]]"|>,

  <|"task_id"->"ux-color-eval",  "label"->"UX color rule evaluation",
    "group_name"->"ux",   "interval_s"->15,  "enabled"->True,
    "deps"->{"heartbeat","theme-rotate"},     "dag_order"->2,
    "action_code"->"Function[PersonalSite`UXColorRules`eval[]]"|>,

  <|"task_id"->"ux-color-apply", "label"->"UX color apply tokens to Settings",
    "group_name"->"ux",   "interval_s"->15,  "enabled"->True,
    "deps"->{"ux-color-eval"},               "dag_order"->3,
    "action_code"->"Function[PersonalSite`UXColorRules`apply[]]"|>,

  <|"task_id"->"ux-color-report","label"->"UX color rules audit report",
    "group_name"->"ux",   "interval_s"->60,  "enabled"->True,
    "deps"->{"ux-color-apply"},              "dag_order"->4,
    "action_code"->"Function[PersonalSite`UXColorRules`report[]]"|>,

  (* ── Dev — SCSS hot-reload pipeline ───────────────────────────────── *)
  (*  DAG: scss-watch → scss-compile → {css-version, css-cache-bust} → scss-report
      enabled=False: no impacta produccion; activar manualmente en dev.    *)
  <|"task_id"->"scss-watch",    "label"->"SCSS watch (detect CSS changes)",
    "group_name"->"dev",  "interval_s"->3,   "enabled"->False,
    "deps"->{},                        "dag_order"->0,
    "action_code"->"Function[PersonalSite`DevStyle`detect[]]"|>,

  <|"task_id"->"scss-compile",  "label"->"SCSS compile (sass / npx sass)",
    "group_name"->"dev",  "interval_s"->3,   "enabled"->False,
    "deps"->{"scss-watch"},            "dag_order"->1,
    "action_code"->"Function[PersonalSite`DevStyle`compile[]]"|>,

  <|"task_id"->"css-version",   "label"->"CSS version hash (CRC32 -> css-version.json)",
    "group_name"->"dev",  "interval_s"->3,   "enabled"->False,
    "deps"->{"scss-compile"},          "dag_order"->2,
    "action_code"->"Function[PersonalSite`DevStyle`hashCss[]]"|>,

  <|"task_id"->"css-cache-bust","label"->"CSS cache bust (clear WL fragment cache)",
    "group_name"->"dev",  "interval_s"->3,   "enabled"->False,
    "deps"->{"scss-compile"},          "dag_order"->2,
    "action_code"->"Function[PersonalSite`DevStyle`cacheBust[]]"|>,

  <|"task_id"->"scss-report",   "label"->"SCSS pipeline report",
    "group_name"->"dev",  "interval_s"->30,  "enabled"->False,
    "deps"->{"css-version"},           "dag_order"->3,
    "action_code"->"Function[PersonalSite`DevStyle`report[]]"|>,

  (* ── DevOps pipeline (17 tareas, enabled=False) ───────────────────── *)
  (* L0 — roots *)
  <|"task_id"->"code-lint",     "label"->"Code lint (SyntaxQ .wl files)",
    "group_name"->"test",  "interval_s"->120,  "enabled"->False,
    "deps"->{},                                 "dag_order"->0,
    "action_code"->"Function[PersonalSite`DevOps`codeLint[]]"|>,
  <|"task_id"->"git-status",    "label"->"Git status (porcelain)",
    "group_name"->"git",   "interval_s"->60,   "enabled"->False,
    "deps"->{},                                 "dag_order"->0,
    "action_code"->"Function[PersonalSite`DevOps`gitStatus[]]"|>,
  (* L1 *)
  <|"task_id"->"test-run",      "label"->"Test suite runner",
    "group_name"->"test",  "interval_s"->300,  "enabled"->False,
    "deps"->{"code-lint"},                      "dag_order"->1,
    "action_code"->"Function[PersonalSite`DevOps`runTests[]]"|>,
  <|"task_id"->"git-diff",      "label"->"Git diff --stat HEAD",
    "group_name"->"git",   "interval_s"->60,   "enabled"->False,
    "deps"->{"git-status"},                     "dag_order"->1,
    "action_code"->"Function[PersonalSite`DevOps`gitDiff[]]"|>,
  (* L2 *)
  <|"task_id"->"test-report",   "label"->"Test report snapshot",
    "group_name"->"test",  "interval_s"->300,  "enabled"->False,
    "deps"->{"test-run"},                       "dag_order"->2,
    "action_code"->"Function[PersonalSite`DevOps`testReport[]]"|>,
  <|"task_id"->"paclet-clean",  "label"->"Paclet clean build/ artifacts",
    "group_name"->"build", "interval_s"->600,  "enabled"->False,
    "deps"->{"test-run"},                       "dag_order"->2,
    "action_code"->"Function[PersonalSite`DevOps`pacletClean[]]"|>,
  (* L3 *)
  <|"task_id"->"paclet-build",  "label"->"Paclet build (build_paclet.py)",
    "group_name"->"build", "interval_s"->600,  "enabled"->False,
    "deps"->{"paclet-clean","test-report"},     "dag_order"->3,
    "action_code"->"Function[PersonalSite`DevOps`pacletBuild[]]"|>,
  <|"task_id"->"git-stage",     "label"->"Git stage all (git add -A)",
    "group_name"->"git",   "interval_s"->3600, "enabled"->False,
    "deps"->{"git-diff"},                       "dag_order"->3,
    "action_code"->"Function[PersonalSite`DevOps`gitStage[]]"|>,
  (* L4 *)
  <|"task_id"->"paclet-verify", "label"->"Paclet verify (size + exists)",
    "group_name"->"build", "interval_s"->600,  "enabled"->False,
    "deps"->{"paclet-build"},                   "dag_order"->4,
    "action_code"->"Function[PersonalSite`DevOps`pacletVerify[]]"|>,
  <|"task_id"->"docker-build",  "label"->"Docker build personalsite:latest",
    "group_name"->"ops",   "interval_s"->3600, "enabled"->False,
    "deps"->{"paclet-build"},                   "dag_order"->4,
    "action_code"->"Function[PersonalSite`DevOps`dockerBuild[]]"|>,
  <|"task_id"->"git-commit",    "label"->"Git commit (auto ISO message)",
    "group_name"->"git",   "interval_s"->3600, "enabled"->False,
    "deps"->{"git-stage","test-report"},        "dag_order"->4,
    "action_code"->"Function[PersonalSite`DevOps`gitCommit[]]"|>,
  (* L5 *)
  <|"task_id"->"docker-verify", "label"->"Docker verify container Up",
    "group_name"->"ops",   "interval_s"->300,  "enabled"->False,
    "deps"->{"docker-build"},                   "dag_order"->5,
    "action_code"->"Function[PersonalSite`DevOps`dockerVerify[]]"|>,
  <|"task_id"->"git-push",      "label"->"Git push origin main",
    "group_name"->"git",   "interval_s"->3600, "enabled"->False,
    "deps"->{"git-commit","paclet-verify"},     "dag_order"->5,
    "action_code"->"Function[PersonalSite`DevOps`gitPush[]]"|>,
  (* L6 *)
  <|"task_id"->"smoke-test",    "label"->"HTTP smoke test GET / (latency)",
    "group_name"->"ops",   "interval_s"->120,  "enabled"->False,
    "deps"->{"docker-verify","git-push"},       "dag_order"->6,
    "action_code"->"Function[PersonalSite`DevOps`smokeTest[]]"|>,
  <|"task_id"->"deploy-notify", "label"->"Deploy notification log",
    "group_name"->"ops",   "interval_s"->3600, "enabled"->False,
    "deps"->{"git-push"},                       "dag_order"->6,
    "action_code"->"Function[PersonalSite`DevOps`deployNotify[]]"|>,
  (* L7 *)
  <|"task_id"->"perf-check",    "label"->"Perf check avg latency (3 probes)",
    "group_name"->"ops",   "interval_s"->120,  "enabled"->False,
    "deps"->{"smoke-test"},                     "dag_order"->7,
    "action_code"->"Function[PersonalSite`DevOps`perfCheck[]]"|>,
  <|"task_id"->"changelog-gen", "label"->"Changelog gen (git log -10)",
    "group_name"->"git",   "interval_s"->3600, "enabled"->False,
    "deps"->{"git-push"},                       "dag_order"->7,
    "action_code"->"Function[PersonalSite`DevOps`changelogGen[]]"|>
};

seedDefaults[] :=
  Module[{rows},
    rows = PersonalSite`Database`execute[
      "SELECT COUNT(*) FROM scheduler_tasks", {}];
    If[rows === $Failed || ! ListQ[rows] ||
       ! (rows[[1,1]] === 0 || rows[[1,1]] === "0"),
      Return["already seeded"]];
    create /@ $defaults;
    "seeded"
  ];

End[];
EndPackage[];
