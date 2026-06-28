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
    r = PersonalSite`Database`execute[
      "INSERT OR IGNORE INTO scheduler_tasks
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
    "action_code"->"Function[PersonalSite`Assets`refreshMetric[]]"|>
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
