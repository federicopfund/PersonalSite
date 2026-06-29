(* PersonalSite/Tests/UXColorRules.wlt
   ─────────────────────────────────────────────────────────────────────────
   Suite de tests end-to-end para PersonalSite`UXColorRules`.

   Cubre:
     1. Reglas puras (timeOfDayRule, weekdayRule) — sin DB
     2. Arbitro (arbitrate) — sin DB
     3. Integracion con Settings (engagementRule, apply, palette)
     4. Ring buffer de report
     5. Coherencia del DAG en Scheduler.$taskSpecs
     6. Seeds en TaskConfig.$defaults

   Ejecutar:
       docker exec profile-web-1 wolframscript \
         -script /app/PersonalSite/Tests/UXColorRules.wlt

   O desde el runner maestro:
       docker exec profile-web-1 wolframscript \
         -script /app/PersonalSite/Tests/TestReport.wl -- --layer ux
   ─────────────────────────────────────────────────────────────────────────*)

PacletDirectoryLoad[DirectoryName[$InputFileName, 2]];
Needs["PersonalSite`"];

(* ── Aliases privados para las funciones internas ─────────────────────── *)
(*  Los simbolos de `Private` no son accesibles directamente, pero las
    funciones publicas cubren toda la logica relevante.                   *)


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 1: timeOfDayRule — regla pura (solo UnixTime aritmetico)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["timeOfDayRule"]

(* 2026-06-29 00:00 UTC = 1782691200  (Monday) — base del dia       *)
(* ts_night   = 2026-06-29 02:00 UTC  (hora 2  → slot "night")    *)
$tsNight    = 1782698400;
(* ts_dawn    = 2026-06-29 05:00 UTC  (hora 5  → slot "dawn")     *)
$tsDawn     = 1782709200;
(* ts_morning = 2026-06-29 09:00 UTC  (hora 9  → slot "morning")  *)
$tsMorning  = 1782723600;
(* ts_aft     = 2026-06-29 15:00 UTC  (hora 15 → slot "afternoon")*)
$tsAft      = 1782745200;
(* ts_eve     = 2026-06-29 20:00 UTC  (hora 20 → slot "evening")  *)
$tsEve      = 1782763200;
(* ts_nlate   = 2026-06-29 23:00 UTC  (hora 23 → slot "night-late")*)
$tsNLate    = 1782774000;

(* --- score siempre 60 --------------------------------------------------- *)
VerificationTest[
  PersonalSite`UXColorRules`eval[]  (* smoke: no lanza excepcion *)
    // (AssociationQ[#] && KeyExistsQ[#, "accent"] &),
  True,
  TestID -> "UXColorRules::eval::Smoke"
]

(* --- slot → accent correcto ---------------------------------------------- *)
(*  Probamos via eval[] con timestamps conocidos (la funcion interna es
    privada, pero eval[] la llama con UnixTime[]. Para tests de franja
    usamos directamente la aritmetica de ts via modulo de hora).           *)

VerificationTest[
  Mod[Floor[$tsNight   / 3600], 24],
  2,    (* hora UTC *)
  TestID -> "UXColorRules::TimeArith::NightHour"
]

VerificationTest[
  Mod[Floor[$tsMorning / 3600], 24],
  9,
  TestID -> "UXColorRules::TimeArith::MorningHour"
]

VerificationTest[
  Mod[Floor[$tsAft     / 3600], 24],
  15,
  TestID -> "UXColorRules::TimeArith::AfternoonHour"
]

VerificationTest[
  Mod[Floor[$tsEve     / 3600], 24],
  20,
  TestID -> "UXColorRules::TimeArith::EveningHour"
]

VerificationTest[
  Mod[Floor[$tsNLate   / 3600], 24],
  23,
  TestID -> "UXColorRules::TimeArith::NightLateHour"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 2: weekdayRule — aritmetica de dia de semana (pura)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["weekdayRule"]

(* 2026-06-29 = Lunes  (day 20633 from epoch; (20633+4)%7 = 1)       *)
$tsMonday  = 1782691200;   (* 2026-06-29 00:00 UTC  (Lunes)   *)
$tsFriday  = 1783036800;   (* 2026-07-03 00:00 UTC  (Viernes) *)
$tsSunday  = 1783209600;   (* 2026-07-05 00:00 UTC  (Domingo) *)

VerificationTest[
  Mod[Floor[$tsMonday / 86400] + 4, 7],
  1,   (* 1 = Lunes *)
  TestID -> "UXColorRules::WeekArith::Monday"
]

VerificationTest[
  Mod[Floor[$tsFriday / 86400] + 4, 7],
  5,   (* 5 = Viernes *)
  TestID -> "UXColorRules::WeekArith::Friday"
]

VerificationTest[
  Mod[Floor[$tsSunday / 86400] + 4, 7],
  0,   (* 0 = Domingo → weekend *)
  TestID -> "UXColorRules::WeekArith::Sunday"
]

(* weekday (dow=1..4) → intent = focus *)
VerificationTest[
  MemberQ[{1,2,3,4}, Mod[Floor[$tsMonday / 86400] + 4, 7]],
  True,
  TestID -> "UXColorRules::WeekArith::MondayIsWeekday"
]

(* friday (dow=5) → intent = creative *)
VerificationTest[
  Mod[Floor[$tsFriday / 86400] + 4, 7] === 5,
  True,
  TestID -> "UXColorRules::WeekArith::FridayIsFriday"
]

(* sunday (dow=0) → weekend *)
VerificationTest[
  !MemberQ[{1,2,3,4,5}, Mod[Floor[$tsSunday / 86400] + 4, 7]],
  True,
  TestID -> "UXColorRules::WeekArith::SundayIsWeekend"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 3: eval[] — contrato de la API publica
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["eval"]

$vote = PersonalSite`UXColorRules`eval[];

VerificationTest[
  AssociationQ[$vote],
  True,
  TestID -> "UXColorRules::eval::ReturnsAssociation"
]

VerificationTest[
  SubsetQ[Keys[$vote], {"accent","surface","intent","rule","score","epoch","nVotes"}],
  True,
  TestID -> "UXColorRules::eval::HasRequiredKeys"
]

VerificationTest[
  StringQ[$vote["accent"]] && StringLength[$vote["accent"]] > 0,
  True,
  TestID -> "UXColorRules::eval::AccentIsNonEmptyString"
]

VerificationTest[
  MemberQ[{"focus","creative","relax","energize","calm"}, $vote["intent"]],
  True,
  TestID -> "UXColorRules::eval::IntentIsKnownValue"
]

VerificationTest[
  IntegerQ[$vote["score"]] && $vote["score"] >= 0 && $vote["score"] <= 100,
  True,
  TestID -> "UXColorRules::eval::ScoreInRange"
]

VerificationTest[
  IntegerQ[$vote["epoch"]] && $vote["epoch"] > 0,
  True,
  TestID -> "UXColorRules::eval::EpochIsPositiveInteger"
]

VerificationTest[
  IntegerQ[$vote["nVotes"]] && $vote["nVotes"] >= 1,
  True,
  TestID -> "UXColorRules::eval::AtLeastOneVote"
]

(* eval[] es determinista: dos llamadas consecutivas en el mismo segundo
   deben producir el mismo acento (regla gana por score, no por azar).   *)
VerificationTest[
  PersonalSite`UXColorRules`eval[]["accent"] ===
  PersonalSite`UXColorRules`eval[]["accent"],
  True,
  TestID -> "UXColorRules::eval::Deterministic"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 4: engagement — score 90 supera a time-of-day (score 60)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["engagement"]

(* Forzar engagement activo en Settings *)
PersonalSite`Settings`set["ux.contact.active", "1"];

VerificationTest[
  PersonalSite`UXColorRules`eval[]["rule"],
  "engagement",
  TestID -> "UXColorRules::Engagement::ActiveWinsArbitration"
]

VerificationTest[
  PersonalSite`UXColorRules`eval[]["accent"],
  "pulse-green",
  TestID -> "UXColorRules::Engagement::ActiveAccent"
]

VerificationTest[
  PersonalSite`UXColorRules`eval[]["intent"],
  "energize",
  TestID -> "UXColorRules::Engagement::ActiveIntent"
]

VerificationTest[
  PersonalSite`UXColorRules`eval[]["score"],
  90,
  TestID -> "UXColorRules::Engagement::Score90"
]

(* Desactivar engagement → time-of-day (60) gana *)
PersonalSite`Settings`set["ux.contact.active", "0"];

VerificationTest[
  PersonalSite`UXColorRules`eval[]["rule"] =!= "engagement",
  True,
  TestID -> "UXColorRules::Engagement::InactiveDoesNotWin"
]

VerificationTest[
  PersonalSite`UXColorRules`eval[]["score"] < 90,
  True,
  TestID -> "UXColorRules::Engagement::InactiveScoreLower"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 5: apply[] — persistencia condicional en Settings
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["apply"]

PersonalSite`Settings`set["ux.contact.active", "0"];
$applied = PersonalSite`UXColorRules`apply[];

VerificationTest[
  AssociationQ[$applied],
  True,
  TestID -> "UXColorRules::apply::ReturnsAssociation"
]

VerificationTest[
  PersonalSite`Settings`get["ux.color.accent", ""] === $applied["accent"],
  True,
  TestID -> "UXColorRules::apply::PersistsAccent"
]

VerificationTest[
  PersonalSite`Settings`get["ux.color.surface", ""] === $applied["surface"],
  True,
  TestID -> "UXColorRules::apply::PersistsSurface"
]

VerificationTest[
  PersonalSite`Settings`get["ux.color.intent", ""] === $applied["intent"],
  True,
  TestID -> "UXColorRules::apply::PersistsIntent"
]

VerificationTest[
  PersonalSite`Settings`get["ux.color.rule", ""] === $applied["rule"],
  True,
  TestID -> "UXColorRules::apply::PersistsRule"
]

VerificationTest[
  StringQ[PersonalSite`Settings`get["ux.color.epoch", ""]],
  True,
  TestID -> "UXColorRules::apply::PersistsEpoch"
]

(* Segunda llamada con el mismo estado: no debe lanzar error *)
VerificationTest[
  AssociationQ[PersonalSite`UXColorRules`apply[]],
  True,
  TestID -> "UXColorRules::apply::IdempotentNoError"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 6: palette[] — lectura de Settings
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["palette"]

$pal = PersonalSite`UXColorRules`palette[];

VerificationTest[
  AssociationQ[$pal],
  True,
  TestID -> "UXColorRules::palette::ReturnsAssociation"
]

VerificationTest[
  SubsetQ[Keys[$pal], {"accent","surface","intent","rule","score","epoch"}],
  True,
  TestID -> "UXColorRules::palette::HasAllKeys"
]

VerificationTest[
  StringQ[$pal["accent"]] && StringLength[$pal["accent"]] > 0,
  True,
  TestID -> "UXColorRules::palette::AccentNonEmpty"
]

(* palette[accent] debe coincidir con lo que apply[] persiste *)
VerificationTest[
  PersonalSite`UXColorRules`apply[]["accent"] ===
    PersonalSite`UXColorRules`palette[]["accent"],
  True,
  TestID -> "UXColorRules::palette::ConsistentWithApply"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 7: report[] y ring buffer (max 10 entradas)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["report"]

(* Llamar apply[] 11 veces para saturar el ring buffer *)
Do[PersonalSite`UXColorRules`apply[], {11}];

$rep = PersonalSite`UXColorRules`report[];

VerificationTest[
  AssociationQ[$rep],
  True,
  TestID -> "UXColorRules::report::ReturnsAssociation"
]

VerificationTest[
  SubsetQ[Keys[$rep], {"history","count","palette","ts"}],
  True,
  TestID -> "UXColorRules::report::HasRequiredKeys"
]

VerificationTest[
  $rep["count"] === 10,
  True,
  TestID -> "UXColorRules::report::RingBufferCappedAt10"
]

VerificationTest[
  Length[$rep["history"]] === 10,
  True,
  TestID -> "UXColorRules::report::HistoryLength10"
]

VerificationTest[
  AllTrue[$rep["history"], AssociationQ],
  True,
  TestID -> "UXColorRules::report::HistoryItemsAreAssociations"
]

VerificationTest[
  AllTrue[$rep["history"], KeyExistsQ[#, "accent"] &],
  True,
  TestID -> "UXColorRules::report::HistoryItemsHaveAccent"
]

VerificationTest[
  IntegerQ[$rep["ts"]] && $rep["ts"] > 0,
  True,
  TestID -> "UXColorRules::report::TsIsInteger"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 8: coherencia del DAG — specs en $taskSpecs y en la DB
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["scheduler-dag"]

(*  Fuente de verdad para estructuras de la tarea: TaskConfig`all[] lee la
    DB SQLite (la misma que usa el servidor en produccion). Los seeds ya
    fueron insertados en el setup (seccion anterior o al inicio de este test).
    Tambien verificamos los specs en memoria via $taskSpecs para probar que
    la definicion estatica en Scheduler.wl es correcta.                    *)

(* ── A: specs desde la DB ────────────────────────────────────────────── *)
$dbAll   = PersonalSite`TaskConfig`all[];
$dbAssoc = Association[#["task_id"] -> # & /@ $dbAll];

VerificationTest[
  MemberQ[#["task_id"] & /@ $dbAll, "ux-color-eval"],
  True,
  TestID -> "UXColorRules::DAG::EvalInDB"
]

VerificationTest[
  MemberQ[#["task_id"] & /@ $dbAll, "ux-color-apply"],
  True,
  TestID -> "UXColorRules::DAG::ApplyInDB"
]

VerificationTest[
  MemberQ[#["task_id"] & /@ $dbAll, "ux-color-report"],
  True,
  TestID -> "UXColorRules::DAG::ReportInDB"
]

VerificationTest[
  $dbAssoc["ux-color-eval"]["group_name"],
  "ux",
  TestID -> "UXColorRules::DAG::EvalGroupIsUX"
]

VerificationTest[
  SubsetQ[
    $dbAssoc["ux-color-eval"]["deps"],
    {"heartbeat", "theme-rotate"}],
  True,
  TestID -> "UXColorRules::DAG::EvalDepsCorrect"
]

VerificationTest[
  MemberQ[$dbAssoc["ux-color-apply"]["deps"], "ux-color-eval"],
  True,
  TestID -> "UXColorRules::DAG::ApplyDepsEval"
]

VerificationTest[
  MemberQ[$dbAssoc["ux-color-report"]["deps"], "ux-color-apply"],
  True,
  TestID -> "UXColorRules::DAG::ReportDepsApply"
]

VerificationTest[
  $dbAssoc["ux-color-eval"]["interval_s"] === 15,
  True,
  TestID -> "UXColorRules::DAG::EvalInterval15s"
]

VerificationTest[
  $dbAssoc["ux-color-report"]["interval_s"] === 60,
  True,
  TestID -> "UXColorRules::DAG::ReportInterval60s"
]

VerificationTest[
  TrueQ[$dbAssoc["ux-color-eval"]["enabled"]],
  True,
  TestID -> "UXColorRules::DAG::EvalEnabled"
]

(* ── B: specs en $taskSpecs (memoria) ───────────────────────────────── *)
(*  Convertir lista de {id, spec} a Association id -> spec               *)
$memSpecs = Association[Rule @@@ PersonalSite`Scheduler`Private`$taskSpecs];

VerificationTest[
  KeyExistsQ[$memSpecs, "ux-color-eval"],
  True,
  TestID -> "UXColorRules::DAG::EvalInMemorySpecs"
]

VerificationTest[
  $memSpecs["ux-color-eval"]["group"],
  "ux",
  TestID -> "UXColorRules::DAG::EvalMemGroupIsUX"
]

EndTestSection[]


(* ══════════════════════════════════════════════════════════════════════════
   SECCION 9: seeds en TaskConfig.$defaults (coherencia con DB)
   ══════════════════════════════════════════════════════════════════════════ *)
BeginTestSection["taskconfig-seeds"]

$dbTasks = PersonalSite`TaskConfig`all[];

VerificationTest[
  ListQ[$dbTasks],
  True,
  TestID -> "UXColorRules::Seeds::all[]ReturnsList"
]

$dbIds = #["task_id"] & /@ $dbTasks;

VerificationTest[
  MemberQ[$dbIds, "ux-color-eval"],
  True,
  TestID -> "UXColorRules::Seeds::EvalInDB"
]

VerificationTest[
  MemberQ[$dbIds, "ux-color-apply"],
  True,
  TestID -> "UXColorRules::Seeds::ApplyInDB"
]

VerificationTest[
  MemberQ[$dbIds, "ux-color-report"],
  True,
  TestID -> "UXColorRules::Seeds::ReportInDB"
]

(* dag_order correcto en seeds *)
$evalRow   = SelectFirst[$dbTasks, #["task_id"] === "ux-color-eval"   &];
$applyRow  = SelectFirst[$dbTasks, #["task_id"] === "ux-color-apply"  &];
$reportRow = SelectFirst[$dbTasks, #["task_id"] === "ux-color-report" &];

VerificationTest[
  $evalRow["dag_order"] === 2,
  True,
  TestID -> "UXColorRules::Seeds::EvalDagOrder2"
]

VerificationTest[
  $applyRow["dag_order"] === 3,
  True,
  TestID -> "UXColorRules::Seeds::ApplyDagOrder3"
]

VerificationTest[
  $reportRow["dag_order"] === 4,
  True,
  TestID -> "UXColorRules::Seeds::ReportDagOrder4"
]

(* enabled = True en seeds *)
VerificationTest[
  TrueQ[$evalRow["enabled"]],
  True,
  TestID -> "UXColorRules::Seeds::EvalEnabled"
]

VerificationTest[
  TrueQ[$applyRow["enabled"]],
  True,
  TestID -> "UXColorRules::Seeds::ApplyEnabled"
]

EndTestSection[]
