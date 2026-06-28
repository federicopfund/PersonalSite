(* ::Package:: *)

(* PersonalSite`Scheduler`
   --------------------------------------------------------------------------
   Registra todas las tareas de runtime en PersonalSite`TaskManager` y las
   arranca. Cada tarea tiene spec completo: label, group, interval, enabled.

   Agregar una tarea nueva: anadir una llamada register[] + start[] aqui,
   o usar POST /tasks/register desde la UI en caliente. *)

BeginPackage["PersonalSite`Scheduler`",
  {"PersonalSite`TaskManager`"}];

start::usage  = "start[] registra e inicia todas las tareas de runtime.";
stop::usage   = "stop[] detiene todas las tareas.";
status::usage = "status[] devuelve snapshot del runtime (delega a TaskManager).";

Begin["`Private`"];

$started   = False;
$startedAt = None;

(* ── Definicion de las tareas del sitio ──────────────────────────────── *)
(*  Cada spec: label, group, interval (s), enabled, action               *)

$taskSpecs = {

  (* ── Sistema ─────────────────────────────────────────────────────── *)
  {"heartbeat", <|
    "label"    -> "Heartbeat",
    "group"    -> "system",
    "interval" -> 30,
    "enabled"  -> True,
    "deps"     -> {},
    "action"   -> Function[True]   (* marca de vida del kernel *)
  |>},

  {"cache-warm", <|
    "label"    -> "Cache warm-up",
    "group"    -> "system",
    "interval" -> 300,
    "enabled"  -> True,
    "deps"     -> {"heartbeat"},
    "action"   -> Function[
      Quiet @ Check[PersonalSite`Post`recent[10]; True, False]]
  |>},

  (* ── Tema ────────────────────────────────────────────────────────── *)
  {"theme-rotate", <|
    "label"    -> "Theme rotation tick",
    "group"    -> "theme",
    "interval" -> 10,
    "enabled"  -> True,
    "deps"     -> {"heartbeat"},
    "action"   -> Function[PersonalSite`Theme`tick[]]
  |>},

  (* ── Assets / cache ─────────────────────────────────────────────── *)
  {"cards-refresh", <|
    "label"    -> "Cards refresh (DB)",
    "group"    -> "cache",
    "interval" -> 20,
    "enabled"  -> True,
    "deps"     -> {"cache-warm"},
    "action"   -> Function[PersonalSite`Assets`refreshCards[]]
  |>},

  {"metric-refresh", <|
    "label"    -> "Metric refresh (heavy)",
    "group"    -> "cache",
    "interval" -> 300,
    "enabled"  -> True,
    "deps"     -> {"cards-refresh"},
    "action"   -> Function[PersonalSite`Assets`refreshMetric[]]
  |>},

  (* ── Flow / NestGraph ───────────────────────────────────────────── *)
  {"nest-refresh", <|
    "label"    -> "NestGraph {2x+1, x+14, x-18} depth=3",
    "group"    -> "flow",
    "interval" -> 300,
    "enabled"  -> True,
    "deps"     -> {"cache-warm"},
    "action"   -> Function[
      PersonalSite`NestScheduler`run[
        {2 # + 1 &, # + 14 &, # - 18 &}, {1}, 3, "session"]]
  |>}

};

(* ── start[] ─────────────────────────────────────────────────────────── *)
start[] :=
  If[TrueQ[$started],
    PersonalSite`TaskManager`summary[],
    (
      (* Registrar todas las tareas en el TaskManager *)
      Scan[Function[pair,
        PersonalSite`TaskManager`register[First[pair], Last[pair]]],
        $taskSpecs];

      (* Iniciar todas (solo las enabled -> True) *)
      PersonalSite`TaskManager`start[];

      $startedAt = Now;
      $started   = True;
      PersonalSite`TaskManager`summary[]
    )
  ];

(* ── stop[] ──────────────────────────────────────────────────────────── *)
stop[] :=
  (PersonalSite`TaskManager`stop[];
   $started = False;);

(* ── status[] ────────────────────────────────────────────────────────── *)
status[] :=
  <|PersonalSite`TaskManager`summary[],
    "startedAt" -> $startedAt,
    "uptime"    -> If[$startedAt === None,
                     Quantity[0, "Seconds"],
                     Now - $startedAt]|>;

End[];
EndPackage[];
