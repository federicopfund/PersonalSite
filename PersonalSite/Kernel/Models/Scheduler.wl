(* ::Package:: *)

(* PersonalSite`Scheduler`
   --------------------------------------------------------------------------
   Registra todas las tareas de runtime en PersonalSite`TaskManager` y las
   arranca. Cada tarea tiene spec completo: label, group, interval, enabled.

   Agregar una tarea nueva: anadir una llamada register[] + start[] aqui,
   o usar POST /tasks/register desde la UI en caliente. *)

BeginPackage["PersonalSite`Scheduler`",
  {"PersonalSite`TaskManager`", "PersonalSite`TaskConfig`"}];

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
    "interval" -> 360,   (* 6 min — corre con algo mas de separacion que cards-refresh *)
    "enabled"  -> True,
    "deps"     -> {"cards-refresh"},
    "action"   -> Function[
      (* Proteccion adicional: si el kernel paralelo no responde dentro de
         computeMetric (TimeConstrained 25s), el fallback corre en el
         kernel principal y esta tarea termina en <30s en cualquier caso. *)
      Quiet @ Check[PersonalSite`Assets`refreshMetric[], False]]
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
  |>},

  (* ── UX ─────────────────────────────────────────────────────────── *)
  (* Regla de color para el boton Contacto: cicla activo/inactivo cada
     20 segundos. El frontend lee /ux/contact (JSON) y agrega .is-running
     al boton para disparar la animacion del anillo conic-gradient. *)
  {"contact-ux", <|
    "label"    -> "Contact UX ring pulse",
    "group"    -> "ux",
    "interval" -> 5,
    "enabled"  -> True,
    "deps"     -> {"heartbeat"},
    "action"   -> Function[
      Module[{active = If[Mod[Floor[UnixTime[] / 20], 2] === 1, "1", "0"]},
        PersonalSite`Settings`set["ux.contact.active", active];
        active === "1"]]  |>},

  (* ── UX Color Rules — motor de reglas de color por dominio ─────────── *)
  (*  DAG: heartbeat + theme-rotate → ux-color-eval
                                      → ux-color-apply → ux-color-report
      Las 3 tareas calculan y persisten los tokens de color activos del
      sitio con logica de dominio UX (hora del dia, dia de semana,
      engagement del CTA de Contacto, armonia con el tema activo).
      Habilitadas en produccion: enabled=True.                              *)

  {"ux-color-eval", <|
    "label"    -> "UX color rule evaluation",
    "group"    -> "ux",
    "interval" -> 15,
    "enabled"  -> True,
    "deps"     -> {"heartbeat", "theme-rotate"},
    "action"   -> Function[PersonalSite`UXColorRules`eval[]]
  |>},

  {"ux-color-apply", <|
    "label"    -> "UX color apply tokens to Settings",
    "group"    -> "ux",
    "interval" -> 15,
    "enabled"  -> True,
    "deps"     -> {"ux-color-eval"},
    "action"   -> Function[PersonalSite`UXColorRules`apply[]]
  |>},

  {"ux-color-report", <|
    "label"    -> "UX color rules audit report",
    "group"    -> "ux",
    "interval" -> 60,
    "enabled"  -> True,
    "deps"     -> {"ux-color-apply"},
    "action"   -> Function[PersonalSite`UXColorRules`report[]]
  |>},

  (* ── Dev — SCSS hot-reload pipeline (disabled en production) ──────── *)
  (*  Las 5 tareas forman un DAG: detect → compile → {hash, bust} → report
      Todas arrancan con enabled=False; activar manualmente en dev:          *)
  {"scss-watch", <|
    "label"    -> "SCSS watch (detect CSS changes)",
    "group"    -> "dev",
    "interval" -> 3,
    "enabled"  -> False,
    "deps"     -> {},
    "action"   -> Function[PersonalSite`DevStyle`detect[]]
  |>},

  {"scss-compile", <|
    "label"    -> "SCSS compile (sass / npx sass)",
    "group"    -> "dev",
    "interval" -> 3,
    "enabled"  -> False,
    "deps"     -> {"scss-watch"},
    "action"   -> Function[PersonalSite`DevStyle`compile[]]
  |>},

  {"css-version", <|
    "label"    -> "CSS version hash (CRC32 → css-version.json)",
    "group"    -> "dev",
    "interval" -> 3,
    "enabled"  -> False,
    "deps"     -> {"scss-compile"},
    "action"   -> Function[PersonalSite`DevStyle`hashCss[]]
  |>},

  {"css-cache-bust", <|
    "label"    -> "CSS cache bust (clear WL fragment cache)",
    "group"    -> "dev",
    "interval" -> 3,
    "enabled"  -> False,
    "deps"     -> {"scss-compile"},
    "action"   -> Function[PersonalSite`DevStyle`cacheBust[]]
  |>},

  {"scss-report", <|
    "label"    -> "SCSS pipeline report",
    "group"    -> "dev",
    "interval" -> 30,
    "enabled"  -> False,
    "deps"     -> {"css-version"},
    "action"   -> Function[PersonalSite`DevStyle`report[]]  |>}

};

(* ── start[] ─────────────────────────────────────────────────────────── *)
start[] :=
  If[TrueQ[$started],
    PersonalSite`TaskManager`summary[],
    (
      (* Seed DB si está vacía, luego cargar specs desde DB *)
      Quiet @ Check[PersonalSite`TaskConfig`seedDefaults[], Null];

      (* Intentar cargar specs desde DB; fallback a $taskSpecs hardcodeados *)
      Module[{dbSpecs, specsToLoad},
        dbSpecs = Quiet @ Check[PersonalSite`TaskConfig`all[], {}];
        specsToLoad = If[Length[dbSpecs] > 0,
          (* Convertir rows de DB al formato {id, spec} del TaskManager *)
          Map[Function[row,
            {row["task_id"],
             <|
               "label"    -> row["label"],
               "group"    -> row["group_name"],
               "interval" -> row["interval_s"],
               "enabled"  -> row["enabled"],
               "deps"     -> row["deps"],
               "dag_order"-> row["dag_order"],
               "action"   -> Quiet @ Check[
                                ToExpression[row["action_code"]],
                                Function[True]]
             |>}],
            dbSpecs],
          $taskSpecs  (* fallback si DB no disponible *)
        ];

        (* Registrar todas las tareas en el TaskManager *)
        Scan[Function[pair,
          PersonalSite`TaskManager`register[First[pair], Last[pair]]],
          specsToLoad];
      ];

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
