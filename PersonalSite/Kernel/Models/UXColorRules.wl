(* ::Package:: *)

(* PersonalSite`UXColorRules`
   --------------------------------------------------------------------------
   Motor de reglas de color UX basado en logica de dominio.

   Determina de forma DETERMINISTA los tokens de color activos en el sitio
   a partir del contexto en tiempo real.  Todos los kernels del pool calculan
   el mismo resultado sin condiciones de carrera porque las reglas solo
   dependen de UnixTime[] y de Settings (lectura).

   Reglas del dominio (4 fuentes, arbitraje por score):
     1. time-of-day   — franja horaria → acento semantico (score 60)
     2. weekday-mode  — dia de la semana → intencion UX   (score 40)
     3. theme-sync    — armoniza con el tema activo        (score 30)
     4. engagement    — CTA de Contacto activo → pulse     (score 90)

   Tokens de color (persisten en Settings → leidos por el frontend):
     ux.color.accent   — nombre del token de acento (clase CSS / CSS var)
     ux.color.surface  — variante de superficie
     ux.color.intent   — intencion UX: focus | creative | relax | energize | calm
     ux.color.rule     — regla ganadora (para debug / audit)
     ux.color.score    — confianza de la regla ganadora (0-100)
     ux.color.epoch    — UnixTime de la ultima evaluacion

   DAG de tareas que dependen de este modulo:
     heartbeat ──┐
                 ├──> ux-color-eval ──> ux-color-apply ──> ux-color-report
     theme-rotate┘

   API publica:
     eval[]     Evalua todas las reglas y retorna el voto ganador como
                Association <|accent, surface, intent, rule, score, epoch|>.
     apply[]    Llama eval[] y persiste solo si algo cambio (protege DB).
     palette[]  Tokens de color actuales desde Settings (para endpoints HTTP).
     report[]   Snapshot del historial en memoria (ring 10) + palette actual.
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`UXColorRules`",
  {"PersonalSite`Settings`", "PersonalSite`Theme`"}];

eval::usage =
  "eval[] evalua las 4 reglas de color UX y retorna el voto ganador como \
Association <|accent, surface, intent, rule, score, epoch|>.";

apply::usage =
  "apply[] llama eval[] y persiste el resultado en Settings solo si \
cambio el acento o la regla ganadora. Retorna la Association aplicada.";

palette::usage =
  "palette[] devuelve los tokens de color actuales desde Settings \
(lectura directa — uso en endpoints HTTP /ux/palette).";

report::usage =
  "report[] devuelve <|history, count, palette, ts|> con el historial \
de las ultimas 10 evaluaciones (ring buffer en memoria).";

Begin["`Private`"];

(* ── Historial en memoria: ring buffer 10 entradas ──────────────────── *)
$history = {};

(* ── Tabla de tokens por franja horaria ─────────────────────────────── *)
(*  Franjas: night (0-3), dawn (4-6), morning (7-11),
             afternoon (12-17), evening (18-21), night-late (22-23)      *)
$timeTokens = <|
  "night"      -> <|"accent" -> "night-blue",      "surface" -> "deep-dark",    "intent" -> "calm"    |>,
  "dawn"       -> <|"accent" -> "dawn-indigo",      "surface" -> "soft-dark",    "intent" -> "focus"   |>,
  "morning"    -> <|"accent" -> "morning-amber",    "surface" -> "warm-light",   "intent" -> "focus"   |>,
  "afternoon"  -> <|"accent" -> "afternoon-cyan",   "surface" -> "cool-light",   "intent" -> "energize"|>,
  "evening"    -> <|"accent" -> "evening-rose",     "surface" -> "warm-mid",     "intent" -> "relax"   |>,
  "night-late" -> <|"accent" -> "night-violet",     "surface" -> "deep-dark",    "intent" -> "calm"    |>
|>;

(* ── Tabla de tokens por dia de la semana ───────────────────────────── *)
(*  Lun-Jue → focus/productividad; Vie → creatividad; Sab-Dom → relax.  *)
$weekdayTokens = <|
  "weekday" -> <|"accent" -> "focus-sky",       "surface" -> "neutral",     "intent" -> "focus"   |>,
  "friday"  -> <|"accent" -> "creative-violet", "surface" -> "warm-light",  "intent" -> "creative"|>,
  "weekend" -> <|"accent" -> "relax-sage",      "surface" -> "soft-warm",   "intent" -> "relax"   |>
|>;

(* ── Tabla de armonizacion con el tema activo del sitio ─────────────── *)
$themeAccentMap = <|
  "slate"  -> <|"accent" -> "slate-steel",   "surface" -> "cool-gray",    "intent" -> "focus"   |>,
  "sand"   -> <|"accent" -> "sand-tan",      "surface" -> "warm-beige",   "intent" -> "relax"   |>,
  "forest" -> <|"accent" -> "forest-moss",   "surface" -> "organic-green","intent" -> "creative"|>,
  "rose"   -> <|"accent" -> "rose-blush",    "surface" -> "warm-pink",    "intent" -> "energize"|>,
  "ocean"  -> <|"accent" -> "ocean-teal",    "surface" -> "cool-blue",    "intent" -> "focus"   |>
|>;

(* ── Regla 1: time-of-day ────────────────────────────────────────────── *)
(*  Pura: solo depende del timestamp Unix (segundos UTC).
    Computa la franja horaria sin llamadas a funciones de fecha WL para
    minimizar dependencias y garantizar idempotencia entre kernels.       *)
timeOfDayRule[ts_Integer] :=
  Module[{h = Mod[Floor[ts / 3600], 24], slot, tok},
    slot = Which[
      h <  4,  "night",
      h <  7,  "dawn",
      h < 12,  "morning",
      h < 18,  "afternoon",
      h < 22,  "evening",
      True,    "night-late"
    ];
    tok = $timeTokens[slot];
    <|tok, "rule" -> "time-of-day", "score" -> 60, "slot" -> slot|>
  ];

(* ── Regla 2: weekday-mode ───────────────────────────────────────────── *)
(*  Calculo aritmetico puro: 1970-01-01 fue jueves (offset +4).
    dow: 0=Domingo, 1=Lunes, ..., 5=Viernes, 6=Sabado.                  *)
weekdayRule[ts_Integer] :=
  Module[{dow = Mod[Floor[ts / 86400] + 4, 7], key, tok},
    key = Which[
      MemberQ[{1, 2, 3, 4}, dow], "weekday",
      dow === 5,                  "friday",
      True,                       "weekend"
    ];
    tok = $weekdayTokens[key];
    <|tok, "rule" -> "weekday-mode", "score" -> 40, "dow" -> dow|>
  ];

(* ── Regla 3: theme-sync ─────────────────────────────────────────────── *)
(*  Armoniza el acento con el tema activo del sitio (lectura Settings).
    Score bajo: solo actua como tiebreaker cuando las otras reglas empatan.*)
themeSyncRule[] :=
  Module[{theme = Quiet @ Check[PersonalSite`Theme`resolve[], "slate"], tok},
    tok = Lookup[$themeAccentMap, theme, $themeAccentMap["slate"]];
    <|tok, "rule" -> "theme-sync", "score" -> 30, "theme" -> theme|>
  ];

(* ── Regla 4: engagement-state ──────────────────────────────────────── *)
(*  Lee ux.contact.active (escrito por la tarea contact-ux).
    Si el boton CTA esta activo: score=90 (supera a time-of-day) y
    fuerza intent=energize para reforzar la llamada a la accion.
    Si esta inactivo: score=0 → descartado por el arbitro.              *)
engagementRule[] :=
  Module[{active = PersonalSite`Settings`get["ux.contact.active", "0"]},
    If[active === "1",
      <|"accent"  -> "pulse-green",
        "surface" -> "warm-light",
        "intent"  -> "energize",
        "rule"    -> "engagement",
        "score"   -> 90|>,
      (* Voto nulo: no compite *)
      <|"accent" -> "", "surface" -> "", "intent" -> "",
        "rule"   -> "engagement-idle", "score" -> 0|>
    ]
  ];

(* ── Arbitro: seleccion del voto ganador ─────────────────────────────── *)
(*  Recoge los 4 votos, descarta los de score=0 y devuelve el de mayor
    score.  En empate exacto gana el primero en la lista (prioridad
    editorial: time-of-day > weekday > theme-sync).                     *)
arbitrate[votes_List] :=
  Module[{valid, best},
    valid = Select[votes, Lookup[#, "score", 0] > 0 &];
    If[Length[valid] === 0,
      Return[<|"accent" -> "morning-amber", "surface" -> "warm-light",
               "intent" -> "focus", "rule" -> "fallback", "score" -> 0|>]];
    best = MaximalBy[valid, Lookup[#, "score", 0] &];
    First[best]
  ];

(* ── eval[] ──────────────────────────────────────────────────────────── *)
(*  Captura el timestamp UNA sola vez para que todas las reglas
    puras trabajen con el mismo instante (coherencia de snapshot).       *)
eval[] :=
  Module[{ts = UnixTime[], votes, winner},
    votes = {
      timeOfDayRule[ts],
      weekdayRule[ts],
      themeSyncRule[],
      engagementRule[]
    };
    winner = arbitrate[votes];
    <|winner,
      "epoch"  -> ts,
      "nVotes" -> Length[Select[votes, Lookup[#, "score", 0] > 0 &]]
    |>
  ];

(* ── apply[] ─────────────────────────────────────────────────────────── *)
(*  Llama eval[] y escribe en Settings SOLO si cambio el acento o la
    regla ganadora.  Siempre actualiza ux.color.epoch.
    Estrategia DELETE+INSERT ya implementada en Settings`set.            *)
apply[] :=
  Module[{w = eval[], prevAccent, prevRule, changed},
    prevAccent = PersonalSite`Settings`get["ux.color.accent", ""];
    prevRule   = PersonalSite`Settings`get["ux.color.rule",   ""];
    changed    = (prevAccent =!= w["accent"]) || (prevRule =!= w["rule"]);
    If[changed,
      PersonalSite`Settings`set["ux.color.accent",  w["accent"]];
      PersonalSite`Settings`set["ux.color.surface", w["surface"]];
      PersonalSite`Settings`set["ux.color.intent",  w["intent"]];
      PersonalSite`Settings`set["ux.color.rule",    w["rule"]];
      PersonalSite`Settings`set["ux.color.score",   ToString[w["score"]]]
    ];
    (* epoch siempre actualiza: marca de actividad sin condicional *)
    PersonalSite`Settings`set["ux.color.epoch", ToString[w["epoch"]]];
    (* Ring buffer en memoria: maximo 10 entradas *)
    $history = Take[Prepend[$history, w], UpTo[10]];
    w
  ];

(* ── palette[] ───────────────────────────────────────────────────────── *)
(*  Lectura directa desde Settings: una sola consulta all[].
    Uso tipico: endpoint HTTP GET /ux/palette.                          *)
palette[] :=
  Module[{s = PersonalSite`Settings`all[]},
    <|"accent"  -> Lookup[s, "ux.color.accent",  "morning-amber"],
      "surface" -> Lookup[s, "ux.color.surface", "warm-light"],
      "intent"  -> Lookup[s, "ux.color.intent",  "focus"],
      "rule"    -> Lookup[s, "ux.color.rule",    "time-of-day"],
      "score"   -> Quiet[Check[ToExpression @ Lookup[s, "ux.color.score", "60"], 60]],
      "epoch"   -> Quiet[Check[ToExpression @ Lookup[s, "ux.color.epoch", "0"],   0]]
    |>
  ];

(* ── report[] ────────────────────────────────────────────────────────── *)
report[] :=
  <|"history" -> $history,
    "count"   -> Length[$history],
    "palette" -> palette[],
    "ts"      -> UnixTime[]
  |>;

End[];
EndPackage[];
