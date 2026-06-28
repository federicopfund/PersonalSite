(* ::Package:: *)

(* PersonalSite`Theme`
   --------------------------------------------------------------------------
   Logica del tema visual del sitio. El usuario elige la REGLA en /apariencia:
     - manual : un tema fijo elegido a mano.
     - auto   : el tema rota en el tiempo segun un orden y un intervalo.

   En modo auto el tema activo es una funcion determinista del tiempo
   (UnixTime), de modo que TODOS los kernels del pool calculan el mismo valor
   sin condiciones de carrera. La ScheduledTask `theme-rotate` solo persiste
   ese valor (ver Scheduler y resolve[]/tick[]). *)

BeginPackage["PersonalSite`Theme`"];

list::usage         = "list[] devuelve el orden de temas configurado.";
active::usage        = "active[] devuelve el tema activo persistido (validado).";
mode::usage          = "mode[] devuelve \"auto\" o \"manual\".";
interval::usage      = "interval[] devuelve los segundos por tema en modo auto.";
computeFromTime::usage = "computeFromTime[t] devuelve el tema que corresponde al instante t (UnixTime).";
secondsToNext::usage = "secondsToNext[t] devuelve los segundos hasta el proximo cambio en modo auto.";
resolve::usage       = "resolve[] devuelve el tema a aplicar AHORA (una sola consulta a settings).";
clientConfig::usage  = "clientConfig[] devuelve el tema resuelto + metadatos (modo, orden, intervalo, epoch del servidor) para rotar en vivo en el navegador.";
panel::usage         = "panel[] devuelve <|mode, order, interval, active, live, secs|> con UNA sola consulta a settings (para /apariencia).";
tick::usage          = "tick[] (lo corre la ScheduledTask): en auto recalcula y persiste el tema.";
valid::usage         = "valid[t] indica si t es un tema del orden configurado.";
setMode::usage       = "setMode[m] guarda el modo (\"auto\"/\"manual\").";
setActive::usage     = "setActive[t] guarda el tema activo (si es valido).";
setInterval::usage   = "setInterval[s] guarda el intervalo de rotacion en segundos.";

Begin["`Private`"];

$fallback = {"slate", "sand", "forest", "rose", "ocean"};

parseOrder[s_String] :=
  Module[{xs = Select[StringTrim /@ StringSplit[s, ","], # =!= "" &]},
    If[Length[xs] > 0, xs, $fallback]];
parseOrder[_] := $fallback;

parseInterval[s_] :=
  Module[{n = Quiet @ ToExpression @ ToString[s]},
    If[IntegerQ[n] && n > 0, n, 20]];

themeFromTime[order_List, iv_Integer, t_] :=
  order[[ Mod[Floor[t / iv], Length[order]] + 1 ]];

(* --- Lectores individuales (frecuencia baja: pagina /apariencia) --------- *)
list[]     := parseOrder @ PersonalSite`Settings`get["theme.order", ""];
mode[]     := If[PersonalSite`Settings`get["theme.mode", "manual"] === "auto", "auto", "manual"];
interval[] := parseInterval @ PersonalSite`Settings`get["theme.interval", "20"];

active[] :=
  Module[{a = PersonalSite`Settings`get["theme.active", First @ $fallback], xs = list[]},
    If[MemberQ[xs, a], a, First @ xs]];

valid[t_] := MemberQ[list[], t];

computeFromTime[t_ : Automatic] :=
  themeFromTime[list[], interval[], If[t === Automatic, UnixTime[], t]];

secondsToNext[t_ : Automatic] :=
  With[{iv = interval[], ts = If[t === Automatic, UnixTime[], t]}, iv - Mod[ts, iv]];

(* --- Camino caliente: una sola consulta a settings (layout en cada vista) - *)
resolve[] :=
  Module[{s = PersonalSite`Settings`all[], order, iv, m, a},
    order = parseOrder @ Lookup[s, "theme.order", ""];
    iv    = parseInterval @ Lookup[s, "theme.interval", "20"];
    m     = If[Lookup[s, "theme.mode", "manual"] === "auto", "auto", "manual"];
    If[m === "auto",
      themeFromTime[order, iv, UnixTime[]],
      a = Lookup[s, "theme.active", First @ order];
      If[MemberQ[order, a], a, First @ order]
    ]
  ];

(* La ScheduledTask llama tick[]: en auto persiste el tema derivado del tiempo
   (idempotente entre kernels: todos calculan el mismo valor). *)
tick[] :=
  If[mode[] === "auto",
    Module[{want = computeFromTime[]},
      If[want =!= active[], PersonalSite`Settings`set["theme.active", want]];
      want],
    active[]];

(* Config para el cliente: tema resuelto + metadatos para rotar EN VIVO en el
   navegador, sincronizado al reloj del servidor (misma formula que tick[]). *)
clientConfig[] :=
  Module[{s = PersonalSite`Settings`all[], order, iv, m, a},
    order = parseOrder @ Lookup[s, "theme.order", ""];
    iv    = parseInterval @ Lookup[s, "theme.interval", "20"];
    m     = If[Lookup[s, "theme.mode", "manual"] === "auto", "auto", "manual"];
    a     = Lookup[s, "theme.active", First @ order];
    <|
      "theme"    -> If[m === "auto",
                       themeFromTime[order, iv, UnixTime[]],
                       If[MemberQ[order, a], a, First @ order]],
      "mode"     -> m,
      "order"    -> StringRiffle[order, ","],
      "interval" -> iv,
      "epoch"    -> UnixTime[]
    |>
  ];

(* Una sola consulta a settings -> todo el estado para el panel /apariencia.
   Evita las ~6 consultas separadas de list/mode/active/interval/etc. *)
panel[] :=
  Module[{s = PersonalSite`Settings`all[], order, iv, m, a, ts},
    order = parseOrder @ Lookup[s, "theme.order", ""];
    iv    = parseInterval @ Lookup[s, "theme.interval", "20"];
    m     = If[Lookup[s, "theme.mode", "manual"] === "auto", "auto", "manual"];
    a     = Lookup[s, "theme.active", First @ order];
    a     = If[MemberQ[order, a], a, First @ order];
    ts    = UnixTime[];
    <|
      "mode"     -> m,
      "order"    -> order,
      "interval" -> iv,
      "active"   -> a,
      "live"     -> themeFromTime[order, iv, ts],
      "secs"     -> iv - Mod[ts, iv]
    |>
  ];

setMode[m_]   := PersonalSite`Settings`set["theme.mode", If[m === "auto", "auto", "manual"]];
setActive[t_] := If[valid[t], PersonalSite`Settings`set["theme.active", t], active[]];
setInterval[s_] :=
  PersonalSite`Settings`set["theme.interval", ToString @ Min[parseInterval[s], 3600]];

End[];
EndPackage[];
