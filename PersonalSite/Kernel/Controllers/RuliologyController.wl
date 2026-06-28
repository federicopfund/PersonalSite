(* ::Package:: *)

(* PersonalSite`Controller` (parte: ruliology)
   --------------------------------------------------------------------------
   "De 3^k a la confluencia: un sistema multiway aritmético causalmente
    invariante con conjunto de estados fractal"

   Rutas expuestas:
     GET  /ruliology          → página interactiva (KaTeX + eval en vivo)
     POST /ruliology/eval     → evalúa expresión NOMBRADA del registro seguro
     GET  /ruliology/metrics  → métricas pre-computadas (JSON, TTL 5 min)

   Diseño de seguridad
   -------------------
   POST /ruliology/eval NO acepta WL arbitrario del cliente.
   Solo despacha por clave («key») desde el registro interno $ruliologyExprs.
   Para WL libre usa el endpoint existente POST /kernel/eval.
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`Controller`"];

ruliologyPage::usage    = "ruliologyPage[req] renderiza /ruliology.";
ruliologyEval::usage    = "ruliologyEval[req] evalúa expresión nombrada del registro; devuelve JSON.";
ruliologyMetrics::usage = "ruliologyMetrics[req] devuelve métricas pre-computadas en JSON.";

Begin["`Private`"];

(* ── Primitivas del sistema multiway ────────────────────────────────── *)
(* Definidas inline en cada HoldForm para que cada evaluación sea         *)
(* auto-contenida y no dependa del estado global del kernel.              *)

(* Función de crecimiento — reutilizada en computeAllMetrics[]           *)
ruliologyGrowthSeries[depth_Integer] :=
  Module[{front = {1}, out = {1}},
    Do[
      front = DeleteDuplicates[Flatten[{2# + 1, # + 14, # - 18} & /@ front]];
      AppendTo[out, Length[front]],
      {depth}];
    out];

(* ── Registro de expresiones nombradas (seguro) ─────────────────────── *)
$ruliologyExprs = <|

  (* Serie de crecimiento por generación                                  *)
  "growth_series" -> HoldForm[
    Module[{front = {1}, out = {1}},
      Do[front = DeleteDuplicates[Flatten[{2#+1, #+14, #-18} & /@ front]];
         AppendTo[out, Length[front]], {20}];
      out]],

  (* Tasa λ, dimensión de caja y valores en generación 21-22             *)
  "growth_rate" -> HoldForm[
    Module[{s},
      s = Module[{front = {1}, out = {1}},
            Do[front = DeleteDuplicates[Flatten[{2#+1,#+14,#-18} & /@ front]];
               AppendTo[out, Length[front]], {22}]; out];
      <|"lambda"      -> N[Last[s] / s[[-2]]],
        "box_dim_log2"-> N[Log[2, Last[s] / s[[-2]]]],
        "states_g22"  -> Last[s],
        "states_g21"  -> s[[-2]]|>]],

  (* Tabla de colapso: hilos 3^d vs estados distinguibles d=1..12       *)
  "collapse_table" -> HoldForm[
    Module[{front = {1}, out = {1}},
      Table[
        front = DeleteDuplicates[Flatten[{2#+1,#+14,#-18} & /@ front]];
        AppendTo[out, Length[front]];
        <|"d"        -> d,
          "threads"  -> 3^d,
          "states"   -> out[[d+1]],
          "collapse" -> N[3^d / out[[d+1]], 5]|>,
        {d, 1, 12}]]],

  (* TEOREMA forma cerrada — verifica S ∩ [-W,W] = {2^(p+1)-1+14q-18r} *)
  "closed_form_check" -> HoldForm[
    Module[{front = {1}, reach = {1}, brute, canon, W = 600},
      Do[front = DeleteDuplicates[Flatten[{2#+1,#+14,#-18} & /@ front]];
         reach = Union[reach, front], {11}];
      brute = Select[reach, -W <= # <= W &];
      canon = DeleteDuplicates @ Select[
        Flatten @ Table[2^(p+1)-1 + 14q - 18r, {p,0,10},{q,0,50},{r,0,50}],
        -W <= # <= W &];
      <|"match"       -> (Sort[brute] === Sort[canon]),
        "brute_count" -> Length[brute],
        "canon_count" -> Length[canon],
        "window"      -> W|>]],

  (* PROPOSICIÓN confluencia — sistema de reescritura + forma canónica   *)
  "confluence_check" -> HoldForm[
    Module[{nf, aw, tests},
      nf[w_] := w //. {
        {h___, "a","b",t___} :> {h,"b","b","a",t},
        {h___, "a","c",t___} :> {h,"c","c","a",t},
        {h___, "c","b",t___} :> {h,"b","c",t}};
      aw[w_] := Fold[Switch[#2,"a",2#1+1,"b",#1+14,"c",#1-18]&,1,Reverse[w]];
      SeedRandom[7];
      tests = Table[RandomChoice[{"a","b","c"}, RandomInteger[{4,11}]], {300}];
      <|"values_preserved" -> AllTrue[tests, aw[#] === aw[nf[#]] &],
        "canonical_shape"  -> AllTrue[tests, MatchQ[nf[#], {"b"...,"c"...,"a"...}] &],
        "sample_size"      -> Length[tests],
        "example_word"     -> RandomChoice[tests],
        "example_normal"   -> nf[RandomChoice[tests]]|>]],

  (* Invariante de paridad — todo estado alcanzable es impar             *)
  "parity_check" -> HoldForm[
    Module[{front = {1}, reach = {1}},
      Do[front = DeleteDuplicates[Flatten[{2#+1,#+14,#-18} & /@ front]];
         reach = Union[reach, front], {10}];
      <|"all_odd"    -> AllTrue[reach, OddQ],
        "even_count" -> Count[reach, _?EvenQ],
        "sample_n"   -> Length[reach],
        "min_state"  -> Min[reach],
        "max_state"  -> Max[reach]|>]],

  (* Verifica identidades de funciones: a∘b = b²∘a, a∘c = c²∘a, b∘c = c∘b *)
  "function_identities" -> HoldForm[
    Module[{a, b, c, pts},
      a[n_] := 2n+1; b[n_] := n+14; c[n_] := n-18;
      pts = Range[-50, 50, 7];
      <|"a_circ_b_eq_b2_circ_a" -> AllTrue[pts, a[b[#]] === b[b[a[#]]] &],
        "a_circ_c_eq_c2_circ_a" -> AllTrue[pts, a[c[#]] === c[c[a[#]]] &],
        "b_circ_c_eq_c_circ_b"  -> AllTrue[pts, b[c[#]] === c[b[#]] &],
        "test_points"           -> Length[pts]|>]],

  (* Info del kernel que responde                                         *)
  "kernel_info" -> HoldForm[
    <|"kernelID"  -> $KernelID,
      "processID" -> $ProcessID,
      "version"   -> $Version,
      "timeUsed"  -> Round[TimeUsed[], .01],
      "uptime"    -> Round[AbsoluteTime[] - $StartTime]|>]
|>;

(* ── Cache de métricas pre-computadas ───────────────────────────────── *)
$ruliologyCache   = <||>;
$ruliologyCacheTs = 0;
$ruliologyTTL     = 300;   (* 5 minutos *)

computeRuliologyMetrics[] :=
  Module[{serie, last, prev, lam, dim},
    serie = ruliologyGrowthSeries[22];
    last  = Last[serie];
    prev  = serie[[-2]];
    lam   = N[last / prev];
    dim   = N[Log[2, lam]];
    <|"generation_count" -> Length[serie] - 1,
      "states_g22"       -> last,
      "states_g21"       -> prev,
      "lambda"           -> lam,
      "box_dim"          -> dim,
      "series_head"      -> Take[serie, 9],
      "collapse_g10"     -> N[3^10 / serie[[11]], 5],
      "threads_g10"      -> 3^10,
      "states_g10"       -> serie[[11]],
      "rules"            -> {"ab\[Rule]b\[CenterDot]b\[CenterDot]a",
                             "ac\[Rule]c\[CenterDot]c\[CenterDot]a",
                             "cb\[Rule]bc"},
      "canonical_form"   -> "b^q c^r a^p",
      "closed_set"       -> "{ 2^(p+1)-1 + 14q - 18r : p,q,r \[Element] Z>=0 }",
      "computed_at"      -> UnixTime[]|>];

cachedRuliologyMetrics[] :=
  If[UnixTime[] - $ruliologyCacheTs < $ruliologyTTL && Length[$ruliologyCache] > 0,
    $ruliologyCache,
    $ruliologyCache   = computeRuliologyMetrics[];
    $ruliologyCacheTs = UnixTime[];
    $ruliologyCache];

(* ── Evaluación segura de expresión nombrada ────────────────────────── *)
evalRuliologyNamed[key_String] :=
  Module[{expr, t0, result, ms},
    If[!KeyExistsQ[$ruliologyExprs, key],
      Return[<|"error" -> ("clave desconocida: " <> key),
               "keys"  -> Keys[$ruliologyExprs]|>]];
    expr   = $ruliologyExprs[key];
    t0     = AbsoluteTime[];
    result = Quiet @ Check[ReleaseHold[expr], $Failed];
    ms     = Round[1000. (AbsoluteTime[] - t0), .01];
    <|"key"    -> key,
      "ms"     -> ms,
      "out"    -> Quiet @ Check[ToString[result, OutputForm], "?"],
      "tex"    -> Quiet @ Check[ToString[result, TeXForm], ""],
      "kernel" -> $KernelID,
      "ts"     -> UnixTime[]|>];

(* ── POST /ruliology/eval ───────────────────────────────────────────── *)
ruliologyEval[req_] :=
  Module[{pairs, key},
    pairs = Quiet[req["FormRules"], {}];
    key   = StringTrim @ Replace[
              Lookup[If[ListQ[pairs], Association[pairs], <||>], "key", ""],
              Except[_String] -> ""];
    If[key === "",
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"error" -> "falta parametro 'key'",
                                       "keys"  -> Keys[$ruliologyExprs]|>],
        <|"StatusCode" -> 400, "Content-Type" -> "application/json"|>]];
    HTTPResponse[
      Developer`WriteRawJSONString[evalRuliologyNamed[key]],
      <|"Content-Type" -> "application/json"|>]];

(* ── GET /ruliology/metrics ─────────────────────────────────────────── *)
ruliologyMetrics[req_] :=
  HTTPResponse[
    Developer`WriteRawJSONString[cachedRuliologyMetrics[]],
    <|"Content-Type"  -> "application/json",
      "Cache-Control" -> "public, max-age=300"|>];

(* ── GET /ruliology ─────────────────────────────────────────────────── *)
ruliologyPage[req_] :=
  PersonalSite`View`render["ruliology", <||>];

End[];
EndPackage[];
