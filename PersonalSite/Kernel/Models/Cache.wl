(* ::Package:: *)

(* PersonalSite`Cache`
   --------------------------------------------------------------------------
   Cache en memoria, por kernel del pool (persiste entre requests dentro del
   mismo kernel). Sirve assets ya calculados para que el request no pague el
   costo de recomputarlos. Una ScheduledTask en segundo plano lo mantiene
   caliente (ver Scheduler + Assets`refresh). *)

(* Usa Begin/End en lugar de BeginPackage/EndPackage para evitar el warning
   General::shdw: los simbolos get/set son internos y todos los llamadores
   ya usan la ruta completa PersonalSite`Cache`get[...]. *)
Begin["PersonalSite`Cache`Private`"];

$store  = <||>;
$hits   = 0;
$misses = 0;

PersonalSite`Cache`has[key_] := KeyExistsQ[$store, key];

PersonalSite`Cache`get[key_] :=
  If[KeyExistsQ[$store, key],
    $hits++;   $store[key]["value"],
    $misses++; Missing["NotCached", key]];

PersonalSite`Cache`set[key_, value_] :=
  ($store[key] = <|"value" -> value, "at" -> AbsoluteTime[]|>; value);

SetAttributes[PersonalSite`Cache`getOr, HoldRest];
PersonalSite`Cache`getOr[key_, expr_] :=
  Module[{v = PersonalSite`Cache`get[key]},
    If[MissingQ[v], PersonalSite`Cache`set[key, expr], v]];

PersonalSite`Cache`age[key_] :=
  If[KeyExistsQ[$store, key], AbsoluteTime[] - $store[key]["at"], Missing["NotCached", key]];

PersonalSite`Cache`stats[] :=
  <|
    "hits"   -> $hits,
    "misses" -> $misses,
    "ratio"  -> If[$hits + $misses > 0, N[$hits/($hits + $misses)], 0.],
    "keys"   -> Keys[$store],
    "count"  -> Length[$store]
  |>;

PersonalSite`Cache`clear[] := ($store = <||>; $hits = 0; $misses = 0;);

End[];
