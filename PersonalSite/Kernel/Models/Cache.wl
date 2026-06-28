(* ::Package:: *)

(* PersonalSite`Cache`
   --------------------------------------------------------------------------
   Cache en memoria, por kernel del pool (persiste entre requests dentro del
   mismo kernel). Sirve assets ya calculados para que el request no pague el
   costo de recomputarlos. Una ScheduledTask en segundo plano lo mantiene
   caliente (ver Scheduler + Assets`refresh). *)

BeginPackage["PersonalSite`Cache`"];

get::usage   = "get[key] devuelve el valor cacheado o Missing; cuenta hit/miss.";
set::usage   = "set[key, value] guarda value con timestamp y lo devuelve.";
getOr::usage = "getOr[key, expr] devuelve el valor cacheado o evalua expr (HoldRest), lo guarda y lo devuelve.";
age::usage   = "age[key] devuelve los segundos desde que se cacheo key, o Missing.";
has::usage   = "has[key] indica si key esta en cache.";
stats::usage = "stats[] devuelve <|\"hits\",\"misses\",\"ratio\",\"keys\",\"count\"|>.";
clear::usage = "clear[] vacia el cache y reinicia los contadores.";

Begin["`Private`"];

$store  = <||>;
$hits   = 0;
$misses = 0;

has[key_] := KeyExistsQ[$store, key];

get[key_] :=
  If[KeyExistsQ[$store, key],
    $hits++;   $store[key]["value"],
    $misses++; Missing["NotCached", key]];

set[key_, value_] :=
  ($store[key] = <|"value" -> value, "at" -> AbsoluteTime[]|>; value);

SetAttributes[getOr, HoldRest];
getOr[key_, expr_] :=
  Module[{v = get[key]}, If[MissingQ[v], set[key, expr], v]];

age[key_] :=
  If[KeyExistsQ[$store, key], AbsoluteTime[] - $store[key]["at"], Missing["NotCached", key]];

stats[] :=
  <|
    "hits"   -> $hits,
    "misses" -> $misses,
    "ratio"  -> If[$hits + $misses > 0, N[$hits/($hits + $misses)], 0.],
    "keys"   -> Keys[$store],
    "count"  -> Length[$store]
  |>;

clear[] := ($store = <||>; $hits = 0; $misses = 0;);

End[];
EndPackage[];
