(* ::Package:: *)

(* PersonalSite`Controller`  (parte: perf)
   --------------------------------------------------------------------------
   Pagina /perf: muestra el cache en runtime y el asset pesado servido desde
   memoria. Cuantifica la mejora: el asset cuesta `ms` computarlo (en subkernel)
   pero la pagina se sirve en `served` ms leyendolo del cache. *)

BeginPackage["PersonalSite`Controller`"];

perf::usage = "perf[request] renderiza /perf: cache + asset pesado + latencia.";

Begin["`Private`"];

esc2[x_] := PersonalSite`View`escape[ToString[x]];

secs[x_] := If[NumericQ[x], ToString[Round[x, 0.1]] <> " s", "&mdash;"];

perf[request_] :=
  Module[{t0 = AbsoluteTime[], s, m, k, served},
    s = PersonalSite`Cache`stats[];
    m = PersonalSite`Assets`metric[];          (* lee de cache (rapido) o computa (frio) *)
    k = Lookup[m, "kernel", 0];
    served = Round[1000. (AbsoluteTime[] - t0)];

    PersonalSite`View`render["perf", <|
      "hits"        -> ToString[Lookup[s, "hits", 0]],
      "misses"      -> ToString[Lookup[s, "misses", 0]],
      "ratio"       -> ToString[Round[100 Lookup[s, "ratio", 0.]]] <> " %",
      "count"       -> ToString[Lookup[s, "count", 0]],
      "keys"        -> esc2[StringRiffle[Lookup[s, "keys", {}], ", "]],
      "metricValue" -> esc2[NumberForm[Lookup[m, "value", 0.], {8, 5}]],
      "metricMs"    -> ToString[Lookup[m, "ms", 0]] <> " ms",
      "metricKernel"-> If[IntegerQ[k] && k > 0, "subkernel #" <> ToString[k], "kernel principal"],
      "metricAge"   -> secs[PersonalSite`Cache`age["metric"]],
      "homeAge"     -> secs[PersonalSite`Cache`age["home.cards"]],
      "blogAge"     -> secs[PersonalSite`Cache`age["blog.cards"]],
      "served"      -> ToString[served] <> " ms"
    |>]
  ];

End[];
EndPackage[];
