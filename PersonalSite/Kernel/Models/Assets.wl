(* ::Package:: *)

(* PersonalSite`Assets`
   --------------------------------------------------------------------------
   Define los "assets" servibles desde cache y como recalcularlos. La parte
   liviana (tarjetas de home/blog) se cachea para evitar consultar la base en
   cada request; la parte pesada (metric) se computa en un SUBKERNEL via
   ParallelSubmit (se reparte fuera del kernel del pool) y se cachea.

   Assets`refresh[] lo corre una ScheduledTask en segundo plano: actualiza el
   asset en runtime sin que ningun request pague el costo. *)

BeginPackage["PersonalSite`Assets`"];

homeCards::usage = "homeCards[] devuelve (desde cache) el HTML de las tarjetas de la home.";
blogCards::usage = "blogCards[] devuelve (desde cache) el HTML de las tarjetas del blog.";
metric::usage    = "metric[] devuelve (desde cache) el asset pesado: <|value, ms, kernel, at|>.";
refresh::usage   = "refresh[] recalcula y cachea TODOS los assets (cards + metric); se usa en el arranque.";
refreshCards::usage = "refreshCards[] recalcula solo las tarjetas (barato); lo corre una ScheduledTask frecuente.";
refreshMetric::usage = "refreshMetric[] recalcula solo el asset pesado (en subkernel); lo corre una ScheduledTask poco frecuente.";

Begin["`Private`"];

computeHomeCards[] :=
  StringRiffle[PersonalSite`View`postItem /@ PersonalSite`Post`recent[3], "\n"];

computeBlogCards[] :=
  StringRiffle[PersonalSite`View`postItem /@ PersonalSite`Post`recent[10], "\n"];

homeCards[] := PersonalSite`Cache`getOr["home.cards", computeHomeCards[]];
blogCards[] := PersonalSite`Cache`getOr["blog.cards", computeBlogCards[]];

(* Asset pesado y autocontenido (solo simbolos de System`), apto para correr en
   un subkernel sin DistributeDefinitions. Devuelve {valor, $KernelID}. *)
heavyValue := Module[{s = 0.}, Do[s += Sin[1. k]/k, {k, 1, 350000}]; s];

computeMetric[] :=
  Module[{t0 = AbsoluteTime[], n, res, val, where},
    n = Quiet @ PersonalSite`Flow`ensureKernels[];
    res =
      If[IntegerQ[n] && n > 0,
        Quiet @ Check[
          (* TimeConstrained evita que WaitAll bloquee el kernel si el
             subkernel falla o no responde (produccion: POOLSIZE=1). *)
          TimeConstrained[
            First @ WaitAll[{ParallelSubmit[
              {Module[{s = 0.}, Do[s += Sin[1. k]/k, {k, 1, 350000}]; s], $KernelID}]}],
            25,    (* maximo 25 s; si vence, cae al fallback heavyValue *)
            $Failed],
          $Failed],
        $Failed];
    {val, where} =
      If[ListQ[res] && Length[res] === 2, res, {heavyValue, $KernelID}];
    <|
      "value"  -> val,
      "ms"     -> Round[1000. (AbsoluteTime[] - t0)],
      "kernel" -> where,
      "at"     -> AbsoluteTime[]
    |>
  ];

metric[] := PersonalSite`Cache`getOr["metric", computeMetric[]];

(* Barato: solo consulta liviana a SQLite + render de tarjetas. Sin spikes. *)
refreshCards[] :=
  (
    PersonalSite`Cache`set["home.cards", computeHomeCards[]];
    PersonalSite`Cache`set["blog.cards", computeBlogCards[]];
    True
  );

(* Pesado: computa en subkernel. Se corre con baja frecuencia para no generar
   latencia en el kernel del pool. *)
refreshMetric[] :=
  (PersonalSite`Cache`set["metric", computeMetric[]]; True);

refresh[] := (refreshCards[]; refreshMetric[]; True);

End[];
EndPackage[];
