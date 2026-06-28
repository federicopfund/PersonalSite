(* ::Package:: *)

(* PersonalSite`Controller`NestController
   --------------------------------------------------------------------------
   Endpoints HTTP para el NestScheduler:

     GET  /nest              → UI interactiva del NestGraph
     POST /nest/run          → ejecuta el NestGraph (params via JSON body)
     GET  /nest/results      → ultimo resultado como JSON (Power BI)
     GET  /nest/export.csv   → ultimo resultado como CSV  (Power BI)
     POST /nest/schedule     → activa ScheduledTask
     POST /nest/cancel       → cancela ScheduledTask
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`Controller`"];

nest::usage    = "nest[req] sirve la UI del NestScheduler.";
nestRun::usage = "nestRun[req] ejecuta el NestGraph y devuelve JSON.";
nestExport::usage = "nestExport[req] exporta resultados para Power BI.";

Begin["`Private`"];

(* Reglas predeterminadas (las del grafo de la imagen) *)
$defaultRules   = {2 # + 1 &, # + 14 &, # - 18 &};
$defaultSeeds   = {1};
$defaultDepth   = 3;
$defaultBackend = "session";

(* WolframWebEngine no expone JSON bodies (Body->None).
   Leer desde FormRules (application/x-www-form-urlencoded)
   o desde Query (query string), en ese orden. *)
parseBody[req_] :=
  Module[{fd = req["FormRules"], qp = req["Query"]},
    If[ListQ[fd] && Length[fd] > 0, Return[Association[fd]]];
    If[ListQ[qp] && Length[qp] > 0, Return[Association[qp]]];
    <||>];

(* Extrae la lista de reglas desde el body o usa las por defecto.
   El cliente puede enviar: "rules": [[2,1,0],[1,0,14],[1,-18,0]]
   (coeficientes [a,b,c] => a*x^2 + b*x + c) o simplemente omitirlas. *)
bodyRules[body_Association] :=
  Module[{raw = Lookup[body, "rules", {}]},
    If[MatchQ[raw, {{__?NumericQ} ..}],
      Map[Function[coeffs,
        With[{a = coeffs[[1]], b = coeffs[[2]], c = coeffs[[3]]},
          (a #^2 + b # + c &)]], raw],
      $defaultRules]
  ];

bodySeeds[body_Association] :=
  With[{s = Lookup[body, "seeds", $defaultSeeds]},
    If[MatchQ[s, {__?NumericQ}], s, $defaultSeeds]];

bodyDepth[body_Association] :=
  With[{d = Lookup[body, "depth", $defaultDepth]},
    If[IntegerQ[d] && 1 <= d <= 5, d, $defaultDepth]];

bodyBackend[body_Association] :=
  With[{b = Lookup[body, "backend", $defaultBackend]},
    If[MemberQ[{"sync", "session", "parallel"}, b], b, $defaultBackend]];

(* ---- GET /nest --------------------------------------------------------- *)
nest[req_] :=
  Module[{info = PersonalSite`NestScheduler`taskInfo[],
          last = PersonalSite`NestScheduler`results[]},
    PersonalSite`View`render["nest",
      <|"taskActive"  -> If[info["active"], "true", "false"],
        "runCount"    -> ToString[info["runCount"]],
        "lastRun"     -> If[info["lastRun"] === None, "—", DateString[info["lastRun"]]],
        "nodeCount"   -> If[KeyExistsQ[last, "built"],
                            ToString[last["built"]["nodeCount"]], "0"],
        "depth"       -> If[KeyExistsQ[last, "built"],
                            ToString[last["built"]["depth"]], ToString[$defaultDepth]],
        "elapsed"     -> If[KeyExistsQ[last, "elapsed"],
                            ToString[Round[last["elapsed"], 0.001]] <> " s", "—"],
        "backend"     -> If[KeyExistsQ[last, "backend"], last["backend"], $defaultBackend],
        "defaultDepth"  -> ToString[$defaultDepth],
        "defaultSeeds"  -> StringRiffle[ToString /@ $defaultSeeds, ","],
        "ruleCount"     -> ToString[Length[$defaultRules]]
      |>]
  ];

(* ---- POST /nest/run ---------------------------------------------------- *)
nestRun[req_] :=
  Module[{body = parseBody[req], res, ok},
    res = Quiet @ Check[
      PersonalSite`NestScheduler`run[
        bodyRules[body],
        bodySeeds[body],
        bodyDepth[body],
        bodyBackend[body]],
      $Failed];
    ok = AssociationQ[res] && TrueQ[res["flow"]["ok"]];
    HTTPResponse[
      ExportString[<|
        "ok"        -> ok,
        "nodeCount" -> If[ok, res["built"]["nodeCount"], 0],
        "depth"     -> If[ok, res["built"]["depth"], 0],
        "elapsed"   -> If[ok, res["elapsed"], 0],
        "backend"   -> If[ok, res["backend"], ""],
        "layers"    -> If[ok, Length[res["flow"]["layers"]], 0],
        "error"     -> If[ok, Null, "execution failed"]
      |>, "JSON"],
      <|"StatusCode" -> If[ok, 200, 500],
        "Headers" -> <|"Content-Type" -> "application/json"|>|>]
  ];

(* ---- GET /nest/results ------------------------------------------------- *)
nestResults[req_] :=
  HTTPResponse[
    PersonalSite`NestScheduler`export["json"],
    <|"StatusCode" -> 200,
      "Headers" -> <|"Content-Type" -> "application/json"|>|>];

(* ---- GET /nest/export.csv --------------------------------------------- *)
nestExport[req_] :=
  HTTPResponse[
    PersonalSite`NestScheduler`export["csv"],
    <|"StatusCode" -> 200,
      "Headers" -> <|
        "Content-Type"        -> "text/csv; charset=utf-8",
        "Content-Disposition" -> "attachment; filename=nestgraph.csv"|>|>];

(* ---- POST /nest/schedule ----------------------------------------------- *)
nestSchedule[req_] :=
  Module[{body = parseBody[req], iv, res},
    iv = With[{i = Lookup[body, "interval", 60]},
           If[NumericQ[i] && i >= 5, i, 60]];
    res = PersonalSite`NestScheduler`schedule[
      bodyRules[body], bodySeeds[body], bodyDepth[body], iv, bodyBackend[body]];
    HTTPResponse[
      ExportString[<|"ok" -> True, "interval" -> iv|>, "JSON"],
      <|"StatusCode" -> 200,
        "Headers" -> <|"Content-Type" -> "application/json"|>|>]
  ];

(* ---- POST /nest/cancel ------------------------------------------------- *)
nestCancel[req_] :=
  HTTPResponse[
    ExportString[<|"ok" -> PersonalSite`NestScheduler`cancel[]|>, "JSON"],
    <|"StatusCode" -> 200,
      "Headers" -> <|"Content-Type" -> "application/json"|>|>];

End[];
EndPackage[];
