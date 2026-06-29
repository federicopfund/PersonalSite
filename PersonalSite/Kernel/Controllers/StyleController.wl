(* ::Package:: *)

(* PersonalSite`Controller`  (parte: style)
   ─────────────────────────────────────────────────────────────────────────
   HTTP API para inspeccionar y modificar en runtime las reglas del
   StyleEngine (pattern-match dispatch).

   Rutas:
     GET  /kernel/style/rules      → lista de reglas activas (JSON)
     POST /kernel/style/rule       → agregar / reemplazar una regla
     POST /kernel/style/remove     → eliminar una regla por id
     POST /kernel/style/reset      → restaurar las 12 reglas predeterminadas
   ─────────────────────────────────────────────────────────────────────────*)

BeginPackage["PersonalSite`Controller`"];

kernelStyleRules::usage   = "kernelStyleRules[req] lista las reglas activas del StyleEngine.";
kernelStyleAddRule::usage = "kernelStyleAddRule[req] agrega o reemplaza una regla por id.";
kernelStyleRemove::usage  = "kernelStyleRemove[req] elimina una regla por id.";
kernelStyleReset::usage   = "kernelStyleReset[req] restaura las reglas predeterminadas.";

Begin["`Private`"];

(* ── GET /kernel/style/rules ────────────────────────────────── *)
kernelStyleRules[_] :=
  HTTPResponse[
    Developer`WriteRawJSONString[<|
      "rules" -> PersonalSite`FrontEnd`StyleEngine`listRules[],
      "total" -> Length[PersonalSite`FrontEnd`StyleEngine`listRules[]]
    |>],
    <|"Content-Type" -> "application/json"|>];

(* ── POST /kernel/style/rule  ───────────────────────────────── *)
(*  FormRules esperados:
      id           (String, requerido)
      label        (String, opcional)
      pattern      (String WL, ej: "_List", "_?NumericQ", "{_Integer..}")
      order        (Integer, default 50)
      rendererCode (String WL que evalúa a Function[{expr,rule},…], opcional)
                   ⚠ ToExpression sobre código de usuario — server personal *)
kernelStyleAddRule[req_] :=
  Module[{form, id, label, pattern, order, rendCode, renderer, spec},
    form = Association @ Quiet[req["FormRules"], {}];

    id       = StringTrim @ Lookup[form, "id",          ""];
    label    = Lookup[form,          "label",        id];
    pattern  = Lookup[form,          "pattern",      "_"];
    order    = Quiet @ Check[
                 ToExpression @ Lookup[form, "order", "50"],
                 50];
    rendCode = Lookup[form,          "rendererCode", ""];

    If[id === "",
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"error" -> "id requerido"|>],
        <|"StatusCode" -> 400, "Content-Type" -> "application/json"|>]];

    (* Compilar renderer personalizado si se provee *)
    renderer = If[rendCode =!= "",
      Quiet @ Check[ToExpression[rendCode], None],
      None];

    spec = <|
      "label"   -> label,
      "pattern" -> pattern,
      "order"   -> If[IntegerQ[order], order, 50],
      If[renderer =!= None, "renderer" -> renderer, Nothing]
    |>;

    PersonalSite`FrontEnd`StyleEngine`addRule[id, spec];

    HTTPResponse[
      Developer`WriteRawJSONString[<|"ok" -> True, "id" -> id|>],
      <|"Content-Type" -> "application/json"|>]
  ];

(* ── POST /kernel/style/remove ─────────────────────────────── *)
kernelStyleRemove[req_] :=
  Module[{form, id},
    form = Association @ Quiet[req["FormRules"], {}];
    id   = StringTrim @ Lookup[form, "id", ""];
    If[id === "",
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"error" -> "id requerido"|>],
        <|"StatusCode" -> 400, "Content-Type" -> "application/json"|>]];
    PersonalSite`FrontEnd`StyleEngine`removeRule[id];
    HTTPResponse[
      Developer`WriteRawJSONString[<|"ok" -> True, "id" -> id|>],
      <|"Content-Type" -> "application/json"|>]
  ];

(* ── POST /kernel/style/reset ──────────────────────────────── *)
kernelStyleReset[_] := (
  PersonalSite`FrontEnd`StyleEngine`resetRules[];
  HTTPResponse[
    Developer`WriteRawJSONString[<|"ok" -> True|>],
    <|"Content-Type" -> "application/json"|>]
);

End[];
EndPackage[];
