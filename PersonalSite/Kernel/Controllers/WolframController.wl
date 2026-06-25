(* ::Package:: *)

(* PersonalSite`Controller`  (parte: wolfram)
   --------------------------------------------------------------------------
   Pagina /ask: formulario de consulta y resultado de Wolfram|Alpha. *)

BeginPackage["PersonalSite`Controller`"];

ask::usage =
  "ask[request] renderiza la pagina /ask, respondiendo el parametro de query 'q' si esta presente.";

Begin["`Private`"];

ask[request_] := askRespond[askQueryParam[request, "q"]];

(* Extrae un parametro de query como String (HTTPRequestData["Query"] devuelve
   una lista de reglas {\"q\" -> \"valor\"} ya URL-decodificada). *)
askQueryParam[request_, key_String] :=
  Module[{pairs = Quiet[request["Query"], {}]},
    Replace[
      Lookup[If[ListQ[pairs], Association[pairs], <||>], key, ""],
      Except[_String] -> ""
    ]
  ];

askRespond[query_String] :=
  Module[{trimmed = StringTrim[query], html = ""},
    If[StringLength[trimmed] > 0,
      html = askResultBox[PersonalSite`WolframAlpha`query[trimmed]]];
    PersonalSite`View`render["ask", <|
      "query"      -> PersonalSite`View`escape[query],
      "resultHtml" -> html
    |>]
  ];

askResultBox[result_Association] :=
  If[TrueQ[result["ok"]],
    "<div class=\"ask-box ask-success\">" <>
      "<span class=\"ask-label\">Wolfram Alpha responde</span>" <>
      "<p class=\"ask-answer\">" <> PersonalSite`View`escape[result["answer"]] <> "</p>" <>
      "<span class=\"ask-src\">Fuente: Wolfram Alpha</span></div>",
    "<div class=\"ask-box ask-fail\">" <>
      "<span class=\"ask-label\">Sin resultado</span>" <>
      "<p>" <> PersonalSite`View`escape[result["error"]] <> "</p></div>"
  ];

End[];
EndPackage[];

