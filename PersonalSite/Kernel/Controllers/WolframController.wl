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
      html = askResultHtml[PersonalSite`WolframAlpha`result[trimmed]]];
    PersonalSite`View`render["ask", <|
      "query"      -> PersonalSite`View`escape[query],
      "resultHtml" -> html
    |>]
  ];

(* Resultado completo: una tarjeta por pod, o un aviso si no hubo resultado. *)
askResultHtml[res_Association] :=
  If[TrueQ[res["ok"]],
    "<div class=\"wa-pods\">" <> StringJoin[podHtml /@ res["pods"]] <> "</div>",
    "<div class=\"ask-box ask-fail\">" <>
      "<span class=\"ask-label\">Sin resultado</span>" <>
      "<p>" <> PersonalSite`View`escape[res["error"]] <> "</p></div>"
  ];

(* Una pod: titulo + cuerpo.
   Si la pod tiene "cellHtml" (celdas nativas exportadas del kernel),
   se usa directamente. Si no, se muestran imagenes y/o texto plano.
   Ambas fuentes pueden coexistir (ej. LLM API siempre usa texto+imagenes). *)
podHtml[pod_Association] :=
  Module[{title, text, images, cellHtml, imgHtml, textHtml, body},
    title    = PersonalSite`View`escape[pod["title"]];
    text     = pod["text"];
    images   = pod["images"];
    cellHtml = Lookup[pod, "cellHtml", ""];
    body = If[StringLength[cellHtml] > 0,
      (* Celdas nativas: HTML con MathML generado por ExportString *)
      "<div class=\"wa-pod-cells\">" <> cellHtml <> "</div>",
      (* Fallback: imagenes y texto plano *)
      Module[{ih, th},
        ih = StringJoin[podImg[#,
          If[StringLength[text] > 0,
            StringTake[text, Min[120, StringLength[text]]], ""]] & /@ images];
        th = If[StringLength[StringTrim[text]] > 0,
          "<pre class=\"wa-pod-text\">" <> PersonalSite`View`escape[text] <> "</pre>",
          ""];
        ih <> th
      ]
    ];
    If[StringLength[body] === 0, Return[""]];
    "<section class=\"wa-pod\">" <>
      "<h3 class=\"wa-pod-title\">" <> title <> "</h3>" <>
      "<div class=\"wa-pod-body\">" <> body <> "</div></section>"
  ];

(* La data URI (base64) no necesita escape; el alt si. *)
podImg[uri_String, alt_String] :=
  "<img class=\"wa-pod-img\" src=\"" <> uri <> "\" alt=\"" <>
    PersonalSite`View`escape[alt] <> "\" loading=\"lazy\">";

End[];
EndPackage[];

