(* ::Package:: *)

(* PersonalSite`WolframAlpha`
   --------------------------------------------------------------------------
   Integracion en tiempo real con Wolfram|Alpha.

   query[q]    responde una pregunta en lenguaje natural en dos capas:
                 1. WolframAlpha[] nativo del kernel (no requiere AppID externo)
                 2. Short Answers HTTP API con WOLFRAM_ALPHA_APPID

   imageURL[q] construye una URL de la Simple API para embeber graficas; el
               browser la carga directamente (via redirect 302 del router). *)

BeginPackage["PersonalSite`WolframAlpha`"];

query::usage =
  "query[q] responde la pregunta q y devuelve <|\"ok\"->_, \"answer\"|\"error\"->_, ...|>.";

imageURL::usage =
  "imageURL[q] devuelve una URL de imagen de la Simple API de Wolfram|Alpha para q, \
o $Failed si no hay AppID configurado.";

Begin["`Private`"];

query[q_String] :=
  Module[{native},
    native = Quiet @ WolframAlpha[q, "ShortAnswer"];
    If[StringQ[native] && StringLength[native] > 0,
      <|"ok" -> True, "answer" -> native, "source" -> "native"|>,
      httpResult[q]
    ]
  ];

(* Segunda capa: Short Answers API (https://api.wolframalpha.com/v1/result). *)
httpResult[q_String] :=
  Module[{appid = PersonalSite`Config`$wolframAppID, response},
    If[appid === "",
      Return[<|"ok" -> False,
        "error" -> "Wolfram Alpha no disponible. Configura WOLFRAM_ALPHA_APPID en el entorno."|>]];
    response = Quiet @ URLRead @ HTTPRequest[
      "https://api.wolframalpha.com/v1/result",
      <|"Parameters" -> <|"i" -> q, "appid" -> appid, "units" -> "metric"|>|>];
    If[!AssociationQ[response],
      Return[<|"ok" -> False, "error" -> "Error de conexion con Wolfram Alpha."|>]];
    Switch[response["StatusCode"],
      200, <|"ok" -> True, "answer" -> response["Body"], "source" -> "api"|>,
      501, <|"ok" -> False,
             "error" -> "No hay respuesta corta para esta consulta. Intenta reformular la pregunta."|>,
      403, <|"ok" -> False, "error" -> "AppID invalido o sin permisos."|>,
      _,   <|"ok" -> False,
             "error" -> "Error de la API (" <> ToString[response["StatusCode"]] <> ")."|>
    ]
  ];

imageURL[q_String] :=
  Module[{appid = PersonalSite`Config`$wolframAppID},
    If[appid === "", Return[$Failed]];
    "https://api.wolframalpha.com/v1/simple?i=" <> URLEncode[q] <>
      "&appid=" <> appid <> "&width=700&units=metric"
  ];

End[];
EndPackage[];
