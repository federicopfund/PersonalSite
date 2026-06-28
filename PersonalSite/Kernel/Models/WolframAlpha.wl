(* ::Package:: *)

(* PersonalSite`WolframAlpha`
   --------------------------------------------------------------------------
   Integracion con Wolfram|Alpha. Dos estrategias, elegidas automaticamente:

   1. LLM API (https://products.wolframalpha.com/llm-api/documentation)
      Se usa cuando WOLFRAM_LLM_APPID esta configurado. Devuelve texto plano
      estructurado que parseamos en secciones (pods) para renderizar.

   2. Pods nativos del kernel WolframAlpha[] (fallback)
      Se usa cuando la LLM API no esta disponible o falla. Pide
      WolframAlpha[q, {All, "Plaintext"}] y WolframAlpha[q, {All, "Image"}].

   result[q]   punto de entrada unico. Prueba (1) y cae a (2) si es necesario.
   imageURL[q] URL de imagen via Simple API (requiere WOLFRAM_ALPHA_APPID). *)

BeginPackage["PersonalSite`WolframAlpha`"];

result::usage =
  "result[q] consulta Wolfram|Alpha (LLM API o pods nativos) y devuelve \
<|\"ok\"->_, \"query\"->_, \"pods\"->{...}|>|<|\"ok\"->False, \"error\"->_|>.";

imageURL::usage =
  "imageURL[q] devuelve una URL de imagen de la Simple API de Wolfram|Alpha para q, \
o $Failed si no hay AppID configurado.";

Begin["`Private`"];

(* -----------------------------------------------------------------------
   result[q] — enrutador principal
   ----------------------------------------------------------------------- *)
result[q_String] :=
  Module[{appid = PersonalSite`Config`$wolframLLMAppID, llm},
    If[StringQ[appid] && appid =!= "",
      llm = llmResult[q, appid];
      If[TrueQ[llm["ok"]], Return[llm]]
      (* Si la LLM API fallo, cae al metodo nativo sin avisar al usuario *)
    ];
    nativeResult[q]
  ];

failure[q_String, msg_String] := <|"ok" -> False, "query" -> q, "error" -> msg|>;

(* -----------------------------------------------------------------------
   Capa 1: LLM API
   Endpoint: https://www.wolframalpha.com/api/v1/llm-api
   Respuesta: texto plano estructurado (secciones separadas por \n\n).
   ----------------------------------------------------------------------- *)
llmResult[q_String, appid_String] :=
  Module[{url, resp, body, pods},
    (* Construimos la URL manualmente para evitar ambiguedad en el encoding
       de los parametros con HTTPRequest <|"Parameters"->...|> *)
    url = "https://www.wolframalpha.com/api/v1/llm-api?input=" <>
          URLEncode[q] <> "&appid=" <> appid;
    resp = Check[URLRead[HTTPRequest[url], TimeConstraint -> 20], $Failed];
    (* URLRead devuelve HTTPResponse[...], no Association *)
    If[Head[resp] =!= HTTPResponse,
      Return[failure[q, "No se pudo conectar con Wolfram|Alpha. Intenta de nuevo."]]];
    body = resp["Body"];
    (* Body puede llegar como ByteArray o String *)
    If[Head[body] === ByteArray, body = ByteArrayToString[body, "UTF-8"]];
    Switch[resp["StatusCode"],
      200,
        pods = parseSections[body];
        If[pods === {},
          Return[failure[q, "Wolfram|Alpha no devolvio contenido para esta consulta."]]];
        <|"ok" -> True, "query" -> q, "pods" -> pods|>,
      501,
        failure[q, "Wolfram|Alpha no tiene resultados para esta consulta. Reformulala."],
      403,
        failure[q, "AppID invalido o sin permisos para la LLM API."],
      _,
        failure[q, "Error LLM API (HTTP " <> ToString[resp["StatusCode"]] <> ") — " <>
          If[StringQ[body], StringTake[body, Min[80, StringLength[body]]], ""]]
    ]
  ];

(* Parser: divide el texto en secciones, cada una se vuelve un pod.
   Formato tipico:
     "Wolfram|Alpha results for ...\n\n
      Input interpretation: ...\n\n
      Result\n67.75 million people\n\n" *)
parseSections[text_String] :=
  Module[{clean, paras},
    clean = StringTrim @ StringReplace[text, "\r\n" -> "\n"];
    paras = Select[
      StringTrim /@ StringSplit[clean, "\n\n"],
      StringLength[#] > 0 &
    ];
    (* Omite cabeceras de la API: comienzan con "Wolfram" o "Query:"
       StringStartsQ funciona en parrafos multi-linea, a diferencia de StringMatchQ *)
    paras = Select[paras,
      !StringStartsQ[StringTrim[#], RegularExpression["(?i)(wolfram|query:)"]] &
    ];
    DeleteCases[sectionPod /@ paras, Null]
  ];

sectionPod[para_String] :=
  Module[{lines, titleLine, bodyLines, imgLines, textLines, title, images, text},
    lines = Select[StringTrim /@ StringSplit[para, "\n"], StringLength[#] > 0 &];
    If[lines === {}, Return[Null]];

    If[Length[lines] === 1,
      titleLine = ""; bodyLines = lines,
      titleLine = First[lines]; bodyLines = Rest[lines]
    ];

    (* Separa lineas "image: https://..." del texto plano *)
    imgLines  = Select[bodyLines,  StringMatchQ[#, RegularExpression["image:\\s+https?://.+"]] &];
    textLines = Select[bodyLines, !StringMatchQ[#, RegularExpression["image:\\s+https?://.+"]] &];

    images = StringTrim[StringDrop[#, 6] & /@ imgLines];  (* quita "image:" *)
    text   = StringRiffle[textLines, "\n"];

    title = If[titleLine === "",
      If[imgLines =!= {}, "Resultado", "Resultado"],
      cleanTitle[titleLine]
    ];

    If[StringLength[StringTrim[text]] === 0 && images === {}, Return[Null]];
    <|"title" -> title, "text" -> text, "images" -> images|>
  ];

(* "Result:" -> "Result", "Input interpretation:" -> "Input interpretation" *)
cleanTitle[s_String] :=
  StringTrim @ StringReplace[s, RegularExpression[":\\s*$"] -> ""];

(* -----------------------------------------------------------------------
   Capa 2: pods nativos del kernel WolframAlpha[] (fallback)
   Dos llamadas traen TODO el resultado:
     WolframAlpha[q, {All, "Plaintext"}] -> {{{podid,n},"Plaintext"} -> texto,...}
     WolframAlpha[q, {All, "Image"}]     -> {{{podid,n},"Image"}     -> Image, ...}
   ----------------------------------------------------------------------- *)
$maxPods   = 8;
$timeLimit = 30;

nativeResult[q_String] :=
  Module[{pt, im, cells, ids, pods},
    TimeConstrained[
      pt    = ruleData @ Quiet @ WolframAlpha[q, {All, "Plaintext"}];
      im    = ruleData @ Quiet @ WolframAlpha[q, {All, "Image"}];
      cells = ruleData @ Quiet @ WolframAlpha[q, {All, "Cell"}];

      If[pt === <||> && im === <||> && cells === <||>,
        Return[failure[q, "Wolfram|Alpha no encontro resultados para esta consulta."]]];

      ids  = Take[DeleteDuplicates[Join[Keys[pt], Keys[im], Keys[cells]]], UpTo[$maxPods]];
      pods = DeleteCases[buildPod[#, pt, im, cells] & /@ ids, Null];

      If[pods === {},
        Return[failure[q, "Wolfram|Alpha no devolvio contenido mostrable."]]];

      <|"ok" -> True, "query" -> q, "pods" -> pods|>,

      $timeLimit,
      failure[q, "La consulta a Wolfram|Alpha tardo demasiado. Intenta de nuevo."]
    ]
  ];

ruleData[expr_List] :=
  GroupBy[Cases[expr, (lhs_ -> v_) :> {podIdOf[lhs], v}], First -> Last];
ruleData[_] := <||>;

podIdOf[{{id_, _}, _}] := id;
podIdOf[{id_, _}]      := id;
podIdOf[_]             := Missing[];

buildPod[id_, pt_Association, im_Association, cells_Association] :=
  Module[{texts, imgs, cellObjs, text, images, cellHtml},
    texts    = Lookup[pt, Key[id], {}];
    imgs     = Lookup[im, Key[id], {}];
    cellObjs = Lookup[cells, Key[id], {}];
    text     = StringRiffle[DeleteCases[StringTrim /@ Cases[texts, _String], ""], "\n"];
    images   = DeleteCases[imageDataURI /@ Cases[imgs, _Image], ""];
    cellHtml = StringJoin[DeleteCases[cellToHTML /@ Cases[cellObjs, _Cell], ""]];
    If[text === "" && images === {} && cellHtml === "", Return[Null]];
    <|"title" -> podTitle[id], "text" -> text, "images" -> images, "cellHtml" -> cellHtml|>
  ];

cellToHTML[cell_Cell] :=
  Module[{html = Quiet @ ExportString[Notebook[{cell}], "HTMLFragment"]},
    If[StringQ[html] && StringLength[html] > 0, html, ""]
  ];

imageDataURI[im_Image] :=
  Module[{bytes = Quiet @ ExportByteArray[im, "PNG"]},
    If[Head[bytes] === ByteArray,
      "data:image/png;base64," <> BaseEncode[bytes],
      ""]
  ];

podTitle[id_] :=
  StringTrim @ StringReplace[
    First[StringSplit[ToString[id], ":"]], 
    RegularExpression["([a-z0-9])([A-Z])"] -> "$1 $2"
  ];

(* -----------------------------------------------------------------------
   imageURL — Simple API (home page /wa-img)
   ----------------------------------------------------------------------- *)
imageURL[q_String] :=
  Module[{appid = PersonalSite`Config`$wolframAppID},
    If[appid === "", Return[$Failed]];
    "https://api.wolframalpha.com/v1/simple?i=" <> URLEncode[q] <>
      "&appid=" <> appid <> "&width=700&units=metric"
  ];

End[];
EndPackage[];

