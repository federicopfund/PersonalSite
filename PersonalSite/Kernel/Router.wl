(* ::Package:: *)

(* PersonalSite`Routing`
   --------------------------------------------------------------------------
   Servido de archivos estaticos y construccion del URLDispatcher.

   Reglas de orden: las rutas mas especificas van primero y todas terminan en
   EndOfString para que el dispatcher haga match exacto del PATH (la query se
   recupera aparte con HTTPRequestData[]). *)

BeginPackage["PersonalSite`Routing`"];

dispatcher::usage =
  "dispatcher[] devuelve el URLDispatcher que mapea URLs a respuestas de los controllers.";

serveStatic::usage =
  "serveStatic[file] sirve un archivo desde Resources/Static/.";

serveImage::usage =
  "serveImage[file] sirve un archivo desde Resources/Img/.";

Begin["`Private`"];

(* Tipos de contenido: los de texto se leen como String, el resto como ByteArray. *)
$textTypes = <|
  "css" -> "text/css; charset=utf-8",
  "js"  -> "application/javascript; charset=utf-8",
  "svg" -> "image/svg+xml; charset=utf-8"
|>;
$binaryTypes = <|
  "jpg"  -> "image/jpeg", "jpeg" -> "image/jpeg",
  "png"  -> "image/png",  "gif"  -> "image/gif",
  "ico"  -> "image/x-icon", "woff2" -> "font/woff2"
|>;

(* Sirve un archivo bajo rootParts (relativo a $Root) de forma segura. *)
serveFile[rootParts_List, file_String] :=
  Module[{path, ext, mime, body},
    path = FileNameJoin[Join[{PersonalSite`$Root}, rootParts,
             {StringReplace[file, "/" -> $PathnameSeparator]}]];
    If[!FileExistsQ[path],
      Return[HTTPResponse["Not Found", <|"StatusCode" -> 404|>]]];
    ext = ToLowerCase[FileExtension[path]];
    Which[
      KeyExistsQ[$textTypes, ext],
        mime = $textTypes[ext];   body = ReadString[path],
      KeyExistsQ[$binaryTypes, ext],
        mime = $binaryTypes[ext]; body = ReadByteArray[path],
      True,
        mime = "application/octet-stream"; body = ReadByteArray[path]
    ];
    HTTPResponse[body, <|"Headers" -> <|"Content-Type" -> mime|>|>]
  ];

serveStatic[file_String] := serveFile[{"Resources", "Static"}, file];
serveImage[file_String]  := serveFile[{"Resources", "Img"}, file];

(* /wa-img: redirige (302) a la grafica de Wolfram|Alpha para la query 'q'.
   Se usa redirect en vez de servir bytes porque wolframwebengine no admite
   un BodyByteArray vacio; el body de texto evita ese caso. *)
imageRedirect[request_] :=
  Module[{q, url},
    q = Replace[
          Lookup[If[ListQ[request["Query"]], Association[request["Query"]], <||>],
            "q", "Argentina inflation rate"],
          Except[_String] -> "Argentina inflation rate"];
    url = PersonalSite`WolframAlpha`imageURL[q];
    If[StringQ[url],
      HTTPResponse["Redirecting", <|
        "StatusCode" -> 302,
        "Headers" -> <|"Location" -> url, "Cache-Control" -> "public, max-age=3600"|>|>],
      HTTPResponse["Image not available", <|"StatusCode" -> 503|>]
    ]
  ];

dispatcher[] := Delayed @ URLDispatcher[{
  "/static/" ~~ file__ ~~ EndOfString :> serveStatic[file],
  "/img/"    ~~ file__ ~~ EndOfString :> serveImage[file],
  "/wa-img"  ~~ EndOfString           :> imageRedirect[HTTPRequestData[]],
  "/blog/"   ~~ slug__ ~~ EndOfString :> PersonalSite`Controller`blogShow[slug, HTTPRequestData[]],
  "/blog"    ~~ EndOfString           :> PersonalSite`Controller`blogIndex[HTTPRequestData[]],
  "/ask"     ~~ EndOfString           :> PersonalSite`Controller`ask[HTTPRequestData[]],
  "/contacto" ~~ EndOfString          :> PersonalSite`Controller`contact[HTTPRequestData[]],
  "/apariencia" ~~ EndOfString        :> PersonalSite`Controller`appearance[HTTPRequestData[]],
  "/flow"          ~~ EndOfString :> PersonalSite`Controller`flow[HTTPRequestData[]],
  "/perf"          ~~ EndOfString :> PersonalSite`Controller`perf[HTTPRequestData[]],
  "/tasks/summary"      ~~ EndOfString :> PersonalSite`Controller`tasksSummary[HTTPRequestData[]],
  "/tasks/history/" ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksHistory[id, HTTPRequestData[]],
  "/tasks/start/"   ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksStart[id, HTTPRequestData[]],
  "/tasks/stop/"    ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksStop[id, HTTPRequestData[]],
  "/tasks/restart/" ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksRestart[id, HTTPRequestData[]],
  "/tasks/configure" ~~ EndOfString :> PersonalSite`Controller`tasksConfigure[HTTPRequestData[]],
  "/tasks/register"  ~~ EndOfString :> PersonalSite`Controller`tasksRegister[HTTPRequestData[]],
  "/tasks"           ~~ EndOfString :> PersonalSite`Controller`tasks[HTTPRequestData[]],
  "/nest/results"   ~~ EndOfString :> PersonalSite`Controller`nestResults[HTTPRequestData[]],
  "/nest/export.csv"~~ EndOfString :> PersonalSite`Controller`nestExport[HTTPRequestData[]],
  "/nest/schedule"  ~~ EndOfString :> PersonalSite`Controller`nestSchedule[HTTPRequestData[]],
  "/nest/cancel"    ~~ EndOfString :> PersonalSite`Controller`nestCancel[HTTPRequestData[]],
  "/nest"           ~~ EndOfString :> PersonalSite`Controller`nest[HTTPRequestData[]],
  "/arch/data"       ~~ EndOfString :> PersonalSite`Controller`archData[HTTPRequestData[]],
  "/arch"            ~~ EndOfString :> PersonalSite`Controller`arch[HTTPRequestData[]],
  "/"        ~~ EndOfString           :> PersonalSite`Controller`home[HTTPRequestData[]],
  ___ :> HTTPResponse["No encontrado", <|"StatusCode" -> 404|>]
}];

End[];
EndPackage[];