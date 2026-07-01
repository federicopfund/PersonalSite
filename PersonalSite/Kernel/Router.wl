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
  Module[{path, ext, mime, body, cacheCtrl},
    path = FileNameJoin[Join[{PersonalSite`$Root}, rootParts,
             {StringReplace[file, "/" -> $PathnameSeparator]}]];
    If[!FileExistsQ[path],
      Return[HTTPResponse["Not Found", <|"StatusCode" -> 404|>]]];
    ext = ToLowerCase[FileExtension[path]];
    Which[
      KeyExistsQ[$textTypes, ext],
        (* Bytes crudos: el charset=utf-8 ya va en el mime; evita que la
           lectura (ISO8859-1 por defecto) y el re-encode dupliquen UTF-8. *)
        mime = $textTypes[ext];   body = ReadByteArray[path];
        cacheCtrl = "no-cache",
      KeyExistsQ[$binaryTypes, ext],
        mime = $binaryTypes[ext]; body = ReadByteArray[path];
        cacheCtrl = "public, max-age=86400",
      True,
        mime = "application/octet-stream"; body = ReadByteArray[path];
        cacheCtrl = "no-cache"
    ];
    HTTPResponse[body, <|"Headers" -> <|
      "Content-Type"  -> mime,
      "Cache-Control" -> cacheCtrl
    |>|>]
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
  "/blog/eval" ~~ EndOfString         :> PersonalSite`Controller`blogEval[HTTPRequestData[]],
  "/blog/"   ~~ slug__ ~~ EndOfString :> PersonalSite`Controller`blogShow[slug, HTTPRequestData[]],
  "/blog"    ~~ EndOfString           :> PersonalSite`Controller`blogIndex[HTTPRequestData[]],
  "/ask"     ~~ EndOfString           :> PersonalSite`Controller`ask[HTTPRequestData[]],
  "/contacto" ~~ EndOfString          :> PersonalSite`Controller`contact[HTTPRequestData[]],
  "/apariencia" ~~ EndOfString        :> PersonalSite`Controller`appearance[HTTPRequestData[]],
  "/flow"          ~~ EndOfString :> PersonalSite`Controller`flow[HTTPRequestData[]],
  "/perf"          ~~ EndOfString :> PersonalSite`Controller`perf[HTTPRequestData[]],
  "/dag"                    ~~ EndOfString :> PersonalSite`Controller`dagDashboard[HTTPRequestData[]],
  "/devops"                 ~~ EndOfString :> PersonalSite`Controller`devopsPage[HTTPRequestData[]],
  "/devops/dag"             ~~ EndOfString :> PersonalSite`Controller`devopsDag[HTTPRequestData[]],
  "/devops/status"          ~~ EndOfString :> PersonalSite`Controller`devopsStatus[HTTPRequestData[]],
  "/devops/run/"    ~~ stage__ ~~ EndOfString :> PersonalSite`Controller`devopsRunStage[stage, HTTPRequestData[]],
  "/devops/pipeline/run"    ~~ EndOfString :> PersonalSite`Controller`devopsPipelineRun[HTTPRequestData[]],
  "/devops/pipeline/history"~~ EndOfString :> PersonalSite`Controller`devopsPipelineHistory[HTTPRequestData[]],
  "/devops/trajectory/" ~~ n__ ~~ EndOfString :> PersonalSite`Controller`devopsTrajectory[n, HTTPRequestData[]],
  "/devops/tests/run/"  ~~ layer__ ~~ EndOfString :> PersonalSite`Controller`devopsTestsRunLayer[layer, HTTPRequestData[]],
  "/devops/tests/run"   ~~ EndOfString :> PersonalSite`Controller`devopsTestsRun[HTTPRequestData[]],
  "/kpi/metrics"        ~~ EndOfString :> PersonalSite`Controller`kpiMetrics[HTTPRequestData[]],
  "/kpi"                ~~ EndOfString :> PersonalSite`Controller`kpiPage[HTTPRequestData[]],
  "/tasks/summary"         ~~ EndOfString :> PersonalSite`Controller`tasksSummary[HTTPRequestData[]],
  "/tasks/dag"             ~~ EndOfString :> PersonalSite`Controller`tasksDag[HTTPRequestData[]],
  "/tasks/config/create"   ~~ EndOfString :> PersonalSite`Controller`tasksConfigCreate[HTTPRequestData[]],
  "/tasks/config/update"   ~~ EndOfString :> PersonalSite`Controller`tasksConfigUpdate[HTTPRequestData[]],
  "/tasks/config/apply"    ~~ EndOfString :> PersonalSite`Controller`tasksConfigApply[HTTPRequestData[]],
  "/tasks/config/seed"     ~~ EndOfString :> PersonalSite`Controller`tasksConfigSeed[HTTPRequestData[]],
  "/tasks/config/delete/"  ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksConfigDelete[id, HTTPRequestData[]],
  "/tasks/config/"         ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksConfigById[id, HTTPRequestData[]],
  "/tasks/config"          ~~ EndOfString :> PersonalSite`Controller`tasksConfigList[HTTPRequestData[]],
  "/tasks/history/"    ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksHistory[id, HTTPRequestData[]],
  "/tasks/start/"      ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksStart[id, HTTPRequestData[]],
  "/tasks/stop/"       ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksStop[id, HTTPRequestData[]],
  "/tasks/restart/"    ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksRestart[id, HTTPRequestData[]],
  "/tasks/unregister/" ~~ id__ ~~ EndOfString :> PersonalSite`Controller`tasksUnregister[id, HTTPRequestData[]],
  "/tasks/configure"       ~~ EndOfString :> PersonalSite`Controller`tasksConfigure[HTTPRequestData[]],
  "/tasks/register"        ~~ EndOfString :> PersonalSite`Controller`tasksRegister[HTTPRequestData[]],
  "/tasks"                 ~~ EndOfString :> PersonalSite`Controller`tasks[HTTPRequestData[]],
  "/nest/results"   ~~ EndOfString :> PersonalSite`Controller`nestResults[HTTPRequestData[]],
  "/nest/export.csv"~~ EndOfString :> PersonalSite`Controller`nestExport[HTTPRequestData[]],
  "/nest/schedule"  ~~ EndOfString :> PersonalSite`Controller`nestSchedule[HTTPRequestData[]],
  "/nest/cancel"    ~~ EndOfString :> PersonalSite`Controller`nestCancel[HTTPRequestData[]],
  "/nest"           ~~ EndOfString :> PersonalSite`Controller`nest[HTTPRequestData[]],
  "/ruliology/eval"    ~~ EndOfString :> PersonalSite`Controller`ruliologyEval[HTTPRequestData[]],
  "/ruliology/metrics" ~~ EndOfString :> PersonalSite`Controller`ruliologyMetrics[HTTPRequestData[]],
  "/ruliology"         ~~ EndOfString :> PersonalSite`Controller`ruliologyPage[HTTPRequestData[]],
  "/arch/data"       ~~ EndOfString :> PersonalSite`Controller`archData[HTTPRequestData[]],
  "/arch/health"     ~~ EndOfString :> PersonalSite`Controller`archHealth[HTTPRequestData[]],
  "/arch/math"       ~~ EndOfString :> PersonalSite`Controller`archMath[HTTPRequestData[]],
  "/arch/dag"        ~~ EndOfString :> PersonalSite`Controller`archDag[HTTPRequestData[]],
  "/arch/tasks"      ~~ EndOfString :> PersonalSite`Controller`archTasks[HTTPRequestData[]],
  "/arch"            ~~ EndOfString :> PersonalSite`Controller`arch[HTTPRequestData[]],
  "/kernel/style/rules"  ~~ EndOfString :> PersonalSite`Controller`kernelStyleRules[HTTPRequestData[]],
  "/kernel/style/rule"   ~~ EndOfString :> PersonalSite`Controller`kernelStyleAddRule[HTTPRequestData[]],
  "/kernel/style/remove" ~~ EndOfString :> PersonalSite`Controller`kernelStyleRemove[HTTPRequestData[]],
  "/kernel/style/reset"  ~~ EndOfString :> PersonalSite`Controller`kernelStyleReset[HTTPRequestData[]],
  "/kernel/eval"     ~~ EndOfString :> PersonalSite`Controller`kernelEval[HTTPRequestData[]],
  "/kernel/cells"    ~~ EndOfString :> PersonalSite`Controller`kernelCells[HTTPRequestData[]],
  "/kernel/schedule" ~~ EndOfString :> PersonalSite`Controller`kernelSchedule[HTTPRequestData[]],
  "/kernel/history"  ~~ EndOfString :> PersonalSite`Controller`kernelHistory[HTTPRequestData[]],
  "/kernel"          ~~ EndOfString :> PersonalSite`Controller`kernelPage[HTTPRequestData[]],
  "/session/create"     ~~ EndOfString :> PersonalSite`Controller`sessionCreate[HTTPRequestData[]],
  "/session/destroy"    ~~ EndOfString :> PersonalSite`Controller`sessionDestroy[HTTPRequestData[]],
  "/session/validate"   ~~ EndOfString :> PersonalSite`Controller`sessionValidate[HTTPRequestData[]],
  "/session/info"       ~~ EndOfString :> PersonalSite`Controller`sessionInfo[HTTPRequestData[]],
  "/session/transition" ~~ EndOfString :> PersonalSite`Controller`sessionTransition[HTTPRequestData[]],
  "/session/graph"      ~~ EndOfString :> PersonalSite`Controller`sessionGraph[HTTPRequestData[]],
  "/session/fsm"        ~~ EndOfString :> PersonalSite`Controller`sessionFsmGraph[HTTPRequestData[]],
  "/session/stats"      ~~ EndOfString :> PersonalSite`Controller`sessionStats[HTTPRequestData[]],
  "/ux/contact"      ~~ EndOfString :> PersonalSite`Controller`uxContactState[HTTPRequestData[]],
  "/"        ~~ EndOfString           :> PersonalSite`Controller`home[HTTPRequestData[]],
  ___ :> HTTPResponse["No encontrado", <|"StatusCode" -> 404|>]
}];

End[];
EndPackage[];