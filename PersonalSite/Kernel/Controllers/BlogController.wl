(* ::Package:: *)

(* PersonalSite`Controller`  (parte: blog)
   --------------------------------------------------------------------------
   Indice del blog y detalle de cada entrada. *)
(* ════════════════════════════════════════════════════════════════════════
   SANDBOX para ejecucion de codigo desde el blog (publico, NO confiable).
   --------------------------------------------------------------------------
   El endpoint /kernel/eval es un REPL de admin SIN sandbox. Exponer eso al
   blog seria un RCE: cualquier visitante podria leer secretos del entorno,
   borrar archivos, lanzar procesos o envenenar el pool de kernels compartido
   (via $Pre/$Post). Por eso blogEval corre en MODO ABIERTO con barreras minimas:
     El usuario puede evaluar CUALQUIER simbolo de calculo de Wolfram Language
     (Plot, Solve, Integrate, Graph, EntityValue, Interpreter, etc.). Solo se
     mantiene lo imprescindible para no comprometer el host ni el pool:
     1. Bloquea contextos internos con secretos (Config`, Mailer`, Database`...).
     2. Parsea SIN evaluar (Hold) y rechaza un set REDUCIDO de operaciones
        criticas ($blogCriticalBlock): IO de disco, red, procesos, escape
        hatches de evaluacion e integridad/estado del pool compartido.
     3. Limita tiempo (TimeConstrained) y memoria (MemoryConstrained).
   Es best-effort: para un deployment hostil conviene aislar el kernel.
   ════════════════════════════════════════════════════════════════════════ *)
BeginPackage["PersonalSite`Controller`"];

blogIndex::usage =
  "blogIndex[request] renderiza la lista de entradas recientes.";

blogShow::usage =
  "blogShow[slug, request] renderiza una entrada, o una respuesta 404 si no existe.";

blogEval::usage =
  "blogEval[request] evalua de forma SANDBOXED una celda de codigo WL enviada desde un post del blog y devuelve JSON con la salida nativa (MathML/SVG/OutputForm).";

Begin["`Private`"];

blogIndex[request_] :=
  PersonalSite`View`render["blog/index", <|"items" -> PersonalSite`Assets`blogCards[]|>];

blogShow[slug_String, request_] :=
  Module[{post = PersonalSite`Post`bySlug[slug]},
    If[MissingQ[post],
      HTTPResponse["404", <|"StatusCode" -> 404|>],
      PersonalSite`View`render["blog/post", <|
        post,
        "title" -> PersonalSite`View`escape[post["title"]],
        "date"  -> PersonalSite`Post`formatDate[post["date"]]
      |>]
    ]
  ];



(* ── MODO ABIERTO ───────────────────────────────────────────────
   El usuario puede correr CUALQUIER simbolo de Wolfram Language. Solo se
   mantiene el bloqueo critico minimo de abajo para proteger el host y el pool
   de kernels compartido. Ningun calculo, grafico o consulta matematica lo toca.

   Operaciones criticas — bloqueadas SIEMPRE aunque todo lo demas este permitido.
   Agrupan compromiso irreversible del host, fuga de datos y evasion del sandbox. *)
$blogCriticalBlock = {
  (* --- Disco (lectura / escritura / borrado) --- *)
  "Import", "Export", "Get", "Put", "PutAppend", "Save", "DumpSave",
  "OpenRead", "OpenWrite", "OpenAppend", "Close", "Read", "ReadString",
  "ReadLine", "ReadList", "BinaryRead", "Write", "WriteString", "BinaryWrite",
  "DeleteFile", "CopyFile", "RenameFile", "CreateFile", "CreateDirectory",
  "DeleteDirectory", "SetDirectory", "ResetDirectory", "FileNames", "Splice",
  "FileTemplate", "CreateArchive", "ExtractArchive",
  (* --- Red / nube / mail (exfiltracion) --- *)
  "URLFetch", "URLRead", "URLSubmit", "URLDownload", "URLExecute",
  "HTTPRequest", "SendMail", "SendMessage", "ServiceConnect", "ServiceExecute",
  "CloudConnect", "CloudDeploy", "CloudPut", "CloudGet", "CloudEvaluate",
  "ChannelSend", "ChannelListen", "DatabaseLink", "SQLExecute", "SQLConnection",
  (* --- Procesos / sistema / control del kernel --- *)
  "Run", "RunProcess", "StartProcess", "ProcessConnection", "SystemOpen",
  "Install", "Uninstall", "LaunchKernels", "ParallelEvaluate", "ParallelSubmit",
  "ExternalEvaluate", "ExternalFunction", "StartExternalSession",
  "Environment", "GetEnvironment", "SetEnvironment", "SystemInformation",
  "SetSystemOptions", "Exit", "Quit", "Pause",
  (* --- Escape hatches de evaluacion (evaden el analisis estatico) --- *)
  "ToExpression", "Symbol", "MakeExpression", "Needs", "BeginPackage",
  (* --- Integridad del kernel compartido (redefinir builtins persiste en pool) --- *)
  "Unprotect", "Protect", "SetAttributes", "ClearAttributes",
  "ClearAll", "Remove",
  (* --- Tareas / sesion (envenenarian el pool de kernels) --- *)
  "RunScheduledTask", "ScheduledTask", "CreateScheduledTask",
  "SessionSubmit", "LocalSubmit", "CloudSubmit",
  "$Pre", "$Post", "$PrePrint", "$PreRead", "$SyntaxHandler",
  "$MessagePrePrint", "$HistoryLength", "$Echo"
};

(* Contextos internos de la app que pueden contener secretos (credenciales de
   DB, SMTP, tokens). Se bloquea cualquier referencia con backtick a ellos,
   pero el resto de contextos (System`, Global`, PersonalSite`Post`...) si corre. *)
$blogSecretContexts = {
  "PersonalSite`Config`", "PersonalSite`Settings`", "PersonalSite`Mailer`",
  "PersonalSite`Database`", "PersonalSite`SessionStore`", "PersonalSite`DevOps`"
};

(* Extrae los nombres de simbolo de una expresion HELD (sin evaluarla). *)
blogHeldNames[held_] :=
  DeleteDuplicates @ Cases[
    held, s_Symbol :> SymbolName[Unevaluated[s]], {0, Infinity}, Heads -> True];

(* Devuelve un String con el motivo si el codigo viola las barreras, o Missing[] si pasa. *)
blogSandboxCheck[code_String] :=
  Module[{held, names, bad},
    If[StringContainsQ[code, $blogSecretContexts],
      Return["acceso a contextos internos protegidos no permitido"]];
    held = Quiet @ ToExpression[code, InputForm, Hold];
    If[held === $Failed || MatchQ[held, Hold[$Failed]],
      Return["error de sintaxis"]];
    names = Quiet @ blogHeldNames[held];
    bad   = Select[names, MemberQ[$blogCriticalBlock, #] &];
    If[bad =!= {},
      Return["operacion bloqueada por seguridad del servidor: " <> StringRiffle[bad, ", "]]];
    Missing["Safe"]
  ];

(* Marcador unico para detectar abort por limite de tiempo/memoria. *)
$blogAbort = "__blog_sandbox_abort__";

blogEval[request_] :=
  Module[{code, viol, t0, result, ms, html, txt},
    code = StringTrim @ Lookup[request["FormRules"], "code", ""];

    If[code === "",
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"ok" -> False, "error" -> "codigo vacio"|>],
        <|"StatusCode" -> 400, "Content-Type" -> "application/json"|>]];

    If[StringLength[code] > 2000,
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"ok" -> False, "error" -> "codigo demasiado largo (max 2000 chars)"|>],
        <|"StatusCode" -> 400, "Content-Type" -> "application/json"|>]];

    viol = blogSandboxCheck[code];
    If[StringQ[viol],
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"ok" -> False, "error" -> viol|>],
        <|"StatusCode" -> 200, "Content-Type" -> "application/json"|>]];

    t0 = AbsoluteTime[];
    result = TimeConstrained[
      MemoryConstrained[
        Quiet @ ToExpression[code, InputForm],
        150 * 1024 * 1024, $blogAbort],
      8, $blogAbort];
    ms = Round[1000. * (AbsoluteTime[] - t0), 0.01];

    If[result === $blogAbort,
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|
          "ok" -> False, "error" -> "limite de tiempo (8s) o memoria (150MB) excedido"|>],
        <|"StatusCode" -> 200, "Content-Type" -> "application/json"|>]];

    html = Quiet @ Check[PersonalSite`FrontEnd`Output`toHtml[result], ""];
    txt  = Quiet @ Check[ToString[result, OutputForm], ""];
    txt  = StringTake[txt, Min[1200, StringLength[txt]]];

    HTTPResponse[
      Developer`WriteRawJSONString[<|
        "ok"   -> True,
        "html" -> html,
        "text" -> txt,
        "ms"   -> ms
      |>],
      <|"Content-Type" -> "application/json"|>]
  ];

End[];
EndPackage[];