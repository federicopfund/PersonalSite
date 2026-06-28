(* ::Package:: *)

(* PersonalSite`Mailer`
   --------------------------------------------------------------------------
   Envio de correo del formulario de contacto via SMTP (por defecto Gmail).

   La configuracion vive en Config.wl (variables de entorno SMTP_USER, etc.).
   Si no hay credenciales, configured[] es False y send[] degrada con gracia
   sin lanzar excepciones, para que el sitio siga funcionando en desarrollo. *)

BeginPackage["PersonalSite`Mailer`"];

configured::usage =
  "configured[] devuelve True si hay usuario y contrasena SMTP configurados.";

send::usage =
  "send[fields_Association] envia un correo a $contactTo y devuelve \
<|\"ok\"->_, \"error\"->_|>. El campo opcional \"kind\" selecciona el tipo de \
mensaje por coincidencia de patrones (\"contact\" por defecto); los demas \
campos dependen del tipo.";

scheduleSend::usage =
  "scheduleSend[<|\"name\"->_, \"email\"->_, \"message\"->_|>] o \
scheduleSend[fields, interval] programa el envio recurrente del mensaje cada \
`interval` (por defecto Quantity[1, \"Weeks\"]) usando RunScheduledTask y \
devuelve <|\"ok\"->_, \"task\"->_, \"error\"->_|>.";

scheduledSends::usage =
  "scheduledSends[] devuelve la lista de tareas de envio programadas activas \
creadas por el Mailer.";

cancelScheduledSend::usage =
  "cancelScheduledSend[task] cancela una tarea de envio programada; \
cancelScheduledSend[All] cancela todas. Devuelve <|\"ok\"->_|>.";

Begin["`Private`"];

configured[] :=
  PersonalSite`Config`$smtpUser =!= "" && PersonalSite`Config`$smtpPassword =!= "";

(* --- Construccion del mensaje por tipo ----------------------------------
   El asunto y el cuerpo se generan segun el "kind" del association, mediante
   despacho por coincidencia de patrones. Anadir un tipo nuevo es solo definir
   otra regla buildMessage["<tipo>", fields]. WL ordena las definiciones por
   especificidad, por lo que la regla generica actua de respaldo. *)

$defaultKind = "contact";

messageKind[fields_Association] :=
  Replace[
    StringTrim @ ToString @ Lookup[fields, "kind", $defaultKind],
    "" -> $defaultKind
  ];

buildMessage["contact", fields_Association] :=
  Module[{name, email, message},
    name    = StringTrim @ Lookup[fields, "name", ""];
    email   = StringTrim @ Lookup[fields, "email", ""];
    message = StringTrim @ Lookup[fields, "message", ""];
    <|
      "subject" -> "Contacto web \[Dash] " <> name,
      "body" -> StringJoin[
        "Nuevo mensaje desde el formulario de contacto del sitio.\n\n",
        "Nombre: ", name, "\n",
        "Email:  ", email, "\n",
        StringRepeat["-", 44], "\n\n",
        message, "\n"
      ]
    |>
  ];

buildMessage["meeting", fields_Association] :=
  Module[{name, email, message, date, time},
    name    = StringTrim @ Lookup[fields, "name", ""];
    email   = StringTrim @ Lookup[fields, "email", ""];
    message = StringTrim @ Lookup[fields, "message", ""];
    date    = StringTrim @ Lookup[fields, "date", ""];
    time    = StringTrim @ Lookup[fields, "time", ""];
    <|
      "subject" -> "Solicitud de reunion \[Dash] " <> name,
      "body" -> StringJoin[
        "Nueva solicitud para planificar una reunion desde el sitio.\n\n",
        "Nombre: ", name, "\n",
        "Email:  ", email, "\n",
        "Fecha:  ", date, "\n",
        "Hora:   ", If[time === "", "(sin especificar)", time], "\n",
        StringRepeat["-", 44], "\n\n",
        If[message === "", "(Sin mensaje adicional)", message], "\n"
      ]
    |>
  ];

(* Tipo generico / respaldo: usa los campos "subject" y "message" tal cual. *)
buildMessage[_, fields_Association] :=
  <|
    "subject" -> Replace[StringTrim @ Lookup[fields, "subject", ""],
                   "" -> "Aviso del sitio"],
    "body"    -> StringTrim @ Lookup[fields, "message", ""]
  |>;

send[fields_Association] :=
  Module[{message, result},
    If[!configured[],
      Return[<|"ok" -> False,
        "error" -> "El envio de correo no esta configurado en el servidor."|>]];

    message = buildMessage[messageKind[fields], fields];

    result = Quiet @ Check[
      SendMail[
        "To"                 -> PersonalSite`Config`$contactTo,
        "Subject"            -> message["subject"],
        "Body"               -> message["body"],
        "From"               -> PersonalSite`Config`$smtpFrom,
        "Server"             -> PersonalSite`Config`$smtpServer,
        "PortNumber"         -> PersonalSite`Config`$smtpPort,
        "UserName"           -> PersonalSite`Config`$smtpUser,
        "Password"           -> PersonalSite`Config`$smtpPassword,
        "EncryptionProtocol" -> "StartTLS"
      ],
      $Failed
    ];

    If[result === $Failed || Head[result] === Failure,
      <|"ok" -> False, "error" -> "No se pudo enviar el correo. Intenta de nuevo mas tarde."|>,
      <|"ok" -> True|>
    ]
  ];

(* --- Envio programado --------------------------------------------------
   Permite encolar un envio recurrente (p.ej. un resumen semanal) sin
   bloquear la sesion. Se apoya en RunScheduledTask, que ejecuta send[] en
   el kernel cada `interval`. Las tareas creadas se registran en $tasks
   para poder listarlas y cancelarlas mas tarde. *)

$tasks = {};

scheduleSend[fields_Association] := scheduleSend[fields, Quantity[1, "Weeks"]];

scheduleSend[fields_Association, interval_] :=
  Module[{task},
    If[!configured[],
      Return[<|"ok" -> False,
        "error" -> "El envio de correo no esta configurado en el servidor."|>]];

    task = Quiet @ Check[RunScheduledTask[send[fields], interval], $Failed];

    If[Head[task] === ScheduledTaskObject,
      $tasks = Append[$tasks, task];
      <|"ok" -> True, "task" -> task|>,
      <|"ok" -> False, "error" -> "No se pudo programar el envio."|>
    ]
  ];

scheduledSends[] :=
  ($tasks = Select[$tasks, Head[#] === ScheduledTaskObject &]; $tasks);

cancelScheduledSend[All] :=
  (Quiet[RemoveScheduledTask /@ $tasks]; $tasks = {}; <|"ok" -> True|>);

cancelScheduledSend[task_] :=
  (Quiet @ RemoveScheduledTask[task];
   $tasks = DeleteCases[$tasks, task];
   <|"ok" -> True|>);

End[];
EndPackage[];
