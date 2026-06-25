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
  "send[<|\"name\"->_, \"email\"->_, \"message\"->_|>] envia el mensaje de \
contacto a $contactTo y devuelve <|\"ok\"->_, \"error\"->_|>.";

Begin["`Private`"];

configured[] :=
  PersonalSite`Config`$smtpUser =!= "" && PersonalSite`Config`$smtpPassword =!= "";

send[fields_Association] :=
  Module[{name, email, message, subject, body, result},
    name    = StringTrim @ Lookup[fields, "name", ""];
    email   = StringTrim @ Lookup[fields, "email", ""];
    message = StringTrim @ Lookup[fields, "message", ""];

    If[!configured[],
      Return[<|"ok" -> False,
        "error" -> "El envio de correo no esta configurado en el servidor."|>]];

    subject = "Contacto web \[Dash] " <> name;
    body = StringJoin[
      "Nuevo mensaje desde el formulario de contacto del sitio.\n\n",
      "Nombre: ", name, "\n",
      "Email:  ", email, "\n",
      StringRepeat["-", 44], "\n\n",
      message, "\n"
    ];

    result = Quiet @ Check[
      SendMail[
        "To"                 -> PersonalSite`Config`$contactTo,
        "Subject"            -> subject,
        "Body"               -> body,
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

End[];
EndPackage[];
