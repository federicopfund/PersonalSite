(* ::Package:: *)

(* PersonalSite`Config`
   --------------------------------------------------------------------------
   Configuracion en tiempo de ejecucion. Cada valor toma por defecto un ajuste
   sensato para desarrollo y puede sobreescribirse con una variable de entorno,
   que es como la imagen Docker inyecta los settings de produccion. *)

BeginPackage["PersonalSite`Config`"];

value::usage =
  "value[\"VAR\", default] devuelve la variable de entorno VAR, o default si no \
esta definida o esta vacia.";

$databasePath::usage =
  "$databasePath es la ruta del archivo SQLite (env PERSONALSITE_DB).";

$siteName::usage =
  "$siteName es el nombre publico del sitio mostrado en el layout (env PERSONALSITE_NAME).";

$wolframAppID::usage =
  "$wolframAppID es el AppID de Wolfram|Alpha para la API HTTP (env WOLFRAM_ALPHA_APPID).";

$contactTo::usage =
  "$contactTo es la direccion que recibe los mensajes del formulario de contacto (env CONTACT_TO).";

$smtpServer::usage =
  "$smtpServer es el host SMTP de salida (env SMTP_SERVER).";

$smtpPort::usage =
  "$smtpPort es el puerto SMTP (env SMTP_PORT).";

$smtpUser::usage =
  "$smtpUser es el usuario de autenticacion SMTP (env SMTP_USER).";

$smtpPassword::usage =
  "$smtpPassword es la contrasena o app-password SMTP (env SMTP_PASSWORD).";

$smtpFrom::usage =
  "$smtpFrom es la direccion remitente; por defecto $smtpUser (env SMTP_FROM).";

Begin["`Private`"];

value[key_String, default_] :=
  Module[{v = Environment[key]},
    If[StringQ[v] && v =!= "", v, default]
  ];

(* Base de datos: por defecto un archivo SQLite local (cero configuracion,
   ideal para un sitio personal de un solo autor). Para apuntar a otro motor,
   cambia el descriptor JDBC en Models/Database.wl y exporta PERSONALSITE_DB. *)
$databasePath =
  value["PERSONALSITE_DB", FileNameJoin[{PersonalSite`$Root, "data", "site.db"}]];

$siteName = value["PERSONALSITE_NAME", "Federico"];

$wolframAppID = value["WOLFRAM_ALPHA_APPID", ""];

(* --- Correo / formulario de contacto -----------------------------------
   El formulario envia un correo via SMTP (por defecto Gmail). El usuario y la
   contrasena (app-password) se inyectan como secretos de entorno; si faltan,
   el envio se desactiva con gracia y el controller avisa al visitante. *)
$contactTo   = value["CONTACT_TO", "federicopfund@gmail.com"];
$smtpServer  = value["SMTP_SERVER", "smtp.gmail.com"];
$smtpPort    = ToExpression @ value["SMTP_PORT", "587"];
$smtpUser    = value["SMTP_USER", ""];
$smtpPassword = value["SMTP_PASSWORD", ""];
$smtpFrom    = value["SMTP_FROM", If[$smtpUser =!= "", $smtpUser, $contactTo]];

End[];
EndPackage[];