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
$wolframLLMAppID::usage =
  "$wolframLLMAppID es el AppID para la Wolfram|Alpha LLM API (env WOLFRAM_LLM_APPID).";
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

$dbDriver::usage =
  "$dbDriver es el motor de base de datos: \"sqlite\" (default) o \"postgresql\" (env PERSONALSITE_DB_DRIVER).";
$pgHost::usage     = "$pgHost host PostgreSQL (env PGHOST).";
$pgPort::usage     = "$pgPort puerto PostgreSQL (env PGPORT).";
$pgDatabase::usage = "$pgDatabase nombre de la base PostgreSQL (env PGDATABASE).";
$pgUser::usage     = "$pgUser usuario PostgreSQL (env PGUSER).";
$pgPassword::usage = "$pgPassword contrasena PostgreSQL (env PGPASSWORD).";
$pgSslMode::usage  = "$pgSslMode modo SSL JDBC: require|disable|verify-full (env PGSSLMODE).";

Begin["`Private`"];

value[key_String, default_] :=
  Module[{v = Environment[key]},
    If[StringQ[v] && v =!= "", v, default]
  ];

(* Base de datos: por defecto un archivo SQLite local (cero configuracion,
   ideal para un sitio personal de un solo autor). En produccion stateless
   (p.ej. Code Engine) conviene PostgreSQL: exporta PERSONALSITE_DB_DRIVER=postgresql
   y los PG* (host/puerto/base/usuario/contrasena/ssl). *)
$databasePath =
  value["PERSONALSITE_DB", FileNameJoin[{PersonalSite`$Root, "data", "site.db"}]];

$dbDriver   = ToLowerCase @ value["PERSONALSITE_DB_DRIVER", "sqlite"];
$pgHost     = value["PGHOST", ""];
$pgPort     = value["PGPORT", "5432"];
$pgDatabase = value["PGDATABASE", "personalsite"];
$pgUser     = value["PGUSER", "postgres"];
$pgPassword = value["PGPASSWORD", ""];
$pgSslMode  = value["PGSSLMODE", "require"];

$siteName = value["PERSONALSITE_NAME", "Federico"];

$wolframAppID    = value["WOLFRAM_ALPHA_APPID", ""];

(* LLM API — https://products.wolframalpha.com/llm-api/documentation
   Devuelve resultados Wolfram|Alpha como texto plano estructurado.
   AppID independiente del de la Simple/Full API. *)
$wolframLLMAppID = value["WOLFRAM_LLM_APPID", ""];

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