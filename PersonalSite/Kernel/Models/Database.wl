(* ::Package:: *)

(* PersonalSite`Database`
   --------------------------------------------------------------------------
   Capa minima de acceso a datos sobre DatabaseLink. Abre y cierra una conexion
   por consulta (sin pool); para alta concurrencia conviene introducir un
   ConnectionPool de DatabaseLink. *)

BeginPackage["PersonalSite`Database`", {"DatabaseLink`"}];

execute::usage =
  "execute[sql] o execute[sql, {params}] ejecuta una sentencia SQL contra la \
base SQLite y devuelve el resultado de SQLExecute.";

setup::usage =
  "setup[] prepara la conexion a la base de datos del paclet. Copia site.db \
a $TemporaryDirectory (escribible en Wolfram Cloud) y devuelve la ruta activa.";

diag::usage =
  "diag[] devuelve un diagnostico del estado de la conexion: ruta, driver, \
existencia del archivo y resultado de SELECT 1.";

Begin["`Private`"];

$conn = Null;

(* ── setup[]: resuelve la ruta de la DB y configura $databasePath ──────
   En Wolfram Cloud el sandbox Java no puede acceder a $TemporaryDirectory
   (ruta UUID profunda). Se usa /tmp/personalsite-<hash>.db que es accesible
   tanto por el kernel WL como por el proceso JDBC Java. *)
setup[] :=
  Module[{bundled, target, active},
    bundled = FileNameJoin[{PersonalSite`$Root, "data", "site.db"}];
    (* En Cloud usar /tmp directo (accesible por JDBC); en local usar $TemporaryDirectory *)
    target = If[TrueQ[$CloudEvaluation],
      "/tmp/personalsite.db",
      FileNameJoin[{$TemporaryDirectory, "personalsite.db"}]];
    active  = PersonalSite`Config`$databasePath;

    Which[
      (* PERSONALSITE_DB apunta a un archivo real: no tocar *)
      StringQ[active] && FileExistsQ[active] && active =!= bundled,
        Null,

      (* Copiar la DB bundleada a $TemporaryDirectory *)
      FileExistsQ[bundled],
        If[! FileExistsQ[target], CopyFile[bundled, target]];
        PersonalSite`Config`$databasePath = target,

      (* Ultimo recurso: crear DB desde init.sql *)
      True,
        Module[{sql = FileNameJoin[{PersonalSite`$Root, "data", "init.sql"}]},
          If[FileExistsQ[sql],
            Run["sqlite3 " <> target <> " < \"" <> sql <> "\""]]];
        PersonalSite`Config`$databasePath = target
    ];

    (* Forzar reconexion con el nuevo path *)
    Quiet[CloseSQLConnection[$conn]];
    $conn = Null;

    PersonalSite`Config`$databasePath
  ];

(* ── diag[]: diagnostico de conexion ──────────────────────────────────── *)
diag[] :=
  Module[{path, ping},
    path = PersonalSite`Config`$databasePath;
    ping = Quiet @ Check[execute["SELECT 1"], $Failed];
    <|
      "driver"      -> PersonalSite`Config`$dbDriver,
      "backend"     -> If[TrueQ[$usePythonFallback], "python/sqlite3", "jdbc"],
      "dbPath"      -> path,
      "fileExists"  -> FileExistsQ[path],
      "pacletRoot"  -> PersonalSite`$Root,
      "bundledDb"   -> FileExistsQ[FileNameJoin[{PersonalSite`$Root,"data","site.db"}]],
      "ping"        -> If[ping === {{1}} || (ListQ[ping] && Length[ping] > 0), "OK", ping],
      "connHead"    -> Head[$conn]
    |>
  ];

(* Abre la conexion JDBC segun el motor configurado:
   - sqlite     : archivo local (default).
   - postgresql : servidor externo (produccion stateless), con SSL.
   Para SQLite prueba primero el alias "SQLite" (WE local); si falla intenta
   la clase explicita "org.sqlite.JDBC" con URL jdbc:sqlite: (necesario en
   algunos entornos como Wolfram Cloud donde el alias puede no estar registrado).
   Intento 3: modo read-only via URI sqlite3 (file:path?mode=ro) para cuando el
   directorio del paclet es read-only (sin posibilidad de crear journal). *)
openConnection[] :=
  If[ToLowerCase[PersonalSite`Config`$dbDriver] === "postgresql",
    OpenSQLConnection[
      JDBC["org.postgresql.Driver",
        "jdbc:postgresql://" <> PersonalSite`Config`$pgHost <> ":" <>
          ToString[PersonalSite`Config`$pgPort] <> "/" <>
          PersonalSite`Config`$pgDatabase <>
          "?sslmode=" <> PersonalSite`Config`$pgSslMode],
      "Username" -> PersonalSite`Config`$pgUser,
      "Password" -> PersonalSite`Config`$pgPassword],
    Module[{path = PersonalSite`Config`$databasePath, conn},
      (* Intento 1: alias "SQLite" rw (Wolfram Engine local, Docker) *)
      conn = Quiet[OpenSQLConnection[JDBC["SQLite", path]], All];
      (* Intento 2: clase JDBC explicita + URL completa (Wolfram Cloud rw) *)
      If[Head[conn] =!= SQLConnection,
        conn = Quiet[OpenSQLConnection[
          JDBC["org.sqlite.JDBC", "jdbc:sqlite:" <> path]], All]];
      (* Intento 3: URI read-only (directorio del paclet es read-only) *)
      If[Head[conn] =!= SQLConnection,
        conn = Quiet[OpenSQLConnection[
          JDBC["org.sqlite.JDBC",
            "jdbc:sqlite:file:" <> path <> "?mode=ro"]], All]];
      conn
    ]
  ];

(* ── Fallback Python para WolframCloud (JDBC nativo no disponible) ─────
   WolframCloud ejecuta Java en un sandbox que bloquea la extraccion de la
   libreria nativa libsqlitejdbc.so. ExternalEvaluate["Python"] accede a
   sqlite3 (stdlib) directamente sin JNI.
   Retorna el mismo formato que SQLExecute: {{row1col1, row1col2}, ...}
   o {} para INSERT/UPDATE/DDL sin filas de resultado.              *)
$usePythonFallback = False;

executePython[sql_String, params_List] :=
  Module[{path = PersonalSite`Config`$databasePath, rows},
    rows = Quiet @ Check[
      ExternalEvaluate["Python",
        "import sqlite3, json\n" <>
        "con = sqlite3.connect('" <> StringReplace[path, "'" -> "\\'"] <> "')\n" <>
        "cur = con.execute(" <> ExportString[sql, "JSON"] <>
              ", json.loads('" <>
              StringReplace[ExportString[params, "JSON"], "'" -> "\\'"] <> "'))\n" <>
        "rows = [list(r) for r in cur.fetchall()]\n" <>
        "con.commit()\ncon.close()\nrows"],
      $Failed];
    rows
  ];

(* Conexion JDBC persistente por kernel: se abre UNA vez y se reutiliza en cada
   query. Evita el costo de OpenSQLConnection/CloseSQLConnection por consulta
   (varios ms cada uno), que dominaba la latencia de las paginas con varias
   consultas (p.ej. /apariencia). *)
connection[] :=
  If[Head[$conn] === SQLConnection,
    $conn,
    $conn = openConnection[];
    (* Si todos los intentos JDBC fallaron en Cloud, activar Python fallback *)
    If[Head[$conn] =!= SQLConnection && TrueQ[$CloudEvaluation],
      $usePythonFallback = True];
    $conn];

closeConnection[] := (Quiet @ CloseSQLConnection[$conn]; $conn = Null);

execute[sql_String, params_List : {}] :=
  Module[{r},
    (* Ruta rapida: Python fallback ya activo (Cloud sin JDBC nativo) *)
    If[TrueQ[$usePythonFallback],
      Return[executePython[sql, params]]];
    r = Quiet @ Check[SQLExecute[connection[], sql, params], $Failed];
    If[r === $Failed,
      (* Conexion posiblemente stale: reabrir y reintentar una vez *)
      closeConnection[];
      r = Quiet @ Check[SQLExecute[connection[], sql, params], $Failed]];
    (* Segunda oportunidad: activar Python fallback si JDBC sigue fallando *)
    If[r === $Failed && TrueQ[$CloudEvaluation],
      $usePythonFallback = True;
      r = executePython[sql, params]];
    r
  ];

End[];
EndPackage[];