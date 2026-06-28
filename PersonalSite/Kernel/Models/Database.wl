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
   Siempre copia site.db del paclet a $TemporaryDirectory (escribible en
   cualquier entorno: Wolfram Cloud, notebooks, etc.).
   Solo omite la copia si PERSONALSITE_DB ya apunta a un archivo existente. *)
setup[] :=
  Module[{bundled, target, active},
    bundled = FileNameJoin[{PersonalSite`$Root, "data", "site.db"}];
    target  = FileNameJoin[{$TemporaryDirectory, "personalsite.db"}];
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
   - postgresql : servidor externo (produccion stateless), con SSL. *)
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
    OpenSQLConnection[JDBC["SQLite", PersonalSite`Config`$databasePath]]
  ];

(* Conexion JDBC persistente por kernel: se abre UNA vez y se reutiliza en cada
   query. Evita el costo de OpenSQLConnection/CloseSQLConnection por consulta
   (varios ms cada uno), que dominaba la latencia de las paginas con varias
   consultas (p.ej. /apariencia). *)
connection[] :=
  If[Head[$conn] === SQLConnection,
    $conn,
    $conn = openConnection[]];

closeConnection[] := (Quiet @ CloseSQLConnection[$conn]; $conn = Null);

execute[sql_String, params_List : {}] :=
  Module[{r},
    r = Quiet @ Check[SQLExecute[connection[], sql, params], $Failed];
    If[r === $Failed,
      (* conexion posiblemente stale: reabrir y reintentar una vez *)
      closeConnection[];
      r = Quiet @ Check[SQLExecute[connection[], sql, params], $Failed]];
    r
  ];

End[];
EndPackage[];