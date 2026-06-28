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
  "setup[] prepara la conexion a la base de datos del paclet. En entornos donde \
el directorio del paclet es de solo lectura (Wolfram Cloud) copia el site.db \
a $TemporaryDirectory y apunta $databasePath ahi. Devuelve la ruta activa.";

Begin["`Private`"];

$conn = Null;

(* ── setup[]: resuelve la ruta de la DB y configura $databasePath ──────
   Prioridad:
     1. PERSONALSITE_DB env var (ya seteada en Config antes de llamar)
     2. Paclet data/site.db si es escribible
     3. Copia en $TemporaryDirectory (Wolfram Cloud: paclet es read-only)   *)
setup[] :=
  Module[{bundled, target},
    (* Ruta del site.db dentro del paclet instalado *)
    bundled = FileNameJoin[{PersonalSite`$Root, "data", "site.db"}];

    Which[
      (* Ya configurado externamente (env var / override manual) y existe *)
      FileExistsQ[PersonalSite`Config`$databasePath] &&
        PersonalSite`Config`$databasePath =!= bundled,
        Null,   (* usar lo que ya esta *)

      (* Bundled existe y su directorio es escribible *)
      FileExistsQ[bundled] &&
        Quiet @ Check[WriteString[
          OpenWrite[bundled, BinaryFormat -> True], ""], $Failed] =!= $Failed,
        PersonalSite`Config`$databasePath = bundled,

      (* Fallback: copiar a $TemporaryDirectory (siempre escribible) *)
      True,
        target = FileNameJoin[{$TemporaryDirectory, "personalsite_paclet.db"}];
        If[FileExistsQ[bundled] && ! FileExistsQ[target],
          CopyFile[bundled, target]];
        If[! FileExistsQ[target],
          (* ultimo recurso: crear DB vacia desde init.sql si sqlite3 disponible *)
          Module[{sql = FileNameJoin[{PersonalSite`$Root, "data", "init.sql"}]},
            If[FileExistsQ[sql],
              Run["sqlite3 " <> target <> " < " <> sql]]]];
        PersonalSite`Config`$databasePath = target
    ];

    (* Reset de la conexion para que use el nuevo path *)
    Quiet @ CloseSQLConnection[$conn];
    $conn = Null;
    PersonalSite`Config`$databasePath
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