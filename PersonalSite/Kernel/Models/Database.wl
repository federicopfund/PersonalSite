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

Begin["`Private`"];

$conn = Null;

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