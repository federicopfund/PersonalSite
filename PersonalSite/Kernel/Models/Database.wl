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

execute[sql_String, params_List : {}] :=
  Module[{conn, result},
    conn   = OpenSQLConnection[JDBC["SQLite", PersonalSite`Config`$databasePath]];
    result = SQLExecute[conn, sql, params];
    CloseSQLConnection[conn];
    result
  ];

End[];
EndPackage[];