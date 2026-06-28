(* ::Package:: *)

(* PersonalSite`Settings`
   --------------------------------------------------------------------------
   Almacen clave/valor sobre la tabla `settings` de SQLite. Lo usan las
   funcionalidades que necesitan estado persistente y editable en runtime
   (por ejemplo, el tema activo y su rotacion programada). *)

BeginPackage["PersonalSite`Settings`"];

get::usage =
  "get[key, default] devuelve el valor de configuracion (String) o default si \
no existe.";

set::usage =
  "set[key, value] inserta o actualiza una clave de configuracion y devuelve el \
valor guardado como String.";

all::usage =
  "all[] devuelve todas las claves de configuracion como Association.";

Begin["`Private`"];

get[key_String, default_ : ""] :=
  Module[{rows},
    rows = Quiet @ PersonalSite`Database`execute[
      "SELECT value FROM settings WHERE key = ?", {key}];
    If[ListQ[rows] && Length[rows] > 0, First @ First @ rows, default]
  ];

(* DELETE + INSERT: independiente del valor de retorno del driver JDBC, que no
   siempre devuelve el conteo de filas para UPDATE. *)
set[key_String, value_] :=
  Module[{v = ToString[value]},
    PersonalSite`Database`execute[
      "DELETE FROM settings WHERE key = ?", {key}];
    PersonalSite`Database`execute[
      "INSERT INTO settings (key, value) VALUES (?, ?)", {key, v}];
    v
  ];

all[] :=
  Module[{rows},
    rows = Quiet @ PersonalSite`Database`execute["SELECT key, value FROM settings"];
    If[ListQ[rows], Association[Rule @@@ rows], <||>]
  ];

End[];
EndPackage[];
