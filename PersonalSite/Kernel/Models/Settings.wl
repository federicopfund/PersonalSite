(* ::Package:: *)

(* PersonalSite`Settings`
   --------------------------------------------------------------------------
   Almacen clave/valor sobre la tabla `settings` de SQLite. Lo usan las
   funcionalidades que necesitan estado persistente y editable en runtime
   (por ejemplo, el tema activo y su rotacion programada). *)

(* Usa Begin/End en lugar de BeginPackage/EndPackage para evitar el warning
   General::shdw: los simbolos get/set son internos y todos los llamadores
   ya usan la ruta completa PersonalSite`Settings`get[...]. *)
Begin["PersonalSite`Settings`Private`"];

PersonalSite`Settings`get[key_String, default_ : ""] :=
  Module[{rows},
    rows = Quiet @ PersonalSite`Database`execute[
      "SELECT value FROM settings WHERE key = ?", {key}];
    If[ListQ[rows] && Length[rows] > 0, First @ First @ rows, default]
  ];

(* DELETE + INSERT: independiente del valor de retorno del driver JDBC, que no
   siempre devuelve el conteo de filas para UPDATE. *)
PersonalSite`Settings`set[key_String, value_] :=
  Module[{v = ToString[value]},
    PersonalSite`Database`execute[
      "DELETE FROM settings WHERE key = ?", {key}];
    PersonalSite`Database`execute[
      "INSERT INTO settings (key, value) VALUES (?, ?)", {key, v}];
    v
  ];

PersonalSite`Settings`all[] :=
  Module[{rows},
    rows = Quiet @ PersonalSite`Database`execute["SELECT key, value FROM settings"];
    If[ListQ[rows], Association[Rule @@@ rows], <||>]
  ];

End[];
