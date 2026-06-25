(* ::Package:: *)

(* PersonalSite`Post`
   --------------------------------------------------------------------------
   Modelo de dominio del blog. Encapsula las consultas SQL y normaliza cada
   fila en una Association con claves estables. *)

BeginPackage["PersonalSite`Post`"];

recent::usage =
  "recent[n] devuelve los n posts mas recientes como lista de Associations.";

bySlug::usage =
  "bySlug[slug] devuelve el post con ese slug, o Missing[\"NotFound\"] si no existe.";

formatDate::usage =
  "formatDate[fecha] formatea una fecha almacenada como DD-MM-YYYY.";

Begin["`Private`"];

$columns = {"slug", "title", "body", "date", "summary"};

toAssoc[row_List] := AssociationThread[$columns, row];

recent[n_Integer : 10] :=
  Module[{rows},
    rows = PersonalSite`Database`execute[
      "SELECT slug, title, body, date, summary FROM posts ORDER BY date DESC LIMIT ?",
      {n}];
    If[ListQ[rows], toAssoc /@ rows, {}]
  ];

bySlug[slug_String] :=
  Module[{rows},
    rows = PersonalSite`Database`execute[
      "SELECT slug, title, body, date, summary FROM posts WHERE slug = ?",
      {slug}];
    If[ListQ[rows] && Length[rows] > 0, toAssoc[First[rows]], Missing["NotFound"]]
  ];

formatDate[d_String] := DateString[DateObject[d], {"Day", "-", "Month", "-", "Year"}];
formatDate[d_]       := DateString[d, {"Day", "-", "Month", "-", "Year"}];

End[];
EndPackage[];