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

insert::usage =
  "insert[spec] inserta un post nuevo. spec debe tener slug,title,summary,body,date. \
Devuelve \"ok\" o $Failed si el slug ya existe.";

update::usage =
  "update[slug, spec] actualiza los campos de spec en el post con ese slug. \
Devuelve \"ok\" o Missing[\"NotFound\"].";

save::usage =
  "save[spec] inserta o actualiza (upsert) el post. Funciona en SQLite y PostgreSQL.";

remove::usage =
  "remove[slug] elimina el post con ese slug. Devuelve \"ok\" o Missing[\"NotFound\"].";

count::usage =
  "count[] devuelve el total de posts en la tabla.";

formatDate::usage =
  "formatDate[fecha] formatea una fecha almacenada como DD-MM-YYYY.";

Begin["`Private`"];

$columns = {"slug", "title", "body", "date", "summary"};

toAssoc[row_List] := AssociationThread[$columns, row];

(* ── Lectura ──────────────────────────────────────────────────────────── *)

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

count[] :=
  Module[{r = PersonalSite`Database`execute["SELECT COUNT(*) FROM posts"]},
    If[ListQ[r] && Length[r] > 0, First @ First @ r, 0]
  ];

(* ── Escritura ────────────────────────────────────────────────────────── *)

(* Valida que spec tenga las claves obligatorias *)
validSpec[spec_Association] :=
  AllTrue[{"slug", "title", "summary", "body", "date"}, KeyExistsQ[spec, #] &];

insert[spec_Association] :=
  If[! validSpec[spec],
    $Failed,
    Module[{r},
      r = Quiet @ PersonalSite`Database`execute[
        "INSERT INTO posts (slug, title, summary, body, date) VALUES (?, ?, ?, ?, ?)",
        {spec["slug"], spec["title"], spec["summary"], spec["body"], spec["date"]}];
      If[r === $Failed, $Failed, "ok"]
    ]
  ];

update[slug_String, spec_Association] :=
  If[MissingQ[bySlug[slug]],
    Missing["NotFound"],
    Module[{fields, setClauses, vals, r},
      (* Solo actualiza las claves presentes en spec, ignorando slug *)
      fields     = Select[{"title", "summary", "body", "date"}, KeyExistsQ[spec, #] &];
      setClauses = StringRiffle[# <> " = ?" & /@ fields, ", "];
      vals       = Append[Lookup[spec, fields], slug];
      r = Quiet @ PersonalSite`Database`execute[
        "UPDATE posts SET " <> setClauses <> " WHERE slug = ?", vals];
      If[r === $Failed, $Failed, "ok"]
    ]
  ];

(* Upsert compatible con SQLite y PostgreSQL:
   intenta UPDATE; si no afecto filas hace INSERT.
   Usa el patron DELETE+INSERT para maxima compatibilidad. *)
save[spec_Association] :=
  If[! validSpec[spec],
    $Failed,
    Module[{existing = bySlug[spec["slug"]]},
      If[MissingQ[existing],
        insert[spec],
        update[spec["slug"], spec]
      ]
    ]
  ];

remove[slug_String] :=
  If[MissingQ[bySlug[slug]],
    Missing["NotFound"],
    Module[{r},
      r = Quiet @ PersonalSite`Database`execute[
        "DELETE FROM posts WHERE slug = ?", {slug}];
      If[r === $Failed, $Failed, "ok"]
    ]
  ];

formatDate[d_String] := DateString[DateObject[d], {"Day", "-", "Month", "-", "Year"}];
formatDate[d_]       := DateString[d, {"Day", "-", "Month", "-", "Year"}];

End[];
EndPackage[];