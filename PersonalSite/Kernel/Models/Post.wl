(* ::Package:: *)

(* PersonalSite`Post`
   --------------------------------------------------------------------------
   Modelo de dominio del blog. Encapsula las consultas SQL y normaliza cada
   fila en una Association con claves estables.

   Usa Begin/End en lugar de BeginPackage/EndPackage para evitar General::shdw:
   los simbolos update/all/count/save/remove son internos y todos los llamadores
   ya usan la ruta completa PersonalSite`Post`recent[...] etc. *)

(* Declare public symbols in their context so callers can reference them
   before this file is loaded (e.g. autocompletion, dependency declarations). *)
PersonalSite`Post`recent;
PersonalSite`Post`bySlug;
PersonalSite`Post`insert;
PersonalSite`Post`update;
PersonalSite`Post`save;
PersonalSite`Post`remove;
PersonalSite`Post`count;
PersonalSite`Post`formatDate;

Begin["PersonalSite`Post`Private`"];

$columns = {"slug", "title", "body", "date", "summary"};

toAssoc[row_List] := AssociationThread[$columns, row];

(* ── Lectura ──────────────────────────────────────────────────────────── *)

PersonalSite`Post`recent[n_Integer : 10] :=
  Module[{rows},
    rows = PersonalSite`Database`execute[
      "SELECT slug, title, body, date, summary FROM posts ORDER BY date DESC LIMIT ?",
      {n}];
    If[ListQ[rows], toAssoc /@ rows, {}]
  ];

PersonalSite`Post`bySlug[slug_String] :=
  Module[{rows},
    rows = PersonalSite`Database`execute[
      "SELECT slug, title, body, date, summary FROM posts WHERE slug = ?",
      {slug}];
    If[ListQ[rows] && Length[rows] > 0, toAssoc[First[rows]], Missing["NotFound"]]
  ];

PersonalSite`Post`count[] :=
  Module[{r = PersonalSite`Database`execute["SELECT COUNT(*) FROM posts"]},
    If[ListQ[r] && Length[r] > 0, First @ First @ r, 0]
  ];

(* ── Escritura ────────────────────────────────────────────────────────── *)

(* Valida que spec tenga las claves obligatorias *)
validSpec[spec_Association] :=
  AllTrue[{"slug", "title", "summary", "body", "date"}, KeyExistsQ[spec, #] &];

PersonalSite`Post`insert[spec_Association] :=
  If[! validSpec[spec],
    $Failed,
    Module[{r},
      r = Quiet @ PersonalSite`Database`execute[
        "INSERT INTO posts (slug, title, summary, body, date) VALUES (?, ?, ?, ?, ?)",
        {spec["slug"], spec["title"], spec["summary"], spec["body"], spec["date"]}];
      If[r === $Failed, $Failed, "ok"]
    ]
  ];

PersonalSite`Post`update[slug_String, spec_Association] :=
  If[MissingQ[PersonalSite`Post`bySlug[slug]],
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
PersonalSite`Post`save[spec_Association] :=
  If[! validSpec[spec],
    $Failed,
    Module[{existing = PersonalSite`Post`bySlug[spec["slug"]]},
      If[MissingQ[existing],
        PersonalSite`Post`insert[spec],
        PersonalSite`Post`update[spec["slug"], spec]
      ]
    ]
  ];

PersonalSite`Post`remove[slug_String] :=
  If[MissingQ[PersonalSite`Post`bySlug[slug]],
    Missing["NotFound"],
    Module[{r},
      r = Quiet @ PersonalSite`Database`execute[
        "DELETE FROM posts WHERE slug = ?", {slug}];
      If[r === $Failed, $Failed, "ok"]
    ]
  ];

PersonalSite`Post`formatDate[d_String] := DateString[DateObject[d], {"Day", "-", "Month", "-", "Year"}];
PersonalSite`Post`formatDate[d_]       := DateString[d, {"Day", "-", "Month", "-", "Year"}];

End[];