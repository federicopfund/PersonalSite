(* ::Package:: *)

(* PersonalSite`Controller`  (parte: blog)
   --------------------------------------------------------------------------
   Indice del blog y detalle de cada entrada. *)

BeginPackage["PersonalSite`Controller`"];

blogIndex::usage =
  "blogIndex[request] renderiza la lista de entradas recientes.";

blogShow::usage =
  "blogShow[slug, request] renderiza una entrada, o una respuesta 404 si no existe.";

Begin["`Private`"];

blogIndex[request_] :=
  PersonalSite`View`render["blog/index", <|"items" -> PersonalSite`Assets`blogCards[]|>];

blogShow[slug_String, request_] :=
  Module[{post = PersonalSite`Post`bySlug[slug]},
    If[MissingQ[post],
      HTTPResponse["404", <|"StatusCode" -> 404|>],
      PersonalSite`View`render["blog/post", <|
        post,
        "title" -> PersonalSite`View`escape[post["title"]],
        "date"  -> PersonalSite`Post`formatDate[post["date"]]
      |>]
    ]
  ];

End[];
EndPackage[];