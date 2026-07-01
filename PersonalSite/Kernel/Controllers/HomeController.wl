(* ::Package:: *)

(* PersonalSite`Controller`  (parte: home)
   --------------------------------------------------------------------------
   Pagina de inicio. Consulta el modelo Post y renderiza la vista "home".
   Las funciones de los tres controllers comparten el contexto
   PersonalSite`Controller` con nombres unicos (home, blogIndex, blogShow, ask). *)

BeginPackage["PersonalSite`Controller`"];

home::usage =
  "home[request] renderiza la home con las ultimas entradas del blog.";

buildHomeGraphImage::usage =
  "buildHomeGraphImage[path, depth:10] genera el NestGraph 3D del hero " <>
  "Graph3D[NestGraph[{2#+1, #+14, #-18}&, {1}, depth]], lo rasteriza con " <>
  "fondo transparente y lo exporta como PNG a `path`. Devuelve el path o $Failed. " <>
  "Se usa fuera de linea para regenerar Resources/Img/nest-graph.png " <>
  "(el render tarda ~20s, por eso NO se computa por request).";

Begin["`Private`"];

home[request_] :=
  PersonalSite`View`render["home", <|"latest" -> PersonalSite`Assets`homeCards[]|>];

(* ── NestGraph 3D del hero  →  imagen estatica ───────────────────────────
   El grafo Graph3D[NestGraph[{2x+1, x+14, x-18}&, {1}, 10]] tiene 1846 nodos:
   su render 3D tarda ~20s, asi que NO se computa por request. En su lugar se
   genera OFFLINE con buildHomeGraphImage[] y se sirve como PNG estatico en
   Resources/Img/nest-graph.png. Los vertices se colorean por profundidad
   (cian -> violeta -> naranja) sobre fondo transparente para el card oscuro. *)
buildHomeGraphImage[path_String, depth_Integer : 10] :=
  Module[{g0, verts, dists, maxd, vstyle, gg, img},
    (* Una sola funcion que DEVUELVE la lista {2x+1, x+14, x-18}: el & envuelve
       toda la lista. ({2#+1&, #+14&, #-18&} seria una LISTA de funciones y
       NestGraph no la evaluaria, dejando vertices sin resolver.)              *)
    g0    = NestGraph[{2 #1 + 1, #1 + 14, #1 - 18} &, {1}, depth];
    verts = VertexList[g0];
    dists = Quiet @ Check[GraphDistance[g0, 1] /. Infinity -> 0, ConstantArray[0, Length[verts]]];
    maxd  = Max[dists, 1];
    vstyle = MapThread[
      #1 -> Blend[{RGBColor[0.18, 0.82, 1.0], RGBColor[0.60, 0.50, 0.98], RGBColor[1.0, 0.60, 0.14]}, #2 / maxd] &,
      {verts, dists}];
    gg = Graph3D[g0,
      VertexStyle -> vstyle,
      VertexSize  -> 0.55,
      EdgeStyle   -> Directive[Opacity[0.32], RGBColor[0.55, 0.68, 0.95]],
      Background   -> None,
      ImageSize   -> 660];
    (* Rasterizar con Background->None da un PNG con canal alfa (transparente);
       exportar el Graph3D directamente descartaria el alfa (RGB opaco). *)
    img = Rasterize[gg, Background -> None, ImageSize -> 660];
    Quiet @ Check[Export[path, img, "PNG"], $Failed]
  ];

End[];
EndPackage[];