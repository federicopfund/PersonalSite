(* ::Package:: *)

(* PersonalSite`Controller`  (parte: home)
   --------------------------------------------------------------------------
   Pagina de inicio. Consulta el modelo Post y renderiza la vista "home".
   Las funciones de los tres controllers comparten el contexto
   PersonalSite`Controller` con nombres unicos (home, blogIndex, blogShow, ask). *)

BeginPackage["PersonalSite`Controller`"];

home::usage =
  "home[request] renderiza la home con las ultimas entradas del blog.";

Begin["`Private`"];

home[request_] :=
  PersonalSite`View`render["home", <|"latest" -> PersonalSite`Assets`homeCards[]|>];

End[];
EndPackage[];