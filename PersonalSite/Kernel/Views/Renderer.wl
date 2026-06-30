(* ::Package:: *)

(* PersonalSite`View`
   --------------------------------------------------------------------------
   Renderizado de vistas HTML sobre StringTemplate. Las plantillas viven en
   Resources/Templates y usan delimitadores <* TemplateSlot["x"] *>. *)

BeginPackage["PersonalSite`View`"];

render::usage =
  "render[view, data] renderiza la vista dentro del layout y devuelve un HTTPResponse.";

fragment::usage =
  "fragment[partial, data] renderiza una plantilla parcial y devuelve el HTML como String.";

escape::usage =
  "escape[s] escapa los 5 caracteres HTML peligrosos de s (previene XSS).";

postItem::usage =
  "postItem[post] renderiza el parcial blog/item para una Association de post.";

Begin["`Private`"];

(* Carga y compila una plantilla por nombre (sin extension). MEMOIZADA: cada
   plantilla se lee de disco y se compila UNA sola vez por kernel; los renders
   siguientes reutilizan el StringTemplate ya compilado (sin I/O ni recompilar). *)
template[name_String] :=
  template[name] = StringTemplate[
    ReadString[FileNameJoin[{
      PersonalSite`$Root, "Resources", "Templates", name <> ".html"}],
      CharacterEncoding -> "UTF-8"],
    Delimiters -> {"<*", "*>"}];

escape[s_String] :=
  StringReplace[s, {"&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
                    "\"" -> "&quot;", "'" -> "&#39;"}];
escape[x_] := escape[ToString[x]];

fragment[partial_String, data_Association] :=
  TemplateApply[template[partial], data];

(* Variables compartidas, inyectadas tanto en la vista como en el layout. *)
shared[] :=
  Module[{cfg},
    cfg = Quiet @ Check[PersonalSite`Theme`clientConfig[],
      <|"theme" -> "slate", "mode" -> "manual", "order" -> "slate", "interval" -> 20, "epoch" -> 0|>];
    <|
      "siteName"      -> PersonalSite`Config`$siteName,
      "year"          -> DateValue[Now, "Year"],
      "theme"         -> cfg["theme"],
      "themeMode"     -> cfg["mode"],
      "themeOrder"    -> cfg["order"],
      "themeInterval" -> ToString @ cfg["interval"],
      "themeEpoch"    -> ToString @ cfg["epoch"]
    |>
  ];

render[view_String, data_Association] :=
  Module[{ctx = shared[], viewHtml, page},
    viewHtml = TemplateApply[template[view], Join[ctx, data]];
    page     = TemplateApply[template["layout"], <|"content" -> viewHtml, ctx|>];
    HTTPResponse[page, <|"Headers" -> <|"Content-Type" -> "text/html; charset=utf-8"|>|>]
  ];

(* Tarjeta de post reutilizada por la home y el indice del blog. *)
postItem[post_Association] :=
  fragment["blog/item", <|
    "slug"    -> post["slug"],
    "title"   -> escape[post["title"]],
    "summary" -> escape[post["summary"]],
    "date"    -> PersonalSite`Post`formatDate[post["date"]]
  |>];

(* Warm-up: precompila (y memoiza) las plantillas conocidas una vez por kernel
   al cargar el modulo, para que ningun request pague la compilacion. *)
Quiet @ Scan[template, {
  "layout", "home", "blog/index", "blog/item", "blog/post",
  "contact", "ask", "apariencia", "flow", "perf", "nest", "tasks", "arch", "kernel", "dag", "kpi"}];

End[];
EndPackage[];