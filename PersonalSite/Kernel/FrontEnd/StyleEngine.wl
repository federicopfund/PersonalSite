(* ::Package:: *)

(* PersonalSite/Kernel/FrontEnd/StyleEngine.wl
   ─────────────────────────────────────────────────────────────────────────
   Motor de renderizado WL→HTML guiado por reglas de pattern matching.

   Cada regla es una Association con:
     "id"       → String   identificador único
     "label"    → String   nombre legible
     "pattern"  → String   expresión WL que se compila con ToExpression[]
                           y se usa como segundo arg de MatchQ[]
     "order"    → Integer  prioridad (menor = primero)
     "renderer" → Function[{expr, rule}] → String HTML

   Las reglas son mutables en runtime:
     addRule["id", spec]  — registra/reemplaza una regla
     removeRule["id"]     — elimina una regla
     listRules[]          — introspección (sin compiledPattern)
     resetRules[]         — restaura las 12 reglas predeterminadas

   El dispatcher:
     render[expr]         — ordena $rules por "order", llama MatchQ en
                           cada compiledPattern, usa el primero que matchea
   ─────────────────────────────────────────────────────────────────────────*)

PersonalSite`FrontEnd`StyleEngine`render;
PersonalSite`FrontEnd`StyleEngine`addRule;
PersonalSite`FrontEnd`StyleEngine`removeRule;
PersonalSite`FrontEnd`StyleEngine`listRules;
PersonalSite`FrontEnd`StyleEngine`resetRules;

Begin["PersonalSite`FrontEnd`StyleEngine`Private`"];

(* ── Estado mutable ─────────────────────────────────────────── *)
$rules = <||>;   (* Ordered Association: id -> spec completo *)

(* ── Profundidad de recursión (guard) ───────────────────────── *)
$maxDepth = 4;

(* ── HTML escape ─────────────────────────────────────────────── *)
esc[s_String] := StringReplace[s, {
  "&"  -> "&amp;",  "<"  -> "&lt;",  ">"  -> "&gt;",
  "\"" -> "&quot;", "'"  -> "&#39;"}];
esc[x_] := esc[ToString[x, OutputForm]];

(* ── Clase CSS por tipo de valor ────────────────────────────── *)
valClass[_Integer]        = "wl-val--int";
valClass[_Real|_Rational] = "wl-val--num";
valClass[_Complex]        = "wl-val--num";
valClass[_String]         = "wl-val--str";
valClass[True|False]      = "wl-val--bool";
valClass[Null]            = "wl-val--null";
valClass[$Failed]         = "wl-val--err";
valClass[_]               = "wl-val--other";

(* ── Expansión nativa: tipos que se colapsan como <details> ──── *)
expandQ[_Association] = True;
expandQ[_List]        = True;
expandQ[_Dataset]     = True;
expandQ[_]            = False;

(* ── Resumen compacto para el <summary> del <details> ────────── *)
(*   Replica la notación nativa de WL: ⟨|n|⟩ para Association,
     {n} para List, Dataset[{dims}] para Dataset.               *)
expandSummary[a_Association] :=
  "\[LeftAngleBracket]\[VerticalSeparator]" <>
    StringRiffle[
      StringTake[esc[ToString[#]], Min[14, StringLength[esc[ToString[#]]]]] & /@
        Take[Keys[a], Min[3, Length[a]]],
      ", "] <>
    If[Length[a] > 3, ", \[Ellipsis]", ""] <>
  "\[VerticalSeparator]\[RightAngleBracket]" <>
  " \[ThinSpace]\[LongDash] " <> ToString[Length[a]] <> " keys";

expandSummary[l_List] :=
  "{ " <>
    StringRiffle[
      (esc[StringTake[ToString[#, OutputForm], Min[12, StringLength[ToString[#, OutputForm]]]]] &) /@
        Take[l, Min[4, Length[l]]],
      ", "] <>
    If[Length[l] > 4, ", \[Ellipsis]", ""] <>
  " }";

expandSummary[d_Dataset] :=
  "Dataset \[Times] " <>
    Quiet @ Check[StringRiffle[ToString /@ Dimensions[d], " \[Times] "], "?"];

expandSummary[expr_] :=
  esc[StringTake[ToString[Head[expr]], Min[18, StringLength[ToString[Head[expr]]]]] <> "[\[Ellipsis]]"];

(* ── Render recursivo con guard + collapsible para hijos ────── *)
(*   - depth=0 → top level, se renderiza directamente
     - depth>0 y expandQ → envuelve en <details><summary>preview
     - depth>=$maxDepth → símbolo de elipsis                     *)
renderVal[expr_, depth_Integer] :=
  Which[
    depth >= $maxDepth,
      "<span class=\"wl-val--ellipsis\">\[Ellipsis]</span>",

    expandQ[expr],
      "<details class=\"wl-expand\">" <>
        "<summary class=\"wl-expand-sum\">" <>
          "<span class=\"wl-expand-tri\"></span>" <>
          "<span class=\"wl-expand-preview\">" <> expandSummary[expr] <> "</span>" <>
        "</summary>" <>
        "<div class=\"wl-expand-body\">" <>
          dispatch[expr, depth + 1] <>
        "</div>" <>
      "</details>",

    True,
      dispatch[expr, depth + 1]
  ];

(* ════════════════════════════════════════════════════════════════
   Renderers predeterminados
   Cada uno tiene firma  renderer[expr_, rule_Association] → String
   ════════════════════════════════════════════════════════════════*)

(* ── Header-badge común ─────────────────────────────────────── *)
badge[ruleId_String, typeLabel_String] :=
  "<div class=\"wl-ds-header\">" <>
    "<span class=\"wl-ds-type\">" <> esc[typeLabel] <> "</span>" <>
    "<span class=\"wl-ds-rule\">\[RightTriangle] " <> esc[ruleId] <> "</span>" <>
  "</div>";

(* ── $Failed ──────────────────────────────────────────────────── *)
rendFailed[$Failed, rule_] :=
  "<span class=\"wl-val--err wl-nb-error\">$Failed</span>";

(* ── Null / Nothing ──────────────────────────────────────────── *)
rendNull[_, rule_] :=
  "<span class=\"wl-val--null wl-nb-void\">\[EmptySet]</span>";

(* ── Boolean ─────────────────────────────────────────────────── *)
rendBool[b_, rule_] :=
  "<span class=\"wl-val--bool\">" <> If[TrueQ[b], "True", "False"] <> "</span>";

(* ── String ──────────────────────────────────────────────────── *)
rendString[s_String, rule_] :=
  "<span class=\"wl-val--str\">&ldquo;" <> esc[s] <> "&rdquo;</span>";

(* ── Number (Integer/Real/Rational/Complex) ──────────────────── *)
rendNumber[n_, rule_] :=
  "<span class=\"wl-val--int wl-nb-number\">" <> esc[ToString[n]] <> "</span>";

(* ── Association → clave/valor table ────────────────────────── *)
rendAssoc[expr_Association, rule_] :=
  Module[{pairs = Normal[expr], depth = rule["depth"] /. _Missing -> 0, rows, extra},
    extra = If[Length[pairs] > 40,
      "<div class=\"wl-ds-more\">\[Ellipsis] " <> ToString[Length[pairs]-40] <> " more</div>",
      ""];
    rows = StringJoin[
      Function[kv,
        "<tr class=\"wl-ds-row\">" <>
          "<td class=\"wl-ds-key\">" <> esc[ToString[kv[[1]]]] <> "</td>" <>
          "<td class=\"wl-ds-val " <> valClass[kv[[2]]] <> "\">" <>
            renderVal[kv[[2]], depth] <>
          "</td>" <>
        "</tr>"] /@ Take[pairs, Min[40, Length[pairs]]]];
    "<div class=\"wl-ds wl-ds--assoc\">" <>
      badge[rule["id"], "Association \[RightAngleBracket]" <> ToString[Length[expr]] <> "\[LeftAngleBracket]"] <>
      "<table class=\"wl-ds-table\"><tbody>" <> rows <> "</tbody></table>" <>
      extra <>
    "</div>"
  ];

(* ── Dataset → normaliza a assoc, delega ─────────────────────── *)
rendDataset[expr_, rule_] :=
  Module[{inner = Quiet @ Check[Normal[expr], expr]},
    "<div class=\"wl-ds wl-ds--dataset\">" <>
      badge[rule["id"], "Dataset"] <>
      "<div class=\"wl-ds-inner\">" <> renderVal[inner, 0] <> "</div>" <>
    "</div>"
  ];

(* ── List → ítems indexados ──────────────────────────────────── *)
rendList[expr_List, rule_] :=
  Module[{items = Take[expr, Min[30, Length[expr]]],
          depth = rule["depth"] /. _Missing -> 0, rows, extra},
    extra = If[Length[expr] > 30,
      "<div class=\"wl-ds-more\">\[Ellipsis] " <> ToString[Length[expr]-30] <> " more</div>",
      ""];
    rows = StringJoin @
      MapIndexed[
        Function[{v, idx},
          "<div class=\"wl-list-row\">" <>
            "<span class=\"wl-list-idx\">" <> ToString[idx[[1]]] <> "</span>" <>
            "<div class=\"wl-list-val " <> valClass[v] <> "\">" <>
              renderVal[v, depth] <>
            "</div>" <>
          "</div>"],
        items];
    "<div class=\"wl-ds wl-ds--list\">" <>
      badge[rule["id"], "List[" <> ToString[Length[expr]] <> "]"] <>
      "<div class=\"wl-list-items\">" <> rows <> "</div>" <>
      extra <>
    "</div>"
  ];

(* ── Rule/RuleDelayed → k → v ────────────────────────────────── *)
rendRule[k_ -> v_, rule_] :=
  "<div class=\"wl-ds wl-ds--rule\">" <>
    "<div class=\"wl-ds-kv\">" <>
      "<span class=\"wl-ds-key\">" <> esc[ToString[k]] <> "</span>" <>
      "<span class=\"wl-ds-arrow\">&rarr;</span>" <>
      "<span class=\"wl-ds-val " <> valClass[v] <> "\">" <> renderVal[v, 0] <> "</span>" <>
    "</div>" <>
  "</div>";

rendRule[k_ :> v_, rule_] := rendRule[k -> v, rule];

(* ── Graphics / Image / Graph → SVG embebido ─────────────────── *)
rendGraphics[expr_, rule_] :=
  Module[{svg = Quiet @ Check[ExportString[expr, "SVG"], ""]},
    "<div class=\"wl-ds wl-ds--graphics\">" <>
      badge[rule["id"], ToString[Head[expr]]] <>
      If[StringLength[svg] > 50,
        "<div class=\"wl-nb-svg\">" <> svg <> "</div>",
        "<pre class=\"wl-nb-text\">" <> esc[ToString[expr, OutputForm]] <> "</pre>"] <>
    "</div>"
  ];

(* ── Math (catch-all) → MathML con fallback OutputForm ─────── *)
rendMath[expr_, rule_] :=
  Module[{mml = Quiet @ Check[ExportString[expr, "MathML", "Content" -> "MathML"], ""],
          s   = Quiet @ Check[ToString[expr, OutputForm], "?"]},
    "<div class=\"wl-ds wl-ds--math\">" <>
      badge[rule["id"], "Math"] <>
      If[StringLength[mml] > 30,
        "<div class=\"wl-nb-mathml\">" <> mml <> "</div>",
        "<pre class=\"wl-nb-text\">" <>
          esc[StringTake[s, Min[2000, StringLength[s]]]] <>
        "</pre>"] <>
    "</div>"
  ];

(* ════════════════════════════════════════════════════════════════
   Public API
   ════════════════════════════════════════════════════════════════*)

(* Registrar / reemplazar una regla *)
PersonalSite`FrontEnd`StyleEngine`addRule[id_String, spec_Association] :=
  Module[{compiled},
    compiled = Quiet @ Check[
      ToExpression @ Lookup[spec, "pattern", "_"],
      _  (* catch-all si el pattern falla al compilar *)
    ];
    AssociateTo[$rules, id -> Join[spec, <|
      "id"              -> id,
      "compiledPattern" -> compiled
    |>]]
  ];

(* Eliminar una regla *)
PersonalSite`FrontEnd`StyleEngine`removeRule[id_String] :=
  ($rules = KeyDrop[$rules, id]; "ok");

(* Listar reglas (sin compiledPattern — no es JSON-serializable) *)
PersonalSite`FrontEnd`StyleEngine`listRules[] :=
  KeyValueMap[
    Function[{id, r},
      <|"id"      -> id,
        "label"   -> Lookup[r, "label",   id],
        "pattern" -> Lookup[r, "pattern", ""],
        "order"   -> Lookup[r, "order",   99]
      |>],
    $rules];

(* Restaurar defaults *)
PersonalSite`FrontEnd`StyleEngine`resetRules[] :=
  ($rules = <||>; initDefaults[]; "ok");

(* ════════════════════════════════════════════════════════════════
   Dispatcher principal
   ════════════════════════════════════════════════════════════════*)

(* Sorted dispatch — primer match gana *)
dispatch[expr_, depth_Integer : 0] :=
  Module[{sorted, match},
    sorted = SortBy[$rules, Lookup[#, "order", 99] &];
    match  = SelectFirst[sorted,
      Function[r,
        Quiet @ Check[
          TrueQ[MatchQ[expr, r["compiledPattern"]]],
          False]]];
    If[MissingQ[match],
      (* Fallback absoluto *)
      "<pre class=\"wl-nb-text\">" <>
        esc[StringTake[
          Quiet @ Check[ToString[expr, OutputForm], "?"],
          Min[2000, StringLength[
            Quiet @ Check[ToString[expr, OutputForm], "?"]]]]] <>
      "</pre>",
      match["renderer"][expr, Append[match, "depth" -> depth]]]
  ];

PersonalSite`FrontEnd`StyleEngine`render[expr_] := dispatch[expr, 0];

(* ════════════════════════════════════════════════════════════════
   Reglas predeterminadas (12 reglas en orden de prioridad)
   ════════════════════════════════════════════════════════════════*)
initDefaults[] := (
  PersonalSite`FrontEnd`StyleEngine`addRule["failed",
    <|"label"->"$Failed",    "pattern"->"$Failed",
      "order"->1,  "renderer"->rendFailed|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["null",
    <|"label"->"Null",       "pattern"->"Null | Nothing",
      "order"->2,  "renderer"->rendNull|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["bool",
    <|"label"->"Boolean",    "pattern"->"True | False",
      "order"->3,  "renderer"->rendBool|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["string",
    <|"label"->"String",     "pattern"->"_String",
      "order"->4,  "renderer"->rendString|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["integer",
    <|"label"->"Integer",    "pattern"->"_Integer",
      "order"->5,  "renderer"->rendNumber|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["number",
    <|"label"->"Number",     "pattern"->"_?NumericQ",
      "order"->6,  "renderer"->rendNumber|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["dataset",
    <|"label"->"Dataset",    "pattern"->"_Dataset",
      "order"->7,  "renderer"->rendDataset|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["assoc",
    <|"label"->"Association","pattern"->"_Association",
      "order"->8,  "renderer"->rendAssoc|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["rule",
    <|"label"->"Rule",       "pattern"->"_Rule | _RuleDelayed",
      "order"->9,  "renderer"->rendRule|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["list",
    <|"label"->"List",       "pattern"->"_List",
      "order"->10, "renderer"->rendList|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["graphics",
    <|"label"->"Graphics",
      "pattern"->"_Graphics | _Graphics3D | _Graph | _Legended | _Image",
      "order"->11, "renderer"->rendGraphics|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["math",
    <|"label"->"Math",       "pattern"->"_",
      "order"->99, "renderer"->rendMath|>];
);

initDefaults[];

End[];
