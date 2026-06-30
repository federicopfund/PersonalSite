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
PersonalSite`FrontEnd`StyleEngine`typeInfo;

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
expandQ[_Association]  = True;
expandQ[_List]         = True;
expandQ[_Dataset]      = True;
expandQ[_SparseArray]  = True;
expandQ[_TimeSeries]   = True;
expandQ[_]             = False;

(* ── Type-check helpers para reglas de simulación ───────────── *)
(*  matrixQ: 2D lista de elementos numéricos, filas uniformes     *)
matrixQ[expr_] :=
  ListQ[expr] && Length[expr] >= 2 &&
  AllTrue[expr, ListQ] &&
  SameQ @@ (Length /@ expr) &&
  AllTrue[Flatten[expr, 1], NumericQ];

(*  numericVectorQ: lista 1D plana de ≥ 3 números                *)
numericVectorQ[expr_] :=
  ListQ[expr] && Length[expr] >= 3 &&
  AllTrue[expr, NumericQ[#] && !ListQ[#] &];

(*  colorQ: tipos de color de WL                                  *)
colorQ[expr_] :=
  MatchQ[Head[expr], RGBColor | Hue | CMYKColor | GrayLevel];

(* ── Sparkline SVG inline (vector numérico) ─────────────────── *)
(*  Genera un <svg> de 120×32 px con polyline normalizado.        *)
sparkline[nums_List] :=
  Module[{n = Length[nums], mn, mx, W = 120, H = 32, pts},
    mn = N[Min[nums]]; mx = N[Max[nums]];
    If[mx == mn, mx = mn + 1*^-9];
    pts = Table[
      ToString[Round[N[(i - 1)/(n - 1) * W], 0.1]] <> "," <>
      ToString[Round[N[H - (nums[[i]] - mn)/(mx - mn) * H], 0.1]],
      {i, 1, n}];
    "<svg class=\"wl-spark\" width=\"" <> ToString[W] <> "\" height=\"" <>
      ToString[H] <> "\" viewBox=\"0 0 " <> ToString[W] <> " " <> ToString[H] <> "\">" <>
      "<polyline points=\"" <> StringRiffle[pts, " "] <>
      "\" fill=\"none\" stroke=\"var(--accent)\" stroke-width=\"1.5\" stroke-linejoin=\"round\"/>" <>
    "</svg>"];

(* ── Heatmap background para celdas de matriz ───────────────── *)
heatBg[v_, mn_, mx_] :=
  Module[{t = If[mx == mn, 0.5, Clip[N[(v - mn)/(mx - mn)], {0, 1}]]},
    "rgba(99,102,241," <> ToString[Round[0.05 + t * 0.5, 0.01]] <> ")"];

(* ── Metadata compacta de tipo ──────────────────────────────── *)
PersonalSite`FrontEnd`StyleEngine`typeInfo[expr_] :=
  Module[{h = Head[expr], dims, nb},
    dims = Quiet @ Check[Dimensions[expr], {}];
    nb   = Quiet @ Check[ByteCount[expr], 0];
    <|"head"  -> ToString[h],
      "dims"  -> dims,
      "bytes" -> nb,
      "depth" -> Quiet @ Check[Depth[expr], 0]|>];

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
  "<span class=\"wl-val--err wl-nb-error\"><span class=\"wl-icon\">&#x26A0;</span> $Failed</span>";

(* ── Complex ─────────────────────────────────────────────────── *)
rendComplex[z_Complex, rule_] :=
  Module[{re = Re[z], im = Im[z], sign},
    sign = If[im >= 0, "+", "-"];
    "<span class=\"wl-ds wl-ds--complex\">" <>
      "<span class=\"wl-qty-val\">" <> esc[ToString[re]] <> "</span>" <>
      "<span class=\"wl-qty-unit\">" <> sign <> "</span>" <>
      "<span class=\"wl-qty-val\">" <> esc[ToString[Abs[im]]] <> "</span>" <>
      "<span class=\"wl-qty-unit\">&#x2148;</span>" <>
    "</span>"];

(* ── Quantity (valor + unidad) ────────────────────────────────── *)
rendQuantity[q_Quantity, rule_] :=
  Module[{val = QuantityMagnitude[q],
          unit = Quiet @ Check[UnitDimensions[q], QuantityUnit[q]]},
    "<span class=\"wl-ds wl-ds--quantity\">" <>
      "<span class=\"wl-qty-val\">" <> esc[ToString[val]] <> "</span>" <>
      "<span class=\"wl-qty-unit\">" <> esc[ToString[QuantityUnit[q], OutputForm]] <> "</span>" <>
    "</span>"];

(* ── DateObject ───────────────────────────────────────────────── *)
rendDate[d_DateObject, rule_] :=
  Module[{s = Quiet @ Check[DateString[d, {"ISODate", " ", "Time"}], ToString[d, OutputForm]]},
    "<span class=\"wl-ds wl-ds--date\">" <>
      "<span class=\"wl-icon\">&#x1F4C5;</span>" <>
      "<time datetime=\"" <> esc[s] <> "\">" <> esc[s] <> "</time>" <>
    "</span>"];

(* ── Missing ─────────────────────────────────────────────────── *)
rendMissing[m_Missing, rule_] :=
  Module[{reason = Quiet @ Check[m[[1]], "Unknown"]},
    "<span class=\"wl-val--missing\">" <>
      "<span class=\"wl-icon\">&#x2205;</span> Missing[" <> esc[ToString[reason]] <> "]" <>
    "</span>"];

(* ── Failure ─────────────────────────────────────────────────── *)
rendFailure[Failure[tag_, assoc_Association], rule_] :=
  Module[{msg    = Lookup[assoc, "Message", Lookup[assoc, "MessageTemplate", ""]],
          extra  = KeyDrop[assoc, {"Message", "MessageTemplate", "MessageParameters"}]},
    "<div class=\"wl-ds wl-ds--failure\">" <>
      "<div class=\"wl-failure-tag\">" <>
        "<span class=\"wl-icon\">&#x26A0;&#xFE0F;</span> Failure: " <> esc[ToString[tag]] <>
      "</div>" <>
      If[msg =!= "",
        "<div class=\"wl-failure-msg\">" <> esc[ToString[msg]] <> "</div>", ""] <>
      If[Length[extra] > 0,
        "<div class=\"wl-failure-detail\">" <>
          StringJoin[Function[kv,
            "<span class=\"wl-failure-kv\"><em>" <> esc[ToString[kv[[1]]]] <> "</em>: " <>
            esc[StringTake[ToString[kv[[2]], OutputForm], UpTo[120]]] <> "</span>"] /@
            Normal[extra]] <>
        "</div>", ""] <>
    "</div>"];
rendFailure[expr_, rule_] := rendMath[expr, rule];

(* ── Color swatch ────────────────────────────────────────────── *)
rendColor[expr_, rule_] :=
  Module[{rgb = Quiet @ Check[
      List @@ ColorConvert[expr, RGBColor][[1 ;; 3]], {0.4, 0.4, 0.4}],
          hex},
    hex = "#" <> StringJoin[IntegerString[Round[255 * #], 16, 2] & /@ N[rgb]];
    "<span class=\"wl-ds wl-ds--color\">" <>
      "<span class=\"wl-color-swatch\" style=\"background:" <> hex <> "\"></span>" <>
      "<code class=\"wl-color-code\">" <> esc[ToString[expr, OutputForm]] <> "</code>" <>
    "</span>"];

(* ── Numeric vector → sparkline + stats ───────────────────────── *)
rendNumericVector[expr_List, rule_] :=
  Module[{n = Length[expr], nums = N[expr],
          mn, mx, avg, sd},
    mn  = Min[nums];  mx  = Max[nums];
    avg = Mean[nums]; sd  = Quiet @ Check[StandardDeviation[nums], None];
    "<div class=\"wl-ds wl-ds--vector\">" <>
      badge[rule["id"], "Vector[" <> ToString[n] <> "]  \"\xD7 \u211D\""] <>
      "<div class=\"wl-vector-body\">" <>
        sparkline[nums] <>
        "<div class=\"wl-vector-stats\">" <>
          "<span class=\"wl-stat\">min <b>" <> esc[ToString[Round[mn, 1*^-4]]] <> "</b></span>" <>
          "<span class=\"wl-stat\">max <b>" <> esc[ToString[Round[mx, 1*^-4]]] <> "</b></span>" <>
          "<span class=\"wl-stat\">&mu; <b>" <> esc[ToString[Round[avg, 1*^-4]]] <> "</b></span>" <>
          If[sd =!= None,
            "<span class=\"wl-stat\">&sigma; <b>" <> esc[ToString[Round[sd, 1*^-4]]] <> "</b></span>",
            ""] <>
          "<span class=\"wl-stat\">n=<b>" <> ToString[n] <> "</b></span>" <>
        "</div>" <>
      "</div>" <>
    "</div>"];

(* ── Numeric matrix → heatmap table ──────────────────────────── *)
rendMatrix[expr_List, rule_] :=
  Module[{rows = Length[expr], cols = Length[expr[[1]]],
          cap  = Min[rows, 20], capc = Min[cols, 20],
          flat, mn, mx, cells},
    flat = Flatten[N[expr[[1 ;; cap, 1 ;; capc]]]];
    mn   = Min[flat];  mx = Max[flat];
    cells = Table[
      "<td class=\"wl-mx-cell\" style=\"background:" <> heatBg[N[expr[[r, c]]], mn, mx] <>
      "\" title=\"[" <> ToString[r] <> "," <> ToString[c] <> "]=" <>
        esc[ToString[Round[N[expr[[r, c]]], 1*^-3]]] <> "\">" <>
        esc[ToString[Round[N[expr[[r, c]]], 1*^-3]]] <>
      "</td>",
      {r, 1, cap}, {c, 1, capc}];
    "<div class=\"wl-ds wl-ds--matrix\">" <>
      badge[rule["id"], ToString[rows] <> "\u00D7" <> ToString[cols] <> " Matrix"] <>
      "<div class=\"wl-mx-wrap\"><table class=\"wl-mx-table\"><tbody>" <>
        StringJoin[("<tr>" <> StringJoin[#] <> "</tr>") & /@ cells] <>
      "</tbody></table></div>" <>
      If[rows > cap || cols > capc,
        "<div class=\"wl-ds-more\">&#x22EF; mostrando " <> ToString[cap] <> "\u00D7" <>
          ToString[capc] <> " de " <> ToString[rows] <> "\u00D7" <> ToString[cols] <> "</div>",
        ""] <>
    "</div>"];

(* ── TimeSeries → sparkline + rango temporal ─────────────────── *)
rendTimeSeries[ts_TimeSeries, rule_] :=
  Module[{vals  = Quiet @ Check[N[ts["Values"]], {}],
          times = Quiet @ Check[ts["Times"], {}]},
    If[Length[vals] < 2,
      Return["<div class=\"wl-ds wl-ds--ts\">" <>
        badge[rule["id"], "TimeSeries (empty)"] <> "</div>"]];
    "<div class=\"wl-ds wl-ds--ts\">" <>
      badge[rule["id"], "TimeSeries[" <> ToString[Length[vals]] <> "]"] <>
      "<div class=\"wl-vector-body\">" <>
        sparkline[vals] <>
        "<div class=\"wl-vector-stats\">" <>
          "<span class=\"wl-stat\">n=<b>" <> ToString[Length[vals]] <> "</b></span>" <>
          "<span class=\"wl-stat\">min <b>" <> esc[ToString[Round[Min[vals], 1*^-4]]] <> "</b></span>" <>
          "<span class=\"wl-stat\">max <b>" <> esc[ToString[Round[Max[vals], 1*^-4]]] <> "</b></span>" <>
          "<span class=\"wl-stat\">&mu; <b>" <> esc[ToString[Round[Mean[vals], 1*^-4]]] <> "</b></span>" <>
        "</div>" <>
      "</div>" <>
    "</div>"];

(* ── SparseArray → info compacta ─────────────────────────────── *)
rendSparseArray[sa_SparseArray, rule_] :=
  Module[{dims = Dimensions[sa],
          nnz  = Quiet @ Check[Length[sa["NonzeroValues"]], 0],
          bg   = Quiet @ Check[sa["Background"], 0]},
    "<div class=\"wl-ds wl-ds--sparse\">" <>
      badge[rule["id"], "SparseArray"] <>
      "<div class=\"wl-vector-stats\">" <>
        "<span class=\"wl-stat\">dims <b>" <> StringRiffle[ToString /@ dims, "\u00D7"] <> "</b></span>" <>
        "<span class=\"wl-stat\">nonzero <b>" <> ToString[nnz] <> "</b></span>" <>
        "<span class=\"wl-stat\">density <b>" <>
          ToString[Round[N[nnz / (Times @@ dims)] * 100, 0.01]] <> "%</b></span>" <>
        "<span class=\"wl-stat\">bg <b>" <> esc[ToString[bg]] <> "</b></span>" <>
      "</div>" <>
    "</div>"];

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
   Reglas predeterminadas — 22 reglas ordenadas por especificidad
   Prioridad: atómicos → tipos WL especiales → simulación → genéricos
   ════════════════════════════════════════════════════════════════*)
initDefaults[] := (
  (* ── Atómicos / símbolos especiales ───────────────────────── *)
  PersonalSite`FrontEnd`StyleEngine`addRule["failed",
    <|"label"->"$Failed",      "pattern"->"$Failed",
      "order"->1,  "renderer"->rendFailed|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["null",
    <|"label"->"Null",         "pattern"->"Null | Nothing",
      "order"->2,  "renderer"->rendNull|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["bool",
    <|"label"->"Boolean",      "pattern"->"True | False",
      "order"->3,  "renderer"->rendBool|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["string",
    <|"label"->"String",       "pattern"->"_String",
      "order"->4,  "renderer"->rendString|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["integer",
    <|"label"->"Integer",      "pattern"->"_Integer",
      "order"->5,  "renderer"->rendNumber|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["complex",
    <|"label"->"Complex",      "pattern"->"_Complex",
      "order"->6,  "renderer"->rendComplex|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["number",
    <|"label"->"Number",       "pattern"->"_?NumericQ",
      "order"->7,  "renderer"->rendNumber|>];

  (* ── Tipos WL con semántica física/temporal ───────────────── *)
  PersonalSite`FrontEnd`StyleEngine`addRule["quantity",
    <|"label"->"Quantity",     "pattern"->"_Quantity",
      "order"->10, "renderer"->rendQuantity|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["date",
    <|"label"->"DateObject",   "pattern"->"_DateObject",
      "order"->11, "renderer"->rendDate|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["missing",
    <|"label"->"Missing",      "pattern"->"_Missing",
      "order"->12, "renderer"->rendMissing|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["failure",
    <|"label"->"Failure",      "pattern"->"_Failure",
      "order"->13, "renderer"->rendFailure|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["color",
    <|"label"->"Color",        "pattern"->"_?colorQ",
      "order"->14, "renderer"->rendColor|>];

  (* ── Contenedores estructurados ───────────────────────────── *)
  PersonalSite`FrontEnd`StyleEngine`addRule["dataset",
    <|"label"->"Dataset",      "pattern"->"_Dataset",
      "order"->20, "renderer"->rendDataset|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["assoc",
    <|"label"->"Association",  "pattern"->"_Association",
      "order"->21, "renderer"->rendAssoc|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["rule",
    <|"label"->"Rule",         "pattern"->"_Rule | _RuleDelayed",
      "order"->22, "renderer"->rendRule|>];

  (* ── Tipos de simulación (ANTES del List genérico) ────────── *)
  PersonalSite`FrontEnd`StyleEngine`addRule["timeseries",
    <|"label"->"TimeSeries",   "pattern"->"_TimeSeries",
      "order"->28, "renderer"->rendTimeSeries|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["sparse",
    <|"label"->"SparseArray",  "pattern"->"_SparseArray",
      "order"->29, "renderer"->rendSparseArray|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["matrix",
    <|"label"->"Matrix",       "pattern"->"_?matrixQ",
      "order"->30, "renderer"->rendMatrix|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["numeric-vector",
    <|"label"->"NumericVector", "pattern"->"_?numericVectorQ",
      "order"->31, "renderer"->rendNumericVector|>];

  (* ── Lista genérica (después de tipos específicos) ─────────── *)
  PersonalSite`FrontEnd`StyleEngine`addRule["list",
    <|"label"->"List",         "pattern"->"_List",
      "order"->40, "renderer"->rendList|>];

  (* ── Gráficos y expresiones matemáticas ───────────────────── *)
  PersonalSite`FrontEnd`StyleEngine`addRule["graphics",
    <|"label"->"Graphics",
      "pattern"->"_Graphics | _Graphics3D | _Graph | _Legended | _Image",
      "order"->50, "renderer"->rendGraphics|>];

  PersonalSite`FrontEnd`StyleEngine`addRule["math",
    <|"label"->"Math",         "pattern"->"_",
      "order"->99, "renderer"->rendMath|>];
);

initDefaults[];

End[];
