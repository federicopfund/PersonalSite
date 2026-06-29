(* ::Package:: *)

(* PersonalSite/Kernel/FrontEnd/Output.wl
   ─────────────────────────────────────────────────────────────────────────
   Renderizador de salida del kernel → HTML con aspecto nativo de WL.

   Convierte cualquier expresion WL en un fragment HTML que replica el look
   del FrontEnd de Wolfram Language:
     · Expresiones matematicas  → MathML  (via ExportString[expr,"MathML"])
     · Graphics / Graph / Image → SVG     (via ExportString[expr,"SVG"])
     · Todo lo demas            → OutputForm en <pre>

   API publica:
     PersonalSite`FrontEnd`Output`toHtml[expr]           → String HTML
     PersonalSite`FrontEnd`Output`inputCell[code, n]     → String HTML
     PersonalSite`FrontEnd`Output`outputCell[expr, n, ms]→ String HTML
   ─────────────────────────────────────────────────────────────────────────*)

PersonalSite`FrontEnd`Output`toHtml;
PersonalSite`FrontEnd`Output`inputCell;
PersonalSite`FrontEnd`Output`outputCell;

Begin["PersonalSite`FrontEnd`Output`Private`"];

(* ── Límite de caracteres en OutputForm ──────────────────── *)
$maxText = 2000;

(* ── HTML escape seguro ──────────────────────────────────── *)
esc[s_String] := StringReplace[s, {
  "&"  -> "&amp;",
  "<"  -> "&lt;",
  ">"  -> "&gt;",
  "\"" -> "&quot;",
  "'"  -> "&#39;"}];
esc[x_] := esc[ToString[x, OutputForm]];

(* ── Tipos graficos conocidos ───────────────────────────── *)
graphQ[expr_] := MatchQ[Head[expr],
  Graphics | Graphics3D | Graph | Legended |
  GraphicsGrid | GraphicsRow | GraphicsColumn | Image | DensityPlot];

(* ── Intentar MathML (headless-safe en WL 14) ───────────── *)
tryMathML[expr_] :=
  Quiet @ Check[
    ExportString[expr, "MathML", "Content" -> "MathML"],
    ""];

(* ── Intentar SVG (headless-safe) ──────────────────────── *)
trySVG[expr_] :=
  Quiet @ Check[ExportString[expr, "SVG"], ""];

(* ── OutputForm como fallback ───────────────────────────── *)
textHtml[expr_] :=
  Module[{s = Quiet @ Check[ToString[expr, OutputForm], "?"]},
    "<pre class=\"wl-nb-text\">" <>
      esc[StringTake[s, Min[$maxText, StringLength[s]]]] <>
    "</pre>"];

(* ════════════════════════════════════════════════════════════
   toHtml  —  conversor principal expr → HTML fragment
   ════════════════════════════════════════════════════════════*)

PersonalSite`FrontEnd`Output`toHtml[Null] :=
  "<span class=\"wl-nb-void\">\[EmptySet]</span>";

PersonalSite`FrontEnd`Output`toHtml[$Failed] :=
  "<span class=\"wl-nb-error\">$Failed</span>";

PersonalSite`FrontEnd`Output`toHtml[s_String] :=
  "<pre class=\"wl-nb-text wl-nb-string\">" <> esc[s] <> "</pre>";

PersonalSite`FrontEnd`Output`toHtml[expr_] :=
  Which[
    (* ── Graficos → SVG embebido ────────────────────── *)
    graphQ[expr],
      With[{svg = trySVG[expr]},
        If[StringLength[svg] > 50,
          "<div class=\"wl-nb-svg\">" <> svg <> "</div>",
          textHtml[expr]]],

    (* ── Numericos simples → texto limpio ───────────── *)
    MatchQ[expr, _Integer | _Real | _Rational | _Complex],
      "<span class=\"wl-nb-number\">" <> esc[ToString[expr]] <> "</span>",

    (* ── Resto → MathML con fallback OutputForm ──────── *)
    True,
      With[{mml = tryMathML[expr]},
        If[StringLength[mml] > 30,
          "<div class=\"wl-nb-mathml\">" <> mml <> "</div>",
          textHtml[expr]]]
  ];

(* ════════════════════════════════════════════════════════════
   inputCell  —  bloque In[n]:=
   ════════════════════════════════════════════════════════════*)
PersonalSite`FrontEnd`Output`inputCell[code_String, n_Integer] :=
  "<div class=\"wl-nb-in\">" <>
    "<span class=\"wl-nb-label wl-nb-label--in\">In[" <> ToString[n] <> "]:=</span>" <>
    "<div class=\"wl-nb-src\"><pre>" <> esc[code] <> "</pre></div>" <>
  "</div>";

(* ════════════════════════════════════════════════════════════
   outputCell  —  bloque Out[n]=
   ════════════════════════════════════════════════════════════*)
PersonalSite`FrontEnd`Output`outputCell[expr_, n_Integer, ms_] :=
  "<div class=\"wl-nb-out\">" <>
    "<span class=\"wl-nb-label wl-nb-label--out\">Out[" <> ToString[n] <> "]=</span>" <>
    "<div class=\"wl-nb-body\">" <>
      PersonalSite`FrontEnd`Output`toHtml[expr] <>
    "</div>" <>
    "<div class=\"wl-nb-timing\">" <> ToString[ms] <> "\[ThinSpace]ms</div>" <>
  "</div>";

End[];
