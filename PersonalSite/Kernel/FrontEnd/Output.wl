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
   toHtml  —  delega al StyleEngine (pattern-match dispatch)
   ════════════════════════════════════════════════════════════*)

(* StyleEngine.render[] usa pattern matching para despachar al renderer
   correcto (Dataset, Association, List, Graphics, Math…) y acepta
   reglas nuevas en runtime via POST /kernel/style/rule.           *)
PersonalSite`FrontEnd`Output`toHtml[expr_] :=
  Quiet @ Check[
    PersonalSite`FrontEnd`StyleEngine`render[expr],
    (* Fallback si StyleEngine no está cargado *)
    textHtml[expr]
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
