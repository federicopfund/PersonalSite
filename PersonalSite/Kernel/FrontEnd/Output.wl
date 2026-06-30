(* PersonalSite/Kernel/FrontEnd/Output.wl
   ─────────────────────────────────────────────────────────────────────────
   Renderizador de salida del kernel → HTML con aspecto nativo de WL.

   API pública:
     toHtml[expr]                          → String HTML (delega a StyleEngine)
     inputCell[code, n]                    → In[n]:= block
     outputCell[expr, n, ms]               → Out[n]= block con timing
     evalBlock[code, expr, n, ms]          → par In/Out con metadata
     evalBlock[code, expr, n, ms, msgs]    → ídem + mensajes WL
     messageCell[msgs, n]                  → bloque de mensajes/warnings
     progressCell[label, value, max]       → barra de progreso (simulaciones)
     typeTag[expr]                         → badge compacto de tipo
     cellGroup[cells]                      → agrupa celdas en un notebook-block
   ─────────────────────────────────────────────────────────────────────────*)

PersonalSite`FrontEnd`Output`toHtml;
PersonalSite`FrontEnd`Output`inputCell;
PersonalSite`FrontEnd`Output`outputCell;
PersonalSite`FrontEnd`Output`evalBlock;
PersonalSite`FrontEnd`Output`messageCell;
PersonalSite`FrontEnd`Output`progressCell;
PersonalSite`FrontEnd`Output`typeTag;
PersonalSite`FrontEnd`Output`cellGroup;

Begin["PersonalSite`FrontEnd`Output`Private`"];

$maxText = 2000;

esc[s_String] := StringReplace[s, {
  "&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
  "\"" -> "&quot;", "'" -> "&#39;"}];
esc[x_] := esc[ToString[x, OutputForm]];

(* ── Compact type badge ───────────────────────────────────────────── *)
(*  Muestra Head + dimensiones en un <span> pequeño.                  *)
PersonalSite`FrontEnd`Output`typeTag[expr_] :=
  Module[{h = Head[expr], dims},
    dims = Quiet @ Check[Dimensions[expr], {}];
    "<span class=\"wl-type-tag\">" <>
      esc[ToString[h]] <>
      If[dims =!= {},
        "<span class=\"wl-type-dims\">[" <> StringRiffle[ToString /@ dims, "\[Times]"] <> "]</span>",
        ""] <>
    "</span>"];

(* ── toHtml — delega al StyleEngine ──────────────────────────────── *)
PersonalSite`FrontEnd`Output`toHtml[expr_] :=
  Quiet @ Check[
    PersonalSite`FrontEnd`StyleEngine`render[expr],
    Module[{s = Quiet @ Check[ToString[expr, OutputForm], "?"]},
      "<pre class=\"wl-nb-text\">" <>
        esc[StringTake[s, Min[$maxText, StringLength[s]]]] <>
      "</pre>"]
  ];

(* ── inputCell ────────────────────────────────────────────────────── *)
PersonalSite`FrontEnd`Output`inputCell[code_String, n_Integer] :=
  "<div class=\"wl-nb-in\">" <>
    "<span class=\"wl-nb-label wl-nb-label--in\">In[" <> ToString[n] <> "]:=</span>" <>
    "<div class=\"wl-nb-src\"><pre>" <> esc[code] <> "</pre></div>" <>
  "</div>";

(* ── outputCell — con timing y typeTag ──────────────────────────── *)
PersonalSite`FrontEnd`Output`outputCell[expr_, n_Integer, ms_] :=
  "<div class=\"wl-nb-out\">" <>
    "<span class=\"wl-nb-label wl-nb-label--out\">Out[" <> ToString[n] <> "]=</span>" <>
    "<div class=\"wl-nb-body\">" <>
      PersonalSite`FrontEnd`Output`toHtml[expr] <>
    "</div>" <>
    "<div class=\"wl-nb-meta\">" <>
      PersonalSite`FrontEnd`Output`typeTag[expr] <>
      "<span class=\"wl-nb-timing\">" <> ToString[ms] <> "\[ThinSpace]ms</span>" <>
    "</div>" <>
  "</div>";

(* ── evalBlock — par In/Out completo (core para simulaciones) ─────── *)
(*  Versión sin mensajes:                                              *)
PersonalSite`FrontEnd`Output`evalBlock[
    code_String, expr_, n_Integer, ms_] :=
  PersonalSite`FrontEnd`Output`evalBlock[code, expr, n, ms, {}];

(*  Versión con mensajes WL capturados:                               *)
PersonalSite`FrontEnd`Output`evalBlock[
    code_String, expr_, n_Integer, ms_, msgs_List] :=
  "<div class=\"wl-nb-cell\" data-n=\"" <> ToString[n] <> "\">" <>
    PersonalSite`FrontEnd`Output`inputCell[code, n] <>
    If[msgs =!= {},
      PersonalSite`FrontEnd`Output`messageCell[msgs, n], ""] <>
    PersonalSite`FrontEnd`Output`outputCell[expr, n, ms] <>
  "</div>";

(* ── messageCell — mensajes / warnings del kernel ────────────────── *)
PersonalSite`FrontEnd`Output`messageCell[msgs_List, n_Integer] :=
  "<div class=\"wl-nb-msgs\">" <>
    StringJoin[Function[m,
      "<div class=\"wl-nb-msg\">" <>
        "<span class=\"wl-icon\">&#x26A0;</span>" <>
        "<code class=\"wl-msg-text\">" <> esc[ToString[m]] <> "</code>" <>
      "</div>"] /@ msgs] <>
  "</div>";

(*  Variante con un solo String (mensaje de texto):                   *)
PersonalSite`FrontEnd`Output`messageCell[msg_String, n_Integer] :=
  PersonalSite`FrontEnd`Output`messageCell[{msg}, n];

(* ── progressCell — barra de progreso para simulaciones largas ────── *)
(*  label   : String descriptivo
    value   : paso actual (Integer o Real)
    max     : pasos totales
    Emite HTML estático; el JS actualiza `value` y `data-pct` por id. *)
PersonalSite`FrontEnd`Output`progressCell[
    label_String, value_, max_] :=
  Module[{pct = If[max == 0, 0, Round[N[value/max * 100], 0.1]],
          pid = "prog-" <> ToString[Unique[]]},
    "<div class=\"wl-progress\" id=\"" <> pid <> "\" data-pct=\"" <>
        ToString[pct] <> "\">" <>
      "<div class=\"wl-progress-header\">" <>
        "<span class=\"wl-progress-label\">" <> esc[label] <> "</span>" <>
        "<span class=\"wl-progress-pct\">" <>
          ToString[value] <> " / " <> ToString[max] <>
          " (" <> ToString[pct] <> "%)" <>
        "</span>" <>
      "</div>" <>
      "<div class=\"wl-progress-bar\">" <>
        "<div class=\"wl-progress-fill\" style=\"width:" <> ToString[pct] <> "%\"></div>" <>
      "</div>" <>
    "</div>"];

(* ── cellGroup — envuelve varias celdas en un bloque de notebook ──── *)
PersonalSite`FrontEnd`Output`cellGroup[cells_List] :=
  "<div class=\"wl-nb-group\">" <>
    StringJoin[cells] <>
  "</div>";

PersonalSite`FrontEnd`Output`cellGroup[cells__String] :=
  PersonalSite`FrontEnd`Output`cellGroup[{cells}];

End[];
