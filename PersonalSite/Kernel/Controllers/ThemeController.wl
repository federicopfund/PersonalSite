(* ::Package:: *)

(* PersonalSite`Controller`  (parte: apariencia)
   --------------------------------------------------------------------------
   Pagina /apariencia: el usuario elige la REGLA del tema visual.
     GET   -> muestra el panel con el estado actual.
     POST  -> guarda modo (manual/auto), tema e intervalo, y confirma.

   En modo auto, la ScheduledTask `theme-rotate` rota el tema en el tiempo. *)

BeginPackage["PersonalSite`Controller`"];

appearance::usage =
  "appearance[request] maneja GET (panel) y POST (guardar) de /apariencia.";

Begin["`Private`"];

appearance[request_] :=
  If[appearanceMethod[request] === "POST",
    appearanceSubmit[request],
    appearanceForm[""]
  ];

appearanceMethod[request_] :=
  ToUpperCase @ Replace[
    Quiet @ Lookup[request, "Method", Quiet @ HTTPRequestData["Method"]],
    Except[_String] -> "GET"
  ];

capitalize[s_String] :=
  If[s === "", s, ToUpperCase @ StringTake[s, 1] <> StringDrop[s, 1]];

themeSwatch[t_String, active_String] :=
  "<button type=\"button\" class=\"theme-swatch theme-" <> t <>
    If[t === active, " is-active", ""] <>
    "\" data-pick=\"" <> t <> "\" aria-label=\"" <> t <> "\">" <>
    "<span class=\"theme-swatch__pal\"><i></i><i></i><i></i></span>" <>
    "<span class=\"theme-swatch__name\">" <> capitalize[t] <> "</span></button>";

themeOption[t_String, active_String] :=
  "<option value=\"" <> t <> "\"" <> If[t === active, " selected", ""] <> ">" <>
    capitalize[t] <> "</option>";

appearanceForm[status_String] :=
  Module[{p, themes, m, act, iv, live, secs},
    p      = PersonalSite`Theme`panel[];
    themes = p["order"];
    m      = p["mode"];
    act    = p["active"];
    iv     = p["interval"];
    live   = p["live"];
    secs   = p["secs"];
    PersonalSite`View`render["apariencia", <|
      "status"     -> status,
      "swatches"   -> StringRiffle[themeSwatch[#, act] & /@ themes, "\n"],
      "options"    -> StringRiffle[themeOption[#, act] & /@ themes, "\n"],
      "selAuto"    -> If[m === "auto", "checked", ""],
      "selManual"  -> If[m === "auto", "", "checked"],
      "active"     -> PersonalSite`View`escape[act],
      "activeName" -> capitalize[act],
      "mode"       -> m,
      "interval"   -> ToString[iv],
      "live"       -> PersonalSite`View`escape[live],
      "liveName"   -> capitalize[live],
      "secs"       -> ToString[secs],
      "autoClass"  -> If[m === "auto", "", " is-hidden"]
    |>]
  ];

appearanceSubmit[request_] :=
  Module[{f, m, theme, iv, applied, msg},
    f     = appearanceFields[request];
    m     = If[StringTrim @ Lookup[f, "mode", "manual"] === "auto", "auto", "manual"];
    theme = StringTrim @ Lookup[f, "theme", PersonalSite`Theme`active[]];
    iv    = StringTrim @ Lookup[f, "interval", "20"];

    PersonalSite`Theme`setMode[m];
    PersonalSite`Theme`setInterval[iv];
    If[m === "manual",
      (PersonalSite`Theme`setActive[theme]; applied = PersonalSite`Theme`active[]),
      (* auto: persistimos de inmediato el tema que toca por tiempo *)
      (applied = PersonalSite`Theme`computeFromTime[];
       PersonalSite`Theme`setActive[applied])
    ];

    msg = If[m === "auto",
      "Rotacion automatica activada: el tema cambia cada " <>
        ToString[PersonalSite`Theme`interval[]] <> " s (ahora: " <>
        capitalize[applied] <> ").",
      "Tema fijado en " <> capitalize[applied] <> "."];

    appearanceForm[statusBox[msg]]
  ];

statusBox[msg_String] :=
  "<div class=\"ask-box ask-success\">" <>
    "<span class=\"ask-label\">Apariencia actualizada</span>" <>
    "<p>" <> PersonalSite`View`escape[msg] <> "</p></div>";

(* Extrae los campos del POST igual que el formulario de contacto. *)
appearanceFields[request_] :=
  Module[{multipart, form},
    multipart = Quiet @ HTTPRequestData["MultipartElements"];
    If[! ListQ[multipart],
      multipart = Quiet @ Lookup[request, "MultipartElements", Missing[]]];
    If[ListQ[multipart],
      Return @ Association[
        Cases[multipart, (k_ -> v_) :> (ToString[k] -> appearanceValue[v])]]];
    form = Quiet @ Lookup[request, "FormRules", Missing[]];
    If[ListQ[form], Return @ Association[form]];
    <||>
  ];

appearanceValue[v_Association] := Lookup[v, "ContentString", ""];
appearanceValue[v_String]      := v;
appearanceValue[_]             := "";

End[];
EndPackage[];
