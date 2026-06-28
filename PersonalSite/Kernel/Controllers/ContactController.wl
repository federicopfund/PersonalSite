(* ::Package:: *)

(* PersonalSite`Controller`  (parte: contacto)
   --------------------------------------------------------------------------
   Pagina /contacto: formulario de contacto.
     GET   -> muestra el formulario vacio.
     POST  -> valida, envia el correo via Mailer y reporta el resultado,
              preservando los valores si hay error.

   Incluye un honeypot ("website") como filtro anti-spam simple. *)

BeginPackage["PersonalSite`Controller`"];

contact::usage =
  "contact[request] maneja GET (formulario) y POST (envio) de /contacto.";

Begin["`Private`"];

contact[request_] :=
  If[contactMethod[request] === "POST",
    contactSubmit[request],
    contactForm[<||>, ""]
  ];

(* Metodo HTTP en mayusculas; por defecto GET. *)
contactMethod[request_] :=
  ToUpperCase @ Replace[
    Quiet @ Lookup[request, "Method", Quiet @ HTTPRequestData["Method"]],
    Except[_String] -> "GET"
  ];

(* Valores por defecto del formulario. "kind" distingue entre enviar un
   mensaje ("message") y planificar una reunion ("meeting"). *)
$contactDefaults = <|
  "name" -> "", "email" -> "", "message" -> "",
  "kind" -> "message", "date" -> "", "time" -> ""
|>;

(* Normaliza el tipo de consulta a "meeting" o "message". *)
contactKind[k_] := If[StringTrim @ ToString[k] === "meeting", "meeting", "message"];

(* Renderiza la vista; status es un bloque HTML opcional (caja de estado).
   fields preserva los valores ingresados al re-renderizar tras un error. *)
contactForm[fields_Association, status_String] :=
  Module[{f = Join[$contactDefaults, fields], kind},
    kind = contactKind[f["kind"]];
    PersonalSite`View`render["contact", <|
      "name"         -> PersonalSite`View`escape[f["name"]],
      "email"        -> PersonalSite`View`escape[f["email"]],
      "message"      -> PersonalSite`View`escape[f["message"]],
      "date"         -> PersonalSite`View`escape[f["date"]],
      "time"         -> PersonalSite`View`escape[f["time"]],
      "selMessage"   -> If[kind === "meeting", "", "selected"],
      "selMeeting"   -> If[kind === "meeting", "selected", ""],
      "meetingClass" -> If[kind === "meeting", "", " is-hidden"],
      "status"       -> status
    |>]
  ];

contactSubmit[request_] :=
  Module[{fields, name, email, message, honeypot, kind, date, time, state, error, payload, result},
    fields   = contactFields[request];
    name     = StringTrim @ Lookup[fields, "name", ""];
    email    = StringTrim @ Lookup[fields, "email", ""];
    message  = StringTrim @ Lookup[fields, "message", ""];
    honeypot = StringTrim @ Lookup[fields, "website", ""];
    kind     = contactKind @ Lookup[fields, "kind", "message"];
    date     = StringTrim @ Lookup[fields, "date", ""];
    time     = StringTrim @ Lookup[fields, "time", ""];

    (* Estado a preservar si hay error de validacion o de envio. *)
    state = <|"name" -> name, "email" -> email, "message" -> message,
              "kind" -> kind, "date" -> date, "time" -> time|>;

    (* Bot detectado: el honeypot debe quedar vacio. Fingimos exito sin enviar. *)
    If[honeypot =!= "",
      Return @ contactForm[<||>,
        contactStatusBox[True, "\[Checkmark] Gracias, tu mensaje fue recibido."]]];

    error = contactValidate[name, email, message, kind, date];
    If[error =!= "",
      Return @ contactForm[state, contactStatusBox[False, error]]];

    payload = <|"name" -> name, "email" -> email, "message" -> message|>;
    If[kind === "meeting",
      payload = Join[payload, <|"kind" -> "meeting", "date" -> date, "time" -> time|>]];

    result = PersonalSite`Mailer`send[payload];

    If[TrueQ[result["ok"]],
      contactForm[<||>,
        contactStatusBox[True, contactSuccessMessage[kind, name, date, time]]],
      contactForm[state,
        contactStatusBox[False, Lookup[result, "error", "No se pudo enviar el mensaje."]]]
    ]
  ];

(* Mensaje de exito segun el tipo de consulta. *)
contactSuccessMessage["meeting", name_String, date_String, time_String] :=
  StringJoin["Gracias ", name, ", tu solicitud de reunion para el ", date,
    If[time === "", "", " a las " <> time],
    " fue enviada. Te confirmare la disponibilidad a la brevedad."];

contactSuccessMessage[_, name_String, _String, _String] :=
  "Gracias " <> name <> ", tu mensaje fue enviado. Te respondere a la brevedad.";

contactValidate[name_String, email_String, message_String, kind_String, date_String] :=
  Which[
    StringLength[name] < 2,
      "Por favor ingresa tu nombre.",
    !StringMatchQ[email, RegularExpression["[^@\\s]+@[^@\\s]+\\.[^@\\s]+"]],
      "Por favor ingresa un email valido.",
    kind === "meeting" && !validMeetingDate[date],
      "Por favor elegi una fecha valida para la reunion.",
    kind === "meeting" && pastMeetingDate[date],
      "La fecha de la reunion debe ser hoy o posterior.",
    kind =!= "meeting" && StringLength[message] < 10,
      "El mensaje debe tener al menos 10 caracteres.",
    True, ""
  ];

(* La fecha llega como "YYYY-MM-DD" desde <input type="date">. *)
validMeetingDate[date_String] :=
  StringMatchQ[date, RegularExpression["\\d{4}-\\d{2}-\\d{2}"]] &&
    Quiet @ DateObjectQ @ DateObject[date];
validMeetingDate[_] := False;

pastMeetingDate[date_String] :=
  Quiet @ TrueQ[AbsoluteTime @ DateObject[date] < AbsoluteTime @ Today];
pastMeetingDate[_] := False;

contactStatusBox[ok : True, msg_String] :=
  "<div class=\"ask-box ask-success\">" <>
    "<span class=\"ask-label\">Mensaje enviado</span>" <>
    "<p>" <> PersonalSite`View`escape[msg] <> "</p></div>";

contactStatusBox[ok : False, msg_String] :=
  "<div class=\"ask-box ask-fail\">" <>
    "<span class=\"ask-label\">No se pudo enviar</span>" <>
    "<p>" <> PersonalSite`View`escape[msg] <> "</p></div>";

(* Extrae los campos del formulario POST de forma robusta. wolframwebengine
   entrega los campos como MultipartElements: {name -> <|"ContentString"->v|>}. *)
contactFields[request_] :=
  Module[{multipart, form},
    multipart = Quiet @ HTTPRequestData["MultipartElements"];
    If[!ListQ[multipart],
      multipart = Quiet @ Lookup[request, "MultipartElements", Missing[]]];
    If[ListQ[multipart],
      Return @ Association[
        Cases[multipart, (k_ -> v_) :> (ToString[k] -> contactValue[v])]]];
    form = Quiet @ Lookup[request, "FormRules", Missing[]];
    If[ListQ[form], Return @ Association[form]];
    <||>
  ];

contactValue[v_Association] := Lookup[v, "ContentString", ""];
contactValue[v_String]      := v;
contactValue[_]             := "";

End[];
EndPackage[];
