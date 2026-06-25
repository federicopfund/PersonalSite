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
    contactForm["", "", "", ""]
  ];

(* Metodo HTTP en mayusculas; por defecto GET. *)
contactMethod[request_] :=
  ToUpperCase @ Replace[
    Quiet @ Lookup[request, "Method", Quiet @ HTTPRequestData["Method"]],
    Except[_String] -> "GET"
  ];

(* Renderiza la vista; status es un bloque HTML opcional (caja de estado). *)
contactForm[name_String, email_String, message_String, status_String] :=
  PersonalSite`View`render["contact", <|
    "name"    -> PersonalSite`View`escape[name],
    "email"   -> PersonalSite`View`escape[email],
    "message" -> PersonalSite`View`escape[message],
    "status"  -> status
  |>];

contactSubmit[request_] :=
  Module[{fields, name, email, message, honeypot, error, result},
    fields   = contactFields[request];
    name     = StringTrim @ Lookup[fields, "name", ""];
    email    = StringTrim @ Lookup[fields, "email", ""];
    message  = StringTrim @ Lookup[fields, "message", ""];
    honeypot = StringTrim @ Lookup[fields, "website", ""];

    (* Bot detectado: el honeypot debe quedar vacio. Fingimos exito sin enviar. *)
    If[honeypot =!= "",
      Return @ contactForm["", "", "",
        contactStatusBox[True, "\[Checkmark] Gracias, tu mensaje fue recibido."]]];

    error = contactValidate[name, email, message];
    If[error =!= "",
      Return @ contactForm[name, email, message, contactStatusBox[False, error]]];

    result = PersonalSite`Mailer`send[<|
      "name" -> name, "email" -> email, "message" -> message|>];

    If[TrueQ[result["ok"]],
      contactForm["", "", "",
        contactStatusBox[True,
          "Gracias " <> name <> ", tu mensaje fue enviado. Te respondere a la brevedad."]],
      contactForm[name, email, message,
        contactStatusBox[False, Lookup[result, "error", "No se pudo enviar el mensaje."]]]
    ]
  ];

contactValidate[name_String, email_String, message_String] :=
  Which[
    StringLength[name] < 2,
      "Por favor ingresa tu nombre.",
    !StringMatchQ[email, RegularExpression["[^@\\s]+@[^@\\s]+\\.[^@\\s]+"]],
      "Por favor ingresa un email valido.",
    StringLength[message] < 10,
      "El mensaje debe tener al menos 10 caracteres.",
    True, ""
  ];

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
