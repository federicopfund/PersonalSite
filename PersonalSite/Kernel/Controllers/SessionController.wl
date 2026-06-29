(* ::Package:: *)

(* PersonalSite`Controller` (parte: session)
   --------------------------------------------------------------------------
   Endpoints HTTP para gestion de sesiones.

   Rutas:
     POST /session/create     — crea sesion; body: {userId, role?, meta?}
     POST /session/destroy    — cierra sesion (requiere token valido)
     GET  /session/validate   — valida token; devuelve sesion activa
     GET  /session/info       — info completa de la sesion actual
     POST /session/transition — aplica evento FSM: {event: "elevate"|...}
     GET  /session/graph      — arbol de permisos NestGraph (JSON)
     GET  /session/fsm        — grafo de transiciones FSM (JSON)
     GET  /session/stats      — metricas de sesiones activas

   Autenticacion:
     Authorization: Bearer <token>
     -o- Cookie: session=<token>

   Todos los endpoints de escritura esperan Content-Type: application/json.
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`Controller`"];

sessionCreate::usage     = "sessionCreate[req] POST /session/create";
sessionDestroy::usage    = "sessionDestroy[req] POST /session/destroy";
sessionValidate::usage   = "sessionValidate[req] GET /session/validate";
sessionInfo::usage       = "sessionInfo[req] GET /session/info";
sessionTransition::usage = "sessionTransition[req] POST /session/transition";
sessionGraph::usage      = "sessionGraph[req] GET /session/graph";
sessionFsmGraph::usage   = "sessionFsmGraph[req] GET /session/fsm";
sessionStats::usage      = "sessionStats[req] GET /session/stats";

Begin["`Private`"];

(* ── Helpers internos ───────────────────────────────────────────────── *)

jsonOk[data_] :=
  HTTPResponse[
    Developer`WriteRawJSONString[data],
    <|"StatusCode" -> 200,
      "Headers" -> <|"Content-Type" -> "application/json"|>|>];

jsonErr[code_Integer, msg_String] :=
  HTTPResponse[
    Developer`WriteRawJSONString[<|"error" -> msg|>],
    <|"StatusCode" -> code,
      "Headers" -> <|"Content-Type" -> "application/json"|>|>];

(* Intenta leer el body: primero como JSON raw, luego como form-encoded.
   El WolframEngine expone el body via req["Body"] (ByteArray o String) y
   los campos form-encoded via req["FormRules"] ({key->val,...}).           *)
parseJsonBody[req_] :=
  Module[{raw, parsed},
    (* Intentar body JSON *)
    raw = Quiet @ Check[
      With[{b = req["Body"]},
        Which[
          StringQ[b],    b,
          ByteArrayQ[b], ByteArrayToString[b, "UTF8"],
          True,          "{}"]],
      "{}"];
    parsed = Quiet @ Check[ImportString[raw, "RawJSON"], $Failed];
    If[AssociationQ[parsed], Return[parsed]];
    (* Fallback: form-encoded via FormRules *)
    With[{fr = Quiet @ Check[req["FormRules"], {}]},
      If[ListQ[fr] && Length[fr] > 0,
        Association[fr],
        <||>]]
  ];

extractToken[req_] :=
  Module[{auth, cookie, headers},
    headers = Quiet @ Check[req["Headers"], <||>];
    auth = Lookup[headers, "Authorization",
             Lookup[headers, "authorization", ""]];
    If[StringStartsQ[auth, "Bearer "],
      Return[StringDrop[auth, 7]]];
    cookie = Lookup[headers, "Cookie",
               Lookup[headers, "cookie", ""]];
    With[{m = StringCases[cookie,
                "session=" ~~ tok : Except[";"].. -> tok]},
      If[Length[m] > 0, First[m], ""]]
  ];

(* ── POST /session/create ───────────────────────────────────────────── *)
(*   Body JSON esperado:
       { "userId": "string",       (requerido)
         "role":   1|2|3,          (opcional, default 1)
         "meta":   { ... }         (opcional)
       }
   En produccion este endpoint deberia estar protegido por un pre-shared
   secret de servicio o por un mecanismo de autenticacion real (OAuth2,
   password check, etc). Aqui se implementa la capa de sesion pura.       *)

PersonalSite`Controller`sessionCreate[req_] :=
  Module[{body, userId, role, meta, result},
    body   = parseJsonBody[req];
    userId = Lookup[body, "userId",
               Lookup[body, "user_id",
                 StringTrim @ Lookup[req["FormRules"], "userId",
                   StringTrim @ Lookup[req["FormRules"], "user_id", ""]]]];
    If[userId === "",
      Return[jsonErr[400, "userId_required"]]];
    role = With[{r = Quiet @ ToExpression @
                       Lookup[body, "role",
                         Lookup[req["FormRules"], "role", 1]]},
             If[IntegerQ[r] && 1 <= r <= 3, r, 1]];
    meta = With[{m = Lookup[body, "meta", <||>]},
             If[AssociationQ[m], m, <||>]];
    result = Quiet @ Check[
               PersonalSite`SessionStore`createSession[userId, role, meta],
               $Failed];
    If[result === $Failed,
      jsonErr[500, "session_creation_failed"],
      jsonOk[<|"status" -> "created", result|>]]
  ];

(* ── POST /session/destroy ──────────────────────────────────────────── *)

PersonalSite`Controller`sessionDestroy[req_] :=
  Module[{token, session},
    token = extractToken[req];
    If[token === "", Return[jsonErr[401, "no_token"]]];
    (* verifyTokenIdentity permite destruir sesiones en cualquier estado *)
    session = Quiet @ Check[
                PersonalSite`SessionStore`verifyTokenIdentity[token], $Failed];
    If[session === $Failed,
      Return[jsonErr[401, "invalid_token"]]];
    PersonalSite`SessionStore`destroySession[session["sessionId"]];
    jsonOk[<|"status" -> "destroyed",
             "sessionId" -> session["sessionId"]|>]
  ];

(* ── GET /session/validate ──────────────────────────────────────────── *)

PersonalSite`Controller`sessionValidate[req_] :=
  Module[{token, session},
    token = extractToken[req];
    If[token === "", Return[jsonErr[401, "no_token"]]];
    session = Quiet @ Check[
                PersonalSite`SessionStore`validateToken[token], $Failed];
    If[session === $Failed,
      jsonErr[401, "invalid_or_expired"],
      jsonOk[<|"valid"     -> True,
               "sessionId" -> session["sessionId"],
               "userId"    -> session["userId"],
               "state"     -> session["state"],
               "role"      -> session["role"],
               "expiresAt" -> session["expiresAt"]|>]]
  ];

(* ── GET /session/info ───────────────────────────────────────────────── *)

PersonalSite`Controller`sessionInfo[req_] :=
  PersonalSite`SessionFSM`withSession[req, "public.read",
    Function[authedReq,
      jsonOk[authedReq["Session"]]]];

(* ── POST /session/transition ────────────────────────────────────────── *)
(*   Body: { "event": "elevate" | "downgrade" | "suspend" | ... }        *)

PersonalSite`Controller`sessionTransition[req_] :=
  Module[{token, session, body, event, result, reason},
    token = extractToken[req];
    If[token === "", Return[jsonErr[401, "no_token"]]];
    (* verifyTokenIdentity permite transiciones desde cualquier estado
       (incluye resume desde suspended, relogin desde expired).          *)
    session = Quiet @ Check[
                PersonalSite`SessionStore`verifyTokenIdentity[token], $Failed];
    If[session === $Failed,
      Return[jsonErr[401, "invalid_token"]]];
    body  = parseJsonBody[req];
    event = StringTrim @ Lookup[body, "event",
              Lookup[req["FormRules"], "event", ""]];
    If[event === "",
      Return[jsonErr[400, "event_required"]]];
    {result, reason} =
      PersonalSite`SessionStore`applyTransition[session["sessionId"], event];
    If[result === $Failed,
      jsonErr[422, "transition_failed:" <> reason],
      jsonOk[<|"status"   -> "ok",
               "event"    -> event,
               "state"    -> result["state"],
               "sessionId"-> result["sessionId"]|>]]
  ];

(* ── GET /session/graph ──────────────────────────────────────────────── *)
(*   Devuelve el arbol de permisos NestGraph para el role del token
     (o role 1 si no hay token). Util para visualizacion en el frontend.  *)

PersonalSite`Controller`sessionGraph[req_] :=
  Module[{token, role, tree},
    token = extractToken[req];
    role = If[token === "", 1,
      With[{s = Quiet @ Check[
                  PersonalSite`SessionStore`validateToken[token], $Failed]},
        If[s === $Failed, 1, Lookup[s, "role", 1]]]];
    tree = PersonalSite`SessionFSM`permissionTree[role];
    jsonOk[<|"role"      -> role,
             "depth"     -> 3,
             "nodeCount" -> Length[tree],
             "ruleCount" -> 3,
             "rules"     -> {"public.read (role>=1)",
                              "content.write (role>=2)",
                              "kernel.eval (role>=3)"},
             "tree"      -> tree|>]
  ];

(* ── GET /session/fsm ────────────────────────────────────────────────── *)

PersonalSite`Controller`sessionFsmGraph[req_] :=
  jsonOk[PersonalSite`SessionFSM`fsmGraph[]];

(* ── GET /session/stats ──────────────────────────────────────────────── *)

PersonalSite`Controller`sessionStats[req_] :=
  jsonOk[PersonalSite`SessionStore`sessionStats[]];

End[];
EndPackage[];
