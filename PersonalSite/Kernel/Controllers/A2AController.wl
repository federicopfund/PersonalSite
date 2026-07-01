(* ::Package:: *)

(* PersonalSite`Controller`  (parte: a2a)
   --------------------------------------------------------------------------
   Endpoints HTTP del protocolo A2A (Agent2Agent) sobre la malla de agentes de
   la Ruliad (PersonalSite`AgentMesh`).

     GET  /.well-known/agent-card.json  -> Agent Card A2A (discovery)
     GET  /.well-known/agent.json       -> alias legacy de la Agent Card
     POST /a2a                          -> endpoint JSON-RPC 2.0 (A2A transport)
     GET  /a2a                          -> UI interactiva de la malla de agentes
     GET  /a2a/agents                   -> lista de agentes (JSON)
     GET  /a2a/graph                    -> ultimo grafo de comunicacion (JSON)
     GET  /a2a/run                      -> corre la malla (query: seed, depth, backend)

   El endpoint JSON-RPC implementa el contrato A2A real (message/send,
   tasks/get, tasks/cancel). El endpoint /a2a/run usa query params para que la
   UI del navegador funcione sin depender del parseo de cuerpos JSON de WWE.
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`Controller`"];

a2a::usage       = "a2a[req] renderiza la UI de la malla de agentes A2A.";
a2aRpc::usage    = "a2aRpc[req] atiende el endpoint JSON-RPC 2.0 del protocolo A2A.";
a2aCard::usage   = "a2aCard[req] devuelve la Agent Card A2A (/.well-known).";
a2aAgents::usage = "a2aAgents[req] devuelve la lista de agentes como JSON.";
a2aGraph::usage  = "a2aGraph[req] devuelve el ultimo grafo de comunicacion como JSON.";
a2aRun::usage    = "a2aRun[req] corre la malla sobre la Ruliad y devuelve el grafo (JSON).";

Begin["`Private`"];

esc[x_] := PersonalSite`View`escape[ToString[x]];

(* ── Helpers de request ─────────────────────────────────────────────────── *)
lowerHeaders[req_] :=
  Association @ Map[
    Function[rule, ToLowerCase[ToString[First[rule]]] -> Last[rule]],
    If[ListQ[req["Headers"]], req["Headers"], {}]];

query[req_] :=
  If[ListQ[req["Query"]], Association[req["Query"]], <||>];

(* URL base publica desde los headers (respeta proxies TLS de produccion). *)
baseUrl[req_] :=
  Module[{h = lowerHeaders[req], host, proto},
    host  = Lookup[h, "host", ""];
    proto = Lookup[h, "x-forwarded-proto", "http"];
    If[! StringQ[host] || host === "",
      "http://localhost:8080",
      ToString[proto] <> "://" <> ToString[host]]
  ];

(* Cuerpo crudo del POST. En Wolfram Web Engine el objeto que devuelve
   HTTPRequestData[] (sin argumento) trae metadata (method/headers/query/
   formrules) pero NO el cuerpo: el body se obtiene con una llamada DIRECTA
   HTTPRequestData["BodyByteArray"] / ["Body"] durante la evaluacion del
   handler. Se intentan, en orden: fetch directo, el objeto recibido y, por
   ultimo, FormRules (JSON crudo como primera clave). *)
rawBody[req_] :=
  Module[{ba, body, fr, k, s},
    (* 1. Fetch directo (via idiomatica de WWE para el cuerpo). *)
    ba = Quiet @ HTTPRequestData["BodyByteArray"];
    If[ByteArrayQ[ba] && Length[ba] > 0,
      s = Quiet @ ByteArrayToString[ba, "UTF-8"];
      If[StringQ[s] && StringTrim[s] =!= "", Return[s]]];
    body = Quiet @ HTTPRequestData["Body"];
    If[StringQ[body] && StringTrim[body] =!= "", Return[body]];
    (* 2. Desde el objeto recibido (por si el runtime si lo adjunta). *)
    ba = Quiet @ req["BodyByteArray"];
    If[ByteArrayQ[ba] && Length[ba] > 0,
      s = Quiet @ ByteArrayToString[ba, "UTF-8"];
      If[StringQ[s] && StringTrim[s] =!= "", Return[s]]];
    body = Quiet @ req["Body"];
    If[StringQ[body] && StringTrim[body] =!= "", Return[body]];
    (* 3. FormRules: JSON crudo puede llegar como la primera clave. *)
    fr = Quiet @ req["FormRules"];
    If[ListQ[fr] && fr =!= {},
      k = First[Keys[Association[fr]]];
      If[StringQ[k] && StringStartsQ[StringTrim[k], "{"], Return[k]]];
    ""
  ];

jsonResponse[data_, status_ : 200] :=
  HTTPResponse[
    Developer`WriteRawJSONString[data],
    <|"StatusCode" -> status,
      "Headers" -> <|
        "Content-Type"                -> "application/json; charset=utf-8",
        "Access-Control-Allow-Origin" -> "*",
        "Cache-Control"               -> "no-store"|>|>];

(* ── GET /.well-known/agent-card.json ──────────────────────────────────── *)
a2aCard[req_] :=
  jsonResponse[PersonalSite`AgentMesh`agentCard[baseUrl[req]], 200];

(* ── POST /a2a  (JSON-RPC 2.0) ─────────────────────────────────────────── *)
a2aRpc[req_] :=
  Module[{raw, body, resp},
    raw  = rawBody[req];
    body = Quiet @ Check[ImportString[raw, "RawJSON"], $Failed];
    If[! AssociationQ[body],
      Return @ jsonResponse[
        PersonalSite`A2A`rpcError[Null,
          PersonalSite`A2A`$errorCodes["ParseError"],
          "Cuerpo JSON-RPC invalido o ausente."],
        400]];
    resp = PersonalSite`A2A`dispatch[body, PersonalSite`AgentMesh`handlers[]];
    jsonResponse[resp, 200]
  ];

(* ── GET /a2a/agents ───────────────────────────────────────────────────── *)
a2aAgents[req_] :=
  jsonResponse[<|
    "protocolVersion" -> PersonalSite`A2A`$protocolVersion,
    "agents"          -> PersonalSite`AgentMesh`agents[],
    "skills"          -> PersonalSite`AgentMesh`skills[]|>, 200];

(* ── GET /a2a/graph ────────────────────────────────────────────────────── *)
a2aGraph[req_] :=
  jsonResponse[PersonalSite`AgentMesh`graph[], 200];

(* ── GET /a2a/run?seed=1&depth=3&backend=session ───────────────────────── *)
a2aRun[req_] :=
  Module[{q = query[req], seed, depth, backend, res},
    seed    = Lookup[q, "seed", "1"];
    depth   = With[{d = Quiet @ ToExpression @ ToString @ Lookup[q, "depth", "3"]},
                If[IntegerQ[d], d, 3]];
    backend = With[{b = ToString @ Lookup[q, "backend", "session"]},
                If[MemberQ[{"sync", "session", "parallel"}, b], b, "session"]];
    res = Quiet @ Check[
      PersonalSite`AgentMesh`run[seed, depth, backend], $Failed];
    If[! AssociationQ[res] || ! KeyExistsQ[res, "graph"],
      Return @ jsonResponse[<|"ok" -> False, "error" -> "run failed"|>, 500]];
    jsonResponse[<|
      "ok"     -> True,
      "taskId" -> res["task"]["id"],
      "state"  -> res["task"]["status"]["state"],
      "stats"  -> res["stats"],
      "graph"  -> res["graph"]|>, 200]
  ];

(* ── GET /a2a  (UI) ────────────────────────────────────────────────────── *)
agentCardHtml[] :=
  StringRiffle[
    Map[
      Function[a,
        "<div class=\"a2a-agent a2a-agent--" <> esc[a["kind"]] <> "\">" <>
          "<div class=\"a2a-agent__head\">" <>
            "<span class=\"a2a-agent__dot\" data-rule=\"" <> esc[a["ruleIdx"]] <> "\"></span>" <>
            "<span class=\"a2a-agent__name\">" <> esc[a["name"]] <> "</span>" <>
            "<code class=\"a2a-agent__id\">" <> esc[a["id"]] <> "</code>" <>
          "</div>" <>
          "<p class=\"a2a-agent__desc\">" <> esc[a["description"]] <> "</p>" <>
          "<div class=\"a2a-agent__meta\">" <>
            "<span class=\"a2a-tag\">" <> esc[a["kind"]] <> "</span>" <>
            "<code class=\"a2a-agent__expr\">" <> esc[a["expr"]] <> "</code>" <>
          "</div>" <>
        "</div>"],
      PersonalSite`AgentMesh`agents[]],
    "\n"];

a2a[req_] :=
  Module[{base = baseUrl[req]},
    PersonalSite`View`render["a2a", <|
      "protocolVersion" -> PersonalSite`A2A`$protocolVersion,
      "agentName"       -> esc[PersonalSite`AgentMesh`$agentName],
      "agentVersion"    -> esc[PersonalSite`AgentMesh`$agentVersion],
      "agentDesc"       -> esc[PersonalSite`AgentMesh`$agentDescription],
      "agentCount"      -> ToString[Length[PersonalSite`AgentMesh`agents[]]],
      "ruleCount"       -> ToString[Length[PersonalSite`AgentMesh`rules[]]],
      "cardUrl"         -> esc[base <> "/.well-known/agent-card.json"],
      "rpcUrl"          -> esc[base <> "/a2a"],
      "agentsHtml"      -> agentCardHtml[]
    |>]
  ];

End[];
EndPackage[];
