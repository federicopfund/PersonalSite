(* ::Package:: *)

(* PersonalSite`AgentMesh`
   --------------------------------------------------------------------------
   Malla de agentes A2A construida sobre la Ruliad generada por NestList.

   Idea central
   ------------
   NestGraph[{2#+1, #+14, #-18}&, {seed}, depth] genera la evolucion multiway
   (la "Ruliad" acotada) de aplicar un conjunto de reglas a una semilla. Aca
   cada REGLA se convierte en un AGENTE A2A con una unica skill: "aplicar mi
   transformacion a un valor". Un agente ORQUESTADOR recibe el mensaje inicial
   y reparte el trabajo entre los agentes-regla.

   El grafo de comunicacion resultante es isomorfo al NestGraph: cada arista
   del arbol es un salto de mensaje A2A (agente padre -> agente-regla hijo).
   Esa red entrelazada de mensajes ES la "comunicacion de la Ruliad".

   La ejecucion real se delega a PersonalSite`NestScheduler` (que a su vez usa
   PersonalSite`Flow` para correr cada nivel como TaskObjects en paralelo), de
   modo que los agentes de verdad computan sus resultados. El envelope de
   protocolo (Message -> Task -> Artifact) lo aporta PersonalSite`A2A`.

   Integracion del stack de funciones
   -----------------------------------
   Cada rama raiz->hoja del arbol es un "stack de funciones": la composicion
   ordenada de las reglas aplicadas. run[] devuelve, para cada hoja, ese stack
   (indices de regla + etiquetas + expresion compuesta) — la evaluacion del
   grafo y la integracion del stack pedidas.
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`AgentMesh`"];

$agentName::usage        = "$agentName es el nombre publico de la malla de agentes A2A.";
$agentVersion::usage     = "$agentVersion es la version del agente A2A.";
$agentDescription::usage = "$agentDescription describe la malla de agentes.";

rules::usage =
  "rules[] devuelve las reglas por defecto de la Ruliad (una por agente-regla).";

agents::usage =
  "agents[] devuelve los descriptores JSON-safe de los agentes (orquestador + \
un agente por regla).";

skills::usage =
  "skills[] devuelve las AgentSkills A2A derivadas de los agentes.";

agentCard::usage =
  "agentCard[baseUrl] devuelve la Agent Card A2A (documento /.well-known) para \
la URL base dada.";

run::usage =
  "run[seed, depth] o run[seed, depth, backend] corre la malla sobre la Ruliad: \
construye el NestGraph, lo ejecuta como agentes A2A y devuelve la Task, el \
grafo de comunicacion, los registros y las estadisticas.";

messageSend::usage =
  "messageSend[params, id] es el handler A2A message/send: extrae seed/depth del \
Message, corre la malla y devuelve una respuesta JSON-RPC con la Task.";

handlers::usage =
  "handlers[] devuelve la Association metodo -> handler JSON-RPC para A2A.";

graph::usage =
  "graph[] devuelve el ultimo grafo de comunicacion de la Ruliad.";

graphJson::usage =
  "graphJson[] serializa el ultimo grafo de comunicacion a JSON.";

lastRun::usage =
  "lastRun[] devuelve el resultado del ultimo run[] (o <||> si no hubo).";

Begin["`Private`"];

$agentName    = "Ruliad Nest Agent Mesh";
$agentVersion = "1.0.0";
$agentDescription =
  "Malla de agentes A2A que ejecuta la Ruliad generada por NestList: cada regla \
de transformacion es un agente con una skill, y las aristas del NestGraph son \
saltos de mensaje A2A entre agentes.";

(* ── Reglas por defecto (las del grafo NestGraph[{2#+1,#+14,#-18}&,{1},3]) ──
   Cada regla: <|"idx","label","coeffs"->{a,b,c},"fn"|> con fn = a x^2 + b x + c.
   fn NO se serializa a JSON — vive solo del lado servidor.                    *)
$rules = {
  <|"idx" -> 1, "label" -> "2x + 1",  "coeffs" -> {0, 2, 1},   "fn" -> (2 # + 1 &)|>,
  <|"idx" -> 2, "label" -> "x + 14",  "coeffs" -> {0, 1, 14},  "fn" -> (# + 14 &)|>,
  <|"idx" -> 3, "label" -> "x - 18",  "coeffs" -> {0, 1, -18}, "fn" -> (# - 18 &)|>
};

rules[] := $rules;

ruleFns[] := Lookup[$rules, "fn"];

agentId[idx_Integer] := "agent-rule-" <> ToString[idx];
$orchestratorId = "ruliad-orchestrator";

(* ── Descriptores de agentes (JSON-safe: sin la Function "fn") ──────────── *)
ruleAgent[r_Association] :=
  <|"id"          -> agentId[r["idx"]],
    "name"        -> "Rule Agent " <> ToString[r["idx"]] <> " (" <> r["label"] <> ")",
    "kind"        -> "rule",
    "ruleIdx"     -> r["idx"],
    "expr"        -> r["label"],
    "coeffs"      -> r["coeffs"],
    "description" -> "Aplica la transformacion f(x) = " <> r["label"] <>
                     " a un valor entrante y devuelve el resultado como Artifact.",
    "tags"        -> {"ruliad", "nest", "transform"}|>;

agents[] :=
  Prepend[
    ruleAgent /@ $rules,
    <|"id"          -> $orchestratorId,
      "name"        -> "Ruliad Orchestrator",
      "kind"        -> "orchestrator",
      "ruleIdx"     -> 0,
      "expr"        -> "NestGraph",
      "coeffs"      -> {},
      "description" -> "Recibe el mensaje inicial (seed, depth), expande la " <>
                       "Ruliad y reparte cada valor a los agentes-regla nivel a nivel.",
      "tags"        -> {"ruliad", "orchestrator", "router"}|>
  ];

(* ── AgentSkills A2A (para la Agent Card) ──────────────────────────────── *)
skills[] :=
  Append[
    Map[
      Function[r,
        <|"id"          -> "apply-rule-" <> ToString[r["idx"]],
          "name"        -> "Apply rule " <> r["label"],
          "description" -> "Aplica f(x) = " <> r["label"] <> " a un valor numerico.",
          "tags"        -> {"transform", "ruliad", "nest"},
          "examples"    -> {"Aplicar " <> r["label"] <> " a 1"},
          "inputModes"  -> {"application/json", "text/plain"},
          "outputModes" -> {"application/json", "text/plain"}|>],
      $rules],
    <|"id"          -> "expand-ruliad",
      "name"        -> "Expand Ruliad (NestGraph)",
      "description" -> "Expande la Ruliad completa a partir de una semilla y " <>
                       "profundidad, ejecutando todos los agentes-regla y " <>
                       "devolviendo la trayectoria y el grafo de comunicacion.",
      "tags"        -> {"ruliad", "nestgraph", "orchestration"},
      "examples"    -> {"{\"seed\": 1, \"depth\": 3}",
                        "Expandir la Ruliad desde 1 con profundidad 3"},
      "inputModes"  -> {"application/json", "text/plain"},
      "outputModes" -> {"application/json"}|>
  ];

(* ── Agent Card A2A (documento /.well-known/agent-card.json) ────────────── *)
agentCard[baseUrl_String] :=
  <|"protocolVersion"    -> PersonalSite`A2A`$protocolVersion,
    "name"               -> $agentName,
    "description"        -> $agentDescription,
    "url"                -> baseUrl <> "/a2a",
    "preferredTransport" -> "JSONRPC",
    "version"            -> $agentVersion,
    "provider"           -> <|"organization" -> "PersonalSite",
                              "url" -> baseUrl|>,
    "capabilities"       -> <|"streaming" -> False,
                              "pushNotifications" -> False,
                              "stateTransitionHistory" -> True|>,
    "defaultInputModes"  -> {"application/json", "text/plain"},
    "defaultOutputModes" -> {"application/json", "text/plain"},
    "skills"             -> skills[],
    "securitySchemes"    -> <||>,
    "security"           -> {},
    "additionalInterfaces" -> {<|"transport" -> "JSONRPC", "url" -> baseUrl <> "/a2a"|>}
  |>;

(* ── Normalizacion de config ───────────────────────────────────────────── *)
toSeeds[s_Integer] := {s};
toSeeds[s_Real]    := {s};
toSeeds[s_List]    := With[{ns = Select[s, NumericQ]}, If[ns === {}, {1}, ns]];
toSeeds[s_String]  :=
  With[{ns = Select[ToExpression /@ StringSplit[s, ","], NumericQ]},
    If[ns === {}, {1}, ns]];
toSeeds[_]         := {1};

clampDepth[d_] :=
  With[{n = Round[d]}, Which[! IntegerQ[n], 3, n < 1, 1, n > 5, 5, True, n]];

(* ── Nucleo: correr la malla sobre la Ruliad ───────────────────────────── *)
$last = <||>;

lastRun[] := $last;

run[seed_, depth_] := run[seed, depth, "session"];

run[seed_, depth_, backend_String] :=
  Module[{seeds, d, nres, records, flowRes, ctxId, task, taskId,
          nodes, edges, stacks, hist, summary, t0 = AbsoluteTime[]},

    seeds = toSeeds[seed];
    d     = clampDepth[depth];

    (* 1. Ejecutar la Ruliad via NestScheduler (Flow -> TaskObjects). *)
    nres    = PersonalSite`NestScheduler`run[ruleFns[], seeds, d, backend];
    records = nres["built"]["records"];
    flowRes = Lookup[nres["flow"], "results", <||>];

    (* Valor computado de un nodo (con respaldo al valor del arbol). *)
    nodeValue[rec_] := Lookup[flowRes, "n" <> ToString[rec["id"]], rec["value"]];

    (* 2. Crear la Task A2A y llevarla submitted -> working. *)
    ctxId = CreateUUID[];
    task  = PersonalSite`A2A`newTask[ctxId];
    taskId = task["id"];
    PersonalSite`A2A`setState[taskId, "working",
      PersonalSite`A2A`message["user",
        {PersonalSite`A2A`dataPart[<|"seed" -> seeds, "depth" -> d,
           "rules" -> Lookup[$rules, "label"], "backend" -> backend|>]},
        <|"taskId" -> taskId, "contextId" -> ctxId|>]];

    (* 3. Nodos del grafo de comunicacion (cada nodo = un valor producido). *)
    nodes = Map[
      Function[rec,
        <|"id"     -> "n" <> ToString[rec["id"]],
          "level"  -> rec["level"],
          "value"  -> nodeValue[rec],
          "parent" -> If[rec["parent"] === None, Null,
                         "n" <> ToString[rec["parent"]]],
          "ruleIdx" -> rec["ruleIdx"],
          "agent"  -> If[rec["level"] === 0, $orchestratorId,
                         agentId[rec["ruleIdx"]]]|>],
      records];

    (* 4. Aristas = mensajes A2A (agente padre -> agente-regla hijo). *)
    edges = Map[
      Function[rec,
        <|"from"      -> "n" <> ToString[rec["parent"]],
          "to"        -> "n" <> ToString[rec["id"]],
          "agent"     -> agentId[rec["ruleIdx"]],
          "ruleIdx"   -> rec["ruleIdx"],
          "messageId" -> CreateUUID[],
          "kind"      -> "a2a.message"|>],
      Select[records, #["parent"] =!= None &]];

    (* 5. Stacks de funciones: para cada hoja, la composicion raiz->hoja. *)
    stacks = functionStacks[records, flowRes];

    (* 6. Historial A2A: un Message por salto (acotado para el payload). *)
    hist = Take[
      Map[edgeMessage[#, records, flowRes] &,
        Select[records, #["parent"] =!= None &]],
      UpTo[60]];
    Scan[PersonalSite`A2A`pushHistory[taskId, #] &, hist];

    (* 7. Artifacts: trayectoria completa + resumen + stacks de funciones. *)
    PersonalSite`A2A`addArtifact[taskId,
      PersonalSite`A2A`artifact["ruliad-trajectory",
        {PersonalSite`A2A`dataPart[<|"nodes" -> nodes, "edges" -> edges|>]},
        <|"nodeCount" -> Length[nodes], "edgeCount" -> Length[edges]|>]];

    PersonalSite`A2A`addArtifact[taskId,
      PersonalSite`A2A`artifact["function-stacks",
        {PersonalSite`A2A`dataPart[<|"stacks" -> stacks|>]},
        <|"leafCount" -> Length[stacks]|>]];

    summary = summaryText[seeds, d, records, edges, nres];
    PersonalSite`A2A`addArtifact[taskId,
      PersonalSite`A2A`artifact["summary",
        {PersonalSite`A2A`textPart[summary]}]];

    (* 8. Cerrar la Task. *)
    PersonalSite`A2A`setState[taskId, "completed"];

    $last = <|
      "task"     -> PersonalSite`A2A`getTask[taskId],
      "graph"    -> <|"agents" -> agents[], "nodes" -> nodes,
                      "edges"  -> edges,     "stacks" -> stacks|>,
      "records"  -> records,
      "stats"    -> <|
        "agents"    -> Length[agents[]],
        "nodes"     -> Length[nodes],
        "messages"  -> Length[edges],
        "artifacts" -> Length[PersonalSite`A2A`getTask[taskId]["artifacts"]],
        "leaves"    -> Length[stacks],
        "depth"     -> d,
        "seeds"     -> seeds,
        "backend"   -> Lookup[nres, "backend", backend],
        "elapsed"   -> AbsoluteTime[] - t0|>
    |>;
    $last
  ];

(* Composicion raiz->hoja de cada nodo hoja: el "stack de funciones". *)
functionStacks[records_List, flowRes_Association] :=
  Module[{byId, childrenOf, leaves, pathTo},
    byId = Association[(#["id"] -> #) & /@ records];
    childrenOf = GroupBy[Select[records, #["parent"] =!= None &], #["parent"] &];
    leaves = Select[records, ! KeyExistsQ[childrenOf, #["id"]] &];

    pathTo[rec_] :=
      NestWhileList[byId[#["parent"]] &, rec, #["parent"] =!= None &];

    Map[
      Function[leaf,
        Module[{chain = Reverse[pathTo[leaf]]},
          <|"leafId" -> "n" <> ToString[leaf["id"]],
            "value"  -> Lookup[flowRes, "n" <> ToString[leaf["id"]], leaf["value"]],
            "stack"  -> Map[
              Function[nd,
                <|"level"   -> nd["level"],
                  "node"    -> "n" <> ToString[nd["id"]],
                  "ruleIdx" -> nd["ruleIdx"],
                  "agent"   -> If[nd["level"] === 0, $orchestratorId,
                                  agentId[nd["ruleIdx"]]],
                  "label"   -> If[nd["level"] === 0, "seed",
                                  ruleLabel[nd["ruleIdx"]]]|>],
              chain],
            "composition" -> compositionText[chain]|>]],
      leaves]
  ];

ruleLabel[idx_Integer] :=
  With[{r = SelectFirst[$rules, #["idx"] === idx &]},
    If[AssociationQ[r], r["label"], "?"]];

compositionText[chain_List] :=
  Module[{fns = Rest[chain]},
    If[fns === {},
      "seed",
      "(" <> StringRiffle[
        Map[ruleLabel[#["ruleIdx"]] &, fns], " ) -> ( "] <> " )"]];

(* Message A2A que representa un salto de arista (agente-regla aplica su regla). *)
edgeMessage[rec_Association, records_List, flowRes_Association] :=
  Module[{parentVal, val},
    parentVal = Lookup[flowRes,
      "n" <> ToString[rec["parent"]],
      With[{p = SelectFirst[records, #["id"] === rec["parent"] &]},
        If[AssociationQ[p], p["value"], Null]]];
    val = Lookup[flowRes, "n" <> ToString[rec["id"]], rec["value"]];
    PersonalSite`A2A`message["agent",
      {PersonalSite`A2A`dataPart[<|
        "from"    -> If[rec["level"] === 1, $orchestratorId,
                        "n" <> ToString[rec["parent"]]],
        "agent"   -> agentId[rec["ruleIdx"]],
        "rule"    -> ruleLabel[rec["ruleIdx"]],
        "input"   -> parentVal,
        "output"  -> val,
        "node"    -> "n" <> ToString[rec["id"]],
        "level"   -> rec["level"]|>]}]
  ];

summaryText[seeds_List, d_Integer, records_List, edges_List, nres_Association] :=
  StringJoin[
    "Ruliad expandida desde seed=", ToString[seeds],
    " profundidad=", ToString[d], ". ",
    ToString[Length[records]], " nodos, ",
    ToString[Length[edges]], " mensajes A2A entre agentes, ",
    ToString[Length[$rules]], " agentes-regla + orquestador. ",
    "Backend de ejecucion: ", ToString[Lookup[nres, "backend", "session"]], ". ",
    "Tiempo: ", ToString[Round[1000. Lookup[nres, "elapsed", 0.]]], " ms."];

(* ── Handler A2A message/send ──────────────────────────────────────────── *)
extractConfig[params_Association] :=
  Module[{msg, parts, seed = 1, depth = 3, backend = "session"},
    msg = Lookup[params, "message", <||>];
    parts = Lookup[msg, "parts", {}];
    If[ListQ[parts],
      Scan[
        Function[p,
          Which[
            Lookup[p, "kind", ""] === "data",
              With[{data = Lookup[p, "data", <||>]},
                If[AssociationQ[data],
                  If[KeyExistsQ[data, "seed"],    seed    = data["seed"]];
                  If[KeyExistsQ[data, "depth"],   depth   = data["depth"]];
                  If[KeyExistsQ[data, "backend"], backend = data["backend"]]]],
            Lookup[p, "kind", ""] === "text",
              With[{parsed = parseTextConfig[Lookup[p, "text", ""]]},
                If[KeyExistsQ[parsed, "seed"],  seed  = parsed["seed"]];
                If[KeyExistsQ[parsed, "depth"], depth = parsed["depth"]]]
          ]],
        parts]];
    <|"seed" -> seed, "depth" -> depth,
      "backend" -> If[MemberQ[{"sync", "session", "parallel"}, backend],
                      backend, "session"]|>
  ];

(* Extrae "seed=.. depth=.." o numeros sueltos de un texto libre. *)
parseTextConfig[text_String] :=
  Module[{out = <||>, seedM, depthM},
    seedM  = StringCases[text, "seed" ~~ Whitespace... ~~ "=" ~~ Whitespace... ~~
               n : NumberString :> n];
    depthM = StringCases[text, "depth" ~~ Whitespace... ~~ "=" ~~ Whitespace... ~~
               n : NumberString :> n];
    If[seedM =!= {},  out["seed"]  = ToExpression[First[seedM]]];
    If[depthM =!= {}, out["depth"] = ToExpression[First[depthM]]];
    out
  ];
parseTextConfig[_] := <||>;

messageSend[params_Association, id_] :=
  Module[{cfg, res},
    cfg = extractConfig[params];
    res = Quiet @ Check[
      run[cfg["seed"], cfg["depth"], cfg["backend"]],
      $Failed];
    If[! AssociationQ[res] || ! KeyExistsQ[res, "task"],
      Return[PersonalSite`A2A`rpcError[id,
        PersonalSite`A2A`$errorCodes["InternalError"],
        "La expansion de la Ruliad fallo."]]];
    PersonalSite`A2A`rpcSuccess[id, res["task"]]
  ];

(* ── Handlers JSON-RPC A2A ──────────────────────────────────────────────── *)
tasksGet[params_Association, id_] :=
  Module[{taskId = Lookup[params, "id", Lookup[params, "taskId", ""]], task},
    If[! StringQ[taskId] || taskId === "",
      Return[PersonalSite`A2A`rpcError[id,
        PersonalSite`A2A`$errorCodes["InvalidParams"], "Falta 'id' de la task."]]];
    task = PersonalSite`A2A`getTask[taskId];
    If[task === $Failed,
      PersonalSite`A2A`rpcError[id,
        PersonalSite`A2A`$errorCodes["TaskNotFound"], "Task no encontrada."],
      PersonalSite`A2A`rpcSuccess[id, task]]
  ];

tasksCancel[params_Association, id_] :=
  Module[{taskId = Lookup[params, "id", Lookup[params, "taskId", ""]], task},
    If[! StringQ[taskId] || taskId === "",
      Return[PersonalSite`A2A`rpcError[id,
        PersonalSite`A2A`$errorCodes["InvalidParams"], "Falta 'id' de la task."]]];
    task = PersonalSite`A2A`getTask[taskId];
    If[task === $Failed,
      Return[PersonalSite`A2A`rpcError[id,
        PersonalSite`A2A`$errorCodes["TaskNotFound"], "Task no encontrada."]]];
    task = PersonalSite`A2A`cancelTask[taskId];
    If[task === $Failed,
      PersonalSite`A2A`rpcError[id,
        PersonalSite`A2A`$errorCodes["TaskNotCancelable"],
        "La task esta en un estado terminal y no puede cancelarse."],
      PersonalSite`A2A`rpcSuccess[id, task]]
  ];

streamUnsupported[params_Association, id_] :=
  PersonalSite`A2A`rpcError[id,
    PersonalSite`A2A`$errorCodes["UnsupportedOperation"],
    "El streaming (message/stream) no esta soportado por este agente."];

handlers[] := <|
  "message/send"   -> messageSend,
  "message/stream" -> streamUnsupported,
  "tasks/get"      -> tasksGet,
  "tasks/cancel"   -> tasksCancel
|>;

graph[] := Lookup[$last, "graph", <|"agents" -> agents[], "nodes" -> {}, "edges" -> {}, "stacks" -> {}|>];

graphJson[] := Developer`WriteRawJSONString[graph[]];

End[];
EndPackage[];
