(* ::Package:: *)

(* PersonalSite`SessionFSM`
   --------------------------------------------------------------------------
   Motor de estados de sesion modelado como NestGraph.

   El diagrama del UI muestra:
       NestGraph[{2#+1, #+14, #-18}&, {1}, 3, "IncludeStepNumber"->True]

   Esta arquitectura mapea eso directamente a la gestion de permisos:
     Seed      = contexto de autenticacion base  <| "role"->R, "perms"->{}|>
     Reglas    = 3 funciones de derivacion de permisos (una por capa)
     Profundidad = 3 (1 seed -> 3 L1 -> 9 L2 -> 27 L3 = 40 nodos)

   El arbol de permisos:
     L0: base auth context            (role cualquiera)
     L1: {public.read}                (rule 1, role>=1)
         {content.write, public.read} (rule 2, role>=2)
         {kernel.eval, admin.*}       (rule 3, role>=3)
     L2: intersecciones de L1 (capacidades compuestas)
     L3: permisos de dominio fino (blog.x, tasks.x, arch.x)

   FSM de estados de sesion (ciclo de vida):
     unauthenticated --login--> active
     active          --elevate--> elevated    (MFA / sudo)
     elevated        --downgrade--> active
     active          --suspend--> suspended
     suspended       --resume--> active
     active/elevated --logout/timeout--> expired
     expired         --login--> active        (re-autenticacion)

   Uso:
     PersonalSite`SessionFSM`derivePermissions[role]
       => lista de permisos transitiva para el role
     PersonalSite`SessionFSM`transition[session, "elevate"]
       => {newSession, "ok"} | {$Failed, reason}
     PersonalSite`SessionFSM`permissionTree[]
       => NestGraph completo (40 nodos) para visualizacion
     PersonalSite`SessionFSM`can[session, "kernel.eval"]
       => True / False
   -------------------------------------------------------------------------- *)

Begin["PersonalSite`SessionFSM`Private`"];

(* ── 3 Reglas de derivacion de permisos (mirrors de los 3 NestGraph rules) ── *)
(*   Cada regla es Function[ctx] -> ctx' con perms ampliados o igual.
     ctx = <| "role" -> Integer, "perms" -> List, "state" -> String |>      *)

$permRule1 = Function[ctx,
  (* Regla 1: 2#+1 style — todo role>=1 obtiene lectura publica *)
  If[ctx["role"] >= 1,
    <|ctx, "perms" -> Union[ctx["perms"],
      {"public.read", "blog.read", "arch.view"}]|>,
    ctx]];

$permRule2 = Function[ctx,
  (* Regla 2: #+14 style — role>=2 obtiene escritura de contenido *)
  If[ctx["role"] >= 2,
    <|ctx, "perms" -> Union[ctx["perms"],
      {"content.write", "blog.write", "tasks.view", "nest.run",
       "contact.send", "flow.run"}]|>,
    ctx]];

$permRule3 = Function[ctx,
  (* Regla 3: #-18 style — role>=3 obtiene acceso completo al kernel *)
  If[ctx["role"] >= 3,
    <|ctx, "perms" -> Union[ctx["perms"],
      {"kernel.eval", "kernel.schedule", "tasks.manage", "admin.*",
       "arch.data", "ruliology.eval", "perf.view"}]|>,
    ctx]];

$permRules = {$permRule1, $permRule2, $permRule3};

(* ── Arbol de permisos: build una vez y cachear ───────────────────────── *)
(*   Reproduce la logica de NestScheduler`buildRecords pero sobre
     Associations en lugar de numeros. Genera los 40 nodos del arbol.      *)

buildPermTree[seedCtx_Association, depth_Integer] :=
  Module[{nodes = {}, counter = 0, current, addNode},

    addNode[level_, ctx_, parent_] :=
      Module[{id = ++counter},
        AppendTo[nodes, <|"id"->id, "level"->level, "ctx"->ctx, "parent"->parent|>];
        id];

    (* Raiz — parent = Null para que Developer`WriteRawJSONString emita null *)
    current = {{addNode[0, seedCtx, Null], seedCtx}};

    (* Niveles 1..depth *)
    Do[
      current = Flatten[
        Map[
          Function[{pair},
            MapIndexed[
              Function[{rule, ri},
                With[{newCtx = rule[Last[pair]],
                      cid    = addNode[lv, rule[Last[pair]], First[pair]]},
                  {cid, newCtx}]],
              $permRules]],
          current],
        1];
    , {lv, 1, depth}];

    nodes
  ];

(* ── Cache del arbol por role ────────────────────────────────────────── *)
$treeCache = <||>;

getTree[role_Integer] :=
  If[KeyExistsQ[$treeCache, role],
    $treeCache[role],
    Module[{seed = <|"role"->role, "perms"->{}, "state"->"active"|>,
            tree},
      tree = buildPermTree[seed, 3];
      $treeCache[role] = tree;
      tree]];

(* ── API publica: derivePermissions ─────────────────────────────────── *)
(*   Recorre el arbol del role y acumula la union de todos los permisos
     en las ramas alcanzables (todas las reglas aplicables al role dado).  *)

PersonalSite`SessionFSM`derivePermissions[role_Integer] :=
  Module[{tree = getTree[role], perms},
    perms = Flatten[Map[#["ctx"]["perms"] &, tree]];
    Union[perms]
  ];

(* ── API publica: permissionTree ─────────────────────────────────────── *)
(*   Devuelve el arbol completo para visualizacion / /session/graph.
     Formato: lista de <|"id", "level", "perms", "parent"|>              *)

PersonalSite`SessionFSM`permissionTree[role_Integer : 1] :=
  Map[
    Function[n,
      <|"id"     -> n["id"],
        "level"  -> n["level"],
        "parent" -> n["parent"],
        "perms"  -> n["ctx"]["perms"],
        "role"   -> n["ctx"]["role"]|>],
    getTree[role]];

PersonalSite`SessionFSM`permissionTreeJson[role_Integer : 1] :=
  Developer`WriteRawJSONString[PersonalSite`SessionFSM`permissionTree[role]];

(* ── FSM de estados ───────────────────────────────────────────────────── *)
(*   Tabla de transiciones: event -> {fromState -> toState, ...}
     Solo se permiten transiciones explicitamente declaradas.             *)

$transitions = <|
  "login"     -> {"unauthenticated" -> "active"},
  "elevate"   -> {"active"    -> "elevated"},
  "downgrade" -> {"elevated"  -> "active"},
  "suspend"   -> {"active"    -> "suspended",
                  "elevated"  -> "suspended"},
  "resume"    -> {"suspended" -> "active"},
  "logout"    -> {"active"    -> "expired",
                  "elevated"  -> "expired",
                  "suspended" -> "expired"},
  "timeout"   -> {"active"    -> "expired",
                  "elevated"  -> "expired",
                  "suspended" -> "expired"},
  "relogin"   -> {"expired"   -> "active"}
|>;

PersonalSite`SessionFSM`validStates =
  {"unauthenticated", "active", "elevated", "suspended", "expired", "revoked"};

PersonalSite`SessionFSM`transition[session_Association, event_String] :=
  Module[{rules, fromState, match},
    rules     = Lookup[$transitions, event, {}];
    fromState = Lookup[session, "state", "unauthenticated"];
    match     = SelectFirst[rules, (First[#] === fromState) &, None];
    If[match === None,
      {$Failed, "invalid_transition:" <> fromState <> "->" <> event},
      {<|session,
         "state"       -> Last[match],
         "permissions" -> PersonalSite`SessionFSM`derivePermissions[
                            Lookup[session, "role", 1]],
         "lastTransition" -> <|"event"->event,
                                "from"->fromState,
                                "to"->Last[match],
                                "at"->DateString[Now]|>|>,
       "ok"}]
  ];

(* ── API publica: can ────────────────────────────────────────────────── *)
PersonalSite`SessionFSM`can[session_Association, permission_String] :=
  Module[{perms = Lookup[session, "permissions", {}],
          state = Lookup[session, "state", "unauthenticated"]},
    MemberQ[{"active", "elevated"}, state] &&
    (MemberQ[perms, permission] || MemberQ[perms, "admin.*"])
  ];

(* ── API publica: fsmGraph ───────────────────────────────────────────── *)
(*   Devuelve el grafo de transiciones FSM como JSON (para /session/graph) *)

PersonalSite`SessionFSM`fsmGraph[] :=
  Module[{edges},
    edges = Flatten @ KeyValueMap[
      Function[{event, rules},
        Map[
          Function[rule,
            <|"from"  -> First[rule],
              "to"    -> Last[rule],
              "event" -> event|>],
          rules]],
      $transitions];
    <|"states"    -> PersonalSite`SessionFSM`validStates,
      "events"    -> Keys[$transitions],
      "edges"     -> edges,
      "nodeCount" -> Length[PersonalSite`SessionFSM`validStates],
      "edgeCount" -> Length[edges]|>
  ];

(* ── Middleware: withSession ─────────────────────────────────────────── *)
(*   HOF que valida la sesion antes de ejecutar un handler.
     Inyecta la sesion en req["Session"] si valida, o devuelve 401.

   Uso en Router.wl:
       "/api/privado" ~~ EndOfString :>
         PersonalSite`SessionFSM`withSession[HTTPRequestData[], "public.read",
           Function[req, PersonalSite`Controller`miRuta[req]]]            *)

PersonalSite`SessionFSM`withSession[
    req_,
    requiredPerm_String,
    handler_] :=
  Module[{token, session, authorized},

    (* Extrae token de Authorization: Bearer <t> o Cookie: session=<t> *)
    token = Module[{auth, cookie},
      auth = Quiet @ Check[
        Lookup[req["Headers"], "Authorization",
          Lookup[req["Headers"], "authorization", ""]],
        ""];
      If[StringStartsQ[auth, "Bearer "],
        StringDrop[auth, 7],
        cookie = Quiet @ Check[
          Lookup[req["Headers"], "Cookie",
            Lookup[req["Headers"], "cookie", ""]],
          ""];
        With[{m = StringCases[cookie,
                    "session=" ~~ tok : Except[";"].. -> tok]},
          If[Length[m] > 0, First[m], ""]]]
    ];

    If[token === "",
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"error"->"no_session_token"|>],
        <|"StatusCode"->401,
          "Headers"-><|"Content-Type"->"application/json"|>|>]];

    session = Quiet @ Check[
      PersonalSite`SessionStore`validateToken[token], $Failed];

    If[session === $Failed,
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[<|"error"->"invalid_or_expired_token"|>],
        <|"StatusCode"->401,
          "Headers"-><|"Content-Type"->"application/json"|>|>]];

    authorized = PersonalSite`SessionFSM`can[session, requiredPerm];
    If[!authorized,
      Return @ HTTPResponse[
        Developer`WriteRawJSONString[
          <|"error"->"forbidden", "required"->requiredPerm|>],
        <|"StatusCode"->403,
          "Headers"-><|"Content-Type"->"application/json"|>|>]];

    (* Inyectar sesion en el request y delegar al handler *)
    handler[<|req, "Session" -> session|>]
  ];

End[];
