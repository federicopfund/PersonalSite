(* ::Package:: *)

(* PersonalSite`Controller`  (parte: arch)
   --------------------------------------------------------------------------
   Pagina /arch: Graph3D interactivo (3d-force-graph / WebGL).
   /arch/data  →  JSON {nodes, links}  (instantaneo, sin computo pesado)
   /arch       →  HTML  con el grafo embebido via CDN JS                 *)

BeginPackage["PersonalSite`Controller`"];

arch::usage     = "arch[req] renderiza /arch: arquitectura del sistema.";
archData::usage = "archData[req] sirve los datos JSON del grafo.";
archHealth::usage = "archHealth[req] sirve el estado de salud en tiempo real.";
archMath::usage   = "archMath[req] sirve el arbol NestGraph como JSON para visualizacion matematica.";
archDag::usage    = "archDag[req] sirve el DAG de dependencias de tareas como JSON.";
archTasks::usage  = "archTasks[req] sirve el estado live de todas las tareas del scheduler (TaskConfig + TaskManager state).";

Begin["`Private`"];

(* ── Datos del grafo — escaneado y actualizado al sistema completo ─────── *)
(* Nodos:  71 (entry×3, router×1, ctrl×19, model×13, a2a×3, session×3,
           devops×2, ux×1, frontend×1, view×1, ext×3, nest-rt×11,
           a2a-rt×10, sentinel×4)                                          *)
(* Aristas: ~172 (base, modelo, a2a, rt-pipeline, heartbeat, sentinels)     *)
$archJSON = .;   (* invalidar caché en cada carga del paquete              *)
$archJSON := $archJSON = buildArchJSON[];

(* Auto-migrate: inserta tareas nuevas en scheduler_tasks si no existen *)
Quiet @ Check[PersonalSite`TaskConfig`migrate[], Null];

buildArchJSON[] :=
  Module[{nodes, links, data},
    nodes = {
      (* ── Entry / Exit ──────────────────────────────────────────── *)
      <|"id"->"HTTP",       "group"->"entry",    "label"->"HTTP Request",
        "unit"->"Punto de entrada HTTP · aiohttp ASGI pool"|>,
      <|"id"->"Router",     "group"->"router",   "label"->"Router",
        "unit"->"URLDispatcher WL · regex pattern → handler"|>,
      <|"id"->"Response",   "group"->"entry",    "label"->"HTTP Response",
        "unit"->"HTTP 200 OK · Content-Type HTML/JSON · exit"|>,

      (* ── Controllers (ctrl) ─────────────────────────────────────── *)
      <|"id"->"Home",       "group"->"ctrl",     "label"->"HomeController",
        "unit"->"Renderiza / con tarjetas y metadatos compartidos"|>,
      <|"id"->"Blog",       "group"->"ctrl",     "label"->"BlogController",
        "unit"->"Lista de posts Markdown + detalle con sesión"|>,
      <|"id"->"Contacto",   "group"->"ctrl",     "label"->"ContactController",
        "unit"->"Formulario de contacto + envío SMTP async"|>,
      <|"id"->"WActrl",     "group"->"ctrl",     "label"->"WolframController",
        "unit"->"Proxy query → api.wolframalpha.com · parse XML"|>,
      <|"id"->"Nest",       "group"->"ctrl",     "label"->"NestController",
        "unit"->"Orquestador NestScheduler · BFS ruliar"|>,
      <|"id"->"Tasks",      "group"->"ctrl",     "label"->"TaskController",
        "unit"->"Dashboard de TaskScheduler · CRUD + live metrics"|>,
      <|"id"->"Perf",       "group"->"ctrl",     "label"->"PerfController",
        "unit"->"Métricas de latencia live · percentiles p50/p95"|>,
      <|"id"->"Theme",      "group"->"ctrl",     "label"->"ThemeController",
        "unit"->"Rotación de tokens de tema · CSS custom properties"|>,
      <|"id"->"ArchCtrl",   "group"->"ctrl",     "label"->"ArchController",
        "unit"->"Grafo 3D + health heartbeat + math DAG + graph-sim tasks"|>,
      <|"id"->"KpiCtrl",    "group"->"ctrl",     "label"->"KpiController",
        "unit"->"KPI time-series · métricas cruzadas · ciencia de números"|>,
      <|"id"->"FlowCtrl",   "group"->"ctrl",     "label"->"FlowController",
        "unit"->"Flujos de datos NestGraph · spec Flow ↔ vizualización"|>,
      <|"id"->"StyleCtrl",  "group"->"ctrl",     "label"->"StyleController",
        "unit"->"SCSS hot-reload API · hash + cache-bust pipeline"|>,
      <|"id"->"RuliCtrl",   "group"->"ctrl",     "label"->"RuliologyController",
        "unit"->"Exploración CA · reglas elementales Wolfram"|>,
      <|"id"->"KernelCtrl", "group"->"ctrl",     "label"->"KernelController",
        "unit"->"WL playground · evalúa expresiones en kernel pool"|>,
      <|"id"->"DagCtrl",    "group"->"ctrl",     "label"->"DagController",
        "unit"->"Vista DAG del scheduler · topological sort JSON"|>,

      (* ── Session subsystem ──────────────────────────────────────── *)
      <|"id"->"SessionCtrl","group"->"session",  "label"->"SessionController",
        "unit"->"Gestión de sesiones de usuario · login/logout/renew"|>,
      <|"id"->"SessionFSM", "group"->"session",  "label"->"SessionFSM",
        "unit"->"FSM de 5 estados · anon→active→idle→expired→guest"|>,
      <|"id"->"SessionStore","group"->"session", "label"->"SessionStore",
        "unit"->"Store persistente de sesiones · SQLite + LRU cache"|>,

      (* ── DevOps subsystem ───────────────────────────────────────── *)
      <|"id"->"DevOpsCtrl", "group"->"devops",   "label"->"DevOpsController",
        "unit"->"Pipeline CI/CD · 17 tareas · 8 niveles DAG"|>,
      <|"id"->"DevOpsModel","group"->"devops",   "label"->"DevOps",
        "unit"->"Acciones git+docker+paclet · L0-L7 DAG"|>,
      <|"id"->"DevStyleM",  "group"->"devops",   "label"->"DevStyle",
        "unit"->"SCSS detect→compile→hash→cache-bust hot-reload"|>,

      (* ── A2A subsystem (Agent2Agent protocol · Ruliad mesh) ─────── *)
      <|"id"->"A2ACtrl",    "group"->"a2a",      "label"->"A2AController",
        "unit"->"A2A HTTP · JSON-RPC 2.0 + Agent Card + UI /a2a"|>,
      <|"id"->"A2AProto",   "group"->"a2a",      "label"->"A2A Protocol",
        "unit"->"Message/Part/Task/Artifact · FSM · JSON-RPC framing · task store"|>,
      <|"id"->"AgentMeshM", "group"->"a2a",      "label"->"AgentMesh ⊛",
        "unit"->"Ruliad → agentes A2A · orquestador + agentes-regla · Agent Card"|>,

      (* ── Core models ────────────────────────────────────────────── *)
      <|"id"->"Database",   "group"->"model",    "label"->"Database",
        "unit"->"SQLite CRUD · connection pool · execute/query API"|>,
      <|"id"->"Post",       "group"->"model",    "label"->"Post",
        "unit"->"Post Markdown ↔ DB · front-matter parsing"|>,
      <|"id"->"Mailer",     "group"->"model",    "label"->"Mailer",
        "unit"->"SMTP async · plantillas de email · rate limit"|>,
      <|"id"->"WAmodel",    "group"->"model",    "label"->"WolframAlpha",
        "unit"->"API client · XML parse · pod extraction"|>,
      <|"id"->"NestSched",  "group"->"model",    "label"->"NestScheduler",
        "unit"->"BFS tree builder · NestGraph layered digraph"|>,
      <|"id"->"TaskMgr",    "group"->"model",    "label"->"TaskManager",
        "unit"->"Runtime pool de TaskObjects · spawn/stop/status"|>,
      <|"id"->"Scheduler",  "group"->"model",    "label"->"Scheduler",
        "unit"->"ScheduledTask WL nativo · cron-like intervals"|>,
      <|"id"->"Cache",      "group"->"model",    "label"->"Cache",
        "unit"->"LRU cache en memoria · TTL · stats · eviction"|>,
      <|"id"->"Assets",     "group"->"model",    "label"->"Assets",
        "unit"->"Cards + metadata · refreshCards + refreshMetric"|>,
      <|"id"->"ThemeM",     "group"->"model",    "label"->"ThemeModel",
        "unit"->"Tokens CSS · paleta · rotación periódica tick[]"|>,
      <|"id"->"FlowModel",  "group"->"model",    "label"->"Flow",
        "unit"->"Spec Flow ↔ NestGraph · engine paralelo"|>,
      <|"id"->"SettingsM",  "group"->"model",    "label"->"Settings",
        "unit"->"KV store persistente · SQLite · get/set/all"|>,
      <|"id"->"TaskConfigM","group"->"model",    "label"->"TaskConfig",
        "unit"->"CRUD de scheduler_tasks · seed + graph-sim tasks"|>,

      (* ── UX / FrontEnd subsystem ────────────────────────────────── *)
      <|"id"->"UXColorM",   "group"->"ux-model", "label"->"UXColorRules",
        "unit"->"Eval reglas de color UX · apply tokens · audit report"|>,
      <|"id"->"FrontEndM",  "group"->"frontend", "label"->"FrontEnd/StyleEngine",
        "unit"->"StyleEngine WL · Output formatting · HTML fragments"|>,

      (* ── View ───────────────────────────────────────────────────── *)
      <|"id"->"Renderer",   "group"->"view",     "label"->"Renderer",
        "unit"->"StringTemplate HTML · layout + vista + datos compartidos"|>,

      (* ── External ───────────────────────────────────────────────── *)
      <|"id"->"SQLite",     "group"->"ext",      "label"->"SQLite",
        "unit"->"Archivo .db local · ACID · WAL mode"|>,
      <|"id"->"WAAPI",      "group"->"ext",      "label"->"WA API",
        "unit"->"api.wolframalpha.com · XML full-result · rate-limited"|>,
      <|"id"->"SMTP",       "group"->"ext",      "label"->"SMTP",
        "unit"->"Servidor SMTP externo · TLS 587 · auth credentials"|>,

      (* ── NestScheduler runtime pipeline ────────────────────────── *)
      <|"id"->"NestTrigger", "group"->"nest-rt", "label"->"POST /nest/schedule",
        "unit"->"RT: HTTP trigger NestSchedule · form rules/seeds/depth"|>,
      <|"id"->"RulesParser", "group"->"nest-rt", "label"->"Parse rules/seeds/depth",
        "unit"->"RT: parsea rules={f1,f2,...} seeds={n1,...} depth=d"|>,
      <|"id"->"RecordsBuild","group"->"nest-rt", "label"->"buildRecords[rules,seeds,d]",
        "unit"->"RT: BFS en árbol ruliar · genera lista de nodos"|>,
      <|"id"->"SpecConvert", "group"->"nest-rt", "label"->"recordsToSpec[records,rules]",
        "unit"->"RT: convierte nodos BFS a spec de Flow paralelo"|>,
      <|"id"->"FlowL0",      "group"->"nest-rt", "label"->"Layer 0 ∥ Seeds",
        "unit"->"RT: Layer 0 · ejecuta seeds en paralelo (raíz)"|>,
      <|"id"->"FlowL1",      "group"->"nest-rt", "label"->"Layer 1 ∥ rule(seed)",
        "unit"->"RT: Layer 1 · aplica rule a cada seed resultado"|>,
      <|"id"->"FlowL2",      "group"->"nest-rt", "label"->"Layer 2 ∥ rule(L1)",
        "unit"->"RT: Layer 2 · composición rule∘rule(seed)"|>,
      <|"id"->"FlowLN",      "group"->"nest-rt", "label"->"Layer N ∥ rule(LN-1)",
        "unit"->"RT: Layer N · última composición · resultados finales"|>,
      <|"id"->"NestStore",   "group"->"nest-rt", "label"->"$lastResults",
        "unit"->"RT: $lastResults guardados · disponibles para /nest/results"|>,
      <|"id"->"SchedLoop",   "group"->"nest-rt", "label"->"ScheduledTask (every N s)",
        "unit"->"RT: ScheduledTask WL · re-ejecuta NestSchedule cada N s"|>,
      <|"id"->"NestAPI",     "group"->"nest-rt", "label"->"GET /nest/results",
        "unit"->"RT: endpoint JSON · expone $lastResults para PowerBI"|>,

      (* ── A2A protocol runtime pipeline (message/send end-to-end) ── *)
      <|"id"->"A2ARpcIn",    "group"->"a2a-rt", "label"->"POST /a2a (JSON-RPC)",
        "unit"->"RT: body JSON-RPC 2.0 · {method, params, id} → dispatch"|>,
      <|"id"->"A2ADispatch", "group"->"a2a-rt", "label"->"dispatch(method)",
        "unit"->"RT: enruta method → handler · errores JSON-RPC A2A"|>,
      <|"id"->"A2AMsgSend",  "group"->"a2a-rt", "label"->"message/send handler",
        "unit"->"RT: extrae seed/depth de los Parts del Message A2A"|>,
      <|"id"->"A2AOrch",     "group"->"a2a-rt", "label"->"⊛ Ruliad Orchestrator",
        "unit"->"RT: agente orquestador · expande la Ruliad (NestGraph)"|>,
      <|"id"->"A2ARule1",    "group"->"a2a-rt", "label"->"Agent · 2x+1",
        "unit"->"RT: agente-regla 1 · aplica 2x+1 al valor entrante"|>,
      <|"id"->"A2ARule2",    "group"->"a2a-rt", "label"->"Agent · x+14",
        "unit"->"RT: agente-regla 2 · aplica x+14 al valor entrante"|>,
      <|"id"->"A2ARule3",    "group"->"a2a-rt", "label"->"Agent · x-18",
        "unit"->"RT: agente-regla 3 · aplica x-18 al valor entrante"|>,
      <|"id"->"A2ATask",     "group"->"a2a-rt", "label"->"A2A Task · FSM",
        "unit"->"RT: submitted → working → completed · status + history"|>,
      <|"id"->"A2AArtifacts","group"->"a2a-rt", "label"->"Artifacts",
        "unit"->"RT: ruliad-trajectory + function-stacks + summary"|>,
      <|"id"->"A2ACard",     "group"->"a2a-rt", "label"->"GET /.well-known/agent-card.json",
        "unit"->"RT: discovery · Agent Card A2A (skills, transport JSONRPC)"|>,

      (* ── Confluent sentinels ────────────────────────────────────── *)
      <|"id"->"SentCtrl",   "group"->"sentinel", "label"->"⦿ Ctrl Health",
        "unit"->"Sentinel: convergen todos los controladores activos"|>,
      <|"id"->"SentModel",  "group"->"sentinel", "label"->"⦿ Model Health",
        "unit"->"Sentinel: convergen todos los modelos · DB + cache + ext"|>,
      <|"id"->"SentExt",    "group"->"sentinel", "label"->"⦿ Ext Health",
        "unit"->"Sentinel: estado de sistemas externos · SQLite/SMTP/WA"|>,
      <|"id"->"SysState",   "group"->"sentinel", "label"->"⦿ System State",
        "unit"->"Sentinel: estado global confluente del sistema completo"|>
    };

    links = {
      (* ── HTTP entry ────────────────────────────────────────────── *)
      <|"source"->"HTTP",       "target"->"Router"|>,

      (* ── Router → all controllers ──────────────────────────────── *)
      <|"source"->"Router",     "target"->"Home"|>,
      <|"source"->"Router",     "target"->"Blog"|>,
      <|"source"->"Router",     "target"->"Contacto"|>,
      <|"source"->"Router",     "target"->"WActrl"|>,
      <|"source"->"Router",     "target"->"Nest"|>,
      <|"source"->"Router",     "target"->"Tasks"|>,
      <|"source"->"Router",     "target"->"Perf"|>,
      <|"source"->"Router",     "target"->"Theme"|>,
      <|"source"->"Router",     "target"->"ArchCtrl"|>,
      <|"source"->"Router",     "target"->"KpiCtrl"|>,
      <|"source"->"Router",     "target"->"FlowCtrl"|>,
      <|"source"->"Router",     "target"->"StyleCtrl"|>,
      <|"source"->"Router",     "target"->"RuliCtrl"|>,
      <|"source"->"Router",     "target"->"SessionCtrl"|>,
      <|"source"->"Router",     "target"->"KernelCtrl"|>,
      <|"source"->"Router",     "target"->"DevOpsCtrl"|>,
      <|"source"->"Router",     "target"->"DagCtrl"|>,
      <|"source"->"Router",     "target"->"A2ACtrl"|>,

      (* ── Controllers → Models (derivado de imports reales) ─────── *)
      (* Home: Assets *)
      <|"source"->"Home",       "target"->"Assets"|>,
      <|"source"->"Home",       "target"->"Cache"|>,
      (* Blog: Post, Assets, DevOps, SessionStore, Settings, Mailer *)
      <|"source"->"Blog",       "target"->"Post"|>,
      <|"source"->"Blog",       "target"->"Assets"|>,
      <|"source"->"Blog",       "target"->"DevOpsModel"|>,
      <|"source"->"Blog",       "target"->"SessionStore"|>,
      <|"source"->"Blog",       "target"->"SettingsM"|>,
      (* Contacto: Mailer *)
      <|"source"->"Contacto",   "target"->"Mailer"|>,
      (* WActrl: WAmodel *)
      <|"source"->"WActrl",     "target"->"WAmodel"|>,
      (* Nest: NestScheduler *)
      <|"source"->"Nest",       "target"->"NestSched"|>,
      (* Tasks/KPI: TaskMgr, Database, DevOps, Settings *)
      <|"source"->"Tasks",      "target"->"TaskMgr"|>,
      <|"source"->"Tasks",      "target"->"Database"|>,
      <|"source"->"Tasks",      "target"->"DevOpsModel"|>,
      <|"source"->"Tasks",      "target"->"SettingsM"|>,
      <|"source"->"KpiCtrl",    "target"->"TaskMgr"|>,
      <|"source"->"KpiCtrl",    "target"->"Database"|>,
      <|"source"->"KpiCtrl",    "target"->"SettingsM"|>,
      (* Perf: Assets, Cache *)
      <|"source"->"Perf",       "target"->"Assets"|>,
      <|"source"->"Perf",       "target"->"Cache"|>,
      (* Theme: ThemeModel *)
      <|"source"->"Theme",      "target"->"ThemeM"|>,
      (* Arch: Cache, Database, NestSched, TaskMgr *)
      <|"source"->"ArchCtrl",   "target"->"Cache"|>,
      <|"source"->"ArchCtrl",   "target"->"Database"|>,
      <|"source"->"ArchCtrl",   "target"->"NestSched"|>,
      <|"source"->"ArchCtrl",   "target"->"TaskMgr"|>,
      (* Flow: FlowModel *)
      <|"source"->"FlowCtrl",   "target"->"FlowModel"|>,
      (* Style/Kernel: FrontEnd *)
      <|"source"->"StyleCtrl",  "target"->"FrontEndM"|>,
      <|"source"->"KernelCtrl", "target"->"FrontEndM"|>,
      <|"source"->"KernelCtrl", "target"->"TaskMgr"|>,
      (* Session: SessionFSM, SessionStore *)
      <|"source"->"SessionCtrl","target"->"SessionFSM"|>,
      <|"source"->"SessionCtrl","target"->"SessionStore"|>,
      (* DevOps: DevOpsModel, TaskMgr *)
      <|"source"->"DevOpsCtrl", "target"->"DevOpsModel"|>,
      <|"source"->"DevOpsCtrl", "target"->"TaskMgr"|>,
      (* Dag: TaskMgr *)
      <|"source"->"DagCtrl",    "target"->"TaskMgr"|>,

      (* ── Controllers → Renderer → Response ─────────────────────── *)
      <|"source"->"Home",       "target"->"Renderer"|>,
      <|"source"->"Blog",       "target"->"Renderer"|>,
      <|"source"->"Contacto",   "target"->"Renderer"|>,
      <|"source"->"Nest",       "target"->"Renderer"|>,
      <|"source"->"Tasks",      "target"->"Renderer"|>,
      <|"source"->"Perf",       "target"->"Renderer"|>,
      <|"source"->"Theme",      "target"->"Renderer"|>,
      <|"source"->"ArchCtrl",   "target"->"Renderer"|>,
      <|"source"->"KpiCtrl",    "target"->"Renderer"|>,
      <|"source"->"FlowCtrl",   "target"->"Renderer"|>,
      <|"source"->"StyleCtrl",  "target"->"Renderer"|>,
      <|"source"->"RuliCtrl",   "target"->"Renderer"|>,
      <|"source"->"KernelCtrl", "target"->"Renderer"|>,
      <|"source"->"SessionCtrl","target"->"Renderer"|>,
      <|"source"->"DevOpsCtrl", "target"->"Renderer"|>,
      <|"source"->"Renderer",   "target"->"Response"|>,

      (* ── Model inter-dependencies (de imports reales) ───────────── *)
      <|"source"->"Post",        "target"->"Database"|>,
      <|"source"->"Assets",      "target"->"Cache"|>,
      <|"source"->"Assets",      "target"->"Post"|>,
      <|"source"->"Assets",      "target"->"FlowModel"|>,
      <|"source"->"FlowModel",   "target"->"NestSched"|>,
      <|"source"->"NestSched",   "target"->"FlowModel"|>,
      <|"source"->"DevOpsModel", "target"->"Database"|>,
      <|"source"->"DevOpsModel", "target"->"FlowModel"|>,
      <|"source"->"DevStyleM",   "target"->"Cache"|>,
      <|"source"->"DevStyleM",   "target"->"TaskMgr"|>,
      <|"source"->"SettingsM",   "target"->"Database"|>,
      <|"source"->"ThemeM",      "target"->"SettingsM"|>,
      <|"source"->"UXColorM",    "target"->"SettingsM"|>,
      <|"source"->"UXColorM",    "target"->"ThemeM"|>,
      <|"source"->"SessionStore","target"->"Cache"|>,
      <|"source"->"SessionStore","target"->"Database"|>,
      <|"source"->"SessionFSM",  "target"->"SessionStore"|>,
      <|"source"->"TaskConfigM", "target"->"Assets"|>,
      <|"source"->"TaskConfigM", "target"->"Database"|>,
      <|"source"->"TaskConfigM", "target"->"DevOpsModel"|>,
      <|"source"->"TaskConfigM", "target"->"NestSched"|>,
      <|"source"->"TaskConfigM", "target"->"SettingsM"|>,
      <|"source"->"TaskConfigM", "target"->"UXColorM"|>,
      <|"source"->"TaskConfigM", "target"->"DevStyleM"|>,
      <|"source"->"Scheduler",   "target"->"TaskMgr"|>,
      <|"source"->"Scheduler",   "target"->"TaskConfigM"|>,
      <|"source"->"Scheduler",   "target"->"NestSched"|>,
      <|"source"->"Scheduler",   "target"->"UXColorM"|>,
      <|"source"->"Scheduler",   "target"->"DevStyleM"|>,
      <|"source"->"Mailer",      "target"->"SMTP"|>,
      <|"source"->"WAmodel",     "target"->"WAAPI"|>,
      <|"source"->"Database",    "target"->"SQLite"|>,

      (* ── A2A subsystem dependencies (imports reales) ────────────── *)
      (* A2AController: A2A protocol + AgentMesh + Renderer (UI) *)
      <|"source"->"A2ACtrl",     "target"->"A2AProto"|>,
      <|"source"->"A2ACtrl",     "target"->"AgentMeshM"|>,
      <|"source"->"A2ACtrl",     "target"->"Renderer"|>,
      (* AgentMesh: protocolo A2A + NestScheduler (Ruliad) + Flow engine *)
      <|"source"->"AgentMeshM",  "target"->"A2AProto"|>,
      <|"source"->"AgentMeshM",  "target"->"NestSched"|>,
      <|"source"->"AgentMeshM",  "target"->"FlowModel"|>,
      (* A2A protocol: persistencia best-effort de a2a_tasks *)
      <|"source"->"A2AProto",    "target"->"Database"|>,

      (* ── A2A protocol runtime pipeline (message/send) ───────────── *)
      <|"source"->"A2ARpcIn",    "target"->"Router",       "rt"->True|>,
      <|"source"->"A2ACtrl",     "target"->"A2ADispatch",  "rt"->True|>,
      <|"source"->"A2ADispatch", "target"->"A2AMsgSend",   "rt"->True|>,
      <|"source"->"A2AMsgSend",  "target"->"A2AOrch",      "rt"->True|>,
      <|"source"->"A2AOrch",     "target"->"A2ARule1",     "rt"->True|>,
      <|"source"->"A2AOrch",     "target"->"A2ARule2",     "rt"->True|>,
      <|"source"->"A2AOrch",     "target"->"A2ARule3",     "rt"->True|>,
      (* el orquestador delega la expansion al runtime del NestScheduler *)
      <|"source"->"A2AOrch",     "target"->"RecordsBuild", "rt"->True|>,
      <|"source"->"A2ARule1",    "target"->"A2ATask",      "rt"->True|>,
      <|"source"->"A2ARule2",    "target"->"A2ATask",      "rt"->True|>,
      <|"source"->"A2ARule3",    "target"->"A2ATask",      "rt"->True|>,
      <|"source"->"A2ATask",     "target"->"A2AArtifacts", "rt"->True|>,
      <|"source"->"A2AArtifacts","target"->"Response",     "rt"->True|>,
      <|"source"->"A2ACtrl",     "target"->"A2ACard",      "rt"->True|>,
      <|"source"->"A2ACard",     "target"->"Response",     "rt"->True|>,

      (* ── NestScheduler runtime pipeline ────────────────────────── *)
      <|"source"->"NestTrigger", "target"->"Router",      "rt"->True|>,
      <|"source"->"Nest",        "target"->"RulesParser", "rt"->True|>,
      <|"source"->"RulesParser", "target"->"RecordsBuild","rt"->True|>,
      <|"source"->"RecordsBuild","target"->"SpecConvert", "rt"->True|>,
      <|"source"->"SpecConvert", "target"->"FlowL0",      "rt"->True|>,
      <|"source"->"FlowL0",      "target"->"FlowL1",      "rt"->True|>,
      <|"source"->"FlowL1",      "target"->"FlowL2",      "rt"->True|>,
      <|"source"->"FlowL2",      "target"->"FlowLN",      "rt"->True|>,
      <|"source"->"FlowLN",      "target"->"NestStore",   "rt"->True|>,
      <|"source"->"NestStore",   "target"->"SchedLoop",   "rt"->True|>,
      <|"source"->"SchedLoop",   "target"->"Nest",        "rt"->True|>,
      <|"source"->"NestStore",   "target"->"NestAPI",     "rt"->True|>,
      <|"source"->"NestAPI",     "target"->"Response",    "rt"->True|>,

      (* ── Heartbeat self-loops ────────────────────────────────────── *)
      <|"source"->"Router",      "target"->"Router",      "hb"->True|>,
      <|"source"->"Database",    "target"->"Database",    "hb"->True|>,
      <|"source"->"NestSched",   "target"->"NestSched",   "hb"->True|>,
      <|"source"->"TaskMgr",     "target"->"TaskMgr",     "hb"->True|>,
      <|"source"->"Scheduler",   "target"->"Scheduler",   "hb"->True|>,
      <|"source"->"Cache",       "target"->"Cache",       "hb"->True|>,
      <|"source"->"Renderer",    "target"->"Renderer",    "hb"->True|>,
      <|"source"->"SessionStore","target"->"SessionStore","hb"->True|>,
      <|"source"->"SettingsM",   "target"->"SettingsM",   "hb"->True|>,
      <|"source"->"AgentMeshM",  "target"->"AgentMeshM",  "hb"->True|>,
      <|"source"->"A2AProto",    "target"->"A2AProto",    "hb"->True|>,

      (* ── Confluent sentinel convergence ─────────────────────────── *)
      <|"source"->"Home",       "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"Blog",       "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"Nest",       "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"Tasks",      "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"ArchCtrl",   "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"KpiCtrl",    "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"KernelCtrl", "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"SessionCtrl","target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"DevOpsCtrl", "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"A2ACtrl",    "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"AgentMeshM", "target"->"SentModel", "conv"->True|>,
      <|"source"->"A2AProto",   "target"->"SentModel", "conv"->True|>,
      <|"source"->"Database",   "target"->"SentModel", "conv"->True|>,
      <|"source"->"NestSched",  "target"->"SentModel", "conv"->True|>,
      <|"source"->"TaskMgr",    "target"->"SentModel", "conv"->True|>,
      <|"source"->"Cache",      "target"->"SentModel", "conv"->True|>,
      <|"source"->"SettingsM",  "target"->"SentModel", "conv"->True|>,
      <|"source"->"SessionStore","target"->"SentModel","conv"->True|>,
      <|"source"->"DevOpsModel","target"->"SentModel", "conv"->True|>,
      <|"source"->"UXColorM",   "target"->"SentModel", "conv"->True|>,
      <|"source"->"SQLite",     "target"->"SentExt",   "conv"->True|>,
      <|"source"->"WAAPI",      "target"->"SentExt",   "conv"->True|>,
      <|"source"->"SMTP",       "target"->"SentExt",   "conv"->True|>,
      <|"source"->"SentCtrl",   "target"->"SysState",  "conv"->True|>,
      <|"source"->"SentModel",  "target"->"SysState",  "conv"->True|>,
      <|"source"->"SentExt",    "target"->"SysState",  "conv"->True|>
    };
    data = <|"nodes" -> nodes, "links" -> links|>;
    Quiet @ Check[
      Developer`WriteRawJSONString[data],
      ExportString[data, "JSON"]
    ]
  ];

(* ELIMINADO buildArchPNG — código muerto, sin referencias *)

(* ── Endpoints ────────────────────────────────────────────────────────── *)

(* GET /arch/data  →  JSON {nodes, links} para 3d-force-graph *)
archData[req_] :=
  HTTPResponse[$archJSON, <|"Headers" -> <|
    "Content-Type"  -> "application/json; charset=utf-8",
    "Cache-Control" -> "public, max-age=3600"
  |>|>];

(* GET /arch/health  →  estado de salud de cada nodo en tiempo real *)
archHealth[req_] :=
  Module[{taskSnap, nestInfo, cacheStats, dbOk, ts, nodes, groups, a2aAgents, a2aTasks},
    ts        = UnixTime[];
    taskSnap  = Quiet @ Check[PersonalSite`TaskManager`summary[], <||>];
    nestInfo  = Quiet @ Check[PersonalSite`NestScheduler`taskInfo[], <||>];
    cacheStats= Quiet @ Check[PersonalSite`Cache`stats[], <||>];
    dbOk      = Quiet @ Check[
      (PersonalSite`Database`execute["SELECT 1", {}]; True), False];
    a2aAgents = Quiet @ Check[Length[PersonalSite`AgentMesh`agents[]], 4];
    a2aTasks  = Quiet @ Check[Length[PersonalSite`A2A`tasksList[]], 0];

    nodes = <|
      "HTTP"       -> hOk["entry"],
      "Router"     -> hOk["routing · " <> ToString[Round[1000 AbsoluteTime[], 1]] <> " epoch"],
      "Home"       -> hOk["ctrl"],     "Blog"       -> hOk["ctrl"],
      "Contacto"   -> hOk["ctrl"],     "WActrl"     -> hOk["ctrl"],
      "Nest"       -> hOk["ctrl"],     "Tasks"      -> hOk["ctrl"],
      "Perf"       -> hOk["ctrl"],     "Theme"      -> hOk["ctrl"],
      "Database"   -> If[dbOk, hOk["SQLite ok"], hErr["DB unreachable"]],
      "Post"       -> hOk["model"],
      "Mailer"     -> hOk["model"],
      "WAmodel"    -> hOk["model"],
      "NestSched"  -> nestNodeHealth[nestInfo],
      "TaskMgr"    -> taskMgrHealth[taskSnap],
      "Scheduler"  -> If[TrueQ[Lookup[taskSnap, "running", 0] > 0],
                       hOk[ToString[Lookup[taskSnap,"running",0]] <> " tasks"],
                       hWarn["0 tasks running"]],
      "Cache"      -> cacheNodeHealth[cacheStats],
      "Assets"     -> hOk["model"],
      "ThemeM"     -> hOk["model"],
      "Renderer"   -> hOk["view"],
      "SQLite"     -> If[dbOk, hOk["connected"], hErr["unreachable"]],
      "WAAPI"      -> hUnknown["external API"],
      "SMTP"       -> hUnknown["external SMTP"],
      "Response"   -> hOk["exit"],
      (* NestScheduler runtime *)
      "NestTrigger"-> hOk["trigger ready"],
      "RulesParser"-> hOk["parser"],
      "RecordsBuild"-> hOk["builder"],
      "SpecConvert"-> hOk["converter"],
      "FlowL0"     -> hOk["Layer 0"],
      "FlowL1"     -> hOk["Layer 1"],
      "FlowL2"     -> hOk["Layer 2"],
      "FlowLN"     -> hOk["Layer N"],
      "NestStore"  -> If[Lookup[nestInfo,"runCount",0]>0,
                       hOk[ToString[Lookup[nestInfo,"runCount",0]]<>" stored"],
                       hIdle["no results yet"]],
      "SchedLoop"  -> If[TrueQ[Lookup[nestInfo,"active",False]],
                       hOk["scheduled"],
                       hIdle["not scheduled"]],
      "NestAPI"    -> hOk["results API"],
      (* ── Nuevos controllers ──────────────────────────────────── *)
      "ArchCtrl"   -> hOk["ctrl"],
      "KpiCtrl"    -> hOk["ctrl"],
      "FlowCtrl"   -> hOk["ctrl"],
      "StyleCtrl"  -> hOk["ctrl"],
      "RuliCtrl"   -> hOk["ctrl"],
      "KernelCtrl" -> hOk["ctrl"],
      "DagCtrl"    -> hOk["ctrl"],
      (* ── Session subsystem ──────────────────────────────────── *)
      "SessionCtrl"  -> hOk["session"],
      "SessionFSM"   -> hOk["fsm · 5 states"],
      "SessionStore" -> If[dbOk, hOk["store · SQLite"], hErr["unreachable"]],
      (* ── DevOps subsystem ────────────────────────────────────── *)
      "DevOpsCtrl"  -> hOk["ctrl"],
      "DevOpsModel" -> If[dbOk, hOk["pipeline · L0-L7"], hWarn["DB warn"]],
      "DevStyleM"   -> hOk["hot-reload"],
      (* ── A2A subsystem (Agent2Agent · Ruliad mesh) ───────────── *)
      "A2ACtrl"     -> hOk["a2a · JSON-RPC 2.0"],
      "A2AProto"    -> If[dbOk, hOk[ToString[a2aTasks] <> " tasks · FSM"],
                              hWarn[ToString[a2aTasks] <> " tasks (mem)"]],
      "AgentMeshM"  -> hOk[ToString[a2aAgents] <> " agentes · Ruliad mesh"],
      "A2ARpcIn"    -> hOk["rpc in"],
      "A2ADispatch" -> hOk["dispatch"],
      "A2AMsgSend"  -> hOk["message/send"],
      "A2AOrch"     -> hOk["orchestrator"],
      "A2ARule1"    -> hOk["agent 2x+1"],
      "A2ARule2"    -> hOk["agent x+14"],
      "A2ARule3"    -> hOk["agent x-18"],
      "A2ATask"     -> hOk["task fsm"],
      "A2AArtifacts"-> hOk["artifacts"],
      "A2ACard"     -> hOk["agent card"],
      (* ── Nuevos modelos ──────────────────────────────────────── *)
      "FlowModel"   -> hOk["NestGraph engine"],
      "SettingsM"   -> If[dbOk, hOk["kv store"], hErr["unreachable"]],
      "TaskConfigM" -> If[dbOk, hOk["10 tasks"], hWarn["DB warn"]],
      (* ── UX / FrontEnd ───────────────────────────────────────── *)
      "UXColorM"   -> hOk["color rules eval"],
      "FrontEndM"  -> hOk["StyleEngine · Output"],
      (* Confluent sentinels *)
      "SentCtrl"   -> hOk["ctrl layer"],
      "SentModel"  -> If[dbOk, hOk["model layer"], hWarn["DB warn"]],
      "SentExt"    -> hUnknown["ext systems"],
      "SysState"   -> If[dbOk && Lookup[taskSnap,"running",0]>0,
                       hOk["system ok"],
                       hWarn["degraded"]]
    |>;

    groups = <|
      "entry"    -> "ok",
      "ctrl"     -> "ok",
      "model"    -> If[dbOk, "ok", "warn"],
      "session"  -> If[dbOk, "ok", "warn"],
      "devops"   -> If[dbOk, "ok", "idle"],
      "ux-model" -> "ok",
      "frontend" -> "ok",
      "ext"      -> "unknown",
      "nest-rt"  -> If[TrueQ[Lookup[nestInfo,"active",False]], "ok", "idle"],
      "a2a"      -> If[dbOk, "ok", "warn"],
      "a2a-rt"   -> "ok",
      "sentinel" -> If[dbOk, "ok", "warn"]
    |>;

    Quiet @ Check[
      HTTPResponse[
        Developer`WriteRawJSONString[<|"ts"->ts, "nodes"->nodes, "groups"->groups|>],
        <|"Headers" -> <|
          "Content-Type"  -> "application/json; charset=utf-8",
          "Cache-Control" -> "no-store"
        |>|>],
      HTTPResponse["{}", <|"Headers" -> <|"Content-Type" -> "application/json"|>|>]
    ]
  ];

(* ── Health helpers ──────────────────────────────────────────────────── *)
hOk[msg_String]      := <|"status"->"ok",      "msg"->msg|>;
hWarn[msg_String]    := <|"status"->"warn",    "msg"->msg|>;
hErr[msg_String]     := <|"status"->"err",     "msg"->msg|>;
hIdle[msg_String]    := <|"status"->"idle",    "msg"->msg|>;
hUnknown[msg_String] := <|"status"->"unknown", "msg"->msg|>;

nestNodeHealth[info_] :=
  If[TrueQ[Lookup[info,"active",False]],
    hOk[ToString[Lookup[info,"runCount",0]] <> " runs"],
    hIdle["not scheduled"]];

taskMgrHealth[snap_] :=
  Module[{n = Lookup[snap,"taskCount",0], r = Lookup[snap,"running",0]},
    Which[
      n == 0, hWarn["no tasks registered"],
      r < n,  hWarn[ToString[r]<>"/"<>ToString[n]<>" running"],
      True,   hOk[ToString[r]<>"/"<>ToString[n]<>" running"]]];

cacheNodeHealth[stats_] :=
  Module[{ratio = Lookup[stats,"ratio",0.]},
    Which[
      ratio > 0.5, hOk[ToString[Round[100 ratio]]<>"% hit"],
      ratio > 0,   hWarn[ToString[Round[100 ratio]]<>"% hit"],
      True,        hIdle["cache empty"]]];

(* GET /arch  →  pagina HTML con el grafo 3D interactivo *)
arch[req_] :=
  PersonalSite`View`render["arch", <||>];

(* GET /arch/math  →  arbol NestGraph como JSON para visualizacion matematica *)
archMath[req_] :=
  Module[
    {rules, rLabels, seed, maxDepth, allNodes, allLinks, makeTree},
    rules     = {2 #1 + 1 &, #1 + 14 &, #1 - 18 &};
    rLabels   = {"2x+1", "x+14", "x-18"};
    seed      = 1;
    maxDepth  = 3;
    allNodes  = {};
    allLinks  = {};

    makeTree[v_, d_, pid_] := (
      AppendTo[allNodes, <|"id" -> pid, "label" -> ToString[v], "depth" -> d, "val" -> v|>];
      If[d < maxDepth,
        Do[
          With[{cv = rules[[ri]][v], cid = pid <> "_" <> ToString[ri]},
            AppendTo[allLinks, <|"source" -> pid, "target" -> cid,
                                  "rule" -> ri, "op" -> rLabels[[ri]]|>];
            makeTree[cv, d + 1, cid]
          ],
          {ri, Length[rules]}
        ]
      ]
    );

    makeTree[seed, 0, "r"];

    HTTPResponse[
      Developer`WriteRawJSONString[<|
        "nodes" -> allNodes,
        "links" -> allLinks,
        "rules" -> rLabels,
        "seed"  -> seed,
        "depth" -> maxDepth,
        "total" -> Length[allNodes]
      |>],
      <|"Content-Type" -> "application/json"|>
    ]
  ];

(* GET /arch/dag  →  DAG de dependencias entre tareas del Scheduler *)
archDag[req_] :=
  Module[{snap, taskMap, dagNodes, links, cp, cpSet, result},
    snap    = Quiet @ Check[PersonalSite`TaskManager`summary[], <||>];
    taskMap = If[AssociationQ[snap], Lookup[snap, "tasks", <||>], <||>];

    dagNodes = {
      <|"id"->"heartbeat",     "group"->"system","interval"->30,  "depth"->0, "deps"->{}|>,
      <|"id"->"cache-warm",    "group"->"system","interval"->300, "depth"->1, "deps"->{"heartbeat"}|>,
      <|"id"->"theme-rotate",  "group"->"theme", "interval"->10,  "depth"->1, "deps"->{"heartbeat"}|>,
      <|"id"->"cards-refresh", "group"->"cache", "interval"->20,  "depth"->2, "deps"->{"cache-warm"}|>,
      <|"id"->"metric-refresh","group"->"cache", "interval"->300, "depth"->3, "deps"->{"cards-refresh"}|>,
      <|"id"->"nest-refresh",  "group"->"flow",  "interval"->300, "depth"->2, "deps"->{"cache-warm"}|>
    };

    links = Flatten[Table[
      Module[{n = dagNodes[[i]], nid},
        nid = Lookup[n, "id", ""];
        Map[Function[{d}, <|"source"->d, "target"->nid|>], Lookup[n,"deps",{}]]
      ], {i, Length[dagNodes]}]];

    cp    = {"heartbeat", "cache-warm", "cards-refresh", "metric-refresh"};
    cpSet = AssociationThread[cp, ConstantArray[True, Length[cp]]];

    result = <|
      "nodes" -> Table[
        Module[{n = dagNodes[[i]], id, grp, iv, dep, live},
          id   = Lookup[n, "id",       ""];
          grp  = Lookup[n, "group",    "user"];
          iv   = Lookup[n, "interval", 60];
          dep  = Lookup[n, "depth",    0];
          live = Lookup[taskMap, id, <||>];
          <|"id"       -> id,
            "label"    -> If[StringQ @ Lookup[live,"label",id], Lookup[live,"label",id], id],
            "group"    -> grp,
            "interval" -> iv,
            "running"  -> TrueQ @ Lookup[live, "running", False],
            "runs"     -> If[IntegerQ @ Lookup[live,"runs",0], Lookup[live,"runs",0], 0],
            "enabled"  -> TrueQ @ Lookup[live, "enabled", True],
            "depth"    -> dep,
            "cp"       -> TrueQ @ Lookup[cpSet, id, False]
          |>],
        {i, Length[dagNodes]}],
      "links"    -> links,
      "topoOrder"-> {"heartbeat","theme-rotate","cache-warm","cards-refresh","nest-refresh","metric-refresh"},
      "critPath" -> cp,
      "nodeCount"-> Length[dagNodes],
      "edgeCount"-> Length[links],
      "cpCost"   -> 620
    |>;

    HTTPResponse[
      Developer`WriteRawJSONString[result],
      <|"Content-Type" -> "application/json"|>
    ]
  ];

(* GET /arch/tasks  →  estado live TaskScheduler (config + runtime state) *)
archTasks[req_] :=
  Module[{taskSnap, configs, ts, tasks},
    ts       = UnixTime[];
    taskSnap = Quiet @ Check[PersonalSite`TaskManager`allTasks[], <||>];
    configs  = Quiet @ Check[PersonalSite`TaskConfig`all[], {}];
    tasks = Map[Function[cfg,
      Module[{id, state, hist},
        id    = Lookup[cfg, "task_id", ""];
        state = Lookup[taskSnap, id, <||>];
        hist  = Quiet @ Check[PersonalSite`TaskManager`history[id], {}];
        <|"id"       -> id,
          "label"    -> Lookup[cfg, "label", id],
          "group"    -> Lookup[cfg, "group_name", "system"],
          "interval" -> Lookup[cfg, "interval_s", 60],
          "dagOrder" -> Lookup[cfg, "dag_order", 0],
          "deps"     -> Lookup[cfg, "deps", {}],
          "enabled"  -> TrueQ[Lookup[cfg, "enabled", False]],
          "running"  -> TrueQ[Lookup[state, "running", False]],
          "runs"     -> (Lookup[state, "runs",   0] /. _Missing -> 0),
          "errors"   -> (Lookup[state, "errors", 0] /. _Missing -> 0),
          "lastMs"   -> (Lookup[state, "lastMs", 0] /. _Missing -> 0),
          "avgMs"    -> (Lookup[state, "avgMs",  0] /. _Missing -> 0),
          "history"  -> Take[hist /. _Missing -> {}, UpTo[8]]
        |>
      ]], configs];
    Quiet @ Check[
      HTTPResponse[
        Developer`WriteRawJSONString[<|"ts" -> ts, "tasks" -> tasks|>],
        <|"Headers" -> <|"Content-Type" -> "application/json; charset=utf-8",
                         "Cache-Control" -> "no-store"|>|>],
      HTTPResponse["{}", <|"Headers" -> <|"Content-Type" -> "application/json"|>|>]
    ]
  ];

End[];
EndPackage[];
