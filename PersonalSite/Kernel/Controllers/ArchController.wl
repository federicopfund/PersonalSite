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

Begin["`Private`"];

(* ── Datos del grafo (estaticos, memoizados) ──────────────────────────── *)
$archJSON := $archJSON = buildArchJSON[];

buildArchJSON[] :=
  Module[{nodes, links, data},
    nodes = {
      (* ── Capa base ─────────────────────────────────────────────── *)
      <|"id"->"HTTP",      "group"->"entry",   "label"->"HTTP Request"|>,
      <|"id"->"Router",    "group"->"router",  "label"->"Router"|>,
      <|"id"->"Home",      "group"->"ctrl",    "label"->"HomeController"|>,
      <|"id"->"Blog",      "group"->"ctrl",    "label"->"BlogController"|>,
      <|"id"->"Contacto",  "group"->"ctrl",    "label"->"ContactController"|>,
      <|"id"->"WActrl",    "group"->"ctrl",    "label"->"WolframController"|>,
      <|"id"->"Nest",      "group"->"ctrl",    "label"->"NestController"|>,
      <|"id"->"Tasks",     "group"->"ctrl",    "label"->"TaskController"|>,
      <|"id"->"Perf",      "group"->"ctrl",    "label"->"PerfController"|>,
      <|"id"->"Theme",     "group"->"ctrl",    "label"->"ThemeController"|>,
      <|"id"->"Database",  "group"->"model",   "label"->"Database"|>,
      <|"id"->"Post",      "group"->"model",   "label"->"Post"|>,
      <|"id"->"Mailer",    "group"->"model",   "label"->"Mailer"|>,
      <|"id"->"WAmodel",   "group"->"model",   "label"->"WolframAlpha"|>,
      <|"id"->"NestSched", "group"->"model",   "label"->"NestScheduler"|>,
      <|"id"->"TaskMgr",   "group"->"model",   "label"->"TaskManager"|>,
      <|"id"->"Scheduler", "group"->"model",   "label"->"Scheduler"|>,
      <|"id"->"Cache",     "group"->"model",   "label"->"Cache"|>,
      <|"id"->"Assets",    "group"->"model",   "label"->"Assets"|>,
      <|"id"->"ThemeM",    "group"->"model",   "label"->"ThemeModel"|>,
      <|"id"->"Renderer",  "group"->"view",    "label"->"Renderer"|>,
      <|"id"->"SQLite",    "group"->"ext",     "label"->"SQLite"|>,
      <|"id"->"WAAPI",     "group"->"ext",     "label"->"WA API"|>,
      <|"id"->"SMTP",      "group"->"ext",     "label"->"SMTP"|>,
      <|"id"->"Response",  "group"->"entry",   "label"->"HTTP Response"|>,
      (* ── NestScheduler runtime (POST /nest/schedule trigger) ────── *)
      <|"id"->"NestTrigger", "group"->"nest-rt", "label"->"POST /nest/schedule"|>,
      <|"id"->"RulesParser", "group"->"nest-rt", "label"->"Parse rules / seeds / depth"|>,
      <|"id"->"RecordsBuild","group"->"nest-rt", "label"->"buildRecords[rules,seeds,depth]"|>,
      <|"id"->"SpecConvert", "group"->"nest-rt", "label"->"recordsToSpec[records,rules]"|>,
      <|"id"->"FlowL0",      "group"->"nest-rt", "label"->"Layer 0 \[DoubleVerticalBar] Seeds"|>,
      <|"id"->"FlowL1",      "group"->"nest-rt", "label"->"Layer 1 \[DoubleVerticalBar] rule(seed)"|>,
      <|"id"->"FlowL2",      "group"->"nest-rt", "label"->"Layer 2 \[DoubleVerticalBar] rule(L1)"|>,
      <|"id"->"FlowLN",      "group"->"nest-rt", "label"->"Layer N \[DoubleVerticalBar] rule(LN-1)"|>,
      <|"id"->"NestStore",   "group"->"nest-rt", "label"->"$lastResults"|>,
      <|"id"->"SchedLoop",   "group"->"nest-rt", "label"->"ScheduledTask (every N s)"|>,
      <|"id"->"NestAPI",     "group"->"nest-rt", "label"->"GET /nest/results"|>,
      (* ── Confluent sentinels (convergencia de salud por capa) ── *)
      <|"id"->"SentCtrl",  "group"->"sentinel", "label"->"⦿ Ctrl Health"|>,
      <|"id"->"SentModel", "group"->"sentinel", "label"->"⦿ Model Health"|>,
      <|"id"->"SentExt",   "group"->"sentinel", "label"->"⦿ Ext Health"|>,
      <|"id"->"SysState",  "group"->"sentinel", "label"->"⦿ System State"|>
    };
    links = {
      (* ── Aristas base ──────────────────────────────────────────── *)
      <|"source"->"HTTP",      "target"->"Router"|>,
      <|"source"->"Router",    "target"->"Home"|>,
      <|"source"->"Router",    "target"->"Blog"|>,
      <|"source"->"Router",    "target"->"Contacto"|>,
      <|"source"->"Router",    "target"->"WActrl"|>,
      <|"source"->"Router",    "target"->"Nest"|>,
      <|"source"->"Router",    "target"->"Tasks"|>,
      <|"source"->"Router",    "target"->"Perf"|>,
      <|"source"->"Router",    "target"->"Theme"|>,
      <|"source"->"Home",      "target"->"Database"|>,
      <|"source"->"Home",      "target"->"Cache"|>,
      <|"source"->"Blog",      "target"->"Post"|>,
      <|"source"->"Post",      "target"->"Database"|>,
      <|"source"->"Contacto",  "target"->"Mailer"|>,
      <|"source"->"WActrl",    "target"->"WAmodel"|>,
      <|"source"->"Nest",      "target"->"NestSched"|>,
      <|"source"->"Tasks",     "target"->"TaskMgr"|>,
      <|"source"->"Scheduler", "target"->"TaskMgr"|>,
      <|"source"->"Perf",      "target"->"Cache"|>,
      <|"source"->"Perf",      "target"->"Assets"|>,
      <|"source"->"Theme",     "target"->"ThemeM"|>,
      <|"source"->"Home",      "target"->"Renderer"|>,
      <|"source"->"Blog",      "target"->"Renderer"|>,
      <|"source"->"Contacto",  "target"->"Renderer"|>,
      <|"source"->"Nest",      "target"->"Renderer"|>,
      <|"source"->"Tasks",     "target"->"Renderer"|>,
      <|"source"->"Perf",      "target"->"Renderer"|>,
      <|"source"->"Theme",     "target"->"Renderer"|>,
      <|"source"->"Database",  "target"->"SQLite"|>,
      <|"source"->"Mailer",    "target"->"SMTP"|>,
      <|"source"->"WAmodel",   "target"->"WAAPI"|>,
      <|"source"->"Renderer",  "target"->"Response"|>,
      (* ── NestScheduler runtime trigger ─────────────────────────── *)
      <|"source"->"NestTrigger","target"->"Router",      "rt"->True|>,
      <|"source"->"Nest",       "target"->"RulesParser", "rt"->True|>,
      <|"source"->"RulesParser","target"->"RecordsBuild","rt"->True|>,
      <|"source"->"RecordsBuild","target"->"SpecConvert","rt"->True|>,
      <|"source"->"SpecConvert","target"->"FlowL0",      "rt"->True|>,
      <|"source"->"FlowL0",    "target"->"FlowL1",       "rt"->True|>,
      <|"source"->"FlowL1",    "target"->"FlowL2",       "rt"->True|>,
      <|"source"->"FlowL2",    "target"->"FlowLN",       "rt"->True|>,
      <|"source"->"FlowLN",    "target"->"NestStore",    "rt"->True|>,
      <|"source"->"NestStore", "target"->"SchedLoop",    "rt"->True|>,
      <|"source"->"SchedLoop", "target"->"Nest",         "rt"->True|>,
      <|"source"->"NestStore", "target"->"NestAPI",      "rt"->True|>,
      <|"source"->"NestAPI",   "target"->"Response",     "rt"->True|>,
      (* ── Heartbeat self-loops (un ciclo por nodo monitoreado) ── *)
      <|"source"->"Router",    "target"->"Router",    "hb"->True|>,
      <|"source"->"Database",  "target"->"Database",  "hb"->True|>,
      <|"source"->"NestSched", "target"->"NestSched", "hb"->True|>,
      <|"source"->"TaskMgr",   "target"->"TaskMgr",   "hb"->True|>,
      <|"source"->"Scheduler", "target"->"Scheduler", "hb"->True|>,
      <|"source"->"Cache",     "target"->"Cache",     "hb"->True|>,
      <|"source"->"Renderer",  "target"->"Renderer",  "hb"->True|>,
      (* ── Convergencia hacia sentinels ─────────────────────────── *)
      <|"source"->"Home",    "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"Blog",    "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"Nest",    "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"Tasks",   "target"->"SentCtrl",  "conv"->True|>,
      <|"source"->"Database","target"->"SentModel", "conv"->True|>,
      <|"source"->"NestSched","target"->"SentModel","conv"->True|>,
      <|"source"->"TaskMgr", "target"->"SentModel", "conv"->True|>,
      <|"source"->"Cache",   "target"->"SentModel", "conv"->True|>,
      <|"source"->"SQLite",  "target"->"SentExt",   "conv"->True|>,
      <|"source"->"WAAPI",   "target"->"SentExt",   "conv"->True|>,
      <|"source"->"SMTP",    "target"->"SentExt",   "conv"->True|>,
      <|"source"->"SentCtrl", "target"->"SysState", "conv"->True|>,
      <|"source"->"SentModel","target"->"SysState", "conv"->True|>,
      <|"source"->"SentExt",  "target"->"SysState", "conv"->True|>
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
  Module[{taskSnap, nestInfo, cacheStats, dbOk, ts, nodes, groups},
    ts        = UnixTime[];
    taskSnap  = Quiet @ Check[PersonalSite`TaskManager`summary[], <||>];
    nestInfo  = Quiet @ Check[PersonalSite`NestScheduler`taskInfo[], <||>];
    cacheStats= Quiet @ Check[PersonalSite`Cache`stats[], <||>];
    dbOk      = Quiet @ Check[
      (PersonalSite`Database`execute["SELECT 1", {}]; True), False];

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
      (* Confluent sentinels *)
      "SentCtrl"   -> hOk["ctrl layer"],
      "SentModel"  -> If[dbOk, hOk["model layer"], hWarn["DB warn"]],
      "SentExt"    -> hUnknown["ext systems"],
      "SysState"   -> If[dbOk && Lookup[taskSnap,"running",0]>0,
                       hOk["system ok"],
                       hWarn["degraded"]]
    |>;

    groups = <|
      "entry"   -> "ok",
      "ctrl"    -> "ok",
      "model"   -> If[dbOk, "ok", "warn"],
      "ext"     -> "unknown",
      "nest-rt" -> If[TrueQ[Lookup[nestInfo,"active",False]], "ok", "idle"],
      "sentinel"-> If[dbOk, "ok", "warn"]
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

End[];
EndPackage[];
