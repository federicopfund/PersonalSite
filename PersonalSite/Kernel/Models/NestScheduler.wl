(* ::Package:: *)

(* PersonalSite`NestScheduler`
   --------------------------------------------------------------------------
   Motor de ejecucion paralela basado en NestGraph.

   Mapeo directo sobre la arquitectura de Flow.wl:
     - Cada nivel del NestGraph  =  una capa topologica de Flow (se ejecuta
       toda en paralelo como TaskObjects via SessionSubmit / ParallelSubmit).
     - Cada nodo del arbol        =  una tarea con su regla de transformacion
       y la referencia al nodo padre (dependencia de Flow).
     - La raiz (seed)             =  nodo sin dependencias; su "action" retorna
       el valor inicial.

   Uso basico:
       rules  = {2 # + 1 &, # + 14 &, # - 18 &};
       res    = PersonalSite`NestScheduler`run[rules, {1}, 3];
       json   = PersonalSite`NestScheduler`export[];

   Uso como ScheduledTask (re-ejecuta cada N segundos):
       PersonalSite`NestScheduler`schedule[rules, {1}, 3, 60, "session"];
   -------------------------------------------------------------------------- *)

BeginPackage["PersonalSite`NestScheduler`"];

build::usage =
  "build[rules, seeds, depth] construye el arbol de nodos y devuelve \
<|\"spec\", \"records\", \"depth\", \"nodeCount\"|> listo para Flow.";

run::usage =
  "run[rules, seeds, depth] o run[rules, seeds, depth, backend] ejecuta el \
NestGraph completo capa por capa en paralelo y devuelve los resultados.";

schedule::usage =
  "schedule[rules, seeds, depth, interval] o \
schedule[rules, seeds, depth, interval, backend] registra un ScheduledTask \
que re-ejecuta el NestGraph cada `interval` segundos.";

cancel::usage =
  "cancel[] elimina el ScheduledTask activo del NestScheduler.";

results::usage =
  "results[] devuelve la ultima ejecucion completa.";

export::usage =
  "export[] o export[\"json\"|\"csv\"] formatea los resultados para consumo \
desde Power BI u otras herramientas de BI.";

taskInfo::usage =
  "taskInfo[] devuelve <|\"active\", \"runCount\", \"lastRun\"|>.";

Begin["`Private`"];

(* --- Estado del modulo -------------------------------------------------- *)
$lastResults = <||>;
$schedTask   = None;
$lastRun     = None;
$runCount    = 0;

(* --- Generacion del arbol de nodos ------------------------------------- *)
(*  Cada nodo es: <|"id" -> n, "level" -> l, "value" -> v,
                    "parent" -> p_o_None, "ruleIdx" -> r|>
    Los IDs son enteros asignados en orden BFS (nivel 0, luego 1, ...).    *)

buildRecords[rules_List, seeds : {__}, depth_Integer] :=
  Module[{counter = 0, records = {}, current, addNode},

    addNode[level_, value_, parent_, ruleIdx_] :=
      (counter++;
       AppendTo[records,
         <|"id" -> counter, "level" -> level, "value" -> value,
           "parent" -> parent, "ruleIdx" -> ruleIdx|>];
       counter);

    (* Raices (nivel 0) *)
    current = MapIndexed[
      Function[{seed, idx}, {addNode[0, seed, None, 0], seed}],
      seeds];

    (* Niveles 1..depth *)
    Do[
      current = Flatten[
        Map[
          Function[{parentPair},
            MapIndexed[
              Function[{rule, ri},
                With[{cval = rule[Last[parentPair]],
                      cid  = addNode[l, rule[Last[parentPair]],
                                     First[parentPair], First[ri]]},
                  {cid, cval}]],
              rules]],
          current],
        1];
    , {l, 1, depth}];

    records
  ];

(* Convierte los registros en un spec compatible con PersonalSite`Flow`run.
   La "action" de cada nodo aplica su regla al resultado del nodo padre.
   La "action" de la raiz simplemente retorna el valor semilla. *)
recordsToSpec[records_List, rules_List] :=
  Association @ Map[
    Function[rec,
      With[{nid    = "n" <> ToString[rec["id"]],
            pid    = If[rec["parent"] === None, None,
                        "n" <> ToString[rec["parent"]]],
            ri     = rec["ruleIdx"],
            seed   = rec["value"]},
        nid -> <|
          "deps"   -> If[pid === None, {}, {pid}],
          "action" ->
            If[pid === None,
              (* raiz: retorna el seed directamente *)
              With[{v = seed}, (v &)],
              (* hijo: aplica la regla al resultado del padre *)
              With[{f = rules[[ri]], p = pid},
                Function[prev, f[prev[p]]]]
            ],
          "meta"   -> rec
        |>]],
    records];

(* --- API publica -------------------------------------------------------- *)

build[rules_List, seeds : {__}, depth_Integer] :=
  Module[{recs = buildRecords[rules, seeds, depth], spec},
    spec = recordsToSpec[recs, rules];
    <|"spec"      -> spec,
      "records"   -> recs,
      "depth"     -> depth,
      "ruleCount" -> Length[rules],
      "seedCount" -> Length[seeds],
      "nodeCount" -> Length[recs]|>
  ];

run[rules_List, seeds : {__}, depth_Integer] :=
  run[rules, seeds, depth, "session"];

run[rules_List, seeds : {__}, depth_Integer, backend_String] :=
  Module[{built, flowResult, t0 = AbsoluteTime[]},
    built      = build[rules, seeds, depth];
    flowResult = PersonalSite`Flow`run[built["spec"], backend];
    $lastResults = <|
      "built"    -> built,
      "flow"     -> flowResult,
      "elapsed"  -> AbsoluteTime[] - t0,
      "runAt"    -> Now,
      "backend"  -> backend
    |>;
    $runCount++;
    $lastRun = Now;
    $lastResults
  ];

schedule[rules_List, seeds : {__}, depth_Integer, interval : _?NumericQ] :=
  schedule[rules, seeds, depth, interval, "session"];

schedule[rules_List, seeds : {__}, depth_Integer,
         interval : _?NumericQ, backend_String] :=
  (If[$schedTask =!= None, Quiet @ RemoveScheduledTask[$schedTask]];
   $schedTask = RunScheduledTask[
     run[rules, seeds, depth, backend],
     interval];
   $schedTask);

cancel[] :=
  If[$schedTask =!= None,
    Quiet @ RemoveScheduledTask[$schedTask];
    $schedTask = None; True,
    False];

results[] := $lastResults;

taskInfo[] := <|
  "active"   -> ($schedTask =!= None),
  "runCount" -> $runCount,
  "lastRun"  -> $lastRun
|>;

(* --- Export para Power BI ----------------------------------------------- *)
(*  JSON:  array de objetos con id, level, value, parent, ruleIdx, result
    CSV:   mismo contenido separado por comas                               *)

$emptyJSON = "[]";
$emptyCSV  = "id,level,value,parent,ruleIdx,result";

nodeRow[rec_, flowRes_Association] :=
  <|"id"      -> rec["id"],
    "level"   -> rec["level"],
    "value"   -> rec["value"],
    "parent"  -> If[rec["parent"] === None, Null, rec["parent"]],
    "ruleIdx" -> rec["ruleIdx"],
    "result"  -> Quiet @ Lookup[flowRes, "n" <> ToString[rec["id"]], Null]|>;

export[] := export["json"];

export["json"] :=
  If[! KeyExistsQ[$lastResults, "built"],
    $emptyJSON,
    ExportString[
      Map[nodeRow[#, $lastResults["flow"]["results"]] &,
          $lastResults["built"]["records"]],
      "JSON"]
  ];

export["csv"] :=
  If[! KeyExistsQ[$lastResults, "built"],
    $emptyCSV,
    $emptyCSV <> "\n" <>
    StringRiffle[
      Map[Function[rec,
        With[{r = nodeRow[rec, $lastResults["flow"]["results"]]},
          StringRiffle[
            ToString /@ {r["id"], r["level"], r["value"],
                         If[r["parent"] === Null, "", r["parent"]],
                         r["ruleIdx"],
                         If[r["result"] === Null, "", r["result"]]},
            ","]]],
        $lastResults["built"]["records"]],
      "\n"]
  ];

End[];
EndPackage[];
