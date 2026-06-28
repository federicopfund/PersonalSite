(* ::Package:: *)

(* PersonalSite`Scheduler`
   --------------------------------------------------------------------------
   Tareas de runtime que mantienen el framework "caliente" mientras el kernel
   del pool de Wolfram Web Engine sigue vivo. Usa RunScheduledTask para correr
   trabajo periodico en segundo plano sin bloquear el servido de requests.

   Es idempotente por kernel: start[] solo registra las tareas una vez, de modo
   que cada kernel del pool corre exactamente un heartbeat y un warm-up. *)

BeginPackage["PersonalSite`Scheduler`"];

start::usage =
  "start[] registra (una sola vez por kernel) las ScheduledTask de runtime y \
devuelve la Association de tareas activas.";

stop::usage =
  "stop[] elimina todas las ScheduledTask registradas por el sitio.";

status::usage =
  "status[] devuelve una Association con metricas del runtime: uptime, \
heartbeats, warm-ups de cache y nombres de tareas activas.";

Begin["`Private`"];

$tasks     = <||>;
$started   = False;
$startedAt = None;
$beats     = 0;
$warms     = 0;

(* App 1 - Cache warm-up: fuerza una consulta para mantener caliente la
   conexion SQLite y las plantillas ya compiladas en memoria. *)
warmCache[] :=
  Quiet @ Check[PersonalSite`Post`recent[10]; $warms++; True, False];

(* App 2 - Heartbeat / health check: marca de vida del kernel del pool. *)
heartbeat[] := ($beats++; True);

start[] :=
  If[TrueQ[$started],
    $tasks,
    (
      (* RunScheduledTask[expr, segundos]: el intervalo va como numero de
         segundos y el cuerpo se mantiene sin evaluar (HoldFirst), corriendo en
         cada disparo del scheduler preventivo del kernel. *)
      $tasks = <|
        "heartbeat"     -> RunScheduledTask[heartbeat[],  30],   (* cada 30 s  *)
        "cache-warm"    -> RunScheduledTask[warmCache[], 300],   (* cada 5 min *)
        "theme-rotate"  -> RunScheduledTask[PersonalSite`Theme`tick[], 10],         (* cada 10 s  *)
        "cards-refresh" -> RunScheduledTask[PersonalSite`Assets`refreshCards[], 20], (* cada 20 s, barato *)
        "metric-refresh"-> RunScheduledTask[PersonalSite`Assets`refreshMetric[], 300], (* cada 5 min, pesado *)
        (* NestScheduler: re-ejecuta el NestGraph de referencia cada 5 min.
           Los resultados quedan en memoria listos para /nest/results (Power BI). *)
        "nest-refresh"  -> RunScheduledTask[
          PersonalSite`NestScheduler`run[{2#+1&, #+14&, #-18&}, {1}, 3, "session"],
          300]  (* cada 5 min *)
      |>;
      $startedAt = Now;
      $started   = True;
      $tasks
    )
  ];

stop[] :=
  (
    RemoveScheduledTask /@ Values[$tasks];
    $tasks   = <||>;
    $started = False;
  );

status[] := <|
  "running"    -> $started,
  "startedAt"  -> $startedAt,
  "uptime"     -> If[$startedAt === None, Quantity[0, "Seconds"], Now - $startedAt],
  "heartbeats" -> $beats,
  "cacheWarms" -> $warms,
  "tasks"      -> Keys[$tasks]
|>;

End[];
EndPackage[];
