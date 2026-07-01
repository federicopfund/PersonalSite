(* Production entrypoint *)

(*
   Entrypoint servido por Wolfram Web Engine for Python en modo file-dispatcher:
       python -m wolframwebengine --poolsize N deploy/app.wl

   Todo el setup pesado (cargar el paclet, correr migraciones, compilar
   plantillas, abrir la conexión a la base) ocurre UNA sola vez por kernel del
   pool. Cada request posterior sólo devuelve el router ya construido. *)

If[! TrueQ[$PersonalSiteReady],
  Module[{root = Environment["PERSONALSITE_ROOT"]},
    (* PERSONALSITE_ROOT debe apuntar al directorio que CONTIENE PersonalSite/.
       En desarrollo: ruta al fuente. En producción (Docker): /app.
       PacletDirectoryLoad registra todos los paclets en ese directorio. *)
    Which[
      StringQ[root] && DirectoryQ[root],
        PacletDirectoryLoad[root],
      DirectoryQ["/app"],
        PacletDirectoryLoad["/app"],
      True,
        Print["[PersonalSite] ADVERTENCIA: no se encontró el directorio del paclet."]
    ]
  ];
  Needs["PersonalSite`"];
  (* Arranca las tareas de runtime (heartbeat + cache warm-up). Es idempotente
     por kernel y se protege con Check para que un fallo al programar nunca
     interrumpa el servido de requests. *)
  Quiet @ Check[PersonalSite`Scheduler`start[], $Failed];
  (* Precalienta SOLO el cache barato (tarjetas) para no penalizar el arranque.
     El asset pesado se computa perezosamente (en subkernel) al primer /perf y
     lo mantiene la ScheduledTask metric-refresh. *)
  Quiet @ Check[PersonalSite`Assets`refreshCards[], Null];
  (* Calentar el modulo A2A: la primera evaluacion carga timezone DB + JSON libs
     de WL (~19 s); las siguientes son < 100 ms. Depth=1 = 4 nodos, muy rapido. *)
  Quiet @ Check[PersonalSite`AgentMesh`run[1, 1, "sync"], Null];
  $PersonalSiteReady = True;
];

PersonalSite`Router[]