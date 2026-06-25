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
  $PersonalSiteReady = True;
];

PersonalSite`Router[]