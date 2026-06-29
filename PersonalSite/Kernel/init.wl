(* ::Package:: *)

(* PersonalSite  —  cargador maestro y fachada publica del paclet.
   --------------------------------------------------------------------------
   Lo carga Needs["PersonalSite`"] via la extension Kernel del PacletInfo.

   El paclet esta dividido en sub-paquetes auto-contenidos, cada uno con su
   propio contexto (BeginPackage / EndPackage). Este archivo:
     1. resuelve $Root (directorio base del paclet),
     2. carga los modulos en orden de dependencias,
     3. expone la fachada publica PersonalSite`Router[]. *)

(* --- 1. Directorio base ------------------------------------------------- *)
PersonalSite`$Root =
  With[{p = PacletObject["PersonalSite"]},
    If[Head[p] === PacletObject,
      p["Location"],
      DirectoryName[$InputFileName, 2]]  (* dev: Kernel/ -> raiz del paclet *)
  ];

(* --- 2. Carga de modulos en orden de dependencias ----------------------- *)
(*  Config  ->  Database  ->  Post  ->  WolframAlpha  ->  Mailer
              ->  Settings  ->  Theme  ->  Scheduler
              ->  View  ->  Controllers  ->  Routing                     *)
With[{load = Function[parts,
       Get[FileNameJoin[Join[{PersonalSite`$Root, "Kernel"}, parts]]]]},
  load[{"Config.wl"}];
  load[{"Models", "Database.wl"}];
  load[{"Models", "Post.wl"}];
  load[{"Models", "WolframAlpha.wl"}];
  load[{"Models", "Mailer.wl"}];
  load[{"Models", "Settings.wl"}];
  load[{"Models", "Theme.wl"}];
  load[{"Models", "Flow.wl"}];
  load[{"Models", "Cache.wl"}];
  load[{"Models", "Assets.wl"}];
  load[{"Models", "NestScheduler.wl"}];
  load[{"Models", "SessionFSM.wl"}];
  load[{"Models", "SessionStore.wl"}];
  load[{"Models", "DevStyle.wl"}];
  load[{"Models", "TaskConfig.wl"}];
  load[{"Models", "TaskManager.wl"}];
  load[{"Models", "Scheduler.wl"}];
  load[{"FrontEnd", "StyleEngine.wl"}];
  load[{"FrontEnd", "Output.wl"}];
  load[{"Views", "Renderer.wl"}];
  load[{"Controllers", "HomeController.wl"}];
  load[{"Controllers", "BlogController.wl"}];
  load[{"Controllers", "WolframController.wl"}];
  load[{"Controllers", "ContactController.wl"}];
  load[{"Controllers", "ThemeController.wl"}];
  load[{"Controllers", "FlowController.wl"}];
  load[{"Controllers", "PerfController.wl"}];
  load[{"Controllers", "NestController.wl"}];
  load[{"Controllers", "TaskController.wl"}];
  load[{"Controllers", "ArchController.wl"}];
  load[{"Controllers", "KernelController.wl"}];
  load[{"Controllers", "StyleController.wl"}];
  load[{"Controllers", "RuliologyController.wl"}];
  load[{"Controllers", "SessionController.wl"}];
  load[{"Router.wl"}];
];

(* --- 3. Fachada publica ------------------------------------------------- *)
BeginPackage["PersonalSite`"];

PersonalSite`$Root::usage =
  "PersonalSite`$Root es el directorio base del paclet.";

Router::usage =
  "PersonalSite`Router[] devuelve el URLDispatcher que maneja todas las rutas del sitio.";

Begin["`Private`"];

Router[] := PersonalSite`Routing`dispatcher[];

End[];
EndPackage[];