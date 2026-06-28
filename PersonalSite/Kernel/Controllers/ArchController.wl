(* ::Package:: *)

(* PersonalSite`Controller`  (parte: arch)
   --------------------------------------------------------------------------
   Pagina /arch: Graph3D del flujo de estados y arquitectura del sistema.
   La imagen PNG se genera una vez por kernel (memoizada) y se sirve desde
   /arch/graph como Content-Type: image/png. *)

BeginPackage["PersonalSite`Controller`"];

arch::usage      = "arch[req] renderiza /arch: arquitectura del sistema.";
archGraph::usage = "archGraph[req] sirve el PNG del Graph3D (cacheado).";

Begin["`Private`"];

(* ── Construccion del grafo (se evalua una vez, luego memoizado) ─────── *)
$archPNG := $archPNG = buildArchPNG[];

buildArchPNG[] :=
  Module[{verts, edges, coords, vStyle, eStyle, g, bytes},

    verts = {
      (* Entrada *)
      "HTTP",
      (* Router *)
      "Router",
      (* Controllers *)
      "Home",  "Blog",   "Contacto", "WA\[Dash]ctrl",
      "Nest",  "Tasks",  "Perf",     "Theme",
      (* Models *)
      "Database", "Post",      "Mailer",    "WA\[Dash]model",
      "NestSched","TaskMgr",   "Scheduler", "Cache",
      "Assets",   "ThemeM",
      (* View *)
      "Renderer",
      (* Externos *)
      "SQLite",   "WA\[Dash]API", "SMTP",
      (* Salida *)
      "Response"
    };

    edges = {
      (* Entry -> Router *)
      "HTTP" -> "Router",
      (* Router -> Controllers *)
      "Router" -> "Home",      "Router" -> "Blog",
      "Router" -> "Contacto",  "Router" -> "WA\[Dash]ctrl",
      "Router" -> "Nest",      "Router" -> "Tasks",
      "Router" -> "Perf",      "Router" -> "Theme",
      (* Controllers -> Models *)
      "Home"      -> "Database",    "Home"      -> "Cache",
      "Blog"      -> "Post",        "Post"      -> "Database",
      "Contacto"  -> "Mailer",
      "WA\[Dash]ctrl"  -> "WA\[Dash]model",
      "Nest"      -> "NestSched",
      "Tasks"     -> "TaskMgr",     "Scheduler" -> "TaskMgr",
      "Perf"      -> "Cache",       "Perf"      -> "Assets",
      "Theme"     -> "ThemeM",
      (* Controllers -> Renderer *)
      "Home" -> "Renderer",  "Blog"     -> "Renderer",
      "Contacto" -> "Renderer", "Nest" -> "Renderer",
      "Tasks" -> "Renderer", "Perf"     -> "Renderer",
      "Theme" -> "Renderer",
      (* Models -> Externos *)
      "Database" -> "SQLite",
      "Mailer"   -> "SMTP",
      "WA\[Dash]model" -> "WA\[Dash]API",
      (* View -> Salida *)
      "Renderer" -> "Response"
    };

    (* Coordenadas 3D por capa (z define la profundidad del flujo) *)
    coords = {
      "HTTP"           -> {0,  0,  5},
      "Router"         -> {0,  0,  4},
      (* Controllers: 8 nodos en dos filas, z=3 *)
      "Home"           -> {-3.5, -1,  3},  "Blog"          -> {-1.5, -1,  3},
      "Contacto"       -> {0.5,  -1,  3},  "WA\[Dash]ctrl" -> {2.5,  -1,  3},
      "Nest"           -> {-3.5,  1,  3},  "Tasks"         -> {-1.5,  1,  3},
      "Perf"           -> {0.5,   1,  3},  "Theme"         -> {2.5,   1,  3},
      (* Models: z=2 *)
      "Database"       -> {-3.5, -1,  2},  "Post"          -> {-1.5, -1,  2},
      "Mailer"         -> {0.5,  -1,  2},  "WA\[Dash]model"-> {2.5,  -1,  2},
      "NestSched"      -> {-3.5,  1,  2},  "TaskMgr"       -> {-1.5,  1,  2},
      "Scheduler"      -> {0,     2.8, 2.5},  (* flota sobre los modelos *)
      "Cache"          -> {0.5,   1,  2},  "Assets"        -> {2.5,   1,  2},
      "ThemeM"         -> {3.5,   0,  2},
      (* View: centro, z=1 *)
      "Renderer"       -> {0,  0,  1},
      (* Externos: z=0 *)
      "SQLite"         -> {-3,  0,  0},
      "WA\[Dash]API"   -> { 3,  0,  0},
      "SMTP"           -> { 0, -2,  0},
      (* Salida: z=-1 *)
      "Response"       -> {0,  0, -1}
    };

    (* Colores por capa *)
    vStyle = Flatten[{
      (* Entrada / Salida: azul *)
      {"HTTP","Response"} -> Directive[RGBColor[0.2,0.55,1.0], Specularity[White,20], EdgeForm[None]],
      (* Router: gris *)
      "Router" -> Directive[RGBColor[0.5,0.5,0.55], Specularity[White,20], EdgeForm[None]],
      (* Controllers: naranja *)
      (# -> Directive[RGBColor[0.92,0.5,0.1], Specularity[White,20], EdgeForm[None]] &) /@
        {"Home","Blog","Contacto","WA\[Dash]ctrl","Nest","Tasks","Perf","Theme"},
      (* Models: verde *)
      (# -> Directive[RGBColor[0.2,0.72,0.38], Specularity[White,20], EdgeForm[None]] &) /@
        {"Database","Post","Mailer","WA\[Dash]model",
         "NestSched","TaskMgr","Scheduler","Cache","Assets","ThemeM"},
      (* Renderer: purpura *)
      "Renderer" -> Directive[RGBColor[0.72,0.28,0.88], Specularity[White,20], EdgeForm[None]],
      (* Externos: rojo *)
      (# -> Directive[RGBColor[0.88,0.22,0.22], Specularity[White,20], EdgeForm[None]] &) /@
        {"SQLite","WA\[Dash]API","SMTP"}
    }, 1];

    eStyle = Directive[RGBColor[0.55,0.55,0.6], Opacity[0.55], Thickness[0.004]];

    g = Graph3D[
      verts, edges,
      VertexCoordinates -> coords,
      VertexLabels   -> (# -> Placed[#, Above,
                           Style[#, White, Bold, FontSize -> 10]] & /@ verts),
      VertexStyle    -> vStyle,
      VertexSize     -> Join[
        {"HTTP"->0.42,"Router"->0.38,"Renderer"->0.38,"Response"->0.42},
        (# -> 0.28 & /@ {"Home","Blog","Contacto","WA\[Dash]ctrl",
                          "Nest","Tasks","Perf","Theme"}),
        (# -> 0.22 & /@ {"Database","Post","Mailer","WA\[Dash]model",
                          "NestSched","TaskMgr","Scheduler",
                          "Cache","Assets","ThemeM"}),
        (# -> 0.22 & /@ {"SQLite","WA\[Dash]API","SMTP"})
      ],
      EdgeStyle      -> eStyle,
      Background     -> GrayLevel[0.06],
      Lighting       -> {{"Ambient", GrayLevel[0.4]},
                         {"Directional", White, {1,1,3}},
                         {"Directional", GrayLevel[0.3], {-1,-1,2}}},
      PlotRange      -> {{-4.5,4.5},{-3,3.5},{-1.5,5.5}},
      Boxed          -> False,
      ImageSize      -> {1000, 640}
    ];

    Quiet @ Check[ExportByteArray[g, "PNG"], $Failed]
  ];

(* ── Endpoints ────────────────────────────────────────────────────────── *)

(* GET /arch/graph  →  PNG del grafo (cacheado en memoria) *)
archGraph[req_] :=
  Module[{bytes = $archPNG},
    If[bytes === $Failed,
      HTTPResponse["Graph generation failed", <|"StatusCode" -> 500|>],
      HTTPResponse[bytes, <|"Headers" -> <|
        "Content-Type"  -> "image/png",
        "Cache-Control" -> "public, max-age=7200"
      |>|>]
    ]
  ];

(* GET /arch  →  pagina HTML con el grafo embebido *)
arch[req_] :=
  PersonalSite`View`render["arch", <||>];

End[];
EndPackage[];
