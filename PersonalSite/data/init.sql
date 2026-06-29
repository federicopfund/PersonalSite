-- PersonalSite/data/init.sql
-- Schema y datos de ejemplo para desarrollo local.
-- Ejecutar con: sqlite3 data/site.db < PersonalSite/data/init.sql

CREATE TABLE IF NOT EXISTS posts (
    id         INTEGER PRIMARY KEY AUTOINCREMENT,
    slug       TEXT    NOT NULL UNIQUE,
    title      TEXT    NOT NULL,
    summary    TEXT    NOT NULL DEFAULT '',
    body       TEXT    NOT NULL DEFAULT '',
    date       TEXT    NOT NULL  -- ISO-8601: YYYY-MM-DD
);

-- ── Scheduler task config (persistent, editable desde UI) ──────────────
-- dag_order : nivel en el NestGraph layered digraph (0 = raíz, 1 = L1, ...)
--             refleja la cardinalidad de la estructura ruliar
-- deps      : JSON array de task_id dependencias  e.g. '["heartbeat"]'
-- action_code: código Wolfram evaluado por ToExpression en el kernel
CREATE TABLE IF NOT EXISTS scheduler_tasks (
    id           INTEGER PRIMARY KEY AUTOINCREMENT,
    task_id      TEXT    NOT NULL UNIQUE,
    label        TEXT    NOT NULL,
    group_name   TEXT    NOT NULL DEFAULT 'user',
    interval_s   INTEGER NOT NULL DEFAULT 60,
    enabled      INTEGER NOT NULL DEFAULT 1,
    deps         TEXT    NOT NULL DEFAULT '[]',
    dag_order    INTEGER NOT NULL DEFAULT 0,
    action_code  TEXT    NOT NULL DEFAULT 'Function[True]',
    created_at   TEXT    NOT NULL DEFAULT (datetime('now')),
    updated_at   TEXT    NOT NULL DEFAULT (datetime('now'))
);

-- Trigger para actualizar updated_at automáticamente
CREATE TRIGGER IF NOT EXISTS scheduler_tasks_updated
  AFTER UPDATE ON scheduler_tasks
  BEGIN
    UPDATE scheduler_tasks SET updated_at = datetime('now') WHERE id = NEW.id;
  END;

-- Seed: 6 tareas del sistema (idempotente via INSERT OR IGNORE)
INSERT OR IGNORE INTO scheduler_tasks
  (task_id, label, group_name, interval_s, enabled, deps, dag_order, action_code)
VALUES
  ('heartbeat',     'Heartbeat',                   'system', 30,  1, '[]',                '0', 'Function[True]'),
  ('cache-warm',    'Cache warm-up',               'system', 300, 1, '["heartbeat"]',     '1', 'Function[Quiet@Check[PersonalSite`Post`recent[10];True,False]]'),
  ('theme-rotate',  'Theme rotation tick',         'theme',  10,  1, '["heartbeat"]',     '1', 'Function[PersonalSite`Theme`tick[]]'),
  ('cards-refresh', 'Cards refresh (DB)',          'cache',  20,  1, '["cache-warm"]',    '2', 'Function[PersonalSite`Assets`refreshCards[]]'),
  ('nest-refresh',  'NestGraph {2x+1,x+14,x-18}', 'flow',   300, 1, '["cache-warm"]',    '2', 'Function[PersonalSite`NestScheduler`run[{2#+1&,#+14&,#-18&},{1},3,"session"]]'),
  ('metric-refresh','Metric refresh (heavy)',      'cache',  300, 1, '["cards-refresh"]', '3', 'Function[PersonalSite`Assets`refreshMetric[]]');

-- Seed: 5 tareas dev SCSS (disabled=0 — no impactan produccion)
-- DAG: scss-watch -> scss-compile -> {css-version, css-cache-bust} -> scss-report
INSERT OR IGNORE INTO scheduler_tasks
  (task_id, label, group_name, interval_s, enabled, deps, dag_order, action_code)
VALUES
  ('scss-watch',    'SCSS watch (detect CSS changes)',            'dev', 3,  0, '[]',               '0', 'Function[PersonalSite`DevStyle`detect[]]'),
  ('scss-compile',  'SCSS compile (sass / npx sass)',             'dev', 3,  0, '["scss-watch"]',   '1', 'Function[PersonalSite`DevStyle`compile[]]'),
  ('css-version',   'CSS version hash (CRC32 -> css-version.json)','dev',3, 0, '["scss-compile"]', '2', 'Function[PersonalSite`DevStyle`hashCss[]]'),
  ('css-cache-bust','CSS cache bust (clear WL fragment cache)',   'dev', 3,  0, '["scss-compile"]', '2', 'Function[PersonalSite`DevStyle`cacheBust[]]'),
  ('scss-report',   'SCSS pipeline report',                       'dev', 30, 0, '["css-version"]',  '3', 'Function[PersonalSite`DevStyle`report[]]');



-- ── Session store (NestGraph permission FSM) ───────────────────────────
-- state     : unauthenticated | active | elevated | suspended | expired | revoked
-- role      : 1=reader  2=writer  3=admin  (maps to NestGraph depth)
-- permissions: JSON array derivado del permission NestGraph (3 rules, depth=3)
-- meta       : JSON libre (IP, user-agent, origin, etc.)
CREATE TABLE IF NOT EXISTS sessions (
    session_id  TEXT    NOT NULL PRIMARY KEY,
    user_id     TEXT    NOT NULL,
    role        INTEGER NOT NULL DEFAULT 1,
    state       TEXT    NOT NULL DEFAULT 'active',
    permissions TEXT    NOT NULL DEFAULT '[]',
    meta        TEXT    NOT NULL DEFAULT '{}',
    created_at  TEXT    NOT NULL DEFAULT '1970-01-01 00:00:00',
    expires_at  TEXT    NOT NULL,
    last_seen   TEXT    NOT NULL DEFAULT '1970-01-01 00:00:00'
);

CREATE INDEX IF NOT EXISTS idx_sessions_expires ON sessions(expires_at);
CREATE INDEX IF NOT EXISTS idx_sessions_user    ON sessions(user_id);
CREATE INDEX IF NOT EXISTS idx_sessions_state   ON sessions(state);

-- Datos de ejemplo
INSERT OR IGNORE INTO posts (slug, title, summary, body, date) VALUES
  ('hola-wolfram',
   'Hola desde Wolfram Language',
   'Un primer vistazo al ecosistema Wolfram para desarrollo web.',
   '<p>Wolfram Language no es solo para cómputo simbólico. Con <code>HTTPResponse</code> y <code>URLDispatcher</code> podés construir aplicaciones web completas.</p>',
   '2026-06-01'),

  ('paclets-en-produccion',
   'Paclets en producción con Docker',
   'Cómo empaquetar un paclet Wolfram y servirlo con Wolfram Web Engine.',
   '<p>Un paclet es la unidad de distribución de código en Wolfram. Combinado con Docker, conseguís un deploy reproducible en minutos.</p>',
   '2026-06-10'),

  ('sqlite-wolfram',
   'SQLite como backend con EntityStore',
   'Usando RelationalDatabase y EntityStore para conectar WL a SQLite sin ORM.',
   '<p>La combinación <code>RelationalDatabase</code> + <code>EntityStore</code> + <code>RegisterEntityStore</code> es la forma idiomática de acceder a bases relacionales desde Wolfram Language.</p>',
   '2026-06-20'),

  ('notebook-wolfram-cloud',
   'Notebook interactivo desde Wolfram Cloud',
   'Explorá computación simbólica en vivo: un notebook de Wolfram Cloud embebido directamente en el blog.',
   '<p>Wolfram Cloud permite publicar notebooks interactivos y embeberlos en cualquier página web. El siguiente notebook muestra algunas de las capacidades del lenguaje Wolfram: manipulaciones simbólicas, visualizaciones y código ejecutable en el navegador.</p>
<div class="nb-embed">
  <div class="nb-embed__frame">
    <iframe src="https://www.wolframcloud.com/obj/98ad2e82-d300-44bc-8888-bbddcfcba78f" frameborder="0" allowfullscreen loading="lazy" title="Notebook Wolfram Cloud"></iframe>
  </div>
  <div class="nb-embed__caption">
    Notebook interactivo &mdash;
    <a href="https://www.wolframcloud.com/obj/98ad2e82-d300-44bc-8888-bbddcfcba78f" target="_blank" rel="noopener">abrir en Wolfram Cloud</a>
  </div>
</div>
<p>El notebook vive en Wolfram Cloud y se actualiza en tiempo real: cualquier cambio que hagas allí aparece aquí de forma inmediata, sin necesidad de redesplegar el sitio.</p>',
   '2026-06-25');

INSERT OR IGNORE INTO posts (slug, title, summary, body, date) VALUES
  ('scheduled-tasks-runtime',
   'ScheduledTask: mantené tu framework en runtime',
   'Siete aplicaciones de ScheduledTask para mantener vivo un framework Wolfram, con una simulación del runtime que corre en tiempo real dentro del artículo.',
   '<p>Un sitio servido con Wolfram Language no se apaga entre visitas: el kernel del pool de <code>Wolfram Web Engine</code> sigue vivo en memoria. Eso abre una puerta enorme &mdash; podemos correr trabajo en segundo plano que mantiene el framework caliente, sincronizado y sano sin bloquear ni una sola request.</p>
<p>La herramienta para eso es la familia <code>ScheduledTask</code>. Repasamos siete aplicaciones prácticas, las ves correr en una simulación en vivo dentro de esta misma página, y al final mostramos el ejemplo real que mantiene este sitio en runtime.</p>
<h2>El runtime que no se duerme</h2>
<p>Wolfram Language programa trabajo periódico de forma declarativa. Definís <em>qué</em> correr y <em>cada cuánto</em>, y el kernel se encarga del resto:</p>
<ul>
  <li><code>ScheduledTask[expr, t]</code> &mdash; describe una tarea: evaluar <code>expr</code> cada <code>t</code>.</li>
  <li><code>RunScheduledTask[...]</code> &mdash; la pone a correr en segundo plano.</li>
  <li><code>SessionSubmit</code> / <code>LocalSubmit</code> &mdash; envían trabajo a la sesión o a un kernel subordinado.</li>
  <li><code>TaskExecute</code> &mdash; dispara una tarea a demanda, sin esperar al próximo tick.</li>
  <li><code>RemoveScheduledTask</code> &mdash; la detiene y libera recursos.</li>
</ul>
<pre><code>(* Una tarea que corre cada 30 segundos, en segundo plano *)
task = RunScheduledTask[
  PersonalSite`Post`recent[10],   (* mantiene la cache caliente *)
  30                              (* intervalo en segundos *)
];

(* Detenerla cuando ya no haga falta *)
RemoveScheduledTask[task];</code></pre>
<h2>7 aplicaciones para mantener el framework en runtime</h2>
<ol>
  <li><strong>Cache warm-up.</strong> Regenera el HTML de las vistas más visitadas y mantiene caliente la conexión SQLite y las plantillas ya compiladas, para que ninguna visita pague el costo del primer render.</li>
  <li><strong>Heartbeat / health check.</strong> Un latido periódico confirma que el kernel del pool sigue vivo y que la base responde; si falla, se puede alertar o reciclar el worker.</li>
  <li><strong>Publicación programada.</strong> Una tarea revisa los posts con fecha futura y los libera automáticamente al llegar su momento, sin redeploy.</li>
  <li><strong>Sincronización de datos externos.</strong> Refresca indicadores de Wolfram|Alpha, feeds o APIs cada cierto intervalo y los deja cacheados para servirlos al instante.</li>
  <li><strong>Limpieza y mantenimiento.</strong> Purga sesiones expiradas, rota logs y corre un <code>VACUUM</code> periódico sobre SQLite para que la base no se degrade.</li>
  <li><strong>Newsletter / digest.</strong> Arma y encola en el <code>Mailer</code> un resumen periódico de las novedades del blog para los suscriptores.</li>
  <li><strong>Métricas en vivo.</strong> Agrega visitas y eventos en ventanas de tiempo y publica un panel de analítica que se actualiza solo.</li>
</ol>
<h2>Simulación del runtime en vivo</h2>
<p>El panel de abajo <strong>corre en tu navegador en tiempo real</strong>: cada barra es una <code>ScheduledTask</code> con su propio intervalo, y la consola registra cada ejecución tal como lo haría el kernel. Podés pausar y reiniciar el runtime.</p>
<div class="sched-sim" id="schedSim">
  <div class="sched-sim__bar">
    <span class="sched-sim__status" id="schedStatus">
      <span class="sched-dot"></span>
      <span class="sched-status-text"></span>
    </span>
    <span class="sched-metric">Uptime<strong id="schedUptime">00:00</strong></span>
    <span class="sched-metric">Ticks<strong id="schedTicks">0</strong></span>
    <span class="sched-metric">Ejecuciones<strong id="schedRuns">0</strong></span>
    <span class="sched-sim__controls">
      <button type="button" class="sched-btn" id="schedToggle">Pausar</button>
      <button type="button" class="sched-btn sched-btn--ghost" id="schedReset">Reiniciar</button>
    </span>
  </div>
  <div class="sched-sim__grid">
    <div class="sched-tasks" id="schedTasks"></div>
    <div class="sched-console">
      <div class="sched-console__head">Consola de ejecución</div>
      <ul class="sched-log" id="schedLog"></ul>
    </div>
  </div>
</div>
<script>
(function(){
  var root=document.getElementById("schedSim");
  if(!root)return;
  var tasksEl=document.getElementById("schedTasks");
  var logEl=document.getElementById("schedLog");
  var uptimeEl=document.getElementById("schedUptime");
  var ticksEl=document.getElementById("schedTicks");
  var runsEl=document.getElementById("schedRuns");
  var statusEl=document.getElementById("schedStatus");
  var toggleBtn=document.getElementById("schedToggle");
  var resetBtn=document.getElementById("schedReset");

  var defs=[
    {id:"warm",  name:"Cache warm-up",          every:3,  msg:"Cache de /blog regenerada (10 posts calientes)"},
    {id:"beat",  name:"Heartbeat",              every:2,  msg:"Kernel vivo, conexion SQLite OK"},
    {id:"pub",   name:"Publicacion programada", every:8,  msg:"Post liberado segun su fecha de publicacion"},
    {id:"sync",  name:"Sync Wolfram|Alpha",     every:5,  msg:"Indicadores externos actualizados"},
    {id:"gc",    name:"Limpieza / sesiones",    every:7,  msg:"Sesiones expiradas purgadas, logs rotados"},
    {id:"mail",  name:"Newsletter digest",      every:13, msg:"Resumen semanal encolado en el Mailer"},
    {id:"stats", name:"Metricas en vivo",       every:4,  msg:"Visitas agregadas y publicadas"}
  ];

  var state=null, timer=null;

  function pad(n){return (n<10?"0":"")+n;}
  function fmt(s){return pad(Math.floor(s/60))+":"+pad(s%60);}

  function build(){
    state={secs:0,runs:0,running:true,tasks:defs.map(function(d){
      return {id:d.id,name:d.name,every:d.every,msg:d.msg,left:d.every,count:0};
    })};
    tasksEl.innerHTML=state.tasks.map(function(t){
      return `
        <div class="sched-task" data-id="${t.id}">
          <div class="sched-task__head">
            <span class="sched-task__name">${t.name}</span>
            <span class="sched-task__interval">cada ${t.every}s</span>
          </div>
          <div class="sched-task__bar"><span class="sched-task__fill"></span></div>
          <div class="sched-task__count">0 ejecuciones</div>
        </div>`;
    }).join("");
    logEl.innerHTML="";
    statusEl.classList.remove("is-paused");
    toggleBtn.textContent="Pausar";
    render();
    log("Runtime iniciado: 7 ScheduledTask registradas con RunScheduledTask");
  }

  function render(){
    uptimeEl.textContent=fmt(state.secs);
    ticksEl.textContent=state.secs;
    runsEl.textContent=state.runs;
    state.tasks.forEach(function(t){
      var el=tasksEl.querySelector(`[data-id="${t.id}"]`);
      if(!el)return;
      var pct=Math.round((t.every-t.left)/t.every*100);
      el.querySelector(".sched-task__fill").style.width=pct+"%";
      el.querySelector(".sched-task__count").textContent=t.count+" ejecuciones";
    });
  }

  function log(text){
    var li=document.createElement("li");
    li.innerHTML=`<span class="sched-log__t">${fmt(state.secs)}</span> ${text}`;
    logEl.insertBefore(li,logEl.firstChild);
    while(logEl.children.length>24){logEl.removeChild(logEl.lastChild);}
  }

  function fire(t){
    t.count++;
    state.runs++;
    var el=tasksEl.querySelector(`[data-id="${t.id}"]`);
    if(el){
      el.classList.add("is-firing");
      setTimeout(function(){el.classList.remove("is-firing");},450);
    }
    log(`<strong>${t.name}</strong> &rarr; ${t.msg}`);
  }

  function tick(){
    state.secs++;
    state.tasks.forEach(function(t){
      t.left--;
      if(t.left<=0){fire(t);t.left=t.every;}
    });
    render();
  }

  function startTimer(){if(!timer){timer=setInterval(tick,1000);}}
  function stopTimer(){if(timer){clearInterval(timer);timer=null;}}

  toggleBtn.addEventListener("click",function(){
    state.running=!state.running;
    if(state.running){
      startTimer();
      statusEl.classList.remove("is-paused");
      toggleBtn.textContent="Pausar";
      log("Runtime reanudado");
    }else{
      stopTimer();
      statusEl.classList.add("is-paused");
      toggleBtn.textContent="Reanudar";
      log("Runtime en pausa: ticks suspendidos");
    }
  });

  resetBtn.addEventListener("click",function(){
    stopTimer();
    build();
    startTimer();
  });

  build();
  startTimer();
})();
</script>
<h2>El ejemplo: heartbeat + cache warm-up</h2>
<p>Esto no es solo teoría: este mismo sitio corre dos <code>ScheduledTask</code> en cada kernel del pool. El módulo <code>Kernel/Models/Scheduler.wl</code> las define de forma declarativa y las arranca una sola vez por kernel:</p>
<pre><code>(* Kernel/Models/Scheduler.wl  (extracto) *)

warmCache[] :=
  Quiet @ Check[PersonalSite`Post`recent[10]; $warms++; True, False];

heartbeat[] := ($beats++; True);

start[] :=
  If[TrueQ[$started], $tasks,
    ( $tasks = &lt;|
        "heartbeat"  -&gt; RunScheduledTask[heartbeat[],  30],   (* cada 30 s  *)
        "cache-warm" -&gt; RunScheduledTask[warmCache[], 300]    (* cada 5 min *)
      |&gt;;
      $startedAt = Now; $started = True; $tasks )
  ];</code></pre>
<p>El arranque vive en el entrypoint de producción, detrás del guard que asegura que el setup pesado ocurra una sola vez por kernel:</p>
<pre><code>(* deploy/app.wl *)
Quiet @ Check[PersonalSite`Scheduler`start[], $Failed];</code></pre>
<p>El <code>heartbeat</code> late cada 30 segundos y el <code>cache-warm</code> mantiene la lista de posts caliente cada 5 minutos. Así, mientras el kernel viva, el framework se mantiene a sí mismo en runtime &mdash; exactamente lo que viste simulado arriba, pero de verdad.</p>',
   '2026-06-27');

INSERT OR IGNORE INTO posts (slug, title, summary, body, date) VALUES
  ('multiway-confluencia',
   'De 3ᵏ a la confluencia: un sistema multiway aritmético causalmente invariante',
   'Tres mapas afines, un monoide con sistema de reescritura completo y un conjunto de estados fractal. Dos teoremas exactos, sus verificaciones que corren, y métricas conectadas a la ruliología.',
   '<p>Wolfram describe la ruliología como la ciencia básica de estudiar qué hacen las reglas simples. Este artículo toma una regla mínima — tres mapas afines sobre los enteros — y la trata como un <strong>sistema multiway determinista</strong>, donde cada hilo de derivación es una computación. El objetivo es doble: medir métricas ruliológicas concretas y demostrar dos hechos exactos que estructuran todo el sistema.</p>

<h2>El objeto: tres mapas, un grafo</h2>
<p>Las tres reglas son simples:</p>
<pre><code>a(n) = 2n+1     (* duplicación *)
b(n) = n+14     (* traslación + *)
c(n) = n-18     (* traslación - *)
</code></pre>
<p>Aplicadas simultáneamente desde la semilla <code>1</code>, generan un grafo multiway. En generación 0 hay 1 estado; en generación 1 hay 3; en generación 2 ya hay 8 — porque los hilos colapsan.</p>
<pre><code>g = NestGraph[{2#+1, #+14, #-18}&amp;, {1}, 4,
     VertexLabels -&gt; Automatic,
     GraphLayout  -&gt; "LayeredDigraphEmbedding"]
</code></pre>

<h2>Métricas ruliológicas</h2>
<p>El <strong>factor de colapso</strong> en generación 10 es <code>3¹⁰ / |S₁₀| ≈ 9.5×</code>: hay casi 10 veces menos estados distinguibles que hilos posibles. El 52 % de los estados son alcanzados por más de un hilo — son nodos de confluencia.</p>
<p>La <strong>tasa de crecimiento</strong> λ ≈ 1.728 implica una dimensión de caja log₂λ ≈ 0.79 — la serie crece sub-exponencialmente pero sin forma cerrada sencilla, lo que sugiere (y se verifica empíricamente) que la serie de conteo no es D-finita.</p>

<h2>Teorema 1 — Forma cerrada</h2>
<p>El conjunto de valores alcanzables desde 1 es exactamente:</p>
<pre><code>S = { 2^(p+1)-1 + 14q - 18r : p,q,r ∈ ℤ≥0 }
</code></pre>
<p>De esto se sigue de inmediato que todo estado es <strong>impar</strong> (la semilla es impar y los tres mapas preservan la paridad), y que el grafo tiene morfología de dos lóbulos (estados con y sin componente de duplicación dominante).</p>
<p>Verificación en WL (corre en tiempo real en <a href="/ruliology">la página interactiva</a>):</p>
<pre><code>brute = deepReach[12];
canon = {2^(p+1)-1+14q-18r, {p,0,11},{q,0,50},{r,0,50}} // Flatten // DeleteDuplicates;
Sort[Select[brute,-600&lt;=#&lt;=600&amp;]] === Sort[Select[canon,-600&lt;=#&lt;=600&amp;]]
(* True *)
</code></pre>

<h2>Teorema 2 — Invariancia causal (confluencia)</h2>
<p>El monoide ⟨a,b,c⟩ admite el sistema de reescritura finito y completo:</p>
<pre><code>ab → b²a      ac → c²a      cb → bc
</code></pre>
<p>con forma canónica <code>b^q c^r a^p</code>. Esto es la versión rigurosa de la "invariancia causal" del marco de Wolfram: toda divergencia de hilos reconverge, el orden de aplicación de reglas no altera el estado alcanzable.</p>
<p>El sistema <strong>termina</strong> porque cada regla mueve una <em>a</em> hacia la derecha. Es <strong>confluente</strong> porque todos los pares críticos reconvergen — verificado sobre 300 palabras aleatorias:</p>
<pre><code>AllTrue[tests, applyWord[#] === applyWord[normalForm[#]] &amp;]   (* True *)
AllTrue[tests, MatchQ[normalForm[#], {"b"...,"c"...,"a"...}] &amp;] (* True *)
</code></pre>

<h2>Evaluación interactiva</h2>
<p>La página <a href="/ruliology"><strong>/ruliology</strong></a> permite evaluar cada métrica directamente en el kernel WL que sirve este sitio: serie de crecimiento, tabla de colapso, verificación de la forma cerrada, confirmación de confluencia e identidades funcionales — todo con tiempos de respuesta reales del proceso.</p>
<p>El endpoint <code>POST /ruliology/eval</code> despacha expresiones por <em>clave nombrada</em> (no código arbitrario), y <code>GET /ruliology/metrics</code> devuelve las métricas pre-computadas con TTL de 5 minutos.</p>',
   '2026-06-28');

-- Estado de apariencia: el usuario elige la regla del tema en /apariencia y,
-- en modo auto, la ScheduledTask `theme-rotate` lo rota en el tiempo.
CREATE TABLE IF NOT EXISTS settings (
    key   TEXT PRIMARY KEY,
    value TEXT NOT NULL
);

INSERT OR IGNORE INTO settings (key, value) VALUES
  ('theme.mode',     'manual'),                       -- manual | auto
  ('theme.active',   'slate'),                         -- tema activo
  ('theme.order',    'slate,sand,forest,rose,ocean'),  -- orden de rotacion
  ('theme.interval', '20');                            -- segundos por tema (modo auto)
