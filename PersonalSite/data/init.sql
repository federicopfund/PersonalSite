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
