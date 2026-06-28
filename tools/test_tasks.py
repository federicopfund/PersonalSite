#!/usr/bin/env python3
"""Integration tests for the TaskManager HTTP API.
Run: python3 tools/test_tasks.py
"""

import json, sys, time
import urllib.request, urllib.error, urllib.parse

BASE = "http://localhost:8080"
PASS, FAIL = [], []


def req(method, path, body=None, *, expect=200, label=None):
    """Send request. body dict is sent as application/x-www-form-urlencoded."""
    url = BASE + path
    if body:
        # WolframWebEngine exposes FormRules for form-encoded, not JSON bodies
        data = urllib.parse.urlencode(body).encode()
        headers = {"Content-Type": "application/x-www-form-urlencoded"}
    else:
        data = None
        headers = {}
    req_obj = urllib.request.Request(url, data=data, headers=headers, method=method)
    try:
        with urllib.request.urlopen(req_obj, timeout=10) as r:
            raw = r.read().decode()
            code = r.status
    except urllib.error.HTTPError as e:
        raw = e.read().decode()
        code = e.code
    except Exception as e:
        tag = label or f"{method} {path}"
        FAIL.append(f"  ✗  {tag}  →  NETWORK ERROR: {e}")
        print(FAIL[-1])
        return None, None

    tag = label or f"{method} {path}"
    ok = code == expect
    icon = "  ✓" if ok else "  ✗"
    snippet = raw[:120].replace("\n", " ")
    line = f"{icon}  [{code}]  {tag}  →  {snippet}"
    (PASS if ok else FAIL).append(line)
    print(line)

    try:
        return code, json.loads(raw)
    except Exception:
        return code, raw


print("\n══════════════════════════════════════════════════════")
print("  TaskManager Integration Test Suite")
print("══════════════════════════════════════════════════════\n")

# ── 1. Página de tareas (HTML) ─────────────────────────────────────────────
print("── 1. Dashboard UI ───────────────────────────────────")
req("GET", "/tasks", label="GET /tasks → HTML 200")

# ── 2. Summary JSON ────────────────────────────────────────────────────────
print("\n── 2. Summary JSON ───────────────────────────────────")
code, snap = req("GET", "/tasks/summary", label="GET /tasks/summary → JSON 200")
if isinstance(snap, dict):
    tasks = snap.get("tasks", {})
    print(f"     Tareas registradas : {list(tasks.keys())}")
    print(f"     Running count      : {snap.get('running', '?')}")
    print(f"     Kernel ID          : {snap.get('kernel', '?')}")
else:
    FAIL.append("  ✗  /tasks/summary no devolvió JSON dict")
    print(FAIL[-1])
    tasks = {}

# ── 3. Stop / Start de heartbeat ──────────────────────────────────────────
print("\n── 3. Stop → Start lifecycle ─────────────────────────")
req("POST", "/tasks/stop/heartbeat",    label="POST /tasks/stop/heartbeat → 200")
time.sleep(1)
_, s = req("GET", "/tasks/summary", label="POST /tasks/summary tras stop")
if isinstance(s, dict):
    hb = s.get("tasks", {}).get("heartbeat", {})
    ok = not hb.get("running", True)
    line = f"  {'✓' if ok else '✗'}  heartbeat.running == False después de stop"
    (PASS if ok else FAIL).append(line); print(line)

req("POST", "/tasks/start/heartbeat",   label="POST /tasks/start/heartbeat → 200")
time.sleep(1)
_, s = req("GET", "/tasks/summary", label="GET /tasks/summary tras start")
if isinstance(s, dict):
    hb = s.get("tasks", {}).get("heartbeat", {})
    ok = hb.get("running", False)
    line = f"  {'✓' if ok else '✗'}  heartbeat.running == True después de start"
    (PASS if ok else FAIL).append(line); print(line)

# ── 4. Restart ────────────────────────────────────────────────────────────
print("\n── 4. Restart ────────────────────────────────────────")
req("POST", "/tasks/restart/cache-warm", label="POST /tasks/restart/cache-warm → 200")
time.sleep(1)
_, s = req("GET", "/tasks/summary")
if isinstance(s, dict):
    cw = s.get("tasks", {}).get("cache-warm", {})
    ok = cw.get("running", False)
    line = f"  {'✓' if ok else '✗'}  cache-warm corriendo tras restart"
    (PASS if ok else FAIL).append(line); print(line)

# ── 5. Historial ──────────────────────────────────────────────────────────
print("\n── 5. History endpoint ───────────────────────────────")
# Esperar que heartbeat acumule al menos 1 run (intervalo 30s → puede estar vacío)
code, hist = req("GET", "/tasks/history/heartbeat", label="GET /tasks/history/heartbeat → 200")
if isinstance(hist, list):
    line = f"  ✓  Historial es lista, {len(hist)} entradas"
    PASS.append(line); print(line)
else:
    FAIL.append("  ✗  /tasks/history/heartbeat no devolvió lista"); print(FAIL[-1])

code, hist404 = req("GET", "/tasks/history/no-existe",
                    expect=404, label="GET /tasks/history/no-existe → 404")

# ── 6. Configure (hot-reconfigure intervalo) ──────────────────────────────
print("\n── 6. Configure interval ─────────────────────────────")
_, r = req("POST", "/tasks/configure",
           body={"id": "heartbeat", "key": "interval", "value": 45},
           label="POST /tasks/configure heartbeat interval=45 → 200")
if isinstance(r, dict):
    ok = r.get("ok", False)
    line = f"  {'✓' if ok else '✗'}  configure devolvió ok=True"
    (PASS if ok else FAIL).append(line); print(line)

time.sleep(1)
_, s = req("GET", "/tasks/summary")
if isinstance(s, dict):
    hb_iv = s.get("tasks", {}).get("heartbeat", {}).get("interval", 0)
    ok = hb_iv == 45
    line = f"  {'✓' if ok else '✗'}  heartbeat.interval == 45 (got {hb_iv})"
    (PASS if ok else FAIL).append(line); print(line)
    # Restaurar a 30s
    req("POST", "/tasks/configure",
        body={"id": "heartbeat", "key": "interval", "value": 30},
        label="POST /tasks/configure heartbeat interval=30 (restaurar)")

# ── 7. Register nueva tarea en runtime ────────────────────────────────────
print("\n── 7. Runtime register ───────────────────────────────")
_, r = req("POST", "/tasks/register",
           body={
               "id":         "test-ping",
               "label":      "Test Ping",
               "group":      "user",
               "interval":   10,
               "actionCode": "Function[True]",
               "enabled":    True
           },
           label="POST /tasks/register test-ping → 200")
if isinstance(r, dict):
    ok = r.get("ok", False)
    line = f"  {'✓' if ok else '✗'}  register devolvió ok=True"
    (PASS if ok else FAIL).append(line); print(line)

time.sleep(1)
_, s = req("GET", "/tasks/summary")
if isinstance(s, dict):
    ok = "test-ping" in s.get("tasks", {})
    line = f"  {'✓' if ok else '✗'}  test-ping aparece en summary"
    (PASS if ok else FAIL).append(line); print(line)

# ── 7b. Unregister (eliminar test-ping) ──────────────────────────────────
print("\n── 7b. Unregister ────────────────────────────────────")
_, r = req("POST", "/tasks/unregister/test-ping",
           label="POST /tasks/unregister/test-ping → 200")
if isinstance(r, dict):
    ok = r.get("ok", False)
    line = f"  {'✓' if ok else '✗'}  unregister devolvió ok=True"
    (PASS if ok else FAIL).append(line); print(line)

time.sleep(0.5)
_, s = req("GET", "/tasks/summary")
if isinstance(s, dict):
    ok = "test-ping" not in s.get("tasks", {})
    line = f"  {'✓' if ok else '✗'}  test-ping ya no aparece en summary"
    (PASS if ok else FAIL).append(line); print(line)

# Unregister de tarea inexistente → ok=False, no crash
_, r = req("POST", "/tasks/unregister/no-existe",
           label="POST /tasks/unregister/no-existe → 200 ok=False")
if isinstance(r, dict):
    ok = r.get("ok") is False
    line = f"  {'✓' if ok else '✗'}  unregister tarea inexistente devuelve ok=False"
    (PASS if ok else FAIL).append(line); print(line)

# ── 7c. DAG endpoint ─────────────────────────────────────────────────────
print("\n── 7c. DAG endpoint ──────────────────────────────────")
code, dag = req("GET", "/tasks/dag", label="GET /tasks/dag → JSON 200")
if isinstance(dag, dict):
    ok = all(k in dag for k in ("nodes", "links", "topoOrder", "critPath", "nodeCount"))
    line = f"  {'✓' if ok else '✗'}  dag contiene nodes/links/topoOrder/critPath/nodeCount"
    (PASS if ok else FAIL).append(line); print(line)

    ok = dag.get("nodeCount", 0) >= 6
    line = f"  {'✓' if ok else '✗'}  dag.nodeCount ≥ 6 (got {dag.get('nodeCount', '?')})"
    (PASS if ok else FAIL).append(line); print(line)

    ok = len(dag.get("links", [])) > 0
    line = f"  {'✓' if ok else '✗'}  dag.links tiene aristas ({len(dag.get('links', []))} aristas)"
    (PASS if ok else FAIL).append(line); print(line)

    cp = dag.get("critPath", [])
    ok = len(cp) >= 2 and "heartbeat" in cp
    line = f"  {'✓' if ok else '✗'}  critPath ≥2 nodos, incluye heartbeat (got {cp})"
    (PASS if ok else FAIL).append(line); print(line)

    topo = dag.get("topoOrder", [])
    # heartbeat debe ser primero (root sin deps)
    ok = len(topo) > 0 and topo[0] == "heartbeat"
    line = f"  {'✓' if ok else '✗'}  topoOrder[0] == 'heartbeat' (got {topo[:3]})"
    (PASS if ok else FAIL).append(line); print(line)
else:
    FAIL.append("  ✗  /tasks/dag no devolvió JSON dict"); print(FAIL[-1])

# ── 8. Rutas principales del sitio siguen respondiendo ────────────────────
print("\n── 8. Smoke test rutas principales ───────────────────")
for path in ["/", "/blog", "/contacto", "/flow", "/nest", "/tasks"]:
    req("GET", path, label=f"GET {path}")

# ── 9. ScheduledTask runtime state (via TaskManager, no $ScheduledTasks) ──
print("\n── 9. ScheduledTask runtime health ───────────────────")
_, snap = req("GET", "/tasks/summary", label="GET /tasks/summary → snapshot")
if isinstance(snap, dict):
    tasks = snap.get("tasks", {})
    expected = {"heartbeat", "cache-warm", "theme-rotate", "cards-refresh",
                "metric-refresh", "nest-refresh"}
    ok = expected.issubset(set(tasks.keys()))
    line = f"  {'✓' if ok else '✗'}  6 tareas del sistema presentes (got {list(tasks.keys())})"
    (PASS if ok else FAIL).append(line); print(line)

    running = {n for n, t in tasks.items() if t.get("running")}
    ok = len(running) >= 5
    line = f"  {'✓' if ok else '✗'}  ≥5 tareas running ({sorted(running)})"
    (PASS if ok else FAIL).append(line); print(line)

    hb = tasks.get("heartbeat", {})
    ok = hb.get("runs", 0) > 0
    line = f"  {'✓' if ok else '✗'}  heartbeat tiene ≥1 run (got {hb.get('runs', 0)})"
    (PASS if ok else FAIL).append(line); print(line)

    tr = tasks.get("theme-rotate", {})
    ok = tr.get("runs", 0) > 0
    line = f"  {'✓' if ok else '✗'}  theme-rotate tiene ≥1 run (got {tr.get('runs', 0)})"
    (PASS if ok else FAIL).append(line); print(line)

    cr = tasks.get("cards-refresh", {})
    ok = cr.get("avgMs", 0) >= 0
    line = f"  {'✓' if ok else '✗'}  cards-refresh.avgMs reportado ({cr.get('avgMs', '?')}ms)"
    (PASS if ok else FAIL).append(line); print(line)

    # Ninguna tarea debe tener errores > 0
    errs = {n: t.get("errors", 0) for n, t in tasks.items() if t.get("errors", 0) > 0}
    ok = len(errs) == 0
    line = f"  {'✓' if ok else '✗'}  0 tareas con errores (errores: {errs})"
    (PASS if ok else FAIL).append(line); print(line)

# ── 10. Ruliology controller (nueva API) ───────────────────────────────────
print("\n── 10. /ruliology endpoints ──────────────────────────")
req("GET", "/ruliology", label="GET /ruliology → HTML 200")

code, m = req("GET", "/ruliology/metrics", label="GET /ruliology/metrics → JSON 200")
if isinstance(m, dict):
    ok = "lambda" in m and "box_dim" in m and "states_g22" in m
    line = f"  {'✓' if ok else '✗'}  metrics contiene lambda, box_dim, states_g22"
    (PASS if ok else FAIL).append(line); print(line)

    ok = 1.5 < m.get("lambda", 0) < 2.0
    line = f"  {'✓' if ok else '✗'}  lambda ∈ (1.5, 2.0) (got {m.get('lambda', '?'):.4f})"
    (PASS if ok else FAIL).append(line); print(line)

    ok = 0.5 < m.get("box_dim", 0) < 1.0
    line = f"  {'✓' if ok else '✗'}  box_dim ∈ (0.5, 1.0) (got {m.get('box_dim', '?'):.4f})"
    (PASS if ok else FAIL).append(line); print(line)

    ok = m.get("states_g22", 0) > 1_000_000
    line = f"  {'✓' if ok else '✗'}  |S₂₂| > 1M (got {m.get('states_g22', 0):,})"
    (PASS if ok else FAIL).append(line); print(line)

print("\n── 10b. /ruliology/eval (named registry) ─────────────")
# growth_series: resultado es lista numérica
_, r = req("POST", "/ruliology/eval",
           body={"key": "growth_series"}, label="POST /ruliology/eval key=growth_series")
if isinstance(r, dict):
    ok = "out" in r and r.get("ms", 0) > 0
    line = f"  {'✓' if ok else '✗'}  growth_series evaluó en {r.get('ms','?')}ms"
    (PASS if ok else FAIL).append(line); print(line)

    # El output debe comenzar con {1, 3, 8
    ok = r.get("out", "").startswith("{1, 3, 8")
    line = f"  {'✓' if ok else '✗'}  growth_series comienza con {{1, 3, 8, ..."
    (PASS if ok else FAIL).append(line); print(line)

# confluence_check: True, True
_, r = req("POST", "/ruliology/eval",
           body={"key": "confluence_check"}, label="POST /ruliology/eval key=confluence_check")
if isinstance(r, dict):
    out = r.get("out", "")
    ok = "values_preserved -> True" in out and "canonical_shape -> True" in out
    line = f"  {'✓' if ok else '✗'}  confluence: values_preserved=True, canonical_shape=True"
    (PASS if ok else FAIL).append(line); print(line)

# closed_form_check: match -> True
_, r = req("POST", "/ruliology/eval",
           body={"key": "closed_form_check"}, label="POST /ruliology/eval key=closed_form_check")
if isinstance(r, dict):
    ok = "match -> True" in r.get("out", "")
    line = f"  {'✓' if ok else '✗'}  closed_form: match=True"
    (PASS if ok else FAIL).append(line); print(line)

# parity_check: all_odd -> True
_, r = req("POST", "/ruliology/eval",
           body={"key": "parity_check"}, label="POST /ruliology/eval key=parity_check")
if isinstance(r, dict):
    ok = "all_odd -> True" in r.get("out", "")
    line = f"  {'✓' if ok else '✗'}  parity: all_odd=True"
    (PASS if ok else FAIL).append(line); print(line)

# key inválido → respuesta con error, no crash
_, r = req("POST", "/ruliology/eval",
           body={"key": "no_existe_key"}, label="POST /ruliology/eval key=inválido → error JSON")
if isinstance(r, dict):
    ok = "error" in r
    line = f"  {'✓' if ok else '✗'}  clave inválida devuelve {{error: ...}}"
    (PASS if ok else FAIL).append(line); print(line)

# blog post
req("GET", "/blog/multiway-confluencia",
    label="GET /blog/multiway-confluencia → HTML 200")

# ── Resumen ───────────────────────────────────────────────────────────────
total = len(PASS) + len(FAIL)
print(f"\n══════════════════════════════════════════════════════")
print(f"  RESULTADO: {len(PASS)}/{total} tests pasaron")
if FAIL:
    print(f"\n  Fallos:")
    for f in FAIL: print(f)
print("══════════════════════════════════════════════════════\n")

sys.exit(0 if not FAIL else 1)
