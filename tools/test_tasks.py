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

# ── 8. Rutas principales del sitio siguen respondiendo ────────────────────
print("\n── 8. Smoke test rutas principales ───────────────────")
for path in ["/", "/blog", "/contacto", "/flow", "/nest", "/tasks"]:
    req("GET", path, label=f"GET {path}")

# ── Resumen ───────────────────────────────────────────────────────────────
total = len(PASS) + len(FAIL)
print(f"\n══════════════════════════════════════════════════════")
print(f"  RESULTADO: {len(PASS)}/{total} tests pasaron")
if FAIL:
    print(f"\n  Fallos:")
    for f in FAIL: print(f)
print("══════════════════════════════════════════════════════\n")

sys.exit(0 if not FAIL else 1)
