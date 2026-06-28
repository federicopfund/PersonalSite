#!/usr/bin/env python3
"""
Integration tests for the TaskConfig HTTP API (scheduler_tasks DB).
Run: python3 tools/test_config.py
"""

import json, sys, time
import urllib.request, urllib.error, urllib.parse

BASE = "http://localhost:8080"
PASS, FAIL = [], []

TEST_ID = "test-e2e-cfg"


def req(method, path, body=None, *, expect=200, label=None):
    url = BASE + path
    if body:
        data    = urllib.parse.urlencode(body).encode()
        headers = {"Content-Type": "application/x-www-form-urlencoded"}
    else:
        data, headers = None, {}
    req_obj = urllib.request.Request(url, data=data, headers=headers, method=method)
    try:
        with urllib.request.urlopen(req_obj, timeout=10) as r:
            raw, code = r.read().decode(), r.status
    except urllib.error.HTTPError as e:
        raw, code = e.read().decode(), e.code
    except Exception as e:
        tag = label or f"{method} {path}"
        FAIL.append(f"  ✗  {tag}  →  NETWORK: {e}")
        print(FAIL[-1]); return None, None

    tag  = label or f"{method} {path}"
    ok   = (code == expect)
    icon = "  ✓" if ok else "  ✗"
    line = f"{icon}  [{code}]  {tag}  →  {raw[:100].replace(chr(10),' ')}"
    (PASS if ok else FAIL).append(line); print(line)
    try:    return code, json.loads(raw)
    except: return code, raw


def chk(cond, msg):
    line = f"  {'✓' if cond else '✗'}  {msg}"
    (PASS if cond else FAIL).append(line); print(line)


print("\n══════════════════════════════════════════════════════")
print("  TaskConfig Integration Test Suite")
print("══════════════════════════════════════════════════════\n")

# ── 1. GET /tasks/config — lista inicial ──────────────────────────────────
print("── 1. GET /tasks/config — lista desde DB ─────────────")
_, rows = req("GET", "/tasks/config", label="GET /tasks/config → JSON 200")
if isinstance(rows, list):
    chk(len(rows) >= 6, f"≥6 configs en DB (got {len(rows)})")
    ids = [r["task_id"] for r in rows]
    for tid in ["heartbeat","cache-warm","theme-rotate","cards-refresh","metric-refresh","nest-refresh"]:
        chk(tid in ids, f'"{tid}" presente en DB')

    # dag_order correcto
    hb  = next((r for r in rows if r["task_id"] == "heartbeat"), {})
    chk(hb.get("dag_order") == 0,    f"heartbeat dag_order == 0 (got {hb.get('dag_order')})")
    cw  = next((r for r in rows if r["task_id"] == "cache-warm"), {})
    chk(cw.get("dag_order") == 1,    f"cache-warm dag_order == 1")
    mr  = next((r for r in rows if r["task_id"] == "metric-refresh"), {})
    chk(mr.get("dag_order") == 3,    f"metric-refresh dag_order == 3 (got {mr.get('dag_order')})")

    # deps correctas
    chk(cw.get("deps") == ["heartbeat"], f'cache-warm deps == ["heartbeat"] (got {cw.get("deps")})')
else:
    FAIL.append("  ✗  /tasks/config no devolvió lista"); print(FAIL[-1])

# ── 2. GET /tasks/config/:id ──────────────────────────────────────────────
print("\n── 2. GET /tasks/config/:id ──────────────────────────")
_, cfg = req("GET", "/tasks/config/heartbeat", label="GET /tasks/config/heartbeat → 200")
if isinstance(cfg, dict):
    chk(cfg.get("task_id") == "heartbeat", "task_id == 'heartbeat'")
    chk(cfg.get("interval_s") == 30,       f"interval_s == 30 (got {cfg.get('interval_s')})")

req("GET", "/tasks/config/no-existe", expect=404, label="GET /tasks/config/no-existe → 404")

# ── 3. POST /tasks/config/create ─────────────────────────────────────────
print("\n── 3. POST /tasks/config/create ─────────────────────")
_, r = req("POST", "/tasks/config/create",
           body={"task_id":     TEST_ID,
                 "label":       "E2E Test Task",
                 "group_name":  "user",
                 "interval_s":  15,
                 "dag_order":   1,
                 "deps":        "heartbeat",
                 "action_code": "Function[42]",
                 "enabled":     "true"},
           label=f"POST /tasks/config/create {TEST_ID} → 200")
if isinstance(r, dict):
    chk(r.get("ok") is True, "create devolvió ok=True")

# Verify appeared in list
_, rows2 = req("GET", "/tasks/config")
if isinstance(rows2, list):
    found = next((x for x in rows2 if x["task_id"] == TEST_ID), None)
    chk(found is not None,          f'"{TEST_ID}" aparece en /tasks/config')
    chk(found and found.get("dag_order") == 1,  "dag_order == 1 persistido")
    chk(found and found.get("interval_s") == 15,"interval_s == 15 persistido")
    chk(found and found.get("deps") == ["heartbeat"], "deps == [heartbeat]")

# Duplicate create → ok=False (INSERT OR IGNORE returns ok=False)
_, r2 = req("POST", "/tasks/config/create",
            body={"task_id": TEST_ID, "label": "dup"},
            label=f"POST /tasks/config/create dup → 200 ok=False")
if isinstance(r2, dict):
    chk(r2.get("ok") is False, "duplicate create devuelve ok=False (INSERT OR IGNORE)")

# ── 4. POST /tasks/config/update ─────────────────────────────────────────
print("\n── 4. POST /tasks/config/update ─────────────────────")
_, u = req("POST", "/tasks/config/update",
           body={"task_id": TEST_ID, "key": "interval_s", "value": 30},
           label="POST /tasks/config/update interval_s=30 → 200")
if isinstance(u, dict):
    chk(u.get("ok") is True, "update devolvió ok=True")

_, u2 = req("POST", "/tasks/config/update",
            body={"task_id": TEST_ID, "key": "dag_order", "value": 2},
            label="POST /tasks/config/update dag_order=2 → 200")

_, u3 = req("POST", "/tasks/config/update",
            body={"task_id": TEST_ID, "key": "label", "value": "Updated Label"},
            label="POST /tasks/config/update label → 200")

# Verificar persistencia
_, cfg2 = req("GET", f"/tasks/config/{TEST_ID}")
if isinstance(cfg2, dict):
    chk(cfg2.get("interval_s") == 30,            "interval_s == 30 persistido")
    chk(cfg2.get("dag_order") == 2,              "dag_order == 2 persistido")
    chk(cfg2.get("label") == "Updated Label",    "label actualizado")

# Campo inválido → ok=False
_, u4 = req("POST", "/tasks/config/update",
            body={"task_id": TEST_ID, "key": "bad_col", "value": "x"},
            label="POST /tasks/config/update campo_invalido → ok=False")
if isinstance(u4, dict):
    chk(u4.get("ok") is False, "campo inválido devuelve ok=False")

# ── 5. POST /tasks/config/apply ──────────────────────────────────────────
print("\n── 5. POST /tasks/config/apply ──────────────────────")
_, ap = req("POST", "/tasks/config/apply", label="POST /tasks/config/apply → 200")
if isinstance(ap, dict):
    chk(ap.get("ok") is True,          "apply devolvió ok=True")
    chk(len(ap.get("applied", [])) >= 6, f"≥6 tareas aplicadas (got {ap.get('applied')})")
    chk(len(ap.get("failed",  [])) == 0, f"0 fallos en apply (got {ap.get('failed')})")

# Verificar que aparecen en el runtime
time.sleep(1)
_, snap = req("GET", "/tasks/summary")
if isinstance(snap, dict):
    tasks = snap.get("tasks", {})
    chk(TEST_ID in tasks, f'"{TEST_ID}" aparece en runtime summary tras apply')
    chk(tasks.get(TEST_ID, {}).get("running", False),
        f'"{TEST_ID}" está running en runtime')

# ── 6. POST /tasks/config/delete ─────────────────────────────────────────
print("\n── 6. POST /tasks/config/delete ─────────────────────")
_, d = req("POST", f"/tasks/config/delete/{TEST_ID}",
           label=f"POST /tasks/config/delete/{TEST_ID} → 200")
if isinstance(d, dict):
    chk(d.get("ok") is True, "delete devolvió ok=True")

_, rows3 = req("GET", "/tasks/config")
if isinstance(rows3, list):
    chk(not any(r["task_id"] == TEST_ID for r in rows3),
        f'"{TEST_ID}" ya no aparece en /tasks/config')

# Limpiar también del runtime
req("POST", f"/tasks/unregister/{TEST_ID}", label=f"cleanup runtime unregister {TEST_ID}")

# ── 7. POST /tasks/config/seed (idempotente) ──────────────────────────────
print("\n── 7. POST /tasks/config/seed (idempotente) ─────────")
_, s = req("POST", "/tasks/config/seed", label="POST /tasks/config/seed → 200")
if isinstance(s, dict):
    chk("ok" in s, "seed devuelve campo ok")
    # Tabla ya tiene datos → result = "already seeded"
    chk("seeded" in str(s.get("result", "")),
        f"result contiene 'seeded' (got {s.get('result')})")

# ── 8. Smoke: sistema de 6 tareas intacto tras el test ───────────────────
print("\n── 8. Sistema intacto tras el test ──────────────────")
_, snap2 = req("GET", "/tasks/summary")
if isinstance(snap2, dict):
    tasks2 = snap2.get("tasks", {})
    sys_ids = {"heartbeat","cache-warm","theme-rotate","cards-refresh","metric-refresh","nest-refresh"}
    chk(sys_ids.issubset(set(tasks2.keys())),
        f"6 tareas del sistema presentes: {sorted(tasks2.keys())}")
    running = {n for n,t in tasks2.items() if t.get("running")}
    chk(len(running) >= 6, f"≥6 tareas running ({sorted(running)})")

# ── Resumen ───────────────────────────────────────────────────────────────
total = len(PASS) + len(FAIL)
print(f"\n══════════════════════════════════════════════════════")
print(f"  RESULTADO: {len(PASS)}/{total} tests pasaron")
if FAIL:
    print("\n  Fallos:")
    for f in FAIL: print(f)
print("══════════════════════════════════════════════════════\n")
sys.exit(0 if not FAIL else 1)
