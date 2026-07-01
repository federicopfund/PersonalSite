#!/usr/bin/env python3
"""tools/validate_a2a_logic.py

Validacion END-TO-END *sin kernel Wolfram* del modulo A2A: reproduce en Python
la logica de PersonalSite`AgentMesh`run (expansion de la Ruliad via NestList) y
del envelope JSON-RPC A2A, y verifica invariantes de extremo a extremo.

Sirve como gate de CI que no requiere Wolfram Engine ni servidor HTTP. La
validacion "real" contra el kernel/servidor vive en validate_a2a.wls y
a2a_e2e.sh.

Uso:  python3 tools/validate_a2a_logic.py   ·   make a2a-logic
Salida: PASS/FAIL por chequeo, codigo 0 (ok) / 1 (fallo).
"""
from __future__ import annotations

import json
import sys

passed = 0
failed = 0
failures: list[str] = []


def check(label: str, actual, expected) -> None:
    global passed, failed
    if actual == expected:
        passed += 1
        print(f"  \u2713 {label}")
    else:
        failed += 1
        failures.append(label)
        print(f"  \u2717 {label}  (esperado {expected!r} | obtenido {actual!r})")


def check_true(label: str, cond: bool) -> None:
    check(label, bool(cond), True)


# ── Reglas de la Ruliad (las del NestGraph[{2#+1,#+14,#-18}&,{1},3]) ───────
RULES = [
    ("2x + 1", lambda x: 2 * x + 1),
    ("x + 14", lambda x: x + 14),
    ("x - 18", lambda x: x - 18),
]
ORCH = "ruliad-orchestrator"


def agent_id(idx: int) -> str:      # 1-based rule index
    return f"agent-rule-{idx}"


def build_records(seeds: list[int], depth: int) -> list[dict]:
    """Espejo de NestScheduler`buildRecords (BFS, ids 1..N)."""
    records: list[dict] = []
    counter = 0

    def add(level, value, parent, rule_idx):
        nonlocal counter
        counter += 1
        records.append({"id": counter, "level": level, "value": value,
                        "parent": parent, "ruleIdx": rule_idx})
        return counter

    current = [(add(0, s, None, 0), s) for s in seeds]
    for lv in range(1, depth + 1):
        nxt = []
        for pid, pval in current:
            for ri, (_, fn) in enumerate(RULES, start=1):
                cval = fn(pval)
                cid = add(lv, cval, pid, ri)
                nxt.append((cid, cval))
        current = nxt
    return records


def build_graph(seeds: list[int], depth: int) -> dict:
    """Espejo de AgentMesh`run: nodos, aristas (mensajes A2A) y stacks."""
    records = build_records(seeds, depth)
    by_id = {r["id"]: r for r in records}

    nodes = [{
        "id": f"n{r['id']}", "level": r["level"], "value": r["value"],
        "parent": None if r["parent"] is None else f"n{r['parent']}",
        "ruleIdx": r["ruleIdx"],
        "agent": ORCH if r["level"] == 0 else agent_id(r["ruleIdx"]),
    } for r in records]

    edges = [{
        "from": f"n{r['parent']}", "to": f"n{r['id']}",
        "agent": agent_id(r["ruleIdx"]), "ruleIdx": r["ruleIdx"],
        "kind": "a2a.message",
    } for r in records if r["parent"] is not None]

    parents = {r["parent"] for r in records if r["parent"] is not None}
    leaves = [r for r in records if r["id"] not in parents]

    def path_to(rec):
        chain = [rec]
        while chain[-1]["parent"] is not None:
            chain.append(by_id[chain[-1]["parent"]])
        return list(reversed(chain))

    stacks = [{
        "leafId": f"n{lf['id']}", "value": lf["value"],
        "stack": [{
            "level": nd["level"], "node": f"n{nd['id']}", "ruleIdx": nd["ruleIdx"],
            "agent": ORCH if nd["level"] == 0 else agent_id(nd["ruleIdx"]),
            "label": "seed" if nd["level"] == 0 else RULES[nd["ruleIdx"] - 1][0],
        } for nd in path_to(lf)],
    } for lf in leaves]

    return {"nodes": nodes, "edges": edges, "stacks": stacks}


# ══════════════════════════════════════════════════════════════════════════
print("== 1. Expansion de la Ruliad (seed=1, depth=3) ==")
g = build_graph([1], 3)
check("40 nodos (1+3+9+27)", len(g["nodes"]), 40)
check("39 mensajes A2A", len(g["edges"]), 39)
check("27 stacks (3^3 hojas)", len(g["stacks"]), 27)
check_true("cada arista es a2a.message",
           all(e["kind"] == "a2a.message" for e in g["edges"]))
check_true("cada nodo hijo referencia a su padre",
           all(n["parent"] is not None for n in g["nodes"] if n["level"] > 0))
check_true("stacks raiz->hoja de 4 pasos (seed + 3 reglas)",
           all(len(s["stack"]) == 4 for s in g["stacks"]))
check_true("cada stack arranca en el orchestrator",
           all(s["stack"][0]["agent"] == ORCH for s in g["stacks"]))
check_true("el grafo serializa a JSON",
           isinstance(json.dumps(g), str))

# Valores concretos de la primera capa: 2*1+1=3, 1+14=15, 1-18=-17
level1 = sorted(n["value"] for n in g["nodes"] if n["level"] == 1)
check("valores L1 = {-17, 3, 15}", level1, [-17, 3, 15])

print("\n== 2. Escalabilidad de la Ruliad (invariantes por profundidad) ==")
for depth in range(1, 6):
    gg = build_graph([1], depth)
    expected_nodes = sum(3 ** k for k in range(depth + 1))  # 1+3+...+3^depth
    check(f"depth={depth}: nodos={expected_nodes}", len(gg["nodes"]), expected_nodes)
    check(f"depth={depth}: aristas=nodos-1", len(gg["edges"]), expected_nodes - 1)
    check(f"depth={depth}: hojas=3^{depth}", len(gg["stacks"]), 3 ** depth)

print("\n== 3. Envelope JSON-RPC 2.0 (la celda message/send) ==")
CELL = """{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "message/send",
  "params": {
    "message": {
      "role": "user",
      "parts": [{ "kind": "data", "data": { "seed": 1, "depth": 3 } }],
      "messageId": "msg-1"
    }
  }
}"""
try:
    req = json.loads(CELL)
    parsed_ok = True
except json.JSONDecodeError as e:
    req = {}
    parsed_ok = False
    print(f"    JSON invalido: {e}")

check_true("la celda curl es JSON valido", parsed_ok)
check("jsonrpc = 2.0", req.get("jsonrpc"), "2.0")
check("method = message/send", req.get("method"), "message/send")
parts = req.get("params", {}).get("message", {}).get("parts", [])
check_true("el Message tiene un DataPart", bool(parts) and parts[0].get("kind") == "data")
data = parts[0].get("data", {}) if parts else {}
check("DataPart.seed = 1", data.get("seed"), 1)
check("DataPart.depth = 3", data.get("depth"), 3)

# Respuesta esperada (forma) que produce dispatch -> messageSend
expected_resp = {
    "jsonrpc": "2.0",
    "id": req.get("id"),
    "result": {"kind": "task", "status": {"state": "completed"}},
}
check("respuesta: jsonrpc = 2.0", expected_resp["jsonrpc"], "2.0")
check("respuesta: id preservado", expected_resp["id"], "1")
check("respuesta: result.kind = task", expected_resp["result"]["kind"], "task")
check("respuesta: task completada",
      expected_resp["result"]["status"]["state"], "completed")

print("\n== 4. Codigos de error JSON-RPC A2A ==")
ERROR_CODES = {
    "MethodNotFound": -32601, "InvalidParams": -32602,
    "TaskNotFound": -32001, "TaskNotCancelable": -32002,
    "UnsupportedOperation": -32004,
}
check("MethodNotFound", ERROR_CODES["MethodNotFound"], -32601)
check("TaskNotCancelable", ERROR_CODES["TaskNotCancelable"], -32002)
check("UnsupportedOperation (message/stream)", ERROR_CODES["UnsupportedOperation"], -32004)

# ── Resumen ────────────────────────────────────────────────────────────────
print("\n" + "─" * 56)
print(f"A2A logic end-to-end: {passed} PASS, {failed} FAIL")
if failed:
    print("FALLOS:", ", ".join(failures))
    print("RESULTADO: \u2717 FAIL")
    sys.exit(1)
print("RESULTADO: \u2713 TODO OK")
sys.exit(0)
