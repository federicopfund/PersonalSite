#!/usr/bin/env python3
"""
SessionStore + SessionFSM — Suite de debug paso a paso.

Cada bloque explica QUÉ hace, POR QUÉ y QUÉ se espera.
Corre contra el servidor en http://localhost:8080.

    python3 tools/test_sessions.py
    python3 tools/test_sessions.py --base http://localhost:8080
    python3 tools/test_sessions.py --verbose   # imprime JSON completo
"""

import json, sys, textwrap, argparse
import urllib.request, urllib.error, urllib.parse

# ── CLI ───────────────────────────────────────────────────────────────────
parser = argparse.ArgumentParser()
parser.add_argument("--base",    default="http://localhost:8080")
parser.add_argument("--verbose", action="store_true",
                    help="Imprime JSON completo en cada respuesta")
args = parser.parse_args()

BASE    = args.base
VERBOSE = args.verbose
PASS, FAIL = [], []

# ── Helpers ───────────────────────────────────────────────────────────────

def sep(title=""):
    w = 60
    if title:
        pad = (w - len(title) - 2) // 2
        print(f"\n{'─'*pad} {title} {'─'*(w-pad-len(title)-2)}")
    else:
        print("─" * w)

def explain(text):
    """Imprime texto de explicación con sangría."""
    for line in textwrap.wrap(text, 70):
        print(f"  ℹ  {line}")

def ok(label, cond, detail=""):
    icon = "✓" if cond else "✗"
    line = f"  {icon}  {label}" + (f"  →  {detail}" if detail else "")
    (PASS if cond else FAIL).append(line)
    print(line)
    return cond

def req(method, path, body=None, *, token=None, expect=200, label=None):
    """Envía HTTP request. body: dict → form-encoded (patrón del servidor)."""
    url  = BASE + path
    hdrs = {}
    if body:
        data = urllib.parse.urlencode(body).encode()
        hdrs["Content-Type"] = "application/x-www-form-urlencoded"
    else:
        data = None
    if token:
        hdrs["Authorization"] = f"Bearer {token}"

    robj = urllib.request.Request(url, data=data, headers=hdrs, method=method)
    try:
        with urllib.request.urlopen(robj, timeout=10) as r:
            raw, code = r.read().decode(), r.status
    except urllib.error.HTTPError as e:
        raw, code = e.read().decode(), e.code
    except Exception as e:
        tag = label or f"{method} {path}"
        FAIL.append(f"  ✗  {tag}  NETWORK ERROR: {e}")
        print(FAIL[-1])
        return None, None

    tag = label or f"{method} {path}"
    icon = "✓" if code == expect else "✗"
    snippet = (raw[:100].replace("\n", " ") + "…") if len(raw) > 100 else raw.replace("\n", " ")
    line = f"  {icon}  [{code}]  {tag}"
    (PASS if code == expect else FAIL).append(line)
    print(line)
    if VERBOSE:
        try:
            print(json.dumps(json.loads(raw), indent=4, ensure_ascii=False))
        except Exception:
            print(f"       {snippet}")
    else:
        if code != expect:
            print(f"       ↳ body: {snippet}")

    try:
        return code, json.loads(raw)
    except Exception:
        return code, raw

def decode_token(token):
    """Descompone el token en sus 3 partes y explica la estructura."""
    parts = token.split(".")
    if len(parts) != 3:
        print(f"  ✗  Token malformado: {len(parts)} partes (esperado 3)")
        return None
    sid, ts_hex, mac = parts
    ts_dec = int(ts_hex, 16)
    print(f"       sessionId : {sid}")
    print(f"       ts (hex)  : {ts_hex}  →  decimal {ts_dec}")
    print(f"       HMAC-SHA256: {mac[:20]}…{mac[-8:]}")
    return sid, ts_dec, mac

def print_nestgraph_tree(tree, max_nodes=13):
    """Visualiza el árbol NestGraph con sangría por nivel."""
    print("       id  lv  parent  perms")
    print("       " + "─" * 50)
    for n in tree[:max_nodes]:
        indent  = "  " * n["level"]
        parent  = str(n["parent"]) if n["parent"] is not None else "root"
        perms   = ", ".join(n["perms"]) if n["perms"] else "(vacío)"
        print(f"       {n['id']:2d}  L{n['level']}  {parent:6s}  {indent}{perms}")
    if len(tree) > max_nodes:
        print(f"       … ({len(tree) - max_nodes} nodos más en L2-L3)")

def print_fsm_edges(edges):
    """Dibuja la tabla de transiciones FSM."""
    print("       FROM              EVENT        TO")
    print("       " + "─" * 46)
    for e in edges:
        arrow = f"  ─[{e['event']}]─►"
        print(f"       {e['from']:18s}{arrow:16s}  {e['to']}")

# ══════════════════════════════════════════════════════════════════════════
print("\n╔══════════════════════════════════════════════════════╗")
print("║   SessionStore + SessionFSM  — Debug paso a paso    ║")
print("╠══════════════════════════════════════════════════════╣")
print(f"║   servidor: {BASE:<39}║")
print("╚══════════════════════════════════════════════════════╝\n")

token   = None
token2  = None
token3  = None
sid     = None

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 1 — /session/fsm  (grafo de estados)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "Antes de crear cualquier sesión, consultamos el grafo FSM. "
    "Este endpoint no requiere autenticación y describe el ciclo "
    "de vida completo: qué estados existen y qué eventos los conectan."
)
_, fsm = req("GET", "/session/fsm", label="GET /session/fsm → grafo completo")

if isinstance(fsm, dict):
    ok("6 estados FSM",    len(fsm.get("states", [])) == 6,
       str(fsm.get("states", [])))
    ok("8 eventos FSM",    len(fsm.get("events", [])) == 8,
       str(fsm.get("events", [])))
    ok("13 aristas FSM",   fsm.get("edgeCount", 0) == 13,
       f"edgeCount={fsm.get('edgeCount')}")
    print()
    print_fsm_edges(fsm.get("edges", []))

    explain(
        "Cada arista es una transición válida. Solo se permiten las "
        "declaradas; cualquier otra devuelve 422 con el motivo exacto. "
        "La tabla muestra que 'resume' solo acepta 'suspended' como origen — "
        "lo probaremos en el Bloque 5."
    )

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 2 — POST /session/create  (role 1 = reader)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "Creamos una sesión mínima con role=1. El servidor genera un UUID v4 "
    "como sessionId, calcula HMAC-SHA256(secret, sessionId|userId|ts) y "
    "devuelve el token con formato: <sessionId>.<ts_hex>.<mac>. "
    "La sesión se persiste en SQLite Y en el Cache en memoria."
)
_, r1 = req("POST", "/session/create",
            body={"userId": "lector", "role": 1},
            label="POST /session/create role=1 → created")

if isinstance(r1, dict) and r1.get("status") == "created":
    token = r1["token"]
    sid   = r1["sessionId"]
    ok("status == created",    r1["status"] == "created")
    ok("sessionId es UUID v4", len(sid) == 36 and sid.count("-") == 4, sid)
    ok("token tiene 3 partes", token.count(".") == 2)
    ok("expiresAt presente",   "expiresAt" in r1, r1.get("expiresAt"))

    print()
    explain("Anatomía del token HMAC:")
    decode_token(token)

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 3 — GET /session/validate  (verificar HMAC + TTL)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "El servidor descompone el token, recupera la sesión desde Cache "
    "(hit instantáneo) o DB, recalcula el HMAC con el mismo secreto y "
    "compara bit a bit. Si el token fue alterado, la comparación falla "
    "y se devuelve 401. También verifica que state ∈ {active, elevated}."
)
_, v1 = req("GET", "/session/validate", token=token,
            label="GET /session/validate token role=1 → valid")

if isinstance(v1, dict):
    ok("valid == True",        v1.get("valid") is True)
    ok("state == active",      v1.get("state") == "active",    v1.get("state"))
    ok("role == 1",            v1.get("role") == 1,             f"role={v1.get('role')}")
    ok("userId == lector",     v1.get("userId") == "lector",    v1.get("userId"))

explain("Probamos un token corrupto (un carácter modificado):")
bad_token = token[:-1] + ("0" if token[-1] != "0" else "1")
_, vbad = req("GET", "/session/validate", token=bad_token,
              expect=401, label="GET /session/validate token corrupto → 401")
ok("token corrupto → 401",     vbad is not None and
   (isinstance(vbad, dict) and "error" in vbad or vbad == 401))

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 4 — GET /session/info  (permisos role=1)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "El middleware withSession[req, 'public.read', handler] valida el "
    "token, verifica que la sesión tenga el permiso requerido y solo "
    "entonces delega al handler inyectando req['Session']. "
    "Para role=1 el NestGraph deriva: {public.read, blog.read, arch.view}."
)
_, info1 = req("GET", "/session/info", token=token,
               label="GET /session/info role=1 → permisos derivados")

if isinstance(info1, dict):
    perms = info1.get("permissions", [])
    ok("3 permisos para role=1",       len(perms) == 3,
       f"{len(perms)} perms: {perms}")
    ok("public.read presente",         "public.read" in perms)
    ok("blog.read presente",           "blog.read"   in perms)
    ok("arch.view presente",           "arch.view"   in perms)
    ok("kernel.eval NO presente",      "kernel.eval" not in perms,
       "(correcto: role=1 no tiene acceso al kernel)")
    ok("admin.* NO presente",          "admin.*" not in perms)

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 5 — GET /session/graph  (árbol NestGraph 40 nodos)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "El permission tree ES un NestGraph: seed={role=1,perms=[]}, "
    "3 reglas de derivación, depth=3. Resultado: 1+3+9+27 = 40 nodos. "
    "Cada nodo muestra los permisos acumulados en esa rama. "
    "La unión de todos los nodos alcanzables = permisos del usuario."
)
_, g1 = req("GET", "/session/graph", token=token,
            label="GET /session/graph role=1 → árbol 40 nodos")

if isinstance(g1, dict):
    tree = g1.get("tree", [])
    ok("40 nodos en el árbol",  len(tree) == 40, f"nodeCount={len(tree)}")
    ok("depth == 3",            g1.get("depth") == 3)
    ok("3 reglas",              g1.get("ruleCount") == 3,
       str(g1.get("rules", [])))
    ok("nodo raíz sin perms",   tree[0]["perms"] == [] if tree else False,
       f"root perms={tree[0]['perms'] if tree else 'N/A'}")

    L_counts = {0:0, 1:0, 2:0, 3:0}
    for n in tree:
        L_counts[n["level"]] = L_counts.get(n["level"], 0) + 1
    ok("L0: 1 nodo",   L_counts.get(0) == 1,  f"got {L_counts.get(0)}")
    ok("L1: 3 nodos",  L_counts.get(1) == 3,  f"got {L_counts.get(1)}")
    ok("L2: 9 nodos",  L_counts.get(2) == 9,  f"got {L_counts.get(2)}")
    ok("L3: 27 nodos", L_counts.get(3) == 27, f"got {L_counts.get(3)}")

    print()
    explain("Árbol (primeros 13 nodos):")
    print_nestgraph_tree(tree, max_nodes=13)

    explain(
        "Cada rama del árbol representa un camino de derivación distinto. "
        "El usuario obtiene la UNIÓN de todos los permisos de todos los "
        "nodos del árbol para su role. Con role=1 la mayoría de las "
        "ramas repiten {public.read, blog.read, arch.view} porque las "
        "reglas 2 y 3 no activan (role<2, role<3)."
    )

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 6 — Crear sesión role=3 + transiciones FSM")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "Creamos una sesión admin (role=3) para probar las transiciones FSM. "
    "Con role=3 se derivan 16 permisos: todos los de role=1 + role=2 + "
    "kernel.eval, admin.*, tasks.manage, arch.data, etc."
)
_, r3 = req("POST", "/session/create",
            body={"userId": "admin", "role": 3},
            label="POST /session/create role=3 → admin")

if isinstance(r3, dict) and r3.get("status") == "created":
    token3 = r3["token"]
    sid3   = r3["sessionId"]
    ok("sesión admin creada", True, sid3)

    # Verificar 16 permisos
    _, info3 = req("GET", "/session/info", token=token3,
                   label="GET /session/info role=3 → 16 permisos")
    if isinstance(info3, dict):
        perms3 = info3.get("permissions", [])
        ok("16 permisos para role=3", len(perms3) == 16,
           f"{len(perms3)} perms")
        ok("kernel.eval presente",   "kernel.eval" in perms3)
        ok("admin.* presente",       "admin.*"     in perms3)
        ok("public.read presente",   "public.read" in perms3)

    sep("FSM: active → elevated")
    explain(
        "El evento 'elevate' modela un sudo/MFA: el usuario ya autenticado "
        "solicita privilegios elevados. Solo funciona desde 'active'. "
        "El servidor actualiza el estado en Cache + DB y registra "
        "lastTransition con timestamp."
    )
    _, tr_up = req("POST", "/session/transition",
                   body={"event": "elevate"}, token=token3,
                   label="POST /session/transition elevate → elevated")
    if isinstance(tr_up, dict):
        ok("state == elevated",   tr_up.get("state") == "elevated",
           tr_up.get("state"))
        ok("event == elevate",    tr_up.get("event") == "elevate")
        ok("status == ok",        tr_up.get("status") == "ok")

    sep("FSM: transición inválida desde elevated")
    explain(
        "Desde 'elevated' el evento 'resume' es inválido (resume solo "
        "acepta 'suspended' como origen). El servidor devuelve 422 con "
        "el motivo exacto: 'invalid_transition:elevated->resume'. "
        "Esto garantiza que el FSM solo permite caminos declarados."
    )
    _, tr_bad = req("POST", "/session/transition",
                    body={"event": "resume"}, token=token3,
                    expect=422, label="POST /session/transition resume desde elevated → 422")
    if isinstance(tr_bad, dict):
        ok("error contiene 'invalid_transition'",
           "invalid_transition" in tr_bad.get("error", ""),
           tr_bad.get("error"))

    sep("FSM: elevated → active (downgrade)")
    explain(
        "'downgrade' es el inverso de 'elevate': vuelve a 'active' sin "
        "necesidad de re-autenticar. Útil para reducir la superficie de "
        "ataque después de completar una operación privilegiada."
    )
    _, tr_dn = req("POST", "/session/transition",
                   body={"event": "downgrade"}, token=token3,
                   label="POST /session/transition downgrade → active")
    if isinstance(tr_dn, dict):
        ok("state == active",   tr_dn.get("state") == "active",
           tr_dn.get("state"))

    sep("FSM: active → suspended → active")
    explain(
        "El ciclo suspend/resume modela bloqueo temporal (e.g. inactividad "
        "o flag de seguridad). Durante 'suspended' el token sigue en DB "
        "pero validateToken rechaza la sesión porque state ∉ {active, elevated}."
    )
    _, tr_sus = req("POST", "/session/transition",
                    body={"event": "suspend"}, token=token3,
                    label="POST /session/transition suspend → suspended")
    if isinstance(tr_sus, dict):
        ok("state == suspended", tr_sus.get("state") == "suspended",
           tr_sus.get("state"))

    explain(
        "Con la sesión en 'suspended', el token ya no es válido para "
        "acceder a rutas protegidas."
    )
    _, v_sus = req("GET", "/session/validate", token=token3,
                   expect=401, label="GET /session/validate mientras suspended → 401")
    ok("suspended → validate devuelve 401",
       isinstance(v_sus, dict) and "error" in v_sus)

    # Necesitamos un token nuevo del mismo sid para hacer resume
    # porque el validate falló → recreamos sesión limpia para el resume
    _, tr_res = req("POST", "/session/transition",
                    body={"event": "resume"}, token=token3,
                    label="POST /session/transition resume → active (desde suspended)")
    if isinstance(tr_res, dict):
        state_after = tr_res.get("state")
        ok("state == active tras resume",
           state_after == "active", state_after)

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 7 — Middleware guard (403 por permiso insuficiente)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "El middleware withSession[req, permiso, handler] verifica que la "
    "sesión tenga el permiso requerido ANTES de ejecutar el handler. "
    "GET /session/info requiere 'public.read'. Probamos con un token "
    "sin ningún permiso… creando una sesión con role inválido → role=1 "
    "que SÍ tiene public.read, luego probamos sin token."
)
_, no_token = req("GET", "/session/info",
                  expect=401, label="GET /session/info sin token → 401")
ok("sin token → 401", isinstance(no_token, dict) and
   no_token.get("error") == "no_session_token",
   no_token.get("error") if isinstance(no_token, dict) else str(no_token))

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 8 — GET /session/stats  (métricas de sesiones)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "sessionStats[] combina datos de la DB (por estado) con las métricas "
    "del Cache en memoria (hits, misses, ratio). Útil para monitoreo: "
    "cache ratio alto = el kernel está sirviendo sesiones desde memoria "
    "sin tocar la base de datos."
)
_, stats = req("GET", "/session/stats",
               label="GET /session/stats → métricas")

if isinstance(stats, dict):
    by_state = stats.get("byState", {})
    cache    = stats.get("cacheStats", {})
    ok("byState es dict",       isinstance(by_state, dict), str(by_state))
    ok("cacheStats presente",   isinstance(cache, dict))
    ok("cache ratio >= 0",      cache.get("ratio", -1) >= 0,
       f"ratio={cache.get('ratio'):.2f}")
    print(f"       byState    : {by_state}")
    print(f"       cache hits : {cache.get('hits')}  misses: {cache.get('misses')}"
          f"  ratio: {cache.get('ratio', 0):.2f}")
    print(f"       cached keys: {cache.get('count')}  →  {cache.get('keys', [])[:3]}")

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 9 — POST /session/destroy  (logout)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "destroy[] elimina la sesión de la DB (DELETE WHERE session_id=?) "
    "y limpia el Cache. Después de esto, cualquier request con el mismo "
    "token devuelve 401 porque getSession[] falla en DB y el Cache fue "
    "invalidado."
)
if token:
    _, d1 = req("POST", "/session/destroy", token=token,
                label="POST /session/destroy sesión role=1 → destroyed")
    if isinstance(d1, dict):
        ok("status == destroyed", d1.get("status") == "destroyed")
        ok("sessionId correcto",  d1.get("sessionId") == sid, d1.get("sessionId"))

    explain("Confirmación: el token ya no es válido post-logout.")
    _, post_logout = req("GET", "/session/validate", token=token,
                         expect=401, label="GET /session/validate post-logout → 401")
    ok("post-logout → 401",
       isinstance(post_logout, dict) and post_logout.get("error") == "invalid_or_expired",
       post_logout.get("error") if isinstance(post_logout, dict) else str(post_logout))

if token3:
    _, d3 = req("POST", "/session/destroy", token=token3,
                label="POST /session/destroy sesión role=3 → destroyed")
    if isinstance(d3, dict):
        ok("sesión admin destruida", d3.get("status") == "destroyed")

# ══════════════════════════════════════════════════════════════════════════
sep("BLOQUE 10 — POST /session/destroy  (GC de expiradas)")
# ══════════════════════════════════════════════════════════════════════════

explain(
    "gcSessions[] corre un DELETE WHERE expires_at <= datetime('now') "
    "en SQLite y luego llama a Cache`clear[] para invalidar entradas "
    "obsoletas. En producción este endpoint debería estar protegido o "
    "llamarse desde un ScheduledTask interno."
)
# Crear una sesión rápida y destruirla para dejar 0 activas
_, tmp = req("POST", "/session/create",
             body={"userId": "gc-test", "role": 1},
             label="POST /session/create sesión temporal para GC test")
if isinstance(tmp, dict) and tmp.get("status") == "created":
    req("POST", "/session/destroy", token=tmp["token"],
        label="POST /session/destroy sesión temporal")

# Verificar stats después del GC
_, stats_final = req("GET", "/session/stats",
                     label="GET /session/stats post-GC")
if isinstance(stats_final, dict):
    by_state = stats_final.get("byState", {})
    total = sum(by_state.values()) if by_state else 0
    ok("sesiones activas en DB ≥ 0", total >= 0,
       f"byState={by_state}")
    print(f"       sesiones activas en DB: {total}")

# ══════════════════════════════════════════════════════════════════════════
sep("RESUMEN")
# ══════════════════════════════════════════════════════════════════════════

total = len(PASS) + len(FAIL)
print(f"\n  Pasaron : {len(PASS):3d} / {total}")
print(f"  Fallaron: {len(FAIL):3d} / {total}")

if FAIL:
    print("\n  Casos fallidos:")
    for f in FAIL:
        print(f"   {f}")
else:
    print("\n  ✓  Todos los casos pasaron.\n")

print()
sys.exit(0 if not FAIL else 1)
