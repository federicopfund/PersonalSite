#!/usr/bin/env bash
# tools/a2a_e2e.sh
# --------------------------------------------------------------------------
# Validacion END-TO-END del protocolo A2A por HTTP contra un servidor vivo.
# Ejecuta exactamente la "celda" JSON-RPC de /a2a (curl message/send) y valida
# la Agent Card, el endpoint JSON-RPC, tasks/get y /a2a/run.
#
# Uso:
#   BASE_URL=http://localhost:8080 tools/a2a_e2e.sh
#   make a2a-e2e                      # usa BASE_URL o el default
#
# Requisitos: curl + python3 (solo stdlib).
# Salida: PASS/FAIL por chequeo y codigo de salida 0 (ok) / 1 (fallo).
# --------------------------------------------------------------------------
set -uo pipefail

BASE_URL="${BASE_URL:-http://localhost:8080}"
PASS=0
FAIL=0

green() { printf '\033[32m%s\033[0m\n' "$1"; }
red()   { printf '\033[31m%s\033[0m\n' "$1"; }
ok()    { PASS=$((PASS+1)); green "  ✓ $1"; }
ko()    { FAIL=$((FAIL+1)); red   "  ✗ $1"; }

# assert_json <json> <python-bool-expr-on-d> <label>
assert_json() {
  local json="$1" expr="$2" label="$3" res
  res=$(printf '%s' "$json" | python3 -c "
import sys, json
try:
    d = json.load(sys.stdin)
except Exception as e:
    print('PARSE_ERROR:' + str(e)); sys.exit(0)
print('OK' if ($expr) else 'NO')
" 2>/dev/null)
  case "$res" in
    OK) ok "$label" ;;
    *)  ko "$label  ($res)" ;;
  esac
}

# rpc_post <compact-json-payload>
# Envia un request JSON-RPC a /a2a. Intenta application/json (transporte A2A
# canonico); si el runtime no expone el body crudo (limitacion de WWE) cae al
# transporte form-urlencoded, que WWE si entrega via FormRules. Imprime el body.
rpc_post() {
  local payload="$1" out code body
  out=$(curl -s -X POST "$BASE_URL/a2a" \
    -H 'Content-Type: application/json' --data-binary "$payload" \
    -w $'\n%{http_code}' --max-time 30)
  code="${out##*$'\n'}"
  body="${out%$'\n'*}"
  if [ "$code" = "200" ]; then printf '%s' "$body"; return 0; fi
  # Fallback: form-urlencoded (WWE FormRules)
  curl -s -X POST "$BASE_URL/a2a" \
    -H 'Content-Type: application/x-www-form-urlencoded' --data-binary "$payload" \
    --max-time 30
}

echo "== A2A end-to-end HTTP · $BASE_URL =="

# ── 0. Servidor accesible ────────────────────────────────────────────────
if ! curl -fsS -o /dev/null --max-time 5 "$BASE_URL/" 2>/dev/null; then
  red "El servidor no responde en $BASE_URL — levantalo con 'make up' o 'make dev'."
  exit 1
fi
ok "servidor accesible"

# ── 1. Agent Card (discovery) ─────────────────────────────────────────────
echo "-- Agent Card /.well-known/agent-card.json"
CARD=$(curl -fsS --max-time 10 "$BASE_URL/.well-known/agent-card.json")
assert_json "$CARD" "d.get('protocolVersion') is not None"        "protocolVersion presente"
assert_json "$CARD" "d.get('preferredTransport')=='JSONRPC'"      "preferredTransport = JSONRPC"
assert_json "$CARD" "d.get('url','').endswith('/a2a')"            "url termina en /a2a"
assert_json "$CARD" "len(d.get('skills',[]))>=4"                  "skills >= 4"
assert_json "$CARD" "d.get('capabilities',{}).get('streaming')==False" "streaming = false"

# ── 2. JSON-RPC message/send (la celda) ──────────────────────────────────
echo "-- POST /a2a (message/send)"
REQ='{"jsonrpc":"2.0","id":"1","method":"message/send","params":{"message":{"role":"user","parts":[{"kind":"data","data":{"seed":1,"depth":3}}],"messageId":"msg-1"}}}'
SEND=$(rpc_post "$REQ")
assert_json "$SEND" "d.get('jsonrpc')=='2.0'"                     "jsonrpc = 2.0"
assert_json "$SEND" "d.get('id')=='1'"                            "id preservado"
assert_json "$SEND" "'error' not in d"                            "sin error"
assert_json "$SEND" "d.get('result',{}).get('kind')=='task'"      "result.kind = task"
assert_json "$SEND" "d.get('result',{}).get('status',{}).get('state')=='completed'" "task completada"
assert_json "$SEND" "len(d.get('result',{}).get('artifacts',[]))==3" "3 artifacts"

TASK_ID=$(printf '%s' "$SEND" | python3 -c "import sys,json;print(json.load(sys.stdin).get('result',{}).get('id',''))" 2>/dev/null)

# ── 3. tasks/get (round-trip) ─────────────────────────────────────────────
echo "-- POST /a2a (tasks/get)"
if [ -n "$TASK_ID" ]; then
  GET=$(rpc_post "{\"jsonrpc\":\"2.0\",\"id\":\"2\",\"method\":\"tasks/get\",\"params\":{\"id\":\"$TASK_ID\"}}")
  assert_json "$GET" "d.get('result',{}).get('id')=='$TASK_ID'"  "tasks/get devuelve la misma task"
else
  ko "no se obtuvo task id de message/send"
fi

# ── 4. /a2a/run (UI backend, query params) ────────────────────────────────
echo "-- GET /a2a/run?seed=1&depth=3"
RUN=$(curl -fsS --max-time 30 "$BASE_URL/a2a/run?seed=1&depth=3&backend=sync")
assert_json "$RUN" "d.get('ok') is True"                         "ok = true"
assert_json "$RUN" "d.get('stats',{}).get('messages')==39"       "39 mensajes A2A"
assert_json "$RUN" "len(d.get('graph',{}).get('nodes',[]))==40"  "40 nodos"
assert_json "$RUN" "len(d.get('graph',{}).get('stacks',[]))==27" "27 stacks"

# ── 5. Errores JSON-RPC ───────────────────────────────────────────────────
echo "-- POST /a2a (errores JSON-RPC)"
UNK=$(rpc_post '{"jsonrpc":"2.0","id":"9","method":"does/notExist","params":{}}')
assert_json "$UNK" "d.get('error',{}).get('code')==-32601"       "MethodNotFound (-32601)"

STREAM=$(rpc_post '{"jsonrpc":"2.0","id":"9","method":"message/stream","params":{}}')
assert_json "$STREAM" "d.get('error',{}).get('code')==-32004"    "UnsupportedOperation (-32004)"

# ── Resumen ───────────────────────────────────────────────────────────────
echo ""
echo "────────────────────────────────────────────────────────"
echo "A2A HTTP end-to-end: $PASS PASS, $FAIL FAIL"
if [ "$FAIL" -gt 0 ]; then
  red "RESULTADO: ✗ FAIL"; exit 1
else
  green "RESULTADO: ✓ TODO OK"; exit 0
fi
