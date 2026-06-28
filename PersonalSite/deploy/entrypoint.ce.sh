#!/bin/sh
# Entrypoint para Code Engine (stateless).
# 1) Siembra una DB efimera desde el seed horneado (las escrituras de
#    settings/tema viven solo durante la vida de la instancia).
# 2) Arranca Wolfram Web Engine en el puerto que Code Engine enruta.
#
# Licencia: si definis WOLFRAMSCRIPT_ENTITLEMENTID (on-demand licensing de
# Wolfram), el kernel la usa automaticamente. Sin licencia valida, los kernels
# no activan y la app no responde (ver README / docs de Wolfram).
set -eu

# Si IBM (o el operador) provee DATABASE_URL (postgres://user:pass@host:port/db?...)
# y no estan los PG* explicitos, se derivan aqui y se activa el backend postgresql.
if [ -n "${DATABASE_URL:-}" ] && [ -z "${PGHOST:-}" ]; then
  _u="${DATABASE_URL#*://}"
  _creds="${_u%%@*}"; _rest="${_u#*@}"
  _hostport="${_rest%%/*}"; _dbpart="${_rest#*/}"
  export PGUSER="${_creds%%:*}"
  export PGPASSWORD="${_creds#*:}"
  export PGHOST="${_hostport%%:*}"
  export PGPORT="${_hostport#*:}"
  export PGDATABASE="${_dbpart%%\?*}"
  export PERSONALSITE_DB_DRIVER="${PERSONALSITE_DB_DRIVER:-postgresql}"
fi

DB_PATH="${PERSONALSITE_DB:-/data/site.db}"
DB_DIR="$(dirname "$DB_PATH")"

mkdir -p "$DB_DIR" 2>/dev/null || true
if [ ! -f "$DB_PATH" ] && [ -f /seed/site.db ]; then
  cp /seed/site.db "$DB_PATH" || true
fi
chmod 666 "$DB_PATH" 2>/dev/null || true

# Wolfram license is pre-baked at /home/wolframengine/.WolframEngine/Licensing/mathpass.
# No runtime activation needed.
echo "[entrypoint] Wolfram mathpass at ${HOME}/.WolframEngine/Licensing/mathpass:"
ls -la "${HOME}/.WolframEngine/Licensing/" 2>/dev/null || echo "[entrypoint] WARNING: license dir not found"

exec python3 -m wolframwebengine \
  --domain 0.0.0.0 \
  --port "${PORT:-18000}" \
  --poolsize "${POOLSIZE:-1}" \
  /app/deploy/app.wl
