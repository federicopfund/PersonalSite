#!/usr/bin/env bash
# =============================================================================
#  Deploy end-to-end de PersonalSite a IBM Cloud Code Engine.
#
#  Requisitos previos:
#    - IBM Cloud CLI + plugins code-engine y container-registry (instalados).
#    - Una API key de IBM Cloud (o login interactivo --sso).
#    - data/site.db sembrada localmente (make seed).
#    - Licencia de Wolfram Engine valida en runtime (ver WOLFRAMSCRIPT_ENTITLEMENTID).
#
#  Variables (exportar antes de correr):
#    IBM_REGION          ej: us-south            (requerida)
#    IBM_RESOURCE_GROUP  ej: Default             (requerida)
#    CR_NAMESPACE        namespace en IBM CR     (requerida)
#    IBMCLOUD_API_KEY    API key (login no interactivo; recomendada)
#    CR_REGISTRY         registry host (default derivado de la region, ej us.icr.io)
#    CE_PROJECT          proyecto Code Engine    (default: personalsite)
#    APP_NAME            nombre de la app        (default: personalsite)
#    PERSONALSITE_NAME, POOLSIZE, FLOW_MAX_KERNELS, WOLFRAM_ALPHA_APPID,
#    WOLFRAM_LLM_APPID, WOLFRAMSCRIPT_ENTITLEMENTID  (opcionales)
# =============================================================================
set -euo pipefail
export PATH="$PATH:/usr/local/bin"

: "${IBM_REGION:?define IBM_REGION (ej: us-south)}"
: "${IBM_RESOURCE_GROUP:?define IBM_RESOURCE_GROUP (ej: Default)}"
: "${CR_NAMESPACE:?define CR_NAMESPACE}"

CE_PROJECT="${CE_PROJECT:-personalsite}"
APP_NAME="${APP_NAME:-personalsite}"
CR_REGISTRY="${CR_REGISTRY:-${IBM_REGION%%-*}.icr.io}"   # us-south -> us.icr.io
IMAGE="${CR_REGISTRY}/${CR_NAMESPACE}/${APP_NAME}:latest"

echo ">> Login a IBM Cloud ($IBM_REGION / $IBM_RESOURCE_GROUP)"
if [ -n "${IBMCLOUD_API_KEY:-}" ]; then
  ibmcloud login --apikey "$IBMCLOUD_API_KEY" -r "$IBM_REGION" -g "$IBM_RESOURCE_GROUP"
else
  ibmcloud login -r "$IBM_REGION" -g "$IBM_RESOURCE_GROUP" --sso
fi

echo ">> Container Registry: namespace + push ($IMAGE)"
ibmcloud cr region-set "$IBM_REGION" >/dev/null 2>&1 || true
ibmcloud cr namespace-add "$CR_NAMESPACE" >/dev/null 2>&1 || true
ibmcloud cr login

docker build -f PersonalSite/deploy/Dockerfile.codeengine -t "$IMAGE" .
docker push "$IMAGE"

echo ">> Code Engine: proyecto $CE_PROJECT"
ibmcloud ce project create --name "$CE_PROJECT" >/dev/null 2>&1 || true
ibmcloud ce project select --name "$CE_PROJECT"

if [ -n "${IBMCLOUD_API_KEY:-}" ]; then
  echo ">> Secret de acceso al registry (iamapikey)"
  ibmcloud ce registry create --name icr-secret --server "$CR_REGISTRY" \
    --username iamapikey --password "$IBMCLOUD_API_KEY" >/dev/null 2>&1 || \
  ibmcloud ce registry update --name icr-secret --server "$CR_REGISTRY" \
    --username iamapikey --password "$IBMCLOUD_API_KEY"
fi

if ibmcloud ce app get --name "$APP_NAME" >/dev/null 2>&1; then
  CE_VERB="update"
else
  CE_VERB="create"
fi

echo ">> Code Engine: app $CE_VERB ($APP_NAME)"
# shellcheck disable=SC2086
ibmcloud ce app "$CE_VERB" --name "$APP_NAME" \
  --image "$IMAGE" \
  ${IBMCLOUD_API_KEY:+--registry-secret icr-secret} \
  --port 18000 \
  --cpu 2 --memory 4G \
  --min-scale 1 --max-scale 3 \
  --env PERSONALSITE_NAME="${PERSONALSITE_NAME:-Federico}" \
  --env PERSONALSITE_ROOT=/app \
  --env PERSONALSITE_DB=/data/site.db \
  --env PERSONALSITE_DB_DRIVER="${PERSONALSITE_DB_DRIVER:-sqlite}" \
  --env POOLSIZE="${POOLSIZE:-2}" \
  --env FLOW_MAX_KERNELS="${FLOW_MAX_KERNELS:-1}" \
  --env WOLFRAM_ALPHA_APPID="${WOLFRAM_ALPHA_APPID:-}" \
  --env WOLFRAM_LLM_APPID="${WOLFRAM_LLM_APPID:-}" \
  ${DATABASE_URL:+--env DATABASE_URL="$DATABASE_URL"} \
  ${PGHOST:+--env PGHOST="$PGHOST"} \
  ${PGPORT:+--env PGPORT="$PGPORT"} \
  ${PGDATABASE:+--env PGDATABASE="$PGDATABASE"} \
  ${PGUSER:+--env PGUSER="$PGUSER"} \
  ${PGPASSWORD:+--env PGPASSWORD="$PGPASSWORD"} \
  ${PGSSLMODE:+--env PGSSLMODE="$PGSSLMODE"} \
  ${WOLFRAMSCRIPT_ENTITLEMENTID:+--env WOLFRAMSCRIPT_ENTITLEMENTID="$WOLFRAMSCRIPT_ENTITLEMENTID"}

echo ">> Listo. URL publica:"
ibmcloud ce app get --name "$APP_NAME" --output url
