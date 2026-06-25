
# Wolfram Web Engine for Python (AIOHTTP) + Wolfram Engine
# Imagen base: https://hub.docker.com/r/wolframresearch/wolframwebengineforpython
FROM wolframresearch/wolframwebengineforpython:latest
 
# ---- Codigo de la aplicacion ----
WORKDIR /app
COPY PersonalSite /app/PersonalSite
COPY PersonalSite/deploy/app.wl /app/deploy/app.wl
 
# ---- Directorio escribible para la base SQLite (montar un volumen aqui) ----
# La imagen base corre como usuario no-root; se necesita root solo para mkdir.
USER root
RUN mkdir -p /data && chmod 777 /data
USER wolframengine
 
# ---- Configuracion ----
# PERSONALSITE_ROOT apunta al directorio que contiene PersonalSite/
# para que PacletDirectoryLoad lo encuentre.
ENV PERSONALSITE_HOME=/app \
    PERSONALSITE_ROOT=/app \
    PERSONALSITE_DB=/data/site.db \
    PERSONALSITE_ENV=production \
    POOLSIZE=4
 
# AIOHTTP escucha en 18000 dentro del contenedor
EXPOSE 18000
 
# Modo dispatcher: TODO request pasa por app.wl.
# --poolsize controla la concurrencia (numero de kernels Wolfram). Ajustar a la CPU.
ENTRYPOINT []
CMD ["sh", "-c", "python3 -m wolframwebengine --domain 0.0.0.0 --port 18000 --poolsize ${POOLSIZE} /app/deploy/app.wl"]