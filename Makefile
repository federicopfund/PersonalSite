# Makefile — PersonalSite deploy helpers
# Uso: make <target>

IMAGE   := personalsite:latest
COMPOSE := docker compose
SCSS_IN  := PersonalSite/Resources/Scss/styles.scss
CSS_OUT  := PersonalSite/Resources/Static/styles.css

.PHONY: build activate seed up down logs shell clean lint css css-watch ce-build ce-deploy paclet paclet-clean

## 0. Validar estructura del paclet (lo mismo que corre CI)
lint:
	python3 tools/check_structure.py

## 0b. Compilar los estilos SCSS -> CSS (paleta monocroma)
css:
	sass $(SCSS_IN) $(CSS_OUT) --style=compressed --no-source-map
	@echo "CSS compilado: $(CSS_OUT)"

## 0c. Recompilar SCSS automaticamente al guardar
css-watch:
	sass --watch $(SCSS_IN):$(CSS_OUT) --style=expanded

## 1. Construir la imagen Docker
build:
	docker build -f PersonalSite/deploy/Dockerfile -t $(IMAGE) .

## 1b. Construir el artifact .paclet → build/alpha-1.0.1.paclet
##     Uso: make paclet
##          make paclet CHANNEL=beta
##          make paclet OUT=dist
CHANNEL ?= alpha
OUT     ?= build

paclet: css
	@python3 tools/build_paclet.py --channel $(CHANNEL) --out $(OUT)

## Limpiar carpeta build/
paclet-clean:
	rm -rf build/
	@echo "build/ eliminada"

## 2. Activar Wolfram Engine en el volumen persistente
##    Requiere: Wolfram ID + contraseña en https://wolfram.com/developer
##    Solo se corre UNA vez.
activate:
	docker run --rm -it \
	  --hostname personalsite-prod \
	  -v profile_personalsite-wolfram:/home/wolframengine/.WolframEngine \
	  $(IMAGE) \
	  wolframscript -activate

## 3. Poblar la base SQLite con el schema y datos de ejemplo
seed:
	@mkdir -p data
	sqlite3 data/site.db < PersonalSite/data/init.sql
	@echo "Base de datos inicializada en data/site.db"

## 4. Copiar la DB al volumen Docker (después de seed)
##    El volumen tapa el `chmod` del Dockerfile, así que /data debe hacerse
##    escribible AQUÍ: el kernel corre como wolframengine (uid 999) y necesita
##    permiso de escritura para persistir settings (tema, rotación, etc.).
load-db:
	docker run --rm \
	  -v profile_personalsite-data:/data \
	  -v $(PWD)/data:/src \
	  alpine sh -c 'cp /src/site.db /data/site.db && chown -R 999:999 /data && chmod 775 /data && chmod 664 /data/site.db'
	@echo "site.db copiada al volumen profile_personalsite-data (escribible por uid 999)"

## 5. Levantar el servidor
up:
	$(COMPOSE) up -d
	@echo "Sitio en: http://localhost:8080"

## 6. Ver logs en tiempo real
logs:
	$(COMPOSE) logs -f

## 7. Bajar el servidor
down:
	$(COMPOSE) down

## 8. Abrir shell en el contenedor en ejecución
shell:
	docker exec -it $$($(COMPOSE) ps -q web) bash

## 9. Limpiar todo (imágenes, volúmenes, contenedores)
clean:
	$(COMPOSE) down -v
	docker rmi $(IMAGE) 2>/dev/null || true

## Flujo completo primera vez
first-run: build seed load-db activate up

## --- IBM Cloud Code Engine -------------------------------------------------
## Construir la imagen lista para Code Engine (DB horneada + entrypoint efimero)
ce-build: seed
	docker build -f PersonalSite/deploy/Dockerfile.codeengine -t personalsite:ce .

## Deploy end-to-end a Code Engine (requiere IBM_REGION, IBM_RESOURCE_GROUP,
## CR_NAMESPACE y, recomendado, IBMCLOUD_API_KEY exportados; ver el script).
ce-deploy:
	bash PersonalSite/deploy/codeengine.sh

## Codificar el mathpass para subirlo como GitHub Secret WOLFRAM_MATHPASS_B64.
## Uso: make encode-mathpass  (copia la salida y pega en Settings > Secrets)
encode-mathpass:
	@cat PersonalSite/deploy/mathpass | base64 -w0 && echo
