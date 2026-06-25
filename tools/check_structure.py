#!/usr/bin/env python3
"""Validacion estructural del paclet PersonalSite (sin kernel Wolfram).

Verifica que existan los modulos de kernel, las plantillas referenciadas por
los controllers/vistas, los assets estaticos y los archivos de deploy. Salida
con codigo != 0 si encuentra problemas, para usarse como gate de CI.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PACLET = ROOT / "PersonalSite"

errors: list[str] = []


def require(rel: str, desc: str) -> None:
    if not (PACLET / rel).exists():
        errors.append(f"Falta {desc}: PersonalSite/{rel}")


# --- 1. Manifest y modulos de kernel ------------------------------------
require("PacletInfo.wl", "manifest del paclet")

kernel_modules = [
    "Kernel/init.wl",
    "Kernel/Config.wl",
    "Kernel/Router.wl",
    "Kernel/Models/Database.wl",
    "Kernel/Models/Post.wl",
    "Kernel/Models/WolframAlpha.wl",
    "Kernel/Models/Mailer.wl",
    "Kernel/Views/Renderer.wl",
    "Kernel/Controllers/HomeController.wl",
    "Kernel/Controllers/BlogController.wl",
    "Kernel/Controllers/WolframController.wl",
    "Kernel/Controllers/ContactController.wl",
]
for module in kernel_modules:
    require(module, "modulo de kernel")

# --- 2. init.wl debe cargar cada modulo ---------------------------------
init_path = PACLET / "Kernel" / "init.wl"
if init_path.exists():
    init_text = init_path.read_text(encoding="utf-8")
    for module in kernel_modules:
        if module in ("Kernel/init.wl",):
            continue
        leaf = Path(module).name
        if leaf not in init_text:
            errors.append(f"init.wl no carga el modulo: {leaf}")

# --- 3. Cada BeginPackage debe tener su EndPackage ----------------------
for wl in (PACLET / "Kernel").rglob("*.wl"):
    text = wl.read_text(encoding="utf-8")
    begins = len(re.findall(r"\bBeginPackage\[", text))
    ends = len(re.findall(r"\bEndPackage\[", text))
    if begins != ends:
        rel = wl.relative_to(PACLET)
        errors.append(f"BeginPackage/EndPackage desbalanceados en {rel} ({begins} vs {ends})")

# --- 4. Plantillas referenciadas por render[...] / fragment[...] --------
template_refs: set[str] = set()
ref_re = re.compile(r'(?:render|fragment|postItem)\[\s*"([^"]+)"')
for wl in (PACLET / "Kernel").rglob("*.wl"):
    for name in ref_re.findall(wl.read_text(encoding="utf-8")):
        template_refs.add(name)
# layout siempre se usa; postItem usa blog/item de forma implicita
template_refs.update({"layout", "blog/item"})
for name in sorted(template_refs):
    require(f"Resources/Templates/{name}.html", f"plantilla '{name}'")

# --- 5. Assets estaticos y deploy ---------------------------------------
require("Resources/Static/styles.css", "hoja de estilos compilada")
require("deploy/Dockerfile", "Dockerfile")
require("deploy/app.wl", "entrypoint de produccion")

# --- 6. Fuente SCSS de los estilos --------------------------------------
require("Resources/Scss/styles.scss", "entrypoint SCSS")
require("Resources/Scss/abstracts/_index.scss", "abstracts SCSS")
scss_entry = PACLET / "Resources" / "Scss" / "styles.scss"
if scss_entry.exists():
    scss_text = scss_entry.read_text(encoding="utf-8")
    used = re.findall(r'@use\s+[\'"]([^\'"]+)[\'"]', scss_text)
    for partial in used:
        parts = partial.split("/")
        parts[-1] = "_" + parts[-1] + ".scss"
        target = PACLET / "Resources" / "Scss" / Path(*parts)
        if not target.exists():
            errors.append(f"styles.scss usa un parcial inexistente: {partial}")

# --- Resultado ----------------------------------------------------------
if errors:
    print("Validacion del paclet FALLIDA:\n")
    for e in errors:
        print(f"  - {e}")
    sys.exit(1)

print("Validacion del paclet OK: estructura, modulos y plantillas presentes.")
