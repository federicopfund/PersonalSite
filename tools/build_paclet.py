#!/usr/bin/env python3
"""
build_paclet.py — Construye el artifact .paclet de PersonalSite.

Uso:
    python3 tools/build_paclet.py [--channel CHANNEL] [--out DIR]

Produce:
    build/alpha-1.0.1.paclet   (channel-version.paclet)

Un .paclet es un ZIP con la estructura:
    PersonalSite/
        PacletInfo.wl
        Kernel/...
        Resources/...
"""

import argparse
import os
import re
import shutil
import sys
import zipfile
from pathlib import Path

# ── Rutas base ────────────────────────────────────────────────────────────
ROOT      = Path(__file__).resolve().parent.parent
SRC       = ROOT / "PersonalSite"
BUILD_DIR = ROOT / "build"

# Archivos/dirs que NO entran al paclet
EXCLUDE_PATTERNS = {
    "__pycache__", ".DS_Store", "*.pyc", "*.pyo",
    ".git", ".gitignore", "mathpass",         # secreto de activación
    "*.css.map", ".sass-cache",
}

# Directorios de primer nivel que NO son parte del paclet Wolfram
# Nota: "data" se incluye porque contiene site.db (pre-sembrada) e init.sql
EXCLUDE_TOP_DIRS = {"deploy"}

def should_exclude(path: Path) -> bool:
    for pat in EXCLUDE_PATTERNS:
        if pat.startswith("*"):
            if path.name.endswith(pat[1:]):
                return True
        elif path.name == pat:
            return True
    return False


def should_exclude_file(f: Path) -> bool:
    """Excluye por patrón de nombre o por directorio top-level prohibido."""
    rel = f.relative_to(SRC)
    # Excluir directorios top-level (deploy/, data/)
    if rel.parts[0] in EXCLUDE_TOP_DIRS:
        return True
    # Excluir por nombre en cualquier nivel
    return any(should_exclude(Path(part)) for part in rel.parts)


def read_version(paclet_info: Path) -> str:
    """Extrae la versión del PacletInfo.wl."""
    text = paclet_info.read_text(encoding="utf-8")
    m = re.search(r'"Version"\s*->\s*"([^"]+)"', text)
    if not m:
        print("ERROR: no se encontró Version en PacletInfo.wl", file=sys.stderr)
        sys.exit(1)
    return m.group(1)


def compile_scss() -> bool:
    """Intenta compilar SCSS. Retorna True si tuvo éxito o si no hay sass."""
    scss_in  = SRC / "Resources" / "Scss" / "styles.scss"
    css_out  = SRC / "Resources" / "Static" / "styles.css"
    if not scss_in.exists():
        return True
    if shutil.which("sass") is None:
        print("  [aviso] sass no encontrado — se incluye el CSS existente tal cual.")
        return True
    ret = os.system(f'sass "{scss_in}" "{css_out}" --style=compressed --no-source-map')
    if ret != 0:
        print("  [aviso] sass falló — se incluye el CSS existente tal cual.")
    else:
        print(f"  CSS compilado → {css_out.relative_to(ROOT)}")
    return True


def build_paclet(channel: str, out_dir: Path) -> Path:
    """Crea el archivo .paclet y devuelve su ruta."""
    paclet_info = SRC / "PacletInfo.wl"
    if not paclet_info.exists():
        print(f"ERROR: no se encontró {paclet_info}", file=sys.stderr)
        sys.exit(1)

    version  = read_version(paclet_info)
    filename = f"{channel}-{version}.paclet"
    out_path = out_dir / filename

    out_dir.mkdir(parents=True, exist_ok=True)

    # Recopilar archivos a incluir
    files = []
    for f in sorted(SRC.rglob("*")):
        if f.is_dir():
            continue
        if should_exclude_file(f):
            continue
        files.append(f)

    print(f"\n  Archivos incluidos: {len(files)}")

    # Construir el ZIP
    with zipfile.ZipFile(out_path, "w", zipfile.ZIP_DEFLATED, compresslevel=6) as zf:
        for f in files:
            # arcname: PersonalSite/<rel_path>
            arcname = Path("PersonalSite") / f.relative_to(SRC)
            zf.write(f, arcname)

    size_kb = out_path.stat().st_size / 1024
    return out_path, version, len(files), size_kb


def validate_paclet(out_path: Path):
    """Verifica que el .paclet tenga los archivos obligatorios."""
    required = [
        "PersonalSite/PacletInfo.wl",
        "PersonalSite/Kernel/init.wl",
        "PersonalSite/Resources/Static/styles.css",
    ]
    with zipfile.ZipFile(out_path) as zf:
        names = set(zf.namelist())
    missing = [r for r in required if r not in names]
    if missing:
        print(f"\n  [ERROR] Archivos obligatorios faltantes en el paclet:")
        for m in missing:
            print(f"    ✗  {m}")
        sys.exit(1)
    print(f"  Validación: OK ({len(required)} archivos obligatorios presentes)")


def main():
    parser = argparse.ArgumentParser(description="Build PersonalSite .paclet")
    parser.add_argument("--channel", default="alpha",
                        help="Canal de distribución (default: alpha)")
    parser.add_argument("--out", default=str(BUILD_DIR),
                        help=f"Directorio de salida (default: {BUILD_DIR})")
    parser.add_argument("--no-css", action="store_true",
                        help="Saltar compilación de SCSS")
    args = parser.parse_args()

    out_dir = Path(args.out).resolve()

    print("══════════════════════════════════════════════════════")
    print("  PersonalSite — Paclet Build")
    print("══════════════════════════════════════════════════════")
    print(f"  Fuente  : {SRC}")
    print(f"  Canal   : {args.channel}")
    print(f"  Salida  : {out_dir}")

    # 1. Compilar CSS
    if not args.no_css:
        print("\n── 1. SCSS → CSS ─────────────────────────────────────")
        compile_scss()

    # 2. Construir paclet
    print("\n── 2. Empaquetando ───────────────────────────────────")
    out_path, version, n_files, size_kb = build_paclet(args.channel, out_dir)

    # 3. Validar estructura
    print("\n── 3. Validando ──────────────────────────────────────")
    validate_paclet(out_path)

    # 4. Resumen
    rel = out_path.relative_to(ROOT)
    print(f"\n══════════════════════════════════════════════════════")
    print(f"  ✓  Artifact: {rel}")
    print(f"     Versión : {version}  |  Canal: {args.channel}")
    print(f"     Archivos: {n_files}  |  Tamaño: {size_kb:.1f} KB")
    print(f"══════════════════════════════════════════════════════\n")

    # Cargar desde wolframscript:
    print("  Para cargar en WolframScript:")
    print(f'    PacletInstall["{out_path}"]')
    print(f'    Needs["PersonalSite`"]')
    print()


if __name__ == "__main__":
    main()
