#!/usr/bin/env python3
"""
PersonalSite DevOps Bridge
==========================
Servidor HTTP minimo (stdlib puro, sin dependencias) que corre en el
devcontainer y expone operaciones git/build/test al kernel WolframWebEngine.

El kernel WL llama a este bridge via URLRead["http://172.18.0.1:8091/..."].

Arrancar:
  python3 tools/devops_bridge.py            # foreground (Ctrl+C para parar)
  python3 tools/devops_bridge.py &          # background

Endpoints:
  GET  /health
  POST /git/status       git status --porcelain
  POST /git/diff         git diff --stat HEAD
  POST /git/stage        git add -A
  POST /git/commit       git commit -m "auto: <ISO>" --allow-empty
  POST /git/push         git push origin main
  POST /git/log          git log --oneline -10
  POST /build/clean      rm build/*.paclet
  POST /build/paclet     python3 tools/build_paclet.py
  POST /build/verify     stat build/*.paclet
  POST /test/run         python3 tools/test_tasks.py
  POST /docker/verify    docker ps --filter name=profile-web-1
"""

import http.server
import json
import os
import subprocess
import sys
from datetime import datetime
from pathlib import Path

# ── Config ────────────────────────────────────────────────────────────
HOST = "0.0.0.0"
PORT = 8091
ROOT = Path(__file__).parent.parent.resolve()   # /workspaces/Profile


def run(args, cwd=None, timeout=60):
    """Execute a subprocess, return {ok, exit, out, err}."""
    try:
        r = subprocess.run(
            args,
            cwd=str(cwd or ROOT),
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        return {
            "ok":   r.returncode == 0,
            "exit": r.returncode,
            "out":  r.stdout[:2000],
            "err":  r.stderr[:600],
        }
    except subprocess.TimeoutExpired:
        return {"ok": False, "exit": -1, "out": "", "err": "timeout"}
    except Exception as e:
        return {"ok": False, "exit": -1, "out": "", "err": str(e)}


# ── Route table ──────────────────────────────────────────────────────
def handle_health(_body):
    return {
        "ok":    True,
        "service": "PersonalSite DevOps Bridge",
        "note":  "Este es el bridge interno (API). El sitio esta en el puerto 8080.",
        "site":  "http://localhost:8080",
        "kernel":"http://localhost:8080/kernel",
        "host":  str(ROOT),
        "ts":    datetime.now().isoformat(timespec="seconds"),
    }


def handle_git_status(_body):
    r = run(["git", "status", "--porcelain"])
    lines = [l for l in r["out"].splitlines() if l]
    return {**r, "changed": len(lines), "raw": r["out"][:400]}


def handle_git_diff(_body):
    r = run(["git", "diff", "--stat", "HEAD"])
    return {**r, "stat": r["out"][:400]}


def handle_git_stage(_body):
    return run(["git", "add", "-A"])


def handle_git_commit(_body):
    msg = "auto: devops bridge deploy " + datetime.now().isoformat(timespec="seconds")
    r = run(["git", "commit", "-m", msg, "--allow-empty"])
    return {**r, "msg": msg}


def handle_git_push(_body):
    r = run(["git", "push", "origin", "main"], timeout=90)
    return {**r, "summary": (r["out"] + r["err"])[:400]}


def handle_git_log(_body):
    r = run(["git", "log", "--oneline", "-10"])
    return {**r, "log": r["out"]}


def handle_build_clean(_body):
    build_dir = ROOT / "build"
    paclets = list(build_dir.glob("*.paclet")) if build_dir.exists() else []
    for p in paclets:
        p.unlink(missing_ok=True)
    return {"ok": True, "deleted": len(paclets)}


def handle_build_paclet(_body):
    r = run(["python3", str(ROOT / "tools" / "build_paclet.py")])
    built = list((ROOT / "build").glob("*.paclet")) if (ROOT / "build").exists() else []
    return {
        **r,
        "paclet": built[-1].name if built else "none",
        "bytes":  built[-1].stat().st_size if built else 0,
    }


def handle_build_verify(_body):
    build_dir = ROOT / "build"
    paclets = list(build_dir.glob("*.paclet")) if build_dir.exists() else []
    if not paclets:
        return {"ok": False, "err": "no .paclet in build/"}
    p = paclets[-1]
    return {"ok": True, "file": p.name, "bytes": p.stat().st_size}


def handle_test_run(_body):
    r = run(["python3", str(ROOT / "tools" / "test_tasks.py")], timeout=120)
    return {**r, "out": r["out"][:800]}


def handle_docker_verify(_body):
    r = run(["docker", "ps", "--filter", "name=profile-web-1", "--format", "{{.Status}}"])
    return {"ok": "Up" in r["out"], "status": r["out"].strip()}


ROUTES = {
    ("GET",  "/"):             handle_health,   # browser friendly fallback
    ("GET",  "/health"):       handle_health,
    ("POST", "/health"):       handle_health,
    ("POST", "/git/status"):     handle_git_status,
    ("POST", "/git/diff"):       handle_git_diff,
    ("POST", "/git/stage"):      handle_git_stage,
    ("POST", "/git/commit"):     handle_git_commit,
    ("POST", "/git/push"):       handle_git_push,
    ("POST", "/git/log"):        handle_git_log,
    ("POST", "/build/clean"):    handle_build_clean,
    ("POST", "/build/paclet"):   handle_build_paclet,
    ("POST", "/build/verify"):   handle_build_verify,
    ("POST", "/test/run"):       handle_test_run,
    ("POST", "/docker/verify"):  handle_docker_verify,
}


class BridgeHandler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        print(f"[bridge] {self.address_string()} {fmt % args}", flush=True)

    def _send_json(self, data, status=200):
        body = json.dumps(data).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        handler = ROUTES.get(("GET", self.path))
        if handler:
            self._send_json(handler({}))
        else:
            self._send_json({"ok": False, "err": f"unknown route GET {self.path}"}, 404)

    def do_POST(self):
        length  = int(self.headers.get("Content-Length", 0))
        body    = self.rfile.read(length).decode() if length else ""
        handler = ROUTES.get(("POST", self.path))
        if handler:
            try:
                result = handler(body)
                self._send_json(result)
            except Exception as e:
                self._send_json({"ok": False, "err": str(e)}, 500)
        else:
            self._send_json({"ok": False, "err": f"unknown route POST {self.path}"}, 404)

    def do_OPTIONS(self):
        self.send_response(204)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        self.end_headers()


if __name__ == "__main__":
    # SO_REUSEADDR: permite reiniciar sin esperar TIME_WAIT
    http.server.ThreadingHTTPServer.allow_reuse_address = True
    server = http.server.ThreadingHTTPServer((HOST, PORT), BridgeHandler)
    print(f"[bridge] DevOps Bridge listening on {HOST}:{PORT}")
    print(f"[bridge] Workspace root: {ROOT}")
    print(f"[bridge] WL kernel can reach at http://172.18.0.1:{PORT}/...")
    print(f"[bridge] Ctrl+C to stop", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\n[bridge] stopped")
        server.shutdown()
