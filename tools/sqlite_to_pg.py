#!/usr/bin/env python3
"""Genera el schema + datos PostgreSQL a partir de la DB SQLite sembrada.

Uso:
    python3 tools/sqlite_to_pg.py [ruta_sqlite] | psql "<conn>"

Reutiliza el contenido canonico de data/site.db (posts + settings) sin
duplicar el SQL. Mapea tipos a PostgreSQL e idempotencia via ON CONFLICT.
"""
from __future__ import annotations
import sqlite3
import sys
from pathlib import Path

SRC = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("data/site.db")


def q(v) -> str:
    if v is None:
        return "NULL"
    return "'" + str(v).replace("'", "''") + "'"


def main() -> int:
    if not SRC.exists():
        print(f"-- ERROR: no existe {SRC}", file=sys.stderr)
        return 1
    con = sqlite3.connect(str(SRC))

    out = [
        "BEGIN;",
        "CREATE TABLE IF NOT EXISTS posts ("
        " id SERIAL PRIMARY KEY,"
        " slug TEXT UNIQUE NOT NULL,"
        " title TEXT NOT NULL,"
        " summary TEXT NOT NULL DEFAULT '',"
        " body TEXT NOT NULL DEFAULT '',"
        " date TEXT NOT NULL);",
        "CREATE TABLE IF NOT EXISTS settings ("
        " key TEXT PRIMARY KEY,"
        " value TEXT NOT NULL);",
    ]

    for slug, title, summary, body, date in con.execute(
        "SELECT slug, title, summary, body, date FROM posts ORDER BY date"
    ):
        out.append(
            "INSERT INTO posts (slug, title, summary, body, date) VALUES ("
            f"{q(slug)}, {q(title)}, {q(summary)}, {q(body)}, {q(date)})"
            " ON CONFLICT (slug) DO NOTHING;"
        )

    # La tabla settings puede no existir en DBs viejas.
    try:
        rows = list(con.execute("SELECT key, value FROM settings ORDER BY key"))
    except sqlite3.OperationalError:
        rows = []
    for key, value in rows:
        out.append(
            "INSERT INTO settings (key, value) VALUES ("
            f"{q(key)}, {q(value)})"
            " ON CONFLICT (key) DO NOTHING;"
        )

    out.append("COMMIT;")
    print("\n".join(out))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
