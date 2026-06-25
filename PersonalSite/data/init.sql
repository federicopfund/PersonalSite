-- PersonalSite/data/init.sql
-- Schema y datos de ejemplo para desarrollo local.
-- Ejecutar con: sqlite3 data/site.db < PersonalSite/data/init.sql

CREATE TABLE IF NOT EXISTS posts (
    id         INTEGER PRIMARY KEY AUTOINCREMENT,
    slug       TEXT    NOT NULL UNIQUE,
    title      TEXT    NOT NULL,
    summary    TEXT    NOT NULL DEFAULT '',
    body       TEXT    NOT NULL DEFAULT '',
    date       TEXT    NOT NULL  -- ISO-8601: YYYY-MM-DD
);

-- Datos de ejemplo
INSERT OR IGNORE INTO posts (slug, title, summary, body, date) VALUES
  ('hola-wolfram',
   'Hola desde Wolfram Language',
   'Un primer vistazo al ecosistema Wolfram para desarrollo web.',
   '<p>Wolfram Language no es solo para cómputo simbólico. Con <code>HTTPResponse</code> y <code>URLDispatcher</code> podés construir aplicaciones web completas.</p>',
   '2026-06-01'),

  ('paclets-en-produccion',
   'Paclets en producción con Docker',
   'Cómo empaquetar un paclet Wolfram y servirlo con Wolfram Web Engine.',
   '<p>Un paclet es la unidad de distribución de código en Wolfram. Combinado con Docker, conseguís un deploy reproducible en minutos.</p>',
   '2026-06-10'),

  ('sqlite-wolfram',
   'SQLite como backend con EntityStore',
   'Usando RelationalDatabase y EntityStore para conectar WL a SQLite sin ORM.',
   '<p>La combinación <code>RelationalDatabase</code> + <code>EntityStore</code> + <code>RegisterEntityStore</code> es la forma idiomática de acceder a bases relacionales desde Wolfram Language.</p>',
   '2026-06-20');
