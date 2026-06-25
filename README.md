# Profile
sitio personal

## Formulario de contacto

La página `/contacto` envía cada mensaje por correo a `CONTACT_TO`
(por defecto `federicopfund@gmail.com`) usando SMTP.

Para activar el envío, exportá las credenciales en el host antes de levantar
el contenedor (con Gmail, generá una *App Password* en
https://myaccount.google.com/apppasswords):

```bash
export SMTP_USER="tucuenta@gmail.com"
export SMTP_PASSWORD="tu-app-password"   # 16 caracteres, sin espacios
# opcional: export SMTP_FROM="tucuenta@gmail.com"
make up   # o: docker compose up -d
```

Variables disponibles (ver `docker-compose.yml`):

| Variable        | Default                     | Descripción                          |
| --------------- | --------------------------- | ------------------------------------ |
| `CONTACT_TO`    | `federicopfund@gmail.com`   | Destinatario de los mensajes         |
| `SMTP_SERVER`   | `smtp.gmail.com`            | Host SMTP                            |
| `SMTP_PORT`     | `587`                       | Puerto (STARTTLS)                    |
| `SMTP_USER`     | *(vacío)*                   | Usuario SMTP (secreto)              |
| `SMTP_PASSWORD` | *(vacío)*                   | Contraseña / app-password (secreto) |
| `SMTP_FROM`     | `SMTP_USER`                 | Dirección remitente                 |

Si `SMTP_USER`/`SMTP_PASSWORD` no están definidos, el formulario sigue
visible pero informa que el envío no está configurado (sin romper el sitio).

