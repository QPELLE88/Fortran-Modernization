# Fortran.io

An MVC web stack written in modern Fortran (2018): FastCGI controllers,
Jade-style HTML templates and a SQLite model layer, served behind nginx.

Major credit due to:

- authors of <a href="http://fortranwiki.org/fortran/show/FLIBS">FLIBS</a> (Arjen Markus and Michael Baudin), which earlier versions of this project were built on
- Ricolindo Carino and Arjen Markus's Fortran FastCGI program and tutorial - in many ways this started as an update to this tutorial :  http://flibs.sourceforge.net/fortran-fastcgi-nginx.html
- String utils by George Benthian http://www.gbenthien.net/strings/index.html
- <a href="https://github.com/branning">Philip Branning</a> for improving and documenting the install process, especially the SQLite parts
- <a href="https://github.com/divVerent">divVerent</a> for fixing a server crash bug due to URL params and SQL

## Layout

```
app/fortran_fcgi.f90    FastCGI entry point: reads the CGI environment, dispatches, writes the response
src/app.f90             controller / routes (/, /test, /search, /all, /healthz)
src/marsupial.f90       model: queries marsupials.sqlite3 with prepared statements
src/jade.f90            Jade-style template renderer (HTML-escapes #{...} by default)
src/http.f90            request/response types, query-string parsing, URL decoding
src/database.f90        thin ISO_C_BINDING wrapper over libsqlite3
src/fcgi.f90            ISO_C_BINDING interface to libfcgi
src/string_helpers.f90  string utilities
template/               .jade templates
static/                 static files served directly by nginx
test/                   unit tests (test_suite.f90) and HTTP smoke test (smoke.sh)
deploy/                 nginx config + entrypoint used by the Docker image
```

## Quick start (Docker)

```
docker build -t fortran-io .
docker run --rm -p 8080:8080 fortran-io
# open http://localhost:8080
```

The image compiles the app, runs the unit tests, and serves it with nginx +
spawn-fcgi as an unprivileged user. `GET /healthz` returns `ok`.

## Building from source

Requirements: a Fortran 2008+ compiler (gfortran 11 or newer is tested), make,
the SQLite and FastCGI development libraries, and nginx + spawn-fcgi to serve.

```
sudo ./install_deps_ubu.sh   # or install_deps_arch.sh / install_deps_osx.sh

make          # builds ./fortran_fcgi
make test     # builds and runs the unit tests
make smoke    # serves the app through a throwaway nginx and checks every route
```

The build uses `-std=f2018 -Wall -Wextra -Wimplicit-interface -fimplicit-none`.
Extra flags can be passed with `FFLAGS`, e.g. runtime checks and sanitizers:

```
make test FFLAGS="-O0 -g -fcheck=all -fsanitize=address,undefined"
```

## Serving with nginx

Point nginx at the checkout and forward dynamic requests to FastCGI, e.g. in
`/etc/nginx/sites-available/default`:

```
server {
  listen 80;
  root /home/fortran/fortran-machine;

  location /static/ { }

  location / {
    include fastcgi_params;
    fastcgi_pass 127.0.0.1:9000;
  }
}
```

Then restart nginx and spawn the server from the repository root (templates
and the database are resolved relative to the working directory):

```
sudo service nginx restart
spawn-fcgi -a 127.0.0.1 -p 9000 -d "$(pwd)" -- ./fortran_fcgi
```

After changing the source code, rebuild and respawn with `./restart.sh`.

`deploy/nginx.conf` is a complete, self-contained example. Don't forget an
HTTPS certificate, e.g. from Let's Encrypt.

## Fortran controller

Routes live in `src/app.f90`. Each handler renders HTML into `content`, which
is wrapped in a shared layout and returned in a `response_t`:

```fortran
select case (path)
case ('/')
  call jade_render_file(TEMPLATE_DIR // 'index.jade', content, ok)
case ('/search')
  call search_page(get_param(req, 'q'), content, ok)
case ('/all')
  call all_page(content, ok)
case ('/healthz')
  res%content_type = 'text/plain; charset=utf-8'
  res%body = 'ok'
  return
case default
  res%status = 404
  ...
end select
```

`app/fortran_fcgi.f90` builds the `request_t` (method, path, GET query string
and URL-encoded POST body) and writes the `Status`/`Content-Type` headers.

## Jade Templates

In the template folder, you can write HTML templates similar to Jade or HAML.

```jade
.container
  .col-sm-6
    h3 Hello #{name}!
  .col-sm-6
    h3 Link
    a.btn.btn-link(href="http://example.com/profile/#{id}", target="_blank") A link
    | plain text line
```

Supported: `tag.class#id(attr="value", other='x') text`, implicit `div` for
`.class`/`#id`, nesting by indentation, void elements (`input`, `br`, ...),
and `| text` lines. `#{key}` values are HTML-escaped; use `!{key}` for
trusted, pre-rendered HTML.

Variables are passed as `key_value_t` pairs:

```fortran
type(key_value_t) :: vars(2)
vars(1)%key = 'name'
vars(1)%value = item%name
vars(2)%key = 'wikiLink'
vars(2)%value = item%wiki_link
call jade_render_file('template/result.jade', html, ok, vars=vars)
```

If you want a loop, render a partial per item in the controller.

## SQLite Database

The example on <a href="https://fortran.io">Fortran.io</a> lets you search
through marsupials! `src/marsupial.f90` uses prepared statements, so user input
is always bound as data:

```fortran
call db%prepare('SELECT ' // COLUMNS // ' FROM marsupials ' // &
                'WHERE INSTR(LOWER(name), LOWER(?1)) > 0 ORDER BY rowid LIMIT 1', stmt, rc)
if (rc == SQLITE_OK) rc = stmt%bind_text(1, trim(query))
if (rc == SQLITE_OK) then
  if (stmt%step() == SQLITE_ROW) then
    found = .true.
    item%name = stmt%column_text(0)
    ...
  end if
end if
call stmt%finalize()
call db%close()
```

`list_marsupials(items, ok, limit)` returns an allocatable array of
`marsupial_t` that the controller loops over, rendering `template/result.jade`
for each one.

# License

This library is available under the BSD license (see LICENSE).
