# Fortran.io

An MVC web stack written in modern Fortran (Fortran 2018): nginx serves static files and
forwards everything else over FastCGI to `fortran_fcgi`, which routes the request, queries
SQLite and renders Jade-style templates.

Major credit due to:

- authors of <a href="http://fortranwiki.org/fortran/show/FLIBS">FLIBS</a> (Arjen Markus and Michael Baudin), which the original stack was built on
- Ricolindo Carino and Arjen Markus's Fortran FastCGI program and tutorial - in many ways this started as an update to this tutorial :  http://flibs.sourceforge.net/fortran-fastcgi-nginx.html
- String utils by George Benthian http://www.gbenthien.net/strings/index.html
- <a href="https://github.com/branning">Philip Branning</a> for improving and documenting the install process, especially the SQLite parts
- <a href="https://github.com/divVerent">divVerent</a> for fixing a server crash bug due to URL params and SQL

## Layout

| Path | What it is |
| --- | --- |
| `src/main.f90` | FastCGI request loop (program `fortran_fcgi`) |
| `src/app.f90` | Controller: routing, page layout, status codes |
| `src/marsupials.f90` | Model: queries on the `marsupials` table |
| `src/jade.f90` | View: Jade-style template engine with HTML escaping |
| `src/sqlite_db.f90`, `src/fastcgi.f90` | `iso_c_binding` interfaces to libsqlite3 and libfcgi |
| `src/strings.f90`, `src/kv_list.f90`, `src/c_strings.f90` | String buffer, escaping, URL/form decoding |
| `template/` | Jade templates (re-read on every request) |
| `static/` | Files served directly by nginx |
| `deploy/nginx.conf` | nginx site config |
| `test/test_suite.f90`, `test/e2e.sh` | Unit tests and nginx + spawn-fcgi end-to-end tests |

`flibs-0.9/` is the vendored FLIBS library the original version used. The build no longer
depends on it.

## Quick start with Docker

```
docker build -t fortran-io .
docker run --rm -p 8080:80 fortran-io
```

Then open http://localhost:8080.

## Build and test

Install gfortran, make, libfcgi, SQLite, nginx and spawn-fcgi (`sudo ./install_deps_ubu.sh`,
`./install_deps_osx.sh` or `sudo ./install_deps_arch.sh`), then:

```
make          # builds ./fortran_fcgi with -std=f2018 -Wall -Wextra -Werror
make test     # unit tests, built with -fcheck=all
make e2e      # starts nginx + spawn-fcgi on local ports and checks every route with curl
```

CI (`.github/workflows/ci.yml`) runs all three plus a Docker build and smoke test.

## Deploy on an Ubuntu server

```
sudo apt-get update && sudo apt-get install -y git
git clone https://github.com/mapmeld/fortran-machine.git
cd fortran-machine
sudo ./install_deps_ubu.sh
make
```

Copy `deploy/nginx.conf` to `/etc/nginx/sites-available/fortran-io`, set `root` to your
checkout and `server_name` to your domain or IP, enable it and reload nginx:

```
sudo ln -s /etc/nginx/sites-available/fortran-io /etc/nginx/sites-enabled/
sudo rm /etc/nginx/sites-enabled/default
sudo nginx -t && sudo service nginx reload
```

Start the FastCGI server from the checkout (templates and `marsupials.sqlite3` are found
relative to the working directory, or to `$FORTRANIO_ROOT`; `$FORTRANIO_DB` overrides the
database path):

```
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
```

After changing the source, `./restart.sh` rebuilds and respawns the server.

Don't forget an HTTPS certificate, e.g. with Let's Encrypt (`sudo certbot --nginx`).

## Controller

Routes live in `handle_request` in `src/app.f90`. Each page appends HTML to a growable
`string_buffer`; the response status, headers and body are returned to `main.f90`, which
writes them to FastCGI.

```fortran
select case (req%path)
case ('/')
  call append_template(ctx, page, 'index.jade', ok, res)
case ('/search')
  call search_page(ctx, page, params%get('q'), ok, res)
case default
  res%status = 404
  res%body = error_page(res%status)
  return
end select
```

Query-string and form parameters are URL-decoded into a `string_map` (`params%get('q')`).
Unknown paths return 404, unsupported methods 405, request bodies over 64 KiB 413, and
template or database failures a generic 500 (details go to the nginx error log).

## Jade templates

Templates in `template/` use indentation for nesting, with `tag.class#id(attr="value") text`
on each line (the tag defaults to `div`) and `|` for plain text lines. Void elements such as
`input`, `br` and `hr` are not closed.

```jade
.container
  .col-sm-6
    h3 Hello #{name}!
  .col-sm-6
    a(href="https://example.com/profile/#{id}") A link
```

`#{name}` is replaced with the HTML-escaped value of a template variable, so data from the
database or the URL cannot inject markup. Template text itself is trusted and emitted as
written. Pass variables with a `string_map`:

```fortran
type(string_map) :: vars
call vars%set('name', 'Nick')
call vars%set('id', '42')
html = render_source(source, vars)   ! or: call render_template('template/x.jade', vars, html, ok)
```

## SQLite model

`src/marsupials.f90` uses prepared statements with bound parameters, so user input is never
spliced into SQL, and every statement is finalized:

```fortran
call run_query(db, SELECT_COLUMNS // &
  'WHERE instr(lower(name), lower(?1)) > 0 ORDER BY rowid LIMIT ?2', &
  query, MAX_SEARCH_RESULTS, results, ok)
```

Results come back as an allocatable array of `marsupial` records with allocatable string
fields, so there are no fixed column widths or row counts. The database is opened read-only
once at startup.

# License

This library, like FLIBS which it's based on, is available under the BSD license
