# Fortran.io

An MVC web stack written in modern Fortran (2018) — so you get arrays, modules and allocatable strings, and it's not punchcards.

## Quick start (Docker)

```
docker build -t fortran-io .
docker run --rm -p 8080:8080 fortran-io
```

Then open http://localhost:8080 — nginx serves `/static` and forwards everything else to the Fortran FastCGI worker.

## Project layout

```
app/fortran_fcgi.f90    FastCGI accept loop (program entry point)
src/controller.f90      routes: /, /test, /search, /all
src/jade.f90            Jade/HAML-like template renderer
src/marsupial.f90       SQLite model (marsupial_t rows)
src/string_helpers.f90  string utilities (replace_all, sql_quote, ...)
test/test_suite.f90     unit tests
test/smoke.sh           end-to-end FastCGI smoke test
template/               Jade templates
static/                 static docs served by nginx
deploy/                 nginx config + container entrypoint
flibs-0.9/              vendored FLIBS (SQLite and FastCGI bindings)
```

## Building and testing

Requires `gfortran`, `make`, `libsqlite3-dev`, `libfcgi-dev` (and `spawn-fcgi` + `libfcgi-bin` for the smoke test).
Objects and `.mod` files go to `build/`; the binary is `./fortran_fcgi`.

```
make                     # optimized build
make DEBUG=1 WERROR=1    # bounds/pointer checks, backtraces, warnings as errors
make test                # unit tests (templates, model, controller)
make smoke               # spawn the server and request every route over FastCGI
make serve               # spawn-fcgi on 127.0.0.1:9000
make clean
```

CI runs the strict build, both test suites and a Docker + nginx check on every pull request.

Major credit due to:

- authors of <a href="http://fortranwiki.org/fortran/show/FLIBS">FLIBS</a> (Arjen Markus and Michael Baudin)
- Ricolindo Carino and Arjen Markus's Fortran FastCGI program and tutorial - in many ways this started as an update to this tutorial :  http://flibs.sourceforge.net/fortran-fastcgi-nginx.html
- String utils by George Benthian http://www.gbenthien.net/strings/index.html
- <a href="https://github.com/branning">Philip Branning</a> for improving and documenting the install process, especially the SQLite parts
- <a href="https://github.com/divVerent">divVerent</a> for fixing a server crash bug due to URL params and SQL

## Create an Ubuntu server

Log in and install dependencies

```
# update Ubuntu
sudo apt-get update
sudo apt-get upgrade

# create the user and home directory
adduser fortran --gecos ""
usermod -a -G sudo fortran

# switch to new user
su fortran
cd ~

# install git and clone the repo
sudo apt-get install -y git
git clone https://github.com/QPELLE88/Fortran-Modernization.git fortran-machine

# Install dependencies
cd fortran-machine
sudo ./install_deps_ubu.sh
```

Go to your IP address - you should see the "Welcome to nginx!" page

Change the location in /etc/nginx/sites-available/default :

```
server_name 101.101.101.101; <- your IP address

location / {
	root /home/fortran/fortran-machine;
	index index.html;
}
```

Restart nginx to make these settings for real:

```
sudo service nginx restart
```

You should now see the test page on your IP address.

```
Test doc
```

## Use Fortran CGI script

Let's go from test page to Fortran script:

```
# compile the test server
make
```

Now change nginx config /etc/nginx/sites-available/default

```
location / {
	root /home/fortran/fortran-machine;
	fastcgi_pass 127.0.0.1:9000;
	fastcgi_index index.html;
	include fastcgi_params;
}
```

Then run ```sudo service nginx restart```

```
# spawn the server
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
```

### Restarting the script

After changing the source code, you can recompile and restart your server with:

```
./restart.sh
```

## Add a static folder

Add to nginx config /etc/nginx/sites-available/default

```
location /static {
    root /home/fortran/fortran-machine;
}
```

And restart nginx

```
sudo service nginx restart
```

## Fortran controller

Routes live in `src/controller.f90`:

```fortran
select case (trim(scriptName))
case ('/')
  call jadefile('template/index.jade', unit)
case ('/search')
  call search_page(dict, unit)
case ('/all')
  call all_page(unit)
case default
  write(unit, '(a)') 'Page not found!'
end select
```

## Jade Templates

In the template folder, you can write HTML templates similar to Jade or HAML.

If you want to have a loop or other structure, it's better to create a partial and run the loop in the Fortran controller.

```jade
.container
  .col-sm-6
    h3 Hello #{name}!
  .col-sm-6
    h3 Link
    a(href="http://example.com/profile/#{id}") A link
```

Template variables are passed as an array of `template_var` key/value pairs:

```fortran
call jadetemplate('template/hello.jade', unit, [ &
  template_var_of('name', 'Nick'), &
  template_var_of('id', '42')])
```

## SQLite Database

You can connect to a SQLite database. The example on <a href="https://fortran.io">Fortran.io</a>
lets you search through marsupials!

`src/marsupial.f90` returns rows as a derived type with allocatable strings, so there is no fixed column width or row limit:

```fortran
type :: marsupial_t
  character(len=:), allocatable :: name, latin_name, wiki_link, description
end type marsupial_t

call find_marsupial(query, found, m)   ! case-insensitive substring match
call list_marsupials(rows)             ! every row
```

User input is escaped with `sql_quote` before it reaches SQLite.

Then in the Fortran controller, you loop through the rows:

```fortran
call list_marsupials(rows)
do i = 1, size(rows)
  call jadetemplate('template/result.jade', unit, marsupial_vars(rows(i)))
end do
```

Then the individual result template:

```jade
.row
  .col-sm-12
    h4
      a(href="https://en.wikipedia.org#{wikiLink}") #{name}
    em #{latinName}
    hr
    p #{description}
```

## HTTPS Certificate

Don't forget to get a free HTTPS Certificate using LetsEncrypt!

https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-14-04

# License

This library, like FLIBS which it's based on, is available under the BSD license
