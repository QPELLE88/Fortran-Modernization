# Fortran.io

An MVC web stack written in modern Fortran (2018) (so you get arrays, and it's not punchcards)

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
git clone https://github.com/mapmeld/fortran-machine.git

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

## Build and run

```
make            # builds ./fortran_fcgi (gfortran -std=f2018 -Wall -Wextra -Werror)
make test       # Fortran unit tests (run with -fcheck=all)
make e2e        # nginx + spawn-fcgi end-to-end checks on 127.0.0.1, no root needed
```

Point nginx at the FastCGI server in /etc/nginx/sites-available/default:

```
location / {
	root /home/fortran/fortran-machine;
	fastcgi_pass 127.0.0.1:9000;
	include fastcgi_params;
}

location /static {
	root /home/fortran/fortran-machine;
}
```

Then run `sudo service nginx restart` and spawn the server from the repo directory
(templates and `marsupials.sqlite3` are opened relative to it; override with
`FORTRAN_IO_TEMPLATE_DIR` and `FORTRAN_IO_DB`):

```
spawn-fcgi -a 127.0.0.1 -p 9000 -d $PWD -- ./fortran_fcgi
```

After changing the source code, rebuild and respawn with `./restart.sh`.

Without nginx, the binary also runs as a plain CGI program, which is handy for debugging:

```
DOCUMENT_URI=/search QUERY_STRING=q=koala ./fortran_fcgi
```

## Layout

| File | Role |
| --- | --- |
| `src/main.f90` | FastCGI accept loop |
| `src/fastcgi.f90` | ISO_C_BINDING interface to libfcgi |
| `src/http.f90` | request parsing (query string, form POST), responses with status codes |
| `src/app.f90` | controller: routes and page assembly |
| `src/jade.f90` | Jade-style template renderer |
| `src/marsupials.f90` | model: queries on the `marsupials` table |
| `src/sqlite_db.f90` | ISO_C_BINDING interface to SQLite (prepared statements) |
| `src/strings.f90` | escaping, URL decoding, string helpers |
| `test/` | unit tests and the end-to-end script |

`flibs-0.9/` is the original library collection the first version of this app was built on; it is kept for
reference but no longer compiled or linked.

## Fortran controller

The controller in `src/app.f90` maps a request to a response:

```fortran
select case (req%path)
case ('/')
	resp = static_page(cfg, 'index.jade')
case ('/search')
	resp = search_page(cfg, get_param(req, 'q', ''))
case ('/all')
	resp = all_page(cfg)
case default
	resp = html_response(layout('<p>Page not found!</p>'), 404)
end select
```

## Jade Templates

In the template folder, you can write HTML templates similar to Jade or HAML.

If you want to have a loop or other structure, it's better to create a partial and run the loop in the Fortran controller.
`#{name}` placeholders are HTML-escaped; template text itself is trusted markup.

```jade
.container
  .col-sm-6
    h3 Hello #{name}!
  .col-sm-6
    h3 Link
    a(href="http://example.com/profile/#{id}") A link
```

## SQLite Database

You can connect to a SQLite database. The example on <a href="https://fortran.io">Fortran.io</a>
lets you search through marsupials! User input is bound as a parameter, never pasted into SQL:

```fortran
call db_open_readonly(db_path, db, ok, errmsg)
call db_prepare(db, 'SELECT name, latinName, wikiLink, description FROM marsupials' // &
                ' WHERE instr(lower(name), lower(?1)) > 0 ORDER BY rowid LIMIT ?2', stmt, ok)
if (ok) call stmt_bind_text(stmt, 1, query, ok)
do while (ok)
	call stmt_step(stmt, has_row, ok)
	if (.not. (ok .and. has_row)) exit
	row%name = stmt_column_text(stmt, 1)
	! ...
end do
call stmt_finalize(stmt)
call db_close(db)
```

The controller then renders `template/result.jade` once per row:

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
