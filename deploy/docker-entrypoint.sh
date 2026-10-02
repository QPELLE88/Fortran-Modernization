#!/bin/sh
set -eu
cd /app
spawn-fcgi -a 127.0.0.1 -p 9000 -d /app -- /app/fortran_fcgi
exec nginx -c /app/deploy/nginx.conf -g 'daemon off;'
