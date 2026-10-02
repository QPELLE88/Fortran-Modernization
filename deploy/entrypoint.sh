#!/bin/sh
set -e
cd /app
spawn-fcgi -a 127.0.0.1 -p 9000 -u www-data -g www-data ./fortran_fcgi
exec nginx -g 'daemon off;'
