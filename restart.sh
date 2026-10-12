#!/bin/sh
# Rebuild fortran_fcgi and restart it under spawn-fcgi.
set -e

pkill -x fortran_fcgi || true
make
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
