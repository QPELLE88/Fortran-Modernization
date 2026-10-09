#!/bin/sh
#
# Rebuild and restart the Fortran.io FastCGI server on 127.0.0.1:9000.
set -e
cd "$(dirname "$0")"

pkill -f fortran_fcgi || true
make
spawn-fcgi -a 127.0.0.1 -p 9000 -d "$PWD" -- "$PWD/fortran_fcgi"
