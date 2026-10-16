#!/bin/sh
#
# Rebuild and restart the Fortran FastCGI server.
set -eu
cd "$(dirname "$0")"

pkill -f fortran_fcgi || true
make
spawn-fcgi -a 127.0.0.1 -p "${FCGI_PORT:-9000}" -d "$(pwd)" -- ./fortran_fcgi
