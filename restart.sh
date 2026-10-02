#!/bin/sh
#
# Recompile and restart fortran-machine
set -e

pkill -x fortran_fcgi || true
make
spawn-fcgi -a 127.0.0.1 -p 9000 ./fortran_fcgi
