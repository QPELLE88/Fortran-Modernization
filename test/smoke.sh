#!/usr/bin/env bash
# Starts ./fortran_fcgi under spawn-fcgi and exercises every route through the
# FastCGI protocol (requires spawn-fcgi and cgi-fcgi from libfcgi-bin).
set -euo pipefail

cd "$(dirname "$0")/.."
PORT=${SMOKE_PORT:-9123}
PIDFILE=$(mktemp)
FAILED=0

spawn-fcgi -a 127.0.0.1 -p "$PORT" -P "$PIDFILE" ./fortran_fcgi >/dev/null
trap 'kill "$(cat "$PIDFILE")" 2>/dev/null || true; rm -f "$PIDFILE"' EXIT
sleep 0.5

request() {
  env -i DOCUMENT_URI="$1" QUERY_STRING="${2:-}" REQUEST_METHOD=GET \
    SCRIPT_FILENAME=fortran_fcgi cgi-fcgi -bind -connect "127.0.0.1:$PORT"
}

expect() {
  local name=$1 needle=$2 uri=$3 query=${4:-} body
  body=$(request "$uri" "$query" || true)
  if grep -qF -- "$needle" <<<"$body"; then
    echo "ok   $name"
  else
    echo "FAIL $name: expected '$needle'"
    FAILED=1
  fi
}

expect "GET /"                'Setup Guide'                  /
expect "GET /test"            'Test button'                  /test
expect "GET /search?q=koala"  'Phascolarctos cinereus'       /search q=koala
expect "GET /search?q=KANG"   'Macropus rufus'               /search q=KANG
expect "GET /search?q=rock+w" 'Petrogale assimilis'          /search 'q=rock+w'
expect "GET /search?q=xyz"    'No results in this database'  /search q=xyz
expect "GET /search?q=o'b"    'No results in this database'  /search 'q=o%27b'
expect "GET /search?q=%"      '<html>'                       /search 'q=%'
expect "GET /search"           'No results in this database'  /search
expect "GET /search?q=x*500"  'No results in this database'  /search "q=$(printf 'x%.0s' {1..500})"
# QUERY_STRING beyond the 1024-char FastCGI buffer is dropped, so q is missing
expect "GET /search?q=x*2000" 'No results in this database'  /search "q=$(printf 'x%.0s' {1..2000})"
expect "GET /all"             'Vombatus ursinus'             /all
expect "GET /missing"         'Page not found!'              /missing
# the same worker must still be alive after serving every route
expect "GET / (again)"        'Setup Guide'                  /

exit $FAILED
