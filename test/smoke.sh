#!/usr/bin/env bash
# End-to-end smoke test: serves the app through a throwaway nginx +
# spawn-fcgi pair and checks the main routes over HTTP.
#
# Requires: nginx, spawn-fcgi, curl and a built ./fortran_fcgi.
# Usage: test/smoke.sh  (HTTP_PORT / FCGI_PORT override the default ports)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HTTP_PORT="${HTTP_PORT:-8089}"
FCGI_PORT="${FCGI_PORT:-9009}"
WORK="$(mktemp -d)"
BASE="http://127.0.0.1:${HTTP_PORT}"
FCGI_PID=""

cleanup() {
  [ -f "$WORK/nginx.pid" ] && kill "$(cat "$WORK/nginx.pid")" 2>/dev/null || true
  [ -n "$FCGI_PID" ] && kill "$FCGI_PID" 2>/dev/null || true
  rm -rf "$WORK"
}
trap cleanup EXIT

[ -x "$ROOT/fortran_fcgi" ] || { echo "build ./fortran_fcgi first (make)"; exit 1; }

FASTCGI_PARAMS=/etc/nginx/fastcgi_params
[ -f "$FASTCGI_PARAMS" ] || FASTCGI_PARAMS="$(dirname "$(nginx -V 2>&1 | sed -n 's/.*--conf-path=\([^ ]*\).*/\1/p')")/fastcgi_params"

cat > "$WORK/nginx.conf" <<CONF
worker_processes 1;
error_log $WORK/error.log;
pid $WORK/nginx.pid;
events {}
http {
  access_log off;
  client_body_temp_path $WORK;
  fastcgi_temp_path $WORK;
  proxy_temp_path $WORK;
  uwsgi_temp_path $WORK;
  scgi_temp_path $WORK;
  types { text/css css; text/html html; }
  server {
    listen 127.0.0.1:${HTTP_PORT};
    root $ROOT;
    location /static/ { }
    location / {
      include $FASTCGI_PARAMS;
      fastcgi_pass 127.0.0.1:${FCGI_PORT};
    }
  }
}
CONF

FCGI_PID="$(cd "$ROOT" && spawn-fcgi -a 127.0.0.1 -p "$FCGI_PORT" -d "$ROOT" -P "$WORK/fcgi.pid" -- "$ROOT/fortran_fcgi" >/dev/null && cat "$WORK/fcgi.pid")"
nginx -c "$WORK/nginx.conf" -p "$WORK" 2>/dev/null

for _ in $(seq 1 50); do
  curl -sf "$BASE/healthz" >/dev/null && break
  sleep 0.1
done

failures=0
expect() { # expect <method> <path> <status> <body substring> [curl args...]
  local method="$1" path="$2" status="$3" needle="$4"
  shift 4
  local out code
  out="$(curl -s -X "$method" -o "$WORK/body" -w '%{http_code}' "$@" "$BASE$path")" || true
  code="$out"
  if [ "$code" = "$status" ] && grep -qF -- "$needle" "$WORK/body"; then
    echo "ok   $method $path -> $code"
  else
    echo "FAIL $method $path -> $code (expected $status containing '$needle')"
    failures=$((failures + 1))
  fi
}

expect GET  /                          200 'finally, a Fortran Web Framework'
expect GET  '/search?q=koala'          200 'Phascolarctos cinereus'
expect GET  '/search?q=KANGA'          200 'Macropus rufus'
expect GET  '/search?q=%27%20OR%201=1' 200 'No results for'
expect GET  '/search?q=%3Cb%3E'        200 'No results for &quot;&lt;b&gt;&quot;'
expect POST /search                    200 'Petaurus breviceps' --data 'q=sugar'
expect GET  /all                       200 'Vombatus ursinus'
expect GET  /healthz                   200 'ok'
expect GET  /does-not-exist            404 'Page not found!'
expect GET  /static/bootstrap.min.css  200 'Bootstrap'

if [ "$failures" -ne 0 ]; then
  echo "$failures smoke check(s) failed"
  cat "$WORK/error.log" || true
  exit 1
fi
echo "all smoke checks passed"
