#!/usr/bin/env bash
# End-to-end check: fortran_fcgi under spawn-fcgi behind nginx, exercised with curl.
# Needs nginx, spawn-fcgi and curl. Run from the repository root via `make e2e`.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
FCGI_PORT=${FCGI_PORT:-9123}
HTTP_PORT=${HTTP_PORT:-8123}
WORK=$(mktemp -d)
BASE="http://127.0.0.1:${HTTP_PORT}"
FAILED=0
FCGI_PID=""

cleanup() {
  [ -f "$WORK/nginx.pid" ] && kill "$(cat "$WORK/nginx.pid")" 2>/dev/null || true
  [ -n "$FCGI_PID" ] && kill "$FCGI_PID" 2>/dev/null || true
  rm -rf "$WORK"
}
trap cleanup EXIT

pass() { echo "ok   - $1"; }
fail() { echo "FAIL - $1"; FAILED=1; }

expect_status() { # name expected-status curl-args...
  local name=$1 want=$2; shift 2
  local got
  got=$(curl -s -o /dev/null -w '%{http_code}' "$@")
  if [ "$got" = "$want" ]; then pass "$name"; else fail "$name (status $got, want $want)"; fi
}

expect_body() { # name pattern curl-args...
  local name=$1 pattern=$2; shift 2
  if curl -s "$@" | grep -qF -- "$pattern"; then pass "$name"; else fail "$name (missing: $pattern)"; fi
}

expect_no_body() { # name pattern curl-args...
  local name=$1 pattern=$2; shift 2
  if curl -s "$@" | grep -qF -- "$pattern"; then fail "$name (found: $pattern)"; else pass "$name"; fi
}

sed -e "s|listen 80;|listen 127.0.0.1:${HTTP_PORT};|" \
    -e "s|root .*;|root ${ROOT};|" \
    -e "s|127.0.0.1:9000|127.0.0.1:${FCGI_PORT}|" \
    -e "s|include fastcgi_params;|include /etc/nginx/fastcgi_params;|" \
    "$ROOT/deploy/nginx.conf" > "$WORK/site.conf"
cat > "$WORK/nginx.conf" <<CONF
worker_processes 1;
pid $WORK/nginx.pid;
error_log $WORK/error.log;
events { worker_connections 64; }
http {
  include /etc/nginx/mime.types;
  access_log off;
  client_body_temp_path $WORK;
  fastcgi_temp_path $WORK;
  proxy_temp_path $WORK;
  uwsgi_temp_path $WORK;
  scgi_temp_path $WORK;
  include $WORK/site.conf;
}
CONF

# Low descriptor limit so a per-request handle leak would kill the server quickly.
(cd "$ROOT" && ulimit -n 64 && exec spawn-fcgi -n -a 127.0.0.1 -p "$FCGI_PORT" ./fortran_fcgi) &
FCGI_PID=$!
nginx -c "$WORK/nginx.conf" -p "$WORK" 2>"$WORK/nginx.stderr" || { cat "$WORK/nginx.stderr"; exit 1; }

for _ in $(seq 50); do
  curl -s -o /dev/null "$BASE/static/bootstrap.min.css" && break
  sleep 0.1
done

expect_status "home page" 200 "$BASE/"
expect_body   "home page content" "finally, a Fortran Web Framework" "$BASE/"
expect_status "static file" 200 "$BASE/static/bootstrap.min.css"
expect_body   "search finds koala" "Phascolarctos cinereus" "$BASE/search?q=koala"
expect_body   "search is case-insensitive" "Macropus rufus" "$BASE/search?q=KANGAROO"
expect_body   "search shows every match" "Petrogale assimilis" "$BASE/search?q=a"
expect_body   "search shows every match (2)" "Petaurus breviceps" "$BASE/search?q=a"
expect_body   "blank search prompts" "Type a marsupial name" "$BASE/search?q="
expect_no_body "SQL injection is inert" "Phascolarctos" "$BASE/search?q=%27%20OR%201%3D1%20--"
expect_no_body "reflected XSS is escaped" "<script>alert" "$BASE/search?q=%3Cscript%3Ealert(1)%3C/script%3E"
expect_body   "long query is not dropped" "No marsupials match" "$BASE/search?q=$(printf 'k%.0s' $(seq 3000))"
expect_body   "/all lists the last row" "Vombatus ursinus" "$BASE/all"
expect_body   "POST search" "Macropus rufus" -d "q=kangaroo" "$BASE/search"
expect_status "unknown page" 404 "$BASE/nope"
expect_status "newline in path" 404 "$BASE/%0A%3Chtml%3E"
expect_status "other methods" 405 -X DELETE "$BASE/"
expect_status "HEAD" 200 -I "$BASE/"
head -c 200000 /dev/zero | tr '\0' 'a' > "$WORK/big"
expect_status "oversized POST is rejected" 413 --data-binary "@$WORK/big" "$BASE/search"

errors=0
for _ in $(seq 300); do
  code=$(curl -s -o /dev/null -w '%{http_code}' "$BASE/all")
  [ "$code" = 200 ] || errors=$((errors + 1))
done
if [ "$errors" = 0 ]; then pass "300 requests with ulimit -n 64"; else fail "$errors of 300 requests failed"; fi
expect_status "server still up" 200 "$BASE/"

if [ "$FAILED" = 0 ]; then echo "e2e: all checks passed"; else echo "e2e: failures"; exit 1; fi
