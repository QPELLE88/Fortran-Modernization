#!/usr/bin/env bash
# End-to-end check: curl -> nginx -> FastCGI -> ./fortran_fcgi. Runs unprivileged from the repo root.
set -euo pipefail
cd "$(dirname "$0")/.."

FCGI_PORT=${FCGI_PORT:-9100}
HTTP_PORT=${HTTP_PORT:-8090}
BASE="http://127.0.0.1:${HTTP_PORT}"
ROOT=$PWD
WORK=$(mktemp -d)
FAILED=0

cleanup() {
  [ -f "$WORK/nginx.pid" ] && kill "$(cat "$WORK/nginx.pid")" 2>/dev/null || true
  [ -f "$WORK/fcgi.pid" ] && kill "$(cat "$WORK/fcgi.pid")" 2>/dev/null || true
  rm -rf "$WORK"
}
trap cleanup EXIT

cat > "$WORK/nginx.conf" <<CONF
pid $WORK/nginx.pid;
error_log $WORK/error.log;
events {}
http {
  access_log off;
  include /etc/nginx/mime.types;
  client_body_temp_path $WORK/body;
  fastcgi_temp_path $WORK/fastcgi;
  proxy_temp_path $WORK/proxy;
  uwsgi_temp_path $WORK/uwsgi;
  scgi_temp_path $WORK/scgi;
  server {
    listen 127.0.0.1:$HTTP_PORT;
    location /static/ { root $ROOT; }
    location / {
      include /etc/nginx/fastcgi_params;
      fastcgi_pass 127.0.0.1:$FCGI_PORT;
    }
  }
}
CONF

spawn-fcgi -a 127.0.0.1 -p "$FCGI_PORT" -d "$ROOT" -P "$WORK/fcgi.pid" -- "$ROOT/fortran_fcgi" >/dev/null
nginx -c "$WORK/nginx.conf" -p "$WORK" 2>/dev/null

for _ in $(seq 50); do
  curl -fs -o /dev/null "$BASE/" && break
  sleep 0.1
done

pass() { echo "ok   - $1"; }
fail() { echo "FAIL - $1"; FAILED=1; }

# expect <label> <expected status> <must contain|-> <must not contain|-> curl-args...
expect() {
  local label=$1 status=$2 want=$3 reject=$4 out code
  shift 4
  out=$(curl -s -w '\n%{http_code}' "$@")
  code=${out##*$'\n'}
  out=${out%$'\n'*}
  if [ "$code" != "$status" ]; then fail "$label (status $code, want $status)"; return; fi
  if [ "$want" != "-" ] && [[ "$out" != *"$want"* ]]; then fail "$label (missing: $want)"; return; fi
  if [ "$reject" != "-" ] && [[ "$out" == *"$reject"* ]]; then fail "$label (unexpected: $reject)"; return; fi
  pass "$label"
}

expect "home page" 200 "finally, a Fortran Web Framework" - "$BASE/"
expect "static asset via nginx" 200 "bootstrap" - "$BASE/static/bootstrap.min.css"
expect "search koala" 200 "Phascolarctos cinereus" - "$BASE/search?q=koala"
expect "search is case-insensitive" 200 "Macropus rufus" - "$BASE/search?q=KANGAROO"
expect "search lists every match" 200 "Vombatus ursinus" - "$BASE/search?q=a"
expect "search with no match" 200 "No results for" "Phascolarctos" "$BASE/search?q=zzz"
expect "empty search" 200 "Type a marsupial name" "Phascolarctos" "$BASE/search?q="
expect "query is escaped" 200 "&lt;script&gt;" "<script>" "$BASE/search?q=%3Cscript%3Ealert(1)%3C%2Fscript%3E"
expect "SQL metacharacters are literal" 200 "No results for" "Phascolarctos" "$BASE/search?q=%27%20OR%201%3D1%20--"
expect "long query (4000 chars)" 200 "No results for" - "$BASE/search?q=$(printf 'k%.0s' $(seq 4000))"
expect "all marsupials" 200 "Petaurus breviceps" - "$BASE/all"
expect "form POST" 200 "Vombatus ursinus" - -X POST --data "q=wombat" "$BASE/search"
expect "oversized POST rejected" 413 "Payload Too Large" - -X POST --data-binary "@-" "$BASE/search" \
  < <(head -c 70000 /dev/zero | tr '\0' 'a')
expect "unknown route" 404 "Page not found!" - "$BASE/nope"
headers=$(curl -s -D - -o /dev/null "$BASE/" | tr -d '\r')
if [[ "$headers" == *"Content-Type: text/html; charset=utf-8"* ]]; then
  pass "content type header"
else
  fail "content type header"
fi

pid=$(cat "$WORK/fcgi.pid")
fds_before=$(ls /proc/"$pid"/fd | wc -l)
for _ in $(seq 200); do curl -s -o /dev/null "$BASE/search?q=a"; done
fds_after=$(ls /proc/"$pid"/fd | wc -l)
if [ "$fds_after" -le "$fds_before" ]; then
  pass "no descriptor leak over 200 requests ($fds_before -> $fds_after)"
else
  fail "descriptor leak over 200 requests ($fds_before -> $fds_after)"
fi
expect "server still serving after load" 200 "Phascolarctos cinereus" - "$BASE/search?q=koala"

exit $FAILED
