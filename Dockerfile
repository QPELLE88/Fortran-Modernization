# syntax=docker/dockerfile:1

FROM ubuntu:24.04 AS build
RUN apt-get update \
 && apt-get install -y --no-install-recommends gfortran make libfcgi-dev libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY makefile ./
COPY src ./src
COPY app ./app
COPY test ./test
COPY template ./template
COPY marsupials.sqlite3 ./
RUN make test && make

FROM ubuntu:24.04
RUN apt-get update \
 && apt-get install -y --no-install-recommends nginx spawn-fcgi libfcgi0t64 libsqlite3-0 libgfortran5 curl \
 && rm -rf /var/lib/apt/lists/* \
 && useradd --system --home-dir /app fortran
WORKDIR /app
COPY --from=build /src/fortran_fcgi ./
COPY template ./template
COPY static ./static
COPY deploy ./deploy
COPY marsupials.sqlite3 ./
USER fortran
EXPOSE 8080
HEALTHCHECK --interval=30s --timeout=3s CMD curl -fsS http://127.0.0.1:8080/healthz || exit 1
ENTRYPOINT ["/app/deploy/docker-entrypoint.sh"]
