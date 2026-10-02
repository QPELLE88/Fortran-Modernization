FROM debian:bookworm-slim AS build
RUN apt-get update \
 && apt-get install -y --no-install-recommends gfortran gcc make libc6-dev libfcgi-dev libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY . .
RUN make clean && make WERROR=1

FROM debian:bookworm-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends nginx-light spawn-fcgi libgfortran5 libfcgi0ldbl libsqlite3-0 \
 && rm -rf /var/lib/apt/lists/* /etc/nginx/sites-enabled/default
WORKDIR /app
COPY --from=build /src/fortran_fcgi ./
COPY index.html marsupials.sqlite3 ./
COPY template ./template
COPY static ./static
COPY deploy/nginx.conf /etc/nginx/conf.d/fortran.conf
COPY deploy/entrypoint.sh /usr/local/bin/entrypoint.sh
EXPOSE 8080
CMD ["entrypoint.sh"]
