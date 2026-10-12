# Build stage: compile the FastCGI server.
FROM ubuntu:22.04 AS build
RUN apt-get update \
 && apt-get install -y --no-install-recommends gfortran make libfcgi-dev libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY makefile ./
COPY src ./src
RUN make fortran_fcgi

# Runtime stage: nginx in front of fortran_fcgi under spawn-fcgi.
FROM ubuntu:22.04
RUN apt-get update \
 && apt-get install -y --no-install-recommends nginx spawn-fcgi libfcgi0ldbl libsqlite3-0 libgfortran5 \
 && rm -rf /var/lib/apt/lists/* \
 && rm -f /etc/nginx/sites-enabled/default \
 && useradd --system --no-create-home fortran
WORKDIR /srv/fortran-io
COPY --from=build /src/fortran_fcgi ./
COPY template ./template
COPY static ./static
COPY marsupials.sqlite3 ./
COPY deploy/nginx.conf /etc/nginx/sites-enabled/fortran-io.conf
RUN sed -i 's|root .*;|root /srv/fortran-io;|' /etc/nginx/sites-enabled/fortran-io.conf
EXPOSE 80
CMD ["sh", "-c", "spawn-fcgi -u fortran -g fortran -a 127.0.0.1 -p 9000 ./fortran_fcgi && exec nginx -g 'daemon off;'"]
