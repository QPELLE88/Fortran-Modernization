# Fortran.io build. Requires gfortran, libfcgi-dev and libsqlite3-dev (see install_deps_*.sh).

FC       = gfortran
FFLAGS   = -std=f2018 -O2 -g -Wall -Wextra -Wimplicit-interface -Werror -fimplicit-none
# gfortran 11 emits false maybe-uninitialized warnings for deferred-length strings at -O0.
TESTFLAGS = -O0 -fcheck=all -fbacktrace -Wno-maybe-uninitialized
LDLIBS   = -lfcgi -lsqlite3
BUILD    = build

# Module sources, in dependency order.
SRC = \
	src/strings.f90 \
	src/http.f90 \
	src/fastcgi.f90 \
	src/sqlite_db.f90 \
	src/jade.f90 \
	src/marsupials.f90 \
	src/app.f90

all: fortran_fcgi

fortran_fcgi: $(SRC) src/main.f90 makefile
	@mkdir -p $(BUILD)/release
	$(FC) $(FFLAGS) -J$(BUILD)/release $(SRC) src/main.f90 -o $@ $(LDLIBS)

$(BUILD)/test/test_suite: $(SRC) test/test_suite.f90 makefile
	@mkdir -p $(BUILD)/test
	$(FC) $(FFLAGS) $(TESTFLAGS) -J$(BUILD)/test $(SRC) test/test_suite.f90 -o $@ $(LDLIBS)

test: $(BUILD)/test/test_suite
	./$(BUILD)/test/test_suite

e2e: fortran_fcgi
	./test/e2e.sh

clean:
	rm -rf $(BUILD) fortran_fcgi *.o *.mod

.PHONY: all test e2e clean
