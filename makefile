# Fortran.io build. Needs gfortran, libfcgi-dev and libsqlite3-dev (see install_deps_*.sh).

FC         = gfortran
FFLAGS     = -std=f2018 -O2 -g -Wall -Wextra -Wimplicit-interface -Werror -fimplicit-none
# gfortran 11 reports false maybe-uninitialized warnings for deferred-length strings at -O0.
TESTFLAGS  = -O0 -g -fcheck=all -fbacktrace -Wno-maybe-uninitialized
LDLIBS     = -lfcgi -lsqlite3
BUILD      = build

# Modules in dependency order.
MODULES = \
	src/strings.f90 \
	src/kv_list.f90 \
	src/c_strings.f90 \
	src/sqlite_db.f90 \
	src/fastcgi.f90 \
	src/jade.f90 \
	src/marsupials.f90 \
	src/app.f90

all: fortran_fcgi

fortran_fcgi: $(MODULES) src/main.f90 makefile
	@mkdir -p $(BUILD)/release
	$(FC) $(FFLAGS) -J$(BUILD)/release $(MODULES) src/main.f90 -o $@ $(LDLIBS)

$(BUILD)/test/test_suite: $(MODULES) test/test_suite.f90 makefile
	@mkdir -p $(BUILD)/test
	$(FC) $(FFLAGS) $(TESTFLAGS) -J$(BUILD)/test $(MODULES) test/test_suite.f90 -o $@ $(LDLIBS)

test: $(BUILD)/test/test_suite
	./$(BUILD)/test/test_suite

e2e: fortran_fcgi
	./test/e2e.sh

clean:
	rm -rf $(BUILD) fortran_fcgi *.o *.mod

.PHONY: all test e2e clean
