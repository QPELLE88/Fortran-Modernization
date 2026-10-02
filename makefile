# Fortran.io build
#
#   make            build ./fortran_fcgi
#   make test       build and run the unit tests
#   make smoke      run the end-to-end nginx + spawn-fcgi smoke test
#   make clean

ifeq ($(origin FC),default)
FC = gfortran
endif

BUILD   := build
FFLAGS  ?= -O2 -g
FFLAGS  += -std=f2018 -Wall -Wextra -Wimplicit-interface -fimplicit-none -J$(BUILD)
LDLIBS  := -lfcgi -lsqlite3

# Library sources in dependency order.
LIB_SRC := \
	src/string_helpers.f90 \
	src/http.f90 \
	src/database.f90 \
	src/fcgi.f90 \
	src/jade.f90 \
	src/marsupial.f90 \
	src/app.f90

LIB_OBJ := $(patsubst src/%.f90,$(BUILD)/%.o,$(LIB_SRC))

.PHONY: all test smoke clean

all: fortran_fcgi

fortran_fcgi: $(BUILD)/fortran_fcgi.o $(LIB_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

$(BUILD)/test_suite: $(BUILD)/test_suite.o $(LIB_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

test: $(BUILD)/test_suite
	./$(BUILD)/test_suite

smoke: fortran_fcgi
	./test/smoke.sh

$(BUILD):
	mkdir -p $@

$(BUILD)/%.o: src/%.f90 | $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@

$(BUILD)/fortran_fcgi.o: app/fortran_fcgi.f90 $(LIB_OBJ) | $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@

$(BUILD)/test_suite.o: test/test_suite.f90 $(LIB_OBJ) | $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@

# Module dependencies
$(BUILD)/http.o: $(BUILD)/string_helpers.o
$(BUILD)/jade.o: $(BUILD)/string_helpers.o
$(BUILD)/marsupial.o: $(BUILD)/database.o
$(BUILD)/app.o: $(BUILD)/http.o $(BUILD)/jade.o $(BUILD)/marsupial.o $(BUILD)/string_helpers.o

clean:
	rm -rf $(BUILD) fortran_fcgi
