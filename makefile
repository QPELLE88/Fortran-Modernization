# Fortran.io build
#
#   make            build ./fortran_fcgi
#   make DEBUG=1    build with runtime checks (bounds, pointers, backtraces)
#   make WERROR=1   treat compiler warnings in app code as errors
#   make test       run the unit tests
#   make smoke      start the FastCGI server and exercise every route
#   make serve      spawn the FastCGI server on 127.0.0.1:9000
#   make clean

ifeq ($(origin FC),default)
FC = gfortran
endif
CC ?= cc

BUILD   := build
FLIBS   := flibs-0.9/flibs/src
BIN     := fortran_fcgi
TESTBIN := $(BUILD)/test_suite

ifeq ($(DEBUG),1)
OPT := -O0 -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow
else
OPT := -O2
endif

WARN := -Wall -Wextra -Wimplicit-interface -Wno-maybe-uninitialized
ifeq ($(WERROR),1)
WARN += -Werror
endif

MODFLAGS     := -J$(BUILD) -I$(BUILD)
FFLAGS       := -std=f2018 -fimplicit-none $(WARN) $(OPT) $(MODFLAGS)
FFLAGS_FLIBS := $(OPT) $(MODFLAGS)
CFLAGS_FLIBS := -O2 -DLOWERCASE -Wno-discarded-qualifiers -Wno-incompatible-pointer-types
LDLIBS       := -lfcgi -lsqlite3

FLIBS_OBJS := \
	$(BUILD)/fsqlite.o \
	$(BUILD)/csqlite.o \
	$(BUILD)/cgi_protocol.o \
	$(BUILD)/fcgi_protocol.o

APP_OBJS := \
	$(BUILD)/string_helpers.o \
	$(BUILD)/jade.o \
	$(BUILD)/marsupial.o \
	$(BUILD)/controller.o

OBJS := $(FLIBS_OBJS) $(APP_OBJS)

all: $(BIN)

$(BIN): $(BUILD)/fortran_fcgi.o $(OBJS)
	$(FC) $(OPT) -o $@ $^ $(LDLIBS)

$(TESTBIN): $(BUILD)/test_suite.o $(OBJS)
	$(FC) $(OPT) -o $@ $^ $(LDLIBS)

$(BUILD):
	mkdir -p $@

# vendored FLIBS
$(BUILD)/fsqlite.o: $(FLIBS)/sqlite/fsqlite.f90 | $(BUILD)
	$(FC) $(FFLAGS_FLIBS) -c $< -o $@
$(BUILD)/cgi_protocol.o: $(FLIBS)/cgi/cgi_protocol.f90 | $(BUILD)
	$(FC) $(FFLAGS_FLIBS) -c $< -o $@
$(BUILD)/fcgi_protocol.o: $(FLIBS)/cgi/fcgi_protocol.f90 $(BUILD)/cgi_protocol.o | $(BUILD)
	$(FC) $(FFLAGS_FLIBS) -c $< -o $@
$(BUILD)/csqlite.o: $(FLIBS)/sqlite/csqlite.c | $(BUILD)
	$(CC) $(CFLAGS_FLIBS) -c $< -o $@

# application
$(BUILD)/%.o: src/%.f90 | $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@
$(BUILD)/fortran_fcgi.o: app/fortran_fcgi.f90 | $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@
$(BUILD)/test_suite.o: test/test_suite.f90 | $(BUILD)
	$(FC) $(FFLAGS) -c $< -o $@

$(BUILD)/jade.o: $(BUILD)/string_helpers.o
$(BUILD)/marsupial.o: $(BUILD)/string_helpers.o $(BUILD)/fsqlite.o
$(BUILD)/controller.o: $(BUILD)/jade.o $(BUILD)/marsupial.o $(BUILD)/fcgi_protocol.o
$(BUILD)/fortran_fcgi.o: $(BUILD)/controller.o $(BUILD)/fcgi_protocol.o
$(BUILD)/test_suite.o: $(BUILD)/controller.o $(BUILD)/fcgi_protocol.o

test: $(TESTBIN)
	./$(TESTBIN)

smoke: $(BIN)
	./test/smoke.sh

serve: $(BIN)
	spawn-fcgi -a 127.0.0.1 -p 9000 ./$(BIN)

clean:
	rm -rf $(BUILD) $(BIN)

.PHONY: all test smoke serve clean
