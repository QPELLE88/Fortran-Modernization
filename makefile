# vim: noexpandtab: tabstop=4:

FLIBS=flibs-0.9/flibs/src

FORTRAN=gfortran
CC=gcc

HARDENING=-fstack-protector-strong -D_FORTIFY_SOURCE=2 -fPIE
FORTRANFLAGS=-O2 -fcheck=bounds $(HARDENING)
CFLAGS=-O2 $(HARDENING)
LDFLAGS=-pie -Wl,-z,relro,-z,now
LDLIBS=-lfcgi -lsqlite3 -pthread

OBJECTS = \
	marsupial.o \
	jade.o \
	string_helpers.o \
	fsqlite.o \
	cgi_protocol.o \
	fcgi_protocol.o \
	csqlite.o

fortran_fcgi: fortran_fcgi.f90 $(OBJECTS)
	$(FORTRAN) $(FORTRANFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

marsupial.o: marsupial.f90 string_helpers.o fsqlite.o
	$(FORTRAN) $(FORTRANFLAGS) -c $<

jade.o: jade.f90 string_helpers.o
	$(FORTRAN) $(FORTRANFLAGS) -c $<

string_helpers.o: string_helpers.f90
	$(FORTRAN) $(FORTRANFLAGS) -c $<

fsqlite.o: $(FLIBS)/sqlite/fsqlite.f90
	$(FORTRAN) $(FORTRANFLAGS) -c $<

cgi_protocol.o: $(FLIBS)/cgi/cgi_protocol.f90
	$(FORTRAN) $(FORTRANFLAGS) -c $<

fcgi_protocol.o: $(FLIBS)/cgi/fcgi_protocol.f90
	$(FORTRAN) $(FORTRANFLAGS) -c $<

csqlite.o: $(FLIBS)/sqlite/csqlite.c
	$(CC) $(CFLAGS) -DLOWERCASE -c $< -o $@

clean:
	rm -f -v fortran_fcgi *.o *.mod $(FLIBS)/sqlite/*.o

.PHONY: clean
