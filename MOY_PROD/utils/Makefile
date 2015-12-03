# Makefile for dmon_cortime.f90


include make.macro

dmon_cortime: dmon_cortime.f90
	$(FC) $(FFLAGS) dmon_cortime.f90 -o dmon_cortime

install: dmon_cortime
	mv dmon_cortime $(INSTALL)
