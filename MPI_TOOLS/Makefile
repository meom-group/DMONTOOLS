# Makefile for MPI_TOOLS 
## ----------------------------------------------------------------------
##  DMONTOOLS_2.0 , MEOM 2012
##  $Id$
##  Copyright (c) 2012, J.-M. Molines
##  Software governed by the CeCILL licence (Licence/DMONTOOLSCeCILL.txt)
## ----------------------------------------------------------------------

# ( make.macro is a link that points to the file macro.xxx where 
#   xxx is representative of your machine )

include make.macro


all: mpi_shell mpi_metamon mpi_plot_monitor

tpar3: tpar3.o
	$(FC) $(FFLAGS) tpar3.f -o tpar3

tpar4: tpar4.f
	$(FC) $(FFLAGS) tpar4.f -o tpar4

mpi_shell: mpi_shell.f90
	$(FC)  mpi_shell.f90 -o mpi_shell $(FFLAGS)

mpi_hostname: mpi_hostname.f90
	$(FC)  mpi_hostname.f90 -o mpi_hostname $(FFLAGS)

mpi_metamon: mpi_metamon.f90
	$(FC)  mpi_metamon.f90 -o mpi_metamon $(FFLAGS)

mpi_plot_monitor: mpi_plot_monitor.f90
	$(FC)  mpi_plot_monitor.f90 -o mpi_plot_monitor $(FFLAGS)

install : mpi_shell mpi_metamon mpi_plot_monitor
	mv mpi_shell mpi_metamon mpi_plot_monitor $(INSTALL)

clean:
	\rm -f *~ mpi_shell mpi_metamon mpi_plot_monitor
