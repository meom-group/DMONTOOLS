PROGRAM mpi_shell
  !!======================================================================
  !!                     ***  PROGRAM  mpi_shell  ***
  !!=====================================================================
  !!  ** Purpose : submit various script given as argument in parallel
  !!
  !!  ** Method  : Use a call system for any process initialize by MPI
  !!
  !! History : 1.0  : 11/2009  : J.M. Molines : Original code
  !!           2.0  : 12/2012  : J.M. Molines : Licence and coding rules
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER(KIND=4)   :: iproc, ierror, nproc, ntask
  INTEGER(KIND=4)   :: iargc, narg

  CHARACTER(LEN=80) :: cdum

  INCLUDE 'mpif.h'
  !!----------------------------------------------------------------------
  !! DMONTOOLS_2.0 , MEOM 2012
  !! $Id$
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/DMONTOOLSCeCILL.txt)
  !!----------------------------------------------------------------------

  ! Initialize MPI
  CALL mpi_init(ierror)
  CALL mpi_comm_rank(mpi_comm_world,iproc,ierror)
  CALL mpi_comm_size(mpi_comm_world,nproc,ierror)

  ! Read taglist from command line, return usage message if no arguments provided
  narg = iargc()

  IF (narg == 0 ) THEN
     PRINT *,' >>> Usage : mpi_shell script_list '
     CALL mpi_finalize(ierror)
     STOP
  END IF

  ! Max number of tags per processors

  !! * Dispatch the work ..
  ntask = narg
   CALL getarg(iproc+1,cdum)
   print *, 'RANK = ' , iproc, 'NTASK= ', ntask,' cmd : ', TRIM(cdum)
   CALL system( cdum )

  CALL mpi_finalize(ierror)

END PROGRAM mpi_shell
