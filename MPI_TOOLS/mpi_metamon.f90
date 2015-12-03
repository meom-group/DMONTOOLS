PROGRAM mpi_metamon
  !!======================================================================
  !!                     ***  PROGRAM  mpi_metamon  ***
  !!=====================================================================
  !!  ** Purpose : Execute various instance of metamon in parallel ( 1 per year)
  !!
  !!  ** Method  : Use call system
  !!
  !! History : 1.0  : 11/2008  : J.M. Molines : Original code
  !!           2.0  : 12/2012  : J.M. Molines : Licence and coding rules
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER(KIND=4)                            :: iproc, ierror
  INTEGER(KIND=4)                            :: nproc, ntask
  INTEGER(KIND=4)                            :: iargc, narg, jarg
  INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: iyears

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
     PRINT *,' >>> Usage : mpi_metamon year_list '
     CALL mpi_finalize(ierror)
     STOP
  END IF

  ALLOCATE (iyears(narg) )

  DO jarg=1,narg
   CALL getarg(jarg,cdum) ; READ(cdum,*) iyears(jarg)
  ENDDO

  ! Max number of tags per processors

  !! * Dispatch the work ..
  IF ( nproc >= narg ) THEN
    WRITE(cdum,'("./monitor_prod.ksh ",i4," > log_monitor_prod_",i4.4," 2>&1 ")')  iyears(iproc+1), iyears(iproc+1)
    PRINT *, TRIM(cdum)
    CALL system( cdum )
  ENDIF

  CALL mpi_barrier(mpi_comm_world,ierror)
  CALL mpi_finalize(ierror)

END PROGRAM mpi_metamon
