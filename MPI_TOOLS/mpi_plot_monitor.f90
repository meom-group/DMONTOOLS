PROGRAM mpi_plot_monitor
  !!=================================================================
  !!         ***  PROGRAM mpi_plot_monitor  ***
  !!
  !!  * Purpose : submit plot_monitor for each years on one processor
  !!
  !!  * Method: 
  !!
  !! * history : Jean-Marc Molines June 2010
  !!   
  !!==================================================================

  IMPLICIT NONE
  INTEGER :: iproc, ierror, nproc, ntask
  INTEGER :: iargc, narg, jarg
  INTEGER, DIMENSION(:), ALLOCATABLE :: iyears

  CHARACTER(LEN=80) :: cdum

  INCLUDE 'mpif.h'

  !! * Initialization

  ! Initialize MPI
  CALL mpi_init(ierror)
  CALL mpi_comm_rank(mpi_comm_world,iproc,ierror)
  CALL mpi_comm_size(mpi_comm_world,nproc,ierror)

  ! Read taglist from command line, return usage message if no arguments provided
  narg = iargc()

  IF (narg == 0 ) THEN
     PRINT *,' >>> Usage : mpi_plot_monitor year_list '
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
    WRITE(cdum,'("./plot_monitor.ksh ",i4.4," > log_plot_monitor_",i4.4," 2>&1 ")')  iyears(iproc+1), iyears(iproc+1)
    PRINT *, TRIM(cdum)
    CALL system( cdum )
  ENDIF

  CALL mpi_barrier(mpi_comm_world,ierror)
  CALL mpi_finalize(ierror)

END PROGRAM mpi_plot_monitor
