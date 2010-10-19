PROGRAM mpi_shell
  !!=================================================================
  !!         ***  PROGRAM mpi_shell  ***
  !!
  !!  * Purpose : submit various script given as argument in parallel
  !!
  !!  * Method: 
  !!
  !! * history : Jean-Marc Molines November 2009
  !!   
  !!==================================================================

  IMPLICIT NONE
  INTEGER :: iproc, ierror, nproc, ntask
  INTEGER :: iargc, narg

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
