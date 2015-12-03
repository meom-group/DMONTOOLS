PROGRAM mpi_hostname
  !!======================================================================
  !!                     ***  PROGRAM  mpi_hostname  ***
  !!=====================================================================
  !!  ** Purpose : for mpi checking purpose. run mpi task indicating
  !!               their rank and the hostname
  !!
  !!  ** Method  : use elementary mpi lib calls
  !!
  !! History : 1.0  : 12/2012  : J.M. Molines : Original code
  !!----------------------------------------------------------------------

  !!----------------------------------------------------------------------
  !! MPI_TOOLS_1.0 , MEOM 2012
  !! $Id$
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/MPITOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INCLUDE 'mpif.h'
  INTEGER :: iproc, ierror, nproc, ntask
  INTEGER :: iargc, narg, ilen

  CHARACTER(LEN=80)                     :: cdum
  CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME) :: cproc
  LOGICAL                               :: ll_debug=.FALSE.
  !!----------------------------------------------------------------------
  narg = iargc()
  IF (narg == 0 ) THEN  ! no argument gives usage message .. but argument is not used at all
     PRINT *,' >>> Usage : mpi_hostname dummy_argument'
     STOP
  END IF

  ! Initialize MPI
  CALL mpi_init(ierror)                           ; CALL ErrHandler( ierror, 'mpi_init')
  CALL mpi_comm_rank(mpi_comm_world,iproc,ierror) ; CALL ErrHandler( ierror, 'mpi_comm_rank')
  CALL mpi_comm_size(mpi_comm_world,nproc,ierror) ; CALL ErrHandler( ierror, 'mpi_comm_size')
  CALL mpi_get_processor_name(cproc,ilen,ierror)  ; CALL ErrHandler( ierror, 'mpi_get_processor_name')

  PRINT *, 'RANK = ' , iproc, 'NTASK= ', nproc, 'HOST NAME = ', TRIM(cproc)
  CALL mpi_finalize(ierror) ; CALL ErrHandler( ierror, 'mpi_init')

CONTAINS

  SUBROUTINE ErrHandler ( kerror, cd_routine )
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE ErrHandler  ***
    !!
    !! ** Purpose :  Print error message if error status indicates error 
    !!
    !!----------------------------------------------------------------------
    INTEGER, INTENT(in)           :: kerror
    CHARACTER(LEN=*), INTENT(in) :: cd_routine 
    !!----------------------------------------------------------------------
    IF ( kerror /= 0 ) THEN 
       PRINT *,' MPI error number ',kerror ,' in routine ',TRIM(cd_routine)
       STOP
    ENDIF
 END SUBROUTINE ErrHandler 


END PROGRAM mpi_hostname
