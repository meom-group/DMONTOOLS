PROGRAM dmon_cortime
   !!======================================================================
   !!                     ***  PROGRAM  dmon_cortime  ***
   !!=====================================================================
   !!  ** Purpose : correct time_counter for files in MEAN
   !
   !!  ** Method  : analyse file name to deduce if it is monthly or annual mean
   !!               use a lookup table for finding time_counter (no leap only)
   !!               correct time_counter:units attribute and add global attribute
   !!
   !! History : 1.0  : J.M. Molines 
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   routines      : description
   !!----------------------------------------------------------------------
   USE netcdf
   !!----------------------------------------------------------------------
   !! DRAKKAR_TOOLS, MEOM 2014
   !! $Id$
   !! Copyright (c) 2012, J.-M. Molines
   !! Software governed by the CeCILL licence (Licence/DRAKKARTOOLSCeCILL.txt)
   !!----------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER(KIND=4),                PARAMETER :: jpy0=15534000
   INTEGER(KIND=4), DIMENSION(12), PARAMETER :: jpm0=(/ 1296000, 3672000, 6264000, 9072000, 11664000, &
        &                             14256000, 16848000, 19440000, 22032000, 24624000, 27216000, 30024000 /)
   INTEGER(KIND=4),                PARAMETER :: jpyref = 1958


   INTEGER(KIND=4)    :: narg, iargc
   INTEGER(KIND=4)    :: ipos1, ipos2, iyear, imonth=-1
   INTEGER(KIND=4)    :: ndastp
   INTEGER(KIND=4)    :: ierr, ncid, idt

   REAL(KIND=8), DIMENSION(1) :: dtime

   CHARACTER(LEN=255) :: cf_in
   CHARACTER(LEN=255) :: cv_time='time_counter'
   CHARACTER(LEN=255) :: cl_config, cl_case, cl_unit, cl_timeo
   CHARACTER(LEN=11)  :: ctag

   LOGICAL ::  lyear=.false., lmonth=.true.

   !!----------------------------------------------------------------------
   narg = iargc()
   IF ( narg == 0 ) THEN
      PRINT *,' usage :  dmon_cortime mean_file'
      PRINT *,'      '
      PRINT *,'     PURPOSE :'
      PRINT *,'       Correct time counter for the file according to its name (tag given'
      PRINT *,'       in name)' 
      PRINT *,'       CAUTION : work only with no-leap year calendar simulations'
      PRINT *,'      '
      PRINT *,'     ARGUMENTS :'
      PRINT *,'       mean_file : name of the mean file. '
      PRINT *,'      '
      PRINT *,'      '
      PRINT *,'     OUTPUT : '
      PRINT *,'       netcdf file : input file is overwritten '
      PRINT *,'      '
      PRINT *,'      '
      STOP
   ENDIF

   CALL getarg(1, cf_in)
   ! look for tag in CONFIG-CASE_tag_gridid.nc
   ipos1 = INDEX(cf_in,'_')    ;   ipos2 = INDEX(cf_in(ipos1+1:),'_')
   ctag = cf_in(ipos1+1:ipos1+ipos2-1)
   IF ( LEN(TRIM(ctag)) == 5 ) THEN 
      lyear=.true.
      lmonth = .false.
   ENDIF
   READ(ctag,'(x,I4)' ) iyear

   IF ( lmonth ) THEN
      READ(ctag,'(6x,I2)') imonth
   ENDIF
   ! Look for config case
   cl_config = cf_in(1 : ipos1-1)
   ipos2=INDEX(cl_config,'-')
   cl_case   = cl_config(ipos2+1:       )
   cl_config = cl_config(1      :ipos2-1)

   PRINT *,  TRIM(cf_in)

   IF ( lyear )  THEN
      dtime(1) = jpy0         + 365.d0*86400.d0 *( iyear - jpyref )
   ELSE
      dtime(1) = jpm0(imonth) + 365.d0*86400.d0 *( iyear - jpyref )
   ENDIF

   ierr = NF90_OPEN(cf_in, NF90_WRITE, ncid)
   ierr = NF90_INQ_VARID(ncid, cv_time, idt)
   ierr = NF90_REDEF(ncid)
   ! time attributes
   WRITE(cl_unit ,'("seconds since ",I4,"-01-01 00:00:00")') jpyref
   WRITE(cl_timeo,'(I4,"-JAN-01 00:00:00")'                ) jpyref
   ierr = NF90_PUT_ATT(ncid, idt, 'calendar', 'gregorian')
   ierr = NF90_PUT_ATT(ncid, idt, 'units',       cl_unit )
   ierr = NF90_PUT_ATT(ncid, idt, 'time_origin', cl_timeo)
   ierr = NF90_PUT_ATT(ncid, idt, 'title', 'time' )
   ierr = NF90_PUT_ATT(ncid, idt, 'long_name', 'Time axis' )
   ! global attribute
   ndastp = jpyref*10000+ 01*100 + 01
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'start_date', ndastp )
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'output_frequency', '5d' )
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CONFIG', TRIM(cl_config) )
   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'CASE', TRIM(cl_case) )

   ierr = NF90_ENDDEF(ncid)
   ierr = NF90_PUT_VAR(ncid, idt, dtime)
   ierr = NF90_CLOSE(ncid)


END PROGRAM dmon_cortime
