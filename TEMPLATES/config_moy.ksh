#!/bin/ksh
##     ***  script  config_moy.ksh  ***
##  Set environment variables for DMONTOOLS/MOY_PROD scripts
## =====================================================================
## History : 1.0  !  2008     J.M. Molines      Original  code
##           2.0  !  2012     all contrib       Rationalization
## ----------------------------------------------------------------------
##  DMONTOOLS_2.0 , MEOM 2012
##  $Id: config_moy.ksh 540 2012-12-19 14:08:40Z molines $
##  Copyright (c) 2012, J.-M. Molines
##  Software governed by the CeCILL licence (Licence/DMONTOOLSCeCILL.txt)
## ----------------------------------------------------------------------
# this is the config file for means computation
# it must be customized for each configuration

CONFIG=<CONFIG>                   
CASE=<CASE>
MACHINE=<MACHINE>

CONFCASE=${CONFIG}-${CASE}

TYP_LIST='gridT gridU gridV gridW flxT dynT icemod trends' # list of grid type
XIOS=5d      # compute mean values from 5d output . If XIOS not used, set XIOS=
NC4=1        # if set to 1 use netcdf4 capabilities of the cdftools 
VVL=1        # if set to 1 use vvl capabilities of the  cdftools
REMDIR=/ccc/scratch/cont003/gen0727/$USER/  # root directory for the -S dirs
                                            # allow the use of another user

MPDIR=$DMON_ROOTDIR/MOY_PROD
# set xiosid according to XIOS FLAG
if [ $XIOS ] ; then
   xiosid=.$XIOS
else
   xiosid=
fi

case $MACHINE in
    ( jade )

    USER=`whoami`
    REMOTE_USER=`whoami`
    P_S_DIR=$WORKDIR/${CONFIG}/${CONFCASE}-S/$XIOS    # input directory
    MOYTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmean      # tmpdir for means (absolute path)
    VTTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmeanvt     # tmpdir for VT (absolute path)
    MEANDIR=$WORKDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN/$XIOS # output directory
    CDFTOOLS=$WORKDIR/bin                             # place of executable
    MPITOOLS=$WORKDIR/bin                             # mpitools executable
    WPDIR=$WORKDIR/WRUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF # Working Pdir for templates scripts
    RUNTOOLS=$DEVDIR/RUN_TOOLS/                       # RUNTOOLS used for run production

    runcode() { mpiexec_mpt -n $@ ; }

    STEP=1
    BATCH=PBS ;;

    ( occigen )

    USER=`whoami`
    REMOTE_USER=`whoami`
    P_S_DIR=$WORKDIR/${CONFIG}/${CONFCASE}-S/$XIOS    # input directory
    MOYTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmean      # tmpdir for means (absolute path)
    VTTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmeanvt     # tmpdir for VT (absolute path)
    MEANDIR=$WORKDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN/$XIOS # output directory
    CDFTOOLS=$WORKDIR/bin                             # place of executable
    MPITOOLS=$WORKDIR/bin                             # mpitools executable
    WPDIR=$WORKDIR/WRUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF # Working Pdir for templates scripts
    RUNTOOLS=$DEVDIR/RUN_TOOLS/                       # RUNTOOLS used for run production

    runcode() { srun --mpi=pmi2  -n $@ ; }

    STEP=1
    BATCH=OCCIGEN ;;


    ( curie )

    USER=`whoami`
    REMOTE_USER=`whoami`
    P_S_DIR=$DDIR/${CONFIG}/${CONFCASE}-S/$XIOS    # input directory
    MOYTMPDIR=$DDIR/${CONFIG}-${CASE}-tmpmean      # tmpdir for means (absolute path)
    VTTMPDIR=$DDIR/${CONFIG}-${CASE}-tmpmeanvt     # tmpdir for VT (absolute path)
    MEANDIR=$DDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN/$XIOS # output directory
    CDFTOOLS=$WORKDIR/bin                             # place of executable
    MPITOOLS=$WORKDIR/bin                             # mpitools executable
    WPDIR=$DDIR/WRUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF # Working Pdir for templates scripts
    RUNTOOLS=$DEVDIR/RUN_TOOLS/                       # RUNTOOLS used for run production
    QUEUE=standard
    ACCOUNT=gen0727

    runcode() { ccc_mprun -n $@ ; }

    # Make batch header for submitted scripts (not used but here for reference)
    # mk_batch_hdr  --name name --wallclock wallclock --account account --nodes nodes --cores cores --par --seq --option "options line" --help
    mk_batch_hdr() {
      # initialization of variables on curie
      name=''
      account=''
      wallclock=01:00:00
      nodes=1
      cores=1
      jobtype='serial'
      cluster='nhm'
      queue='test'
      mk_batch_hdr_core $@     # pass all input argument to the core of the function (in function_all)

      # on curie wall clock must be passed in seconds ( need to translate argument given as hh:mm:ss )
      wallclock_second=$( echo $wallclock | awk -F: '{print $1*3600 +$2*60 +$3}')

      #  Build header for curie 
     cat << eof 
#!/bin/bash
#MSUB -r $name
#MSUB -n $cores
#MSUB -T $wallclock_second
#MSUB -q $queue
#MSUB -o $name.o%I
#MSUB -e $name.e%I
#MSUB -A $account
eof
               }


    STEP=1
    BATCH=SLURM ;;

   ( ada )
    USER=`whoami`
    REMOTE_USER=`whoami`
    P_S_DIR=$WORKDIR/${CONFIG}/${CONFCASE}-S/$XIOS    # input directory
    MOYTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmean      # tmpdir for means (absolute path)
    VTTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmeanvt     # tmpdir for VT (absolute path)
    MEANDIR=$WORKDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN/$XIOS # output directory
    CDFTOOLS=$WORKDIR/bin                             # place of executable
    MPITOOLS=$WORKDIR/bin                             # mpitools executable
    TMPDIR=$WORKDIR                                   # replace LL TMPDIR
    WPDIR=$WORKDIR/WRUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF # Working Pdir for templates scripts
    RUNTOOLS=$DEVDIR/RUN_TOOLS/                       # RUNTOOLS used for run production

    STEP=1
    BATCH=ADA ;;

    ( vayu )

    USER=`whoami`
    REMOTE_USER=`whoami`
    P_S_DIR=$WORKDIR/${CONFIG}/${CONFCASE}-S          # input directory
    MOYTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmean      # tmpdir for means (absolute path)
    VTTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmeanvt     # tmpdir for VT (absolute path)
    MEANDIR=$WORKDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN # output directory
    CDFTOOLS=$WORKDIR/bin                             # place of executable
    MPITOOLS=$WORKDIR/bin                             # mpitools executable
    WPDIR=$WORKDIR/WRUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF # Working Pdir for templates scripts
    RUNTOOLS=$DEVDIR/RUN_TOOLS/                       # RUNTOOLS used for run production

    runcode() { mpirun -n $@ ; }
    STEP=1
    BATCH=PBS ;;

    *)
    echo available machines are jade ulam vargas  ; exit 1 ;;

esac
