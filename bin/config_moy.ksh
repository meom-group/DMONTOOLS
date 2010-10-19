#!/bin/ksh

# this is the config file for means computation
# it must be customized for each configuration

CONFIG=<CONFIG>                   
CASE=<CASE>
MACHINE=<MACHINE>

CONFCASE=${CONFIG}-${CASE}

TYP_LIST='gridT gridU gridV gridW flxT dynT icemod trends' # list of grid type

case $MACHINE in
    'jade')

    USER=`whoami`
    REMOTE_USER=`whoami`
    P_S_DIR=$WORKDIR/${CONFIG}/${CONFCASE}-S          # input directory
    MOYTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmean      # tmpdir for means (absolute path)
    VTTMPDIR=$WORKDIR/${CONFIG}-${CASE}-tmpmeanvt     # tmpdir for VT (absolute path)
    MEANDIR=$WORKDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN # output directory
    MPDIR=$HOME/DMONTOOLS/MOY_PROD
    MPITOOLS=$HOME/DMONTOOLS/MPI_TOOLS
    CDFTOOLS=$WORKDIR/bin                             # place of executable

    STEP=1
    BATCH=PBS
    SUB=qsub ;;

    'ulam')

    USER=`whoami`
    REMOTE_USER=`whoami`
    SDIR=${SDIR}                                      # root of storage directory
    P_S_DIR=$SDIR/${CONFIG}/${CONFCASE}-S             # input directory
    MOYTMPDIR=${CONFIG}-${CASE}-tmpmean               # tmpdir for means (relative path)
    VTTMPDIR=${CONFIG}-${CASE}-tmpmeanvt              # tmpdir for VT (relative path)
    MEANDIR=$SDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN    # output directory
    MPDIR=$HOME/DMONTOOLS/MOY_PROD
    MPITOOLS=$HOME/DMONTOOLS/MPI_TOOLS
    CDFTOOLS=$WORKDIR/CDFTOOLS_forge                  # place of executable

    STEP=3
    BATCH=LoadLev
    SUB=llsubmit ;;

    'vargas')

    USER=`whoami`
    REMOTE_USER=`whoami`
    SDIR=${SDIR}                                      # root of storage directory
    P_S_DIR=$SDIR/${CONFIG}/${CONFCASE}-S             # input directory
    MOYTMPDIR=${CONFIG}-${CASE}-tmpmean               # tmpdir for means (relative path)
    VTTMPDIR=${CONFIG}-${CASE}-tmpmeanvt              # tmpdir for VT (relative path)
    MEANDIR=$SDIR/${CONFIG}/${CONFIG}-${CASE}-MEAN    # output directory
    MPDIR=$HOME/DMONTOOLS/MOY_PROD
    MPITOOLS=$HOME/DMONTOOLS/MPI_TOOLS
    CDFTOOLS=$WORKDIR/CDFTOOLS_forge                  # place of executable

    STEP=1
    BATCH=LoadLev
    SUB=llsubmit ;;

    *)
    echo available machines are jade ulam vargas  ; exit 1 ;;

esac
