#!/bin/ksh
if [ $# = 0 ] ; then
   echo USAGE: RUN_calinter.ksh  year1 year2
   exit 1
fi
#set -x

# get configuration settings
. ./config_moy.ksh
. ./function_moy.ksh

# set additional variables
mkdir -p  $WPDIR


# copy required scripts there:
cp -f config_moy.ksh $WPDIR
cp -f function_moy.ksh $WPDIR

# get year to work with
year1=$1
year2=$2


cat << eof > zcalinter_${year1}-${year2}
#!/bin/ksh
#PBS -N zint_${year1}-${year2}
#PBS -l select=1:ncpus=8:mpiprocs=8
#PBS -l walltime=1:30:00
#PBS -l cluster=hpt

CTL_DIR=$WPDIR
cd \$CTL_DIR
. ./config_moy.ksh
. ./function_moy.ksh

interannual $year1 $year2

eof

qsub zcalinter_${year1}-${year2}


