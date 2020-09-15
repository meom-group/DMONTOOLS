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

### LoadLeveler on ULAM and VARGAS
## title of the run
# @ job_name = zint_${year1}-${year2}
## Output listing location
# @ output = \$(job_name).\$(jobid)
# @ error  = \$(output)
# @ job_type = serial
# specifique Adapp
# @ requirements = (Feature == "prepost")
# @ wall_clock_limit = 10:00:00
# @ as_limit = 3.2gb
# @ queue


CTL_DIR=$WPDIR
cd \$CTL_DIR
. ./config_moy.ksh
. ./function_moy.ksh

interannual $year1 $year2

eof

llsubmit zcalinter_${year1}-${year2}


