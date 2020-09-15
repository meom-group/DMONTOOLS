#!/bin/ksh
#  Script to be use on ADA, with multi step LL job
# It basically prepare temporary monthly script in WPDIR, ready to be launched in parallel by
# the multistep job

if [ $# = 0 ] ; then
   echo USAGE: RUN_calmoy.ksh  year
   exit 1
fi
set -x

# get configuration settings
. ./config_moy.ksh
. ./function_moy.ksh

# set additional variables
WPDIR=$WORKDIR/RUN_${CONFIG}/${CONFIG}-${CASE}/CTL/CDF
mkdir -p $WPDIR

# copy required scripts there:
cp -f config_moy.ksh $WPDIR
cp -f function_moy.ksh $WPDIR

# copy scripts used for annual mean (step 3)
cp -f $MPDIR/mvcalmoy.ksh $WPDIR
cp -f $MPDIR/calannual.ksh $WPDIR

# get year to work with
year=$1

#prepare the 12 scripts in WPDIR for cdfmoy ( step 1)
for m in $(seq 1 12 ) ; do
  mm=$(printf "%02d" $m )
  cat $MPDIR/calmoy_mm.ksh | sed -e "s/<year>/$year/g" -e "s/<mm>/$mm/g" -e "s/<m>/$m/g" > $WPDIR/zz_calmoy_${mm}_$year.ksh
  chmod 755 $WPDIR/zz_calmoy_${mm}_$year.ksh
done

#prepare the 12 scripts in WPDIR for VT (step 2)
for m in $(seq 1 12 ) ; do
  mm=$(printf "%02d" $m )
  cat $MPDIR/calmoyvt_mm.ksh | sed -e "s/<year>/$year/g" -e "s/<mm>/$mm/g" -e "s/<m>/$m/g" > $WPDIR/zz_calmoyvt_${mm}_$year.ksh
  chmod 755 $WPDIR/zz_calmoyvt_${mm}_$year.ksh
done

# prepare zcalmoy.ada.YEAR.ll  (3 steps job, step 3 depending on step 1 and step 2)
cat $MPDIR/calmoy.ada.skel.ll | sed -e "s/<year>/$year/g" -e "s/<CONFIG>/$CONFIG/g" -e "s/<CASE>/$CASE/g" > ./zcalmoy.ada.$year.ll

submit ./zcalmoy.ada.$year.ll

# clean temporary job
\rm ./zcalmoy.ada.$year.ll
