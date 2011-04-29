#!/bin/ksh

usage() {
      echo USAGE: $(basename $0) [-h ] [-a agrif_grid] [-i year1-year2 ] year
      echo "    -h : this help message"
      echo "    -a agrif_grid : number of the agrif grid to process "
      echo "    -i year1-year2 : interannual mean "
      echo "    year : the year to be processed. Not required with -i option "
      exit 0
        }

nagrif=''
tmp=''
while getopts :ha:i: opt ; do
  case $opt in
   (h) usage ;;
   (a) nagrif=${OPTARG} ;;
   (i) tmp=${OPTARG} ;;
   (\?) echo $( basename $0 ): option -$OPTARG not valid. ; usage ;;
  esac
done
if [ $tmp ] ; then
  year1=${tmp%-*}
  year2=${tmp#*-}
else
  shift  $((OPTIND - 1 ))
  year=$1
fi

if [ $nagrif ] ; then
  ag_pref=${nagrif}_
  ag_suff=_${nagrif}
else
  ag_pref=''
  ag_suff=''
fi

export ag_pref
export ag_suff

. ./config_moy.ksh

if [ $tmp ] ; then
  ./RUN_calinter_$BATCH.ksh $year1 $year2
else
  ./RUN_calmoy_$BATCH.ksh $year
fi

###
