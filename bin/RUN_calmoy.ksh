#!/bin/ksh

usage() {
      echo USAGE: $(basename $0) [-h ] [-a agrif_grid]  year
      echo "    -h : this help message"
      echo "    -a agrif_grid : number of the agrif grid to process "
      echo "    year : the year to be processed "
      exit 0
        }

nagrif=''
while getopts :ha: opt ; do
  case $opt in
   (h) usage ;;
   (a) nagrif=${OPTARG} ;;
   (\?) echo $( basename $0 ): option -$OPTARG not valid. ; usage ;;
  esac
done
shift  $((OPTIND - 1 ))

year=$1

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

./RUN_calmoy_$BATCH.ksh $year

###
