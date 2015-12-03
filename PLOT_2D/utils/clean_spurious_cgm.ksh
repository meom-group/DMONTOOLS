#!/bin/ksh

#set -vx

usage() {
     echo "USAGE : $(basename $0 ) [-a ] [-c <CONFCASE> ] [-h ] "
     echo "     PURPOSE : "
     echo "         Clean spurious cgm files in the PLOTDIR directory"
     echo "         Spurious cgm file have a size of either 0 or 1440 bytes"
     echo "         They are produced when chart or coupe get into an error."
     echo "         Cleaning the files is necessary before trying to redo the files,"
     echo "         because the existence of the file is checked before doing it."
     echo "         "
     echo "     OPTIONS : "
     echo "         -c  <CONFCASE> : indicate the config-case to check. Assume that plots are"
     echo "               in \$DDIR/<CONFIF>/PLOTS/<CONFCASE>. If \$DDIR not set, use \$WORKDIR "
     echo "         -a  : automatically infer CONFCASE from the current directory"
     echo "         -h  : print this help message"
     exit 0
        }    


auto=0
if [ $# = 0 ] ; then usage ; fi

while getopts :hc:a  opt ; do
  case $opt in
  (h) usage ;;
  (c) CONFCASE=$OPTARG ;;
  (a) auto=1 ;;
  (\?) echo $( basename $0 ): option -$OPTARG not valid. ; usage ;;
  esac
done

DDIR=${DDIR:=$WORKDIR}

if [ $auto = 1 ] ; then
  CONFCASE=$( basename $(pwd) )
  CONFIG=${CONFCASE%-*}
  CASE=${CONFCASE#*-}
  PLOTDIR=./
  HERE=./
else
  CONFIG=${CONFCASE%-*}
  CASE=${CONFCASE#*-}
  PLOTDIR=$WORKDIR/$CONFIG/PLOTS/$CONFIG-$CASE
  HERE=$( pwd )
fi

echo "Files removed " > $HERE/log.clean
echo "------------- " >> $HERE/log.clean

for subdir in $( ls $PLOTDIR ) ; do
    if [ -d $PLOTDIR/$subdir ] ; then
       cd $PLOTDIR/$subdir
       pwd

       for file in *.cgm   ; do
          if [ -f $file ] ; then
           sz=$( ls -l $file | awk '{ print $5}' )
           if [ $sz = 1440 ] ; then
              rm $file 
              echo $file has been removed  
              echo $file has been removed  >> $HERE/log.clean
           fi
          fi
       done
       cd ../
    fi

done
