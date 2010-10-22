#!/bin/ksh

year=<year>        # year to use
mm=<mm>            # month

# get variables
. ./config_moy.ksh
# functions to  used here
. ./function_moy.ksh

# main program

chkdir $MOYTMPDIR
chkdir $MOYTMPDIR/$year

monthly $mm
     
# Only for load leveler : add the following years
##LL##  mbeg=$(( $m + 1 ))  
##LL##  mend=$(( $m + $STEP -1 ))  
##LL##  lmonth=$( seq $mbeg $mend )  
##LL##  for zmonth in $lmonth ; do
##LL##     zmonth=$( printf "%02d" $zmonth )
##LL##     monthly $zmonth
##LL##  done  
