#!/bin/ksh
#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------
prefix=..
dir=$( basename `pwd` )
if [ $dir = 'TXT' ] ; then prefix=../.. ; tmp=$( dirname `pwd` ) ; dir=$( basename $tmp ) ; fi

CONFCASE=${dir%-DIAGS}
CONFIG=${CONFCASE%-*}
CASE=${CONFCASE#*-}
MONITORDIR=${prefix}/${CONFCASE}-MONITOR

if [  ! -d $MONITORDIR ] ; then mkdir $MONITORDIR ; fi

# if no NINO.txt files in the dir, skip
 ls *NINO* 1> /dev/null 2>&1
 if [ $? != 0 ] ; then echo no nino to deal with ... ; exit ; fi

cat *NINO* > $MONITORDIR/${CONFCASE}_nino.mtl
