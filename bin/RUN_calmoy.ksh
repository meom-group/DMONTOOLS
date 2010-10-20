#!/bin/ksh

year=$1

. ./config_moy.ksh

./RUN_calmoy_$BATCH.ksh $year

###
