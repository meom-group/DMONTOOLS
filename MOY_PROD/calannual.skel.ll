#!/bin/ksh

year=<YYYY>

set -x

. ./config_moy.ksh
. ./function_moy.ksh

cd $TMPDIR

annual

annualvt
