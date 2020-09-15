#!/bin/bash

year=<YYYY>

set -x

. ./config_moy
. ./function_moy

cd $TMPDIR

annual

annualvt
