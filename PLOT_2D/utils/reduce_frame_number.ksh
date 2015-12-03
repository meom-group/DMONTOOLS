#!/bin/ksh
###############################
CONFIG=ORCA025.L75
CASE=OCCITSPIN
delete_list='#10-'
###############################

CONFCASE=${CONFIG}-${CASE}

cd $CONFCASE

for d in * ; do 
  cd $d 
  for gif in *.gif ; do   
    gifsicle  -b  $gif  --delete "$delete_list"
  done
  echo finish with $d
  cd - 
done

