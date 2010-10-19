#!/bin/ksh

### This script provides a section.dat (to use with cdftransportiz)
### from the drakkar_sections_table.txt file
### Example : ./create_sections_list.ksh ORCA025
#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------

if [ $# == 0 ] ; then
    echo ' USAGE: create_sections_list.ksh CONFIG '
    exit 0
fi

CONFIG=$1

### Transports along sections :

grep -e" $CONFIG " drakkar_sections_table.txt | cat > temp.txt

linebeg=1 
lineend=`wc -l temp.txt | awk '{print $1}' `

lines=''
n=$linebeg

\rm -f section.dat # remove (if any)

while (( $n <= $lineend )) ; do

  line=` sed -n ${n},${n}p temp.txt `
  section=` echo $line | awk '{print $1}' `
  indices=` echo $line | awk '{print $3 " " $4 " " $5 " " $6}' `
  echo $section | cat >> section.dat
  echo $indices | cat >> section.dat

  n=$(( n + 1 ))
done

echo 'EOF' | cat >> section.dat
\rm -f temp.txt

### Transports in sigma classes :

grep -e " $CONFIG " drakkar_trpsig_table.txt | cat > temp.txt

linebeg=1
lineend=`wc -l temp.txt | awk '{print $1}' `

lines=''
n=$linebeg

\rm -f dens_section.dat # remove (if any)

while (( $n <= $lineend )) ; do

  line=` sed -n ${n},${n}p temp.txt `
  section=` echo $line | awk '{print $1}' `
  indices=` echo $line | awk '{print $3 " " $4 " " $5 " " $6}' `
  echo $section | cat >> dens_section.dat
  echo $indices | cat >> dens_section.dat

  n=$(( n + 1 ))
done

echo 'EOF' | cat >> dens_section.dat
\rm -f temp.txt

