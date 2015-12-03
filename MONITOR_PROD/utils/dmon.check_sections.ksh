#!/bin/ksh
#   This script can be used to check the position of sections  in
# the drakkar_sections_table.txt and drakkar_trpsig_table.txt.
# It provides all.cgm file, containing plots of the bathymetry nearby the
# beginning and end position of a section. This is very usefull to check
# that the position is correct
#
# History : Original : Albanne Lecointre, March 2011.

#--------------------------------------------------------------------------------
# $Id$
#--------------------------------------------------------------------------------

#set -x

# adapt the following line according to your settings
##################################################################################
config=SOSMOD
sig=no           # yes or no
halo=5
chart=/home/users/molines/DEV/CHART_7.0/chart
##################################################################################

\rm *.cgm
case $config in 
  ( 'ORCA12.L46' ) bathy=bathy_meter.nc ;;
  ( 'ORCA025.L75' ) bathy=ORCA025_bathy_etopo1_gebco1_smoothed_coast_corrected_mar10.nc ;;
  ( 'ORCA05' ) bathy=ORCA05_bathy_meter_v2.nc ;;
  ( 'SOSMOD' ) bathy=bathy_updated_etopo2v1_ok.nc ;;
  ( * ) echo config $config not supported yet ;;
esac

case $sig in
  ('yes' ) filesection=drakkar_trpsig_table.txt ;;
  ('no'  ) filesection=drakkar_sections_table.txt ;;
esac

nbsect=`grep $config $filesection | wc -l`
nosect=0

while [ $nosect -ne $nbsect ] # 4 ] #$nbsect ]
do

nosect=$((nosect+1))

sect=`grep $config $filesection | head -$nosect | tail -1 | awk '{print $1}'`
imin=`grep $config $filesection | head -$nosect | tail -1 | awk '{print $3}'`
imax=`grep $config $filesection | head -$nosect | tail -1 | awk '{print $4}'`
jmin=`grep $config $filesection | head -$nosect | tail -1 | awk '{print $5}'`
jmax=`grep $config $filesection | head -$nosect | tail -1 | awk '{print $6}'`

titre="$sect $config $imin $jmin"

ibeg=$((imin-$halo))
iend=$((imin+$halo))
jbeg=$((jmin-$halo))
jend=$((jmin+$halo))
zoom="$ibeg $iend $jbeg $jend"

cat << eof > ovdta
$((imin+1)) $((jmin+1)) 
$((imax+1)) $((jmax+1))
eof

filemin="${sect}_min.cgm"
echo WARNING $titre

$chart -clrdata $bathy -clrvar Bathymetry -noproj -noint -ijgrid -zoom $zoom \
      -xstep 1 -ystep 1 -xgrid -ygrid -spval 0 \
      -overdata ovdta -overlw 3 -overmark ovdta -overmk 4 -overmksc 2 \
      -title "$titre" -o "$filemin"

titre="$sect $config $imax $jmax"

ibeg=$((imax-$halo))
iend=$((imax+$halo))
jbeg=$((jmax-$halo))
jend=$((jmax+$halo))
zoom="$ibeg $iend $jbeg $jend"

cat << eof > ovdta
$((imin+1)) $((jmin+1)) 
$((imax+1)) $((jmax+1))
eof

filemax="${sect}_max.cgm"
echo WARNING $titre

$chart -clrdata $bathy -clrvar Bathymetry -noproj -noint -ijgrid -zoom $zoom \
      -xstep 1 -ystep 1 -xgrid -ygrid -spval 0 \
      -overdata ovdta -overlw 3 -overmark ovdta -overmk 4 -overmksc 2 \
      -title "$titre" -o "$filemax"

cat $filemin $filemax > ${sect}.cgm
rm $filemin $filemax

done

cat *.cgm > all.cgm
