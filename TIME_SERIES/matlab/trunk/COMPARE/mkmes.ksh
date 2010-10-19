#!/bin/ksh
#  $Rev$
#  $Date$
#  $Id$
#--------------------------------------------------------------


 file=$1
 tmp=$(basename $file); config=${tmp%_*}
 if [ ! -d ../tmp/ ] ; then mkdir ../tmp ; fi
 m=03
 while (( $m <= 9 )) ; do 
 month=$( printf "%02d" $m )


 vn=$(( month +1 ))
 vs=$(( vn + 12 ))
 an=$(( vs + 12 ))
 as=$(( an + 12 ))
 en=$(( as + 12 ))
 es=$(( en + 12 ))
 outfile=../tmp/${config}_m${month}_ice.mtl
  echo "%  Ice diags for " $config month $month >  $outfile

cat $file | awk ' BEGIN {printf "%s\n%s\n" ,\
                                                   "%      N         S         N        S         N       S ",\
                                                   "% y    V         V         A        A         E       E" }\
    { if ( match($0,"%") != 1 ) \
    {if ($1 != '0000' )  printf "%4d % 7.3f % 7.3f   % 7.3f % 7.3f   % 7.3f % 7.3f \n",\
        $1,$vn/1000., $vs/1000., $an/1000.,$as/1000.,$en/1000.,$es/1000. } }'\
        vn=$vn vs=$vs an=$an as=$as en=$en es=$es config=$config month=$month >> $outfile



 echo "% Ice observation from NOAA SSM/R (before 06/87) and SSM/I (after 06/87) " > ../tmp/N_noaa_m${month}_ice.mtl
 echo "% Before 06/87 add 1.19 million sq km to the area" >> ../tmp/N_noaa_m${month}_ice.mtl
 echo "% After 06/87 add 0.31  million sq km to the area ( north pole limit of the sensors) " >> ../tmp/N_noaa_m${month}_ice.mtl
 echo "% yr mo   E   A  " >> ../tmp/N_noaa_m${month}_ice.mtl
 cat ${DATAOBSDIR}/North.mtl | awk '{ if ( $2 == month ) print $0 }' month=$month >> ../tmp/N_noaa_m${month}_ice.mtl

 echo "% Ice observation from NOAA SSM/R (before 06/87) and SSM/I (after 06/87) " > ../tmp/S_noaa_m${month}_ice.mtl
 echo "% yr mo   E   A  " >> ../tmp/S_noaa_m${month}_ice.mtl
 cat ${DATAOBSDIR}/South.mtl | awk '{ if ( $2 == month ) print $0 }' month=$month >> ../tmp/S_noaa_m${month}_ice.mtl

 m=$(( m + 6 ))
 done
