#!/bin/ksh

mkgif() {
      ctrans -device sun -res 1024x1024 $1 > toto.sun
      convert toto.sun GIFS/${1%.cgm}.gif
      rm toto.sun
        }

for d in ATLANTIC  ATLS CIRCUM DRAKE GLOBAL KERGUELEN MXL SECTIONS ATLN CAMPBELL CONTOURS DWBC ICE MEDSEA OVT SECTIONS1 ; do
     cd  $d
       echo work in $d
       for f in *.cgm ; do
         mkgif $f
       done 
     cd ../
done

