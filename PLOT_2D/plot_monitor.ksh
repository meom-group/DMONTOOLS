#!/bin/ksh
set -x
#-------------------------------------------------------------------------------
#  $Date$
#  $Rev$
#  $Id$
#  $History: (02/2009) adapted to PERIANT configuration by C. Dufour $
#-------------------------------------------------------------------------------
if [ $# = 0 ] ; then
   echo 'USAGE : plot_monitor.ksh year'
   exit 1
fi

# take YEAR as the argument to this script
YEAR=$1

#-----------------------------------------------------------------------------
# define some config dependent variable
. ./config_def.ksh

#  Define some functions to get/put file from/to gaya (can be easily customized)
. ./function_def.ksh

# PALDIR hold color palettes for these plots
# it is distributed with PLOT_2D scripts, and copied ther by the main script
PALDIR=$(pwd)/PALDIR
export BIMG_PALDIR=$PALDIR   # chart will look in this dir for palettes

chkdir $YEAR

cd $YEAR

#------------------------------------------------------------------------------
# PATH:
#------------------------------------------------------------------------------
export PATH=$CHARTTOOLS/:$CDFTOOLS/:$PATH
# check if required cdftools are available, exit if missing
err=0
for cdfprog in cdfmoy cdfmltmask cdfsig0 cdfsigi ; do 
 if [ ! -x $CDFTOOLS/$cdfprog ] ; then
  err=$(( err + 1 ))
  echo $cdfprog executable missing. Check your $CDFTOOLS installation
 fi
done


#------------------------------------------------------------------------------
# GENERIC SCRIPT FUNCTIONS:
#------------------------------------------------------------------------------
# mklim min max step : on std output value to use as limit, between min max with step
mklim() {
    low=$1 ; high=$2 ;  ntick=$3
    n=1
    nn=$( echo $low $high $ntick | awk '{ print  ($2 -$1)/$3 + 2}' )
    while [ $n -le $nn ] ; do
      x=$( echo 0 | awk '{ print  low + (n - 1 ) * ntick  }' low=$low n=$n high=$high ntick=$ntick )
      echo $x
      n=$(( n + 1 ))
    done ; mkformat $low $high $ntick ; }

# set gif to 'gif' if gif required else set it to ''
# if $gif='' then for f in $gif is an empty loop !
if [ $create_gif == 1 ] ; then   gif='gif' ; else gif=''; fi

    # puttogaya : used to dispose the plots in $listplt on PLOTDIR/specific
    puttogaya() {
       chkdirg  $PLOTDIR ; chkdirg  $PLOTDIR/$1 ; chkdirg  $PLOTDIR/$1/GIFS
       for f in  $listplt  ; do
            for ftyp in cgm  ; do expatrie $f.$ftyp $PLOTDIR/$1  $f.$ftyp  ; done
            for ftyp in $gif  ; do expatrie $f.$ftyp $PLOTDIR/$1/GIFS  $f.$ftyp  ; done
       done ;
                }
    # puttogayatrc : used to dispose the plots in $listplt on PLOTDIR/TRACER/specific
    puttogayatrc() {
       chkdirg  $PLOTDIR ; chkdirg  $PLOTDIR/$1 ; chkdirg  $PLOTDIR/$1/$2 ; chkdirg  $PLOTDIR/$1/$2/GIFS
       for f in  $listplt  ; do
            for ftyp in cgm  ; do expatrie $f.$ftyp $PLOTDIR/$1/$2  $f.$ftyp  ; done
            for ftyp in $gif  ; do expatrie $f.$ftyp $PLOTDIR/$1/$2/GIFS  $f.$ftyp  ; done
       done ;
                }

    # mkplt : Usage mkplt  filout
    #         convert gmeta ( created by chart/coupe) into filout.cgm and hold the list
    mkplt() { mv gmeta $1.cgm  ; listplt="$listplt $1 " ; 
              for ftyp in $gif ; do
               ctrans -d sun -res 1024x1024 $1.cgm > $1.sun ;
               convert $1.sun $1.$gif ; \rm -f $1.sun 
              done ; }

# mergeframe: mergeframe cgm1 cgm2 cgmout
mergeframe () { med -e "r $1" -e "r $2" -e "1,2merge" -e "1w $3" ;}

# mkformat : return the more appropriate format for the palette, regarding low high and ntick
mkformat () { q=0 ; ntick=$3 ; low=$1 ; high=$2
 for n in 1  10 100 1000 10000 100000 1000000; do
  reste=$( echo $ntick | awk '{ p=($1 * n - int($1)*n) ; r= p-int(p) ; print  int(r * 1000 )   }' n=$n q=$q )
  if (( $reste == 0 )) ; then break ; fi
  q=$(( q +1 ))
 done

  dmin=$( echo $low | awk '{ printf "%s", int($1)}' | wc -c )
  dmax=$( echo $high | awk '{ printf "%s", int($1)}' | wc -c )

   if (( $dmin < $dmax )) ; then d=$dmax ; else d=$dmin ; fi
   if (( $q == 0 )) ; then
    tl=$(( d ))
   else
    tl=$(( $d + 1 + $q ))
   fi

   if (( $q == 0 )) ; then
      FORMAT='-format PALETTE 'I$tl
   else
      FORMAT='-format PALETTE 'F$tl.$q
   fi ;}

# function to compare config name: return 0 if ref is not within CONFIG
chconf() { echo $CONFIG | awk '{print index($1,ref)}' ref=$1 ; }

#-------------------------------------------------------------------------------
# LIST OF PALETTES (available in PALDIR
#-------------------------------------------------------------------------------
 PAL1=pal_Testu
 PAL2=pal_Testu2
 PALDIFF=pal_diff
 PALBLUE2RED=blue2red2.pal
 PALBLUE2RED3=blue2red3.pal
 PALBLUE2RED4=ferretblue2red.pal
 PALNOAA=noaa_blue2white.pal

#------------------------------------------------------------------------------
# DIRECTORY NAMES FREQUENTLY USED
#------------------------------------------------------------------------------
 MEANY=$CONFIG/${CONFCASE}-MEAN/$YEAR
 SDIRY=$CONFIG/${CONFCASE}-S/$YEAR
 DIAGS=${CONFIG}/${CONFCASE}-DIAGS
 IDIR=$CONFIG/${CONFIG}-I
 PLOTDIR=${CONFIG}/PLOTS/${CONFCASE}
 CLIMY=${IDIR}/CLIM_PLOT

 # check for existence
 chkdirg ${CONFIG}/PLOTS/
 chkdirg ${CONFIG}/PLOTS/${CONFCASE} # PLOTDIR

 DATE=${YEAR}  

#------------------------------------------------------------------------------
# NOW PERFORMS PLOTS REGARDING THE SETTINGS IN THE MENU
#------------------------------------------------------------------------------
# 1. MOC = OVT on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^
                       if [ $moc  == 1 ] ; then
  # generic function for moc plot
  mocplt() { $COUPE -clrdata $mocf  -clrmodif 'deptht=depthw' \
             -clrmet 1 -clrmin $clrmin -clrmax $clrmax  -p $PALBLUE2RED4\
             -clrvar $clrvar  -format PALETTE I3 -noteam -pmax -5500 \
             -cntdata $mocf  -cntmodif 'deptht=depthw' -cntmin -32 -cntint 2 \
             -cntvar $clrvar  -zstep 250 -zgrid -ystep 5 -ygrid \
             $PTS $STRING ;}

  # get files
  mocf=${CONFCASE}_y${DATE}_MOC.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # set common min and max (sv) for plots
  clrmin=-18  ;  clrmax=18
  
  # appropriate list of basins to work with
  listbas="atl glo inp aus"  #default for ORCA configs
  if [ $(chconf PERIANT ) != 0 ] ; then listbas=AUSTRAL ; fi
  if [ $(chconf NATL ) != 0 ] ; then listbas=glo ; fi

  # Loop for all basins
  for bas in $listbas ; do
    clrvar=zomsf${bas}
    case $bas in
     atl) PTS='-pts 0 0 -30 70' ;
          STRING="-string 0.5 0.98 1.3 0 MOC_ATLANTIC_(sv)_${CONFCASE}_y$YEAR " ;;
     glo) PTS='-pts 0 0 -70 70' ;
          STRING="-string 0.5 0.98 1.3 0 MOC_GLOBAL_(sv)_${CONFCASE}_y$YEAR " ;;
     inp) PTS='-pts 0 0 -30 70' ;
          STRING="-string 0.5 0.98 1.3 0 MOC_INDO-PACIF_(sv)_${CONFCASE}_y$YEAR " ;;
     aus) clrvar=zomsfglo ; PTS='-pts 0 0 -75 0' ;
          STRING="-string 0.5 0.98 1.3 0 MOC_AUSTRAL_(sv)_${CONFCASE}_y$YEAR " ;;
     AUSTRAL) clrvar=zomsfglo ; PTS='-pts 0 0 -70 -30' ;
               STRING="-string 0.5 0.98 1.3 0 MOC_AUSTRAL_(sv)_${CONFCASE}_y$YEAR " ;;
    esac

    filout=${CONFIG}_OVT_${bas}_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/OVT/$filout.cgm) == absent ] ; then
     rapatrie $mocf $MEANY $mocf
     mocplt ; mkplt $filout
    fi
  done

  puttogaya  OVT 
                       fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. GLOBAL (on the web site)
#^^^^^^^^^^^^^^^^^^^^^^^^
 # generic function for global plots
 gloplt() { set -x ; $CHART $CLRDATA -clrnoint -ijgrid -noproj   \
            -clrvar $clrvar -english $CLRXYPAL $XYPLOT\
            $LEV $DEP $PAL $STRING $CLRLIM $CNTICE  $MEAN $FORMAT; }
#--------
                       if [ $global == 1 ] ; then

 # particular plotting distorsion for non ORCA plots
 XYPLOT=''   # Default ORCA model
 CLRXYPAL='' # Defaut ORCA model
 xstring=0.5 ; ystring=0.95

 if [ $(chconf PERIANT) != 0 ] ; then
  XYPLOT="-xyplot 0.1 0.95 0.4 0.8"
  CLRXYPAL="-clrxypal 0.1 0.95 0.2 0.3"
  xstring=0.52 ; ystring=0.9
 fi

 # get files  gridT, icemod
 t=${CONFCASE}_y${YEAR}_gridT.nc
 ice=${CONFCASE}_y${YEAR}_icemod.nc

 # reset the list of plots that are produced ( list is updated in mkplt )
 listplt=' '

 # Common values for this group of plots
 CLRDATA="-clrdata $t"
 PAL="-p $PAL1"
 CLRLIM="-clrmark zclrmark"
 CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;

 # SSH GLobal  mean value set to 0
 rapatrie $t $MEANY $t
 clrvar=sossheig
 if [ $( chkvar $clrvar $t) == 0  ] ; then
 var=SSHGLp ;  STRING=" -string $xstring $ystring 1.0 0 ${CONFCASE}_${var}_${DATE}_DEPTH=@CLR_DEPTH@" ;
 MEAN="-mean 0" ; DEP="" ; LEV="-lev 1" ; dep=0  ;
 min=-2 ; max=1.5  ; pas=.5  # ORCA Default
 if [ $(chconf PERIANT) != 0 ] ; then min=-1.2 ; max=1.2  ; pas=.4  ; fi
 mklim $min $max $pas > zclrmark
 filout=${CONFIG}_${var}_${dep}_${DATE}-${CASE}

 if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent  ] ; then
     rapatrie $t $MEANY $t
     rapatrie $ice $MEANY $ice
     gloplt  ; mkplt $filout
 fi
 fi

 # T and S GLOBAL at various depth
 for var in Tgl Sgl ; do
   MEAN=""
   for dep in 0 150 ; do
     DEP="-dep $dep" ; LEV="" ; CNTICE=""
     filout=${CONFIG}_${var}_${dep}_${DATE}-${CASE}
     if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent  ] ; then
        rapatrie $t $MEANY $t
        rapatrie $ice $MEANY $ice
        case $var in
          Tgl) clrvar=votemper
            case $dep in
                0)  min=-2 ; max=26  ; pas=4 
                     if [ $(chconf PERIANT) != 0 ] ; then min=-2 ; max=22  ; pas=2  ; fi
                     LEV="-lev 1" ; DEP="" ;
                    CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;;
              150)  min=-2 ; max=28  ; pas=4
                    if [ $(chconf PERIANT) != 0 ] ; then min=-2 ; max=20  ; pas=2  ; fi ;;
            esac ;;
          Sgl) clrvar=vosaline
            case $dep in
                0)  min=30 ; max=38  ; pas=2 
                     if [ $(chconf PERIANT) != 0 ] ; then min=32 ; max=35  ; pas=1  ; fi
                     LEV="-lev 1" ; DEP="" ;
                    CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;;
              150)  min=30 ; max=38  ; pas=2 
                     if [ $(chconf PERIANT) != 0 ] ; then min=33 ; max=35  ; pas=1  ; fi ;;
            esac ;;
        esac
        mklim $min $max $pas > zclrmark
       STRING=" -string $xstring $ystring 1.0 0 ${CONFCASE}_${var}_${DATE}_DEPTH=@CLR_DEPTH@"
       gloplt  ; mkplt $filout
     fi
   done
 done

 puttogaya  GLOBAL
                       fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Difference between model and Levitus at different levels ( In GLOBAL)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                       if [ $glodifmap == 1 ] ; then
  # mkdiff : Usage mkdiff file1 file2 cdfvar level outfile
  mkdiff() { \rm -f $5
            ncks  -F -O -v $3,nav_lon,nav_lat -d deptht,$4,$4 $2  zz2.nc
            ncks  -F -O -v $3,nav_lon,nav_lat -d deptht,$4,$4 $1  zz1.nc
            ncatted -O -a missing_value,,d,, zz2.nc
            ncatted -O -a missing_value,,d,, zz1.nc
            ncbo --op_typ=- -v $3 zz1.nc zz2.nc $5
            ncks  -F -A -v nav_lon,nav_lat,deptht,time_counter -d deptht,$4,$4 $1 $5 ;}


  # mkdiffrey : Usage mkdiffrey file1 file2 cdfvar outfile
  mkdiffrey() { \rm -f $4
            ncks  -F -O -v $3,nav_lon,nav_lat  $2  zz2.nc
            ncks  -F -O -v $3,nav_lon,nav_lat -d deptht,1,1 $1  zz1.nc
            ncatted -O -a missing_value,,d,, zz2.nc
            ncatted -O -a missing_value,,d,, zz1.nc
            ncbo --op_typ=- -v $3 zz1.nc zz2.nc $4 
            ncks  -F -A -v nav_lon,nav_lat,deptht,time_counter -d deptht,1,1 $1 $4 ;}

  # ndep : return the level corresponding to dep (m)
  ndep() { ncks -H -F -C -v deptht $t | \
           sed -e 's/(/ /' -e 's/)/ /' -e 's/=/ /' | \
           awk '{if ( $3 > dep ) {print $2 ; nextfile }   }' dep=$1  ; }
#-----

 # particular plotting distorsion for non ORCA plots
 XYPLOT=''   # Default ORCA model
 CLRXYPAL='' # Defaut ORCA model

 if [ $(chconf PERIANT) != 0 ] ; then
  XYPLOT="-xyplot 0.1 0.95 0.4 0.8"
  CLRXYPAL="-clrxypal 0.1 0.95 0.2 0.3"
 fi

  # get files  gridT, icemod  T-Levitus, S-Levitus , SST reynolds
  t=${CONFCASE}_y${YEAR}_gridT.nc
  tlev=${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$(echo $CONFIG | tr '[A-Z]' '[a-z]' ).nc
  slev=${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$(echo $CONFIG | tr '[A-Z]' '[a-z]' ).nc
  sstr=sst_REYNOLDS-${CONFIG}_${YEAR}.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # plot the differences for the following levels
#  MEAN="" ; DEP="" ; LEV="" ; PAL="-p $PALBLUE2RED" ; CNTICE=""
   MEAN="" ; DEP="" ; LEV="" ; PAL="-p $PALBLUE2RED4" ; CNTICE=""

  # Anomaly with SST Reynolds, same year
    if (( $YEAR > 1981 ))  ; then
     clrvar=votemper ; FORMAT="-format PALETTE F5.2"
     CLRDATA=" -clrdata dt.nc"; min=-3.5 ; max=3.5  ; pas=2 ;
#    CLRLIM="-clrmin $min -clrmax $max -clrmet 1"
     CLRLIM="-clrlim lili.lim"
cat << eof > lili.lim
-5
-2
-1.75
-1.5
-1.25
-1
-0.75
-0.5
-0.25
0
0.25
0.5
0.75
1
1.25
1.5
1.75
2
3
eof
    # Difference with Reynolds SST (both for model and TSCLIM)
    \rm -f dt.nc ds.nc
    for var in difTRey difLevRey ; do
     filout=${CONFIG}_${var}_SST2_${YEAR}-${CASE}
     if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
       rapatrie $t    $MEANY $t
       rapatrie $tlev $IDIR $tlev
       rapatrie $sstr $IDIR/REYNOLDS/ANNUAL $sstr
       case $var in
         difTRey )  mkdiffrey $t $sstr votemper dt.nc ;;
         difLevRey) mkdiffrey $tlev $sstr votemper dt.nc ;;
       esac
       STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${DATE}_SST_DEPTH=@CLR_DEPTH@"
       gloplt  ; mkplt $filout
     fi
    done
    fi

    # Differences between model and TSCLIM at various depths
    #  level are specified according to vertical discretization
    #  need adaptation for L75 ..; (use of ndep ? ) but need level for mkdiff
   MEAN="" ; DEP="" ; LEV="" ; PAL="-p $PALBLUE2RED" ; CNTICE=""
#  for level in 1 12 19  ; do
  rapatrie $t    $MEANY $t 
  for dep in 0 134 452  ; do  # about level 1 12 19 of L46
    level=$( ndep $dep )
    for var in difTgl difSgl ; do
     filout=${CONFIG}_${var}_k${level}_${YEAR}-${CASE}
     if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
        STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${DATE}_DEPTH=@CLR_DEPTH@"
        case $var in
         difTgl) rapatrie $t    $MEANY $t
                 rapatrie $tlev $IDIR $tlev
                 mkdiff $t $tlev votemper $level dt.nc
                 clrvar=votemper ; FORMAT="-format PALETTE F5.2"
                 CLRDATA=" -clrdata dt.nc"; min=-3.5 ; max=3.5  ; pas=2 ;;
         difSgl) rapatrie $t    $MEANY $t
                 rapatrie $slev $IDIR $slev
                 mkdiff $t $slev vosaline $level ds.nc
                 clrvar=vosaline ; FORMAT="-format PALETTE F5.2"
                 CLRDATA=" -clrdata ds.nc"; min=-1.05 ; max=1.05  ; pas=2 ;;
        esac
        CLRLIM="-clrmin $min -clrmax $max -clrmet 1"
        gloplt  ; mkplt $filout
     fi
    done

    # clean dt.nc ds.nc
    \rm -f dt.nc ds.nc
  done

    # dispose to gaya all plots in the listplt
  puttogaya  GLOBAL 
                       fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. Surface heat fluxes( W/m2) and freshwater flux (mm/days)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                       if [ $fluxes == 1 ] ; then

 # particular plotting distorsion for non ORCA plots
 XYPLOT=''   # Default ORCA model
 CLRXYPAL='' # Defaut ORCA model
 xstring=0.5 ; ystring=0.95

 if [ $(chconf PERIANT) != 0 ] ; then
  XYPLOT="-xyplot 0.1 0.95 0.4 0.8"
  CLRXYPAL="-clrxypal 0.1 0.95 0.2 0.3"
  xstring=0.52 ; ystring=0.9
 fi

  # get files  gridT
  t=${CONFCASE}_y${YEAR}_gridT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # plot the heat flux (w/m2) and fresh water flux (mm/day) (scaled by 86400 from kg/m2/s)
  MEAN="" ; DEP="" ; LEV="" ; PAL="-p $PALBLUE2RED3" ; CNTICE="" ; FORMAT='-format PALETTE I4'
    for var in HeatFlx WaterFlx WaterDmp ; do
      STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${DATE}_DEPTH=@CLR_DEPTH@"
      filout=${CONFIG}_${var}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
         rapatrie $t    $MEANY $t
         case $var in
           HeatFlx) clrvar=sohefldo ; CLRDATA=" -clrdata $t"; min=-140 ; max=140  ; pas=15 ;;
           WaterFlx) clrvar=sowaflup ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
           WaterDmp) clrvar=sowafldp ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
         esac
         CLRLIM="-clrmin $min -clrmax $max -clrmet 1"
         gloplt  ; mkplt $filout
      fi
    done

    # dispose to gaya all plots in the listplt
    puttogaya  GLOBAL 
                         fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. Local details of the TS fields
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # Generic function for Basin plot
  basplt() { $CHART $CLRDATA -forcexy $zoom $OPTIONS  \
             -clrvar $clrvar   -english \
              $LEV $DEP $PAL $STRING $CLRLIM $CNTICE  $MEAN $FORMAT; }
#---

# 5.1 Some details focused on the Atlantic
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 5.1.1 ; Temperature and Salinity
#---------------------------------
			 if [ $atlTS == 1 ] ; then 

  # get files  gridT, icemod 
  t=${CONFCASE}_y${YEAR}_gridT.nc
  ice=${CONFCASE}_y${YEAR}_icemod.nc

 
  for BASIN in  ATLN ATLS ; do
      case $BASIN in
         ATLN) zoom='-zoom -100 10 -5 70 -proj ME -xstep 10 -ystep 5' ; bas=ATLN ;;
         ATLS) zoom='-zoom -70 30 -70 5 -proj ME -xstep 10 -ystep 10' ; bas=ATLS ;;
      esac

    listplt=' '
  # Common values for this group of plots
  OPTIONS=""
  CLRDATA="-clrdata $t"
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;

  # SSH  mean value set to 0
  clrvar=sossheig
 if [ $( chkvar $clrvar $t) == 0  ] ; then
  var=SSHp ;  STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${DATE}_DEPTH=@CLR_DEPTH@" ;
  MEAN="-mean 0" ; DEP="" ; LEV="-lev 1" ; dep=0  ;
  min=-1.5 ; max=.75  ; pas=.25 ; mklim $min $max $pas > zclrmark  
  filout=${CONFIG}_${var}_${bas}_${dep}_${DATE}-${CASE}
  if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then 
    rapatrie $t $MEANY $t
    rapatrie $ice $MEANY $ice
    basplt  ; mkplt $filout
  fi
 fi

 # T and S at various depth
 for var in T S ; do 
  MEAN=""
  for dep in 0 200 1000 2000 3000 4000 5000 ; do
     DEP="-dep $dep" ; LEV="" ; CNTICE="" 
     filout=${CONFIG}_${var}_${bas}_${dep}_${DATE}-${CASE}
     if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
        rapatrie $t $MEANY $t
        rapatrie $ice $MEANY $ice
        case $var in
        T) clrvar=votemper
          case $dep in
              0)  min=-2. ; max=26  ; pas=4 ; LEV="-lev 1" ; DEP=""
                  CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;;
            200)  min=-2. ; max=22  ; pas=4  ;;
           1000) min=0   ; max=12  ; pas=2  ;;
           2000) min=0   ; max=5   ; pas=.5 ;;
           3000) min=0   ; max=4   ; pas=.5 ;;
           4000) min=-1 ; max=5  ; pas=.5 ;;
           5000) min=-1 ; max=5  ; pas=.5 ;;
         esac ;;
        S) clrvar=vosaline
         case $dep in
             0)  min=30 ; max=36  ; pas=2 ; LEV="-lev 1" ; DEP="" ;
                 CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;;
           200)  min=34   ; max=37.5 ; pas=.5 ;;
           1000) min=34   ; max=36   ; pas=.5 ;;
           2000) min=34.6 ; max=35.4 ; pas=.1 ;;
           3000) min=34.6 ; max=35   ; pas=.05 ;;
           4000) min=34.6 ; max=35  ; pas=.05 ;;
           5000) min=34.6 ; max=35  ; pas=.05 ;;
         esac ;;
        esac
        mklim $min $max $pas > zclrmark
        STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${DATE}_DEPTH=@CLR_DEPTH@"
        basplt  ; mkplt $filout
     fi
  done
 done

 puttogaya  $BASIN 
 done

             	 	fi	
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.1.2 : Barotropic Stream Function
#------------------------------------
                        if [ $atlUV == 1 ] ; then

  # get files  for PSI
  psi=${CONFCASE}_y${DATE}_PSI.nc

  for BASIN in  ATLN ATLS ; do
    case $BASIN in
       ATLN) zoom='-zoom -100 10 -5 70 -proj ME -xstep 10 -ystep 5 -cntint 10e6' ; bas=ATLN ;;
       ATLS) zoom='-zoom -70 30 -70 5 -proj ME -xstep 10 -ystep 10 -cntint 20e6' ; bas=ATLS ;;
    esac

    listplt=' '

    var=PSI ;STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${DATE}"
    filout=${CONFIG}_${var}_${bas}_${DATE}-${CASE}
    if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent  ] ; then
       rapatrie $psi $MEANY $psi
       $CHART -forcexy  -english $zoom $STRING -cntdata $psi -cntvar sobarstf -cntmin -300e6 \
             -cntmax 220e6 -cntexp 6 -clrvar sobarstf -cntshade -cntlis 5 -cntlw 1:2
       mkplt $filout
    fi

    puttogaya  $BASIN 
  done
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.1.3 : Bottom sigma-4
#------------------------------------
                        if [ $botsig4atl == 1 ] ; then
    t=${CONFCASE}_y${YEAR}_gridT.nc
    listplt=' '

    var=botsigma4 ;STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${DATE}"
    filout=${CONFIG}_${var}_${DATE}-${CASE}
    ZOOM="-zoom -65 20 45 75"
    if [ $( chkfile $PLOTDIR/ATLN/$filout.cgm ) == absent  ] ; then
       rapatrie $t $MEANY $t
       cdfbottomsig $t 4000
       cat << eof > botsigma4.lim
0
44.8
45
45.2
45.4
45.6
45.8
46
50
eof
       $CHART -clrdata botsig.nc -proj ME  -p botsigma4.pal -clrlim botsigma4.lim $ZOOM \
                   -cntdata botsig.nc -cntmin 45.8 -cntmax 45.81 -cntint 2  -cntllp 0 $STRING  \
                   -clrvar sobotsigi -cntvar sobotsigi -xstep 5 -ystep 5 -xgrid -ygrid

       mkplt $filout
    fi

    puttogaya  ATLN
                        fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2 : Some details focused on Drake, Kerguelen and Campbell Plateau regions
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 5.2.1 : Temperature and Salinity
#---------------------------------
                        if [ $zoomTS == 1 ] ; then

  # get files  gridT, icemod
  t=${CONFCASE}_y${YEAR}_gridT.nc
  ice=${CONFCASE}_y${YEAR}_icemod.nc

  for BASIN in DRAKE KERGUELEN CAMPBELL ; do
    case $BASIN in
          DRAKE) zoom='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) zoom='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) zoom='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac

  listplt=' '
  # Common values for this group of plots
  OPTIONS='-proj ME -xstep 10 -ystep 5'
  CLRDATA="-clrdata $t"
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;

  # SSH  mean value set to 0
  clrvar=sossheig
 if [ $( chkvar $clrvar $t) == 0  ] ; then
  var=SSHp ;  STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${DATE}_DEPTH=@CLR_DEPTH@" ;
  MEAN="-mean 0" ; DEP="" ; LEV="-lev 1" ; dep=0  ;
  min=-1.2 ; max=1.2  ; pas=.2 ; mklim $min $max $pas > zclrmark
  filout=${CONFIG}_${var}_${bas}_${dep}_${DATE}-${CASE}
  if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
    rapatrie $t $MEANY $t
    rapatrie $ice $MEANY $ice
    basplt  ; mkplt $filout
  fi
 fi

# T and S at various depth
 for var in T S ; do
   MEAN=""
   for dep in 0 200 1000 2000 3000 4000 5000 ; do
     DEP="-dep $dep" ; LEV="" ; CNTICE=""
     filout=${CONFIG}_${var}_${bas}_${dep}_${DATE}-${CASE}
     if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
        rapatrie $t $MEANY $t
        rapatrie $ice $MEANY $ice
        case $var in
          T) clrvar=votemper
             case $dep in
                  0)  min=-2. ; max=20  ; pas=2 ; LEV="-lev 1" ; DEP=""
                      CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;;
                200)  min=-2. ; max=16  ; pas=2  ;;
               1000)  min=0   ; max=5  ; pas=1  ;;
               2000)  min=-0.5   ; max=3   ; pas=0.5 ;;
               3000)  min=-0.5   ; max=2.5   ; pas=0.5 ;;
               4000)  min=-1 ; max=1.5  ; pas=.5 ;;
               5000)  min=-1 ; max=1.5  ; pas=.5 ;;
             esac ;;
          S) clrvar=vosaline
             case $dep in
                  0) min=32 ; max=35  ; pas=1 ; LEV="-lev 1" ; DEP="" ;
                     CNTICE=" -cntdata $ice -cntvar ileadfra -cntmin 0 -cntmax 1 -cntint .1" ;;
                200) min=34   ; max=35.6 ; pas=.2 ;;
               1000) min=34.2 ; max=34.7 ; pas=.1 ;;
               2000) min=34.6 ; max=34.8 ; pas=0.05 ;;
               3000) min=34.6 ; max=34.8   ; pas=.05 ;;
               4000) min=34.6 ; max=34.8  ; pas=.05 ;;
               5000) min=34.6 ; max=34.75  ; pas=.05 ;;
             esac ;;
        esac
        mklim $min $max $pas > zclrmark
        STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${DATE}_DEPTH=@CLR_DEPTH@"
        basplt  ; mkplt $filout
     fi
   done
 done

  puttogaya  $BASIN 
 done

                fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2.2 : Barotropic Stream Function
#------------------------------------
                        if [ $zoomUV == 1 ] ; then

  # get files  for PSI
  psi=${CONFCASE}_y${DATE}_PSI.nc


  for BASIN in DRAKE KERGUELEN CAMPBELL ; do
    case $BASIN in
          DRAKE) zoom='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) zoom='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) zoom='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac

    listplt=' '

    var=PSI ;STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${DATE}"
    filout=${CONFIG}_${var}_${bas}_${DATE}-${CASE}
    OPTIONS='-proj ME -xstep 10 -ystep 5'
    CNTOPTION="-cntilxy 0.5 0.12 -cntils 0.015 -cntilp 0 -cntmin -300.e+06 -cntmax 300.e+06 \
             -cntint 20.e+06 -cntdash -cntshade -cntlls 0.015 -cntexp 6 -cntrc2 0.4 -cntlis 3 -cntlw 1:2"
    if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent  ] ; then
       rapatrie $psi $MEANY $psi
       $CHART -forcexy $zoom $OPTIONS -clrvar sobarstf -cntdata $psi -cntvar sobarstf $CNTOPTION \
              $STRING -english -cslab 1.1
       mkplt $filout
    fi

    puttogaya  $BASIN 
  done
  rm *.cgm *.jpg
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6. Sections in the ocean
#^^^^^^^^^^^^^^^^^^^^^^^^^
# Generic function for coupe plot
  coupplt() {  echo COUPPLT for section $var ;
             $COUPE $CLRDATA $PTS -forcexy -clrnoint \
             -string 0.5 0.95 1.0 0 ${CONFIG}_${var}_${DATE}-${CASE}  \
             -clrvar $clrvar -pmax $pmax -pmin $pmin -zstep $zstep  -english \
             $PAL  $CLRLIM $CLRMODIF $FORMAT; }
#---
# 6.1 General Section for ORCA configs
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $coupes == 1 ] ; then
  listplt=' '
  # get files  gridT, gridU, gridV
  t=${CONFCASE}_y${YEAR}_gridT.nc
  u=${CONFCASE}_y${YEAR}_gridU.nc
  v=${CONFCASE}_y${YEAR}_gridV.nc

  CLRLIM='-clrmark zclrmark'
  PAL="-p $PAL1"
  CLRMODIF=''


  CLRDATA="-clrdata $t"
  for sec in ZAP ATLN AR7W 59N 14W 30W 45W 65W 160W 90E ; do
    for fld in t s ; do
      skip=0
      var=${fld}${sec}
        case $fld in
           t) clrvar=votemper ;;
           s) clrvar=vosaline ;;
        esac

        case $sec in
          ZAP)  PTS='-pts -65  -25 -45 -45 '
                pmin=0 ; pmax=-5800 ; zstep=1000
                case $fld in
                 t) min=0 ; max=12; pas=1 ;;
                 s) min=34.1 ; max=35.1 ; pas=.1 ;;
                esac ;;
          59N)  PTS='-pts  -65    0  59  59 '
                pmin=0 ; pmax=-5800 ; zstep=1000
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=0 ; max=9; pas=1 ;;
                 s) min=34.2 ; max=35.2 ; pas=0.2 ;;
                esac ;;
          14W)  PTS='-pts  -14  -14 -60 60 '
                pmin=0 ; pmax=-5800 ; zstep=1000
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=-2 ; max=26; pas=4 ;;
                 s) min=34 ; max=36.5 ; pas=0.5 ;;
                esac ;;
          45W)  PTS='-pts   -45  -45 -70  62 '
                pmin=0 ; pmax=-5800 ; zstep=1000
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=-2 ; max=30; pas=4 ;;
                 s) min=32 ; max=36 ; pas=0.5 ;;
                esac ;;
          ATLN)  PTS='-pts  -100  0   38  38 '
                pmin=0 ; pmax=-5000 ; zstep=250
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=-2 ; max=30; pas=4 ;;
                 s) min=35 ; max=36.4 ; pas=0.2 ;;
                esac ;;
          AR7W)  PTS='-pts  -56 -48 53.5 60.5'
                pmin=0 ; pmax=-4000 ; zstep=250
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=1 ; max=6; pas=1 ;;
                 s) min=34.7 ; max=35.2 ; pas=0.1 ;;
                esac ;;
          65W)  PTS='-pts  -65 -65 10  45  '
                pmin=0 ; pmax=-5500 ; zstep=250
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=0 ; max=28; pas=4 ;;
                 s) min=34.7 ; max=36.3 ; pas=0.2 ;;
                esac ;;
          30W)  PTS='-pts -30 -30 -75 65'
                pmin=0 ; pmax=-6000 ; zstep=500
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=-2 ; max=30; pas=2 ;;
                 s) min=34 ; max=36.75 ; pas=0.25 ;;
                esac ;;
          90E)  PTS='-pts  90 90 -70 30 '
                pmin=0 ; pmax=-6000 ; zstep=500
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=-2 ; max=30; pas=2 ;;
                 s) min=34 ; max=36.75 ; pas=0.25 ;;
                esac ;;
          160W)  PTS='-pts  -160 -160 -75 65 '
                pmin=0 ; pmax=-6000 ; zstep=500
                skip=$(chconf PERIANT)
                case $fld in
                 t) min=-2 ; max=30; pas=2 ;;
                 s) min=34 ; max=36.75 ; pas=0.25 ;;
                esac ;;
            * ) skip=1 ;;
        esac
       if [ $skip != 1 ] ; then
         filout=${CONFIG}_${var}_${DATE}-${CASE}
         if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $t $MEANY $t
            rapatrie $u $MEANY $u
            rapatrie $v $MEANY $v
            mklim $min $max $pas > zclrmark
            coupplt ; mkplt $filout
         fi
       fi
    done
  done

  # velocities
  for sec in equat ZAP 70W ; do
    for fld in U v ; do
      skip=0
      var=${fld}${sec}
        case $fld in
           U) CLRDATA="-clrdata $u" ;clrvar=vozocrtx ; CLRMODIF='-clrmodif deptht=depthu';;
           v) CLRDATA="-clrdata $v" ;clrvar=vomecrty ; CLRMODIF='-clrmodif deptht=depthv';;
        esac
        case $sec in
          ZAP)  PTS='-pts -65  -25 -45 -45 '
                pmin=0 ; pmax=-5800 ; zstep=1000
                case $fld in
                 U)  skip=1 ;;
                 v) min=-.5 ; max=.25 ; pas=.25 ;;
                esac ;;
          equat)  PTS='-pts -287  73  0   0  '
                pmin=0 ; pmax=-500 ; zstep=50
                skip=$(chconf PERIANT)
                case $fld in
                 U) min=0 ; max=1.25 ;  pas=0.25 ;;
                 v) skip=1;;
                esac ;;
          70W)  PTS='-pts  -70  -70  10  45 '
                pmin=0 ; pmax=-5800 ; zstep=1000
                skip=$(chconf PERIANT)
                case $fld in
                 U) min=-.5 ; max=.25 ;  pas=0.25 ;;
                 v) skip=1;;
                esac ;;
        esac

        if [ $skip != 1 ] ; then
         filout=${CONFIG}_${var}_${DATE}-${CASE}
         if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $t $MEANY $t
            rapatrie $u $MEANY $u
            rapatrie $v $MEANY $v
            mklim $min $max $pas > zclrmark
            coupplt ; mkplt $filout
         fi
        fi
    done
  done

# additional sections of equatorial velocities for Atlantic, Pacific, Indian 
equat_section() {
    $COUPE -clrdata $u \
      -clrmodif deptht=depthu -pmax -1000 $PTS \
      -clrmet 1 $CLRLIMIT -p nrl21.pal  \
      -title "$title"  -clrvar vozocrtx -zgrid \
      -format PALETTE f4.1
                }

equat_section_indian() {
    $COUPE -clrdata $u \
      -clrmodif deptht=depthu -pmax -1000 $PTS1 \
      -clrmet 1 $CLRLIMIT -p nrl21.pal  \
      -title "$title"  -clrvar vozocrtx -zgrid \
      -format PALETTE f4.1 -o gmetaw -xyplot  0.1 0.5289 0.3 0.90 \
      -xstep 5

    $COUPE -clrdata $u \
      -clrmodif deptht=depthu -pmax -1000 $PTS2 \
      -clrmet 1 $CLRLIMIT -p nrl21.pal  \
      -clrvar vozocrtx -zgrid -clrnopal \
      -o gmetae -xyplot  0.5289 0.95 0.3 0.90 -noperim -nomap \
      -xstep 5 -nozlab -xaxist '_' -nolat

    med -e 'r gmetaw' -e 'r gmetae' -e '1,2 merge' -e '1 w gmeta '
                }

     var=Uequat
# Atlantic 
     filout=${CONFIG}_${var}_ATL_${DATE}-${CASE}
     title="${CONFCASE} U at Equator ATL $YEAR"
     PTS="-pts -50 10 0 0"
     CLRLIMIT="-clrmin -1.0 -clrmax 1.2"

     if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $u $MEANY $u
            equat_section  ; mkplt $filout
     fi

# Pacific
     filout=${CONFIG}_${var}_PACIF_${DATE}-${CASE}
     title="${CONFCASE} U at Equator PAC $YEAR"
     PTS="-pts 130 280 0 0 -360 "
     CLRLIMIT="-clrmin -1.0 -clrmax 1.3"

     if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $u $MEANY $u
            equat_section  ; mkplt $filout
     fi

# Indian
     filout=${CONFIG}_${var}_INDIAN_${DATE}-${CASE}
     title="${CONFCASE} U at Equator IND $YEAR"
     PTS1="-pts 40   72.8  0 0 "
     PTS2="-pts 72.8 105   0 0 "
     CLRLIMIT="-clrmin -0.5 -clrmax 0.5"

     if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $u $MEANY $u
            equat_section_indian  ; mkplt $filout
     fi

  puttogaya  SECTIONS 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6.2 : AR7W section in the Labrador Sea, OVIDE section in the NATL (m03, m10)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $ar7w == 1 ] ; then
  coupar7w() { $COUPE $CLRDATA $PTS -clrnoint \
             -string 0.5 0.95 1.0 0 ${CONFIG}_${var}_${YEAR}-${CASE}  \
             -clrvar $clrvar -pmax $pmax -pmin $pmin -zstep $zstep  -english \
             $PAL  $CLRLIM $CLRMODIF $FORMAT -cntdata sig0.nc -cntvar vosigma0 \
             -cntmin 25 -cntint 0.05 -cntlw 1:2 -cntlis 2 ; }
#-------

  listplt=' '
  CLRLIM='-clrmark zclrmark'
  PAL="-p $PAL1"
  CLRMODIF=''

   for sec in AR7W OVIDE; do
    for m in m03 m10 ; do

# JMM adapt this syntax and check if tlev_03 tlev_09 exist
#  tlev=${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$(echo $CONFIG | tr '[A-Z]' '[a-z]' ).nc
#  slev=${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$(echo $CONFIG | tr '[A-Z]' '[a-z]' ).nc

     tlev=Levitus_p2.1_${m}_TS_masked_$(echo $CONFIG | tr '[A-Z]' '[a-z]' ).nc
     topa=${CONFCASE}_y${YEAR}${m}_gridT.nc

    for fld in tlev t slev s ; do
      skip=0
      var=${fld}${m}${sec}
        case $fld in
           t) clrvar=votemper ;    t=$topa ;;
           tlev) clrvar=votemper ; t=$tlev ;;
           s) clrvar=vosaline ;    t=$topa ;;
           slev) clrvar=vosaline ; t=$tlev ;;
        esac

        case $sec in
         AR7W)  PTS='-pts -56 -48 53.5 60.5'
                pmin=0 ; pmax=-4000 ; zstep=250
                case $fld in
                 t|tlev) min=1 ; max=6; pas=1 ;;
                 s|slev) min=34.7 ; max=35.2 ; pas=.1 ;;
                esac ;;
#               skip=1 ;;
         OVIDE) PTS='-pts -44 -30 60.0 58.5'
                pmin=0 ; pmax=-3500 ; zstep=250
                case $fld in
                 t|tlev) min=1 ; max=6; pas=1 ;;
                 s|slev) min=34.7 ; max=35.2 ; pas=.1 ;;
                esac ;;
#                skip=1 ;;
           *) skip=1 ;;
        esac
        CLRDATA="-clrdata $t"
       if [ $skip != 1 ] ; then
         filout=${CONFIG}_${var}_${YEAR}-${CASE}
         if [ $( chkfile $PLOTDIR/SECTIONS1/$filout.cgm ) == absent ] ; then
            rapatrie $topa $MEANY $topa
            rapatrie $tlev $IDIR $tlev
            $CDFTOOLS/cdfsig0 $t
            mklim $min $max $pas > zclrmark
            coupar7w; mkplt $filout
         fi
       fi
    done
    done
  done
  puttogaya  SECTIONS1 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6.3 : Sections in the Southern Ocean (designed from PERIANT runs)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  # Generic function for coupes_aus plot
  # Top panel (0<depth<1000m zoom)
  couptop() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -1000 -clrvar $clrvar $CLRMODIF \
              $CNTDATA -cntvar $clrvar $CNTLIM -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.70 0.95 -o gmetatop -nolon -ylat 0.10 -english $CLRMARK $PAL \
              -clrxypal 0.1 0.95 0.08 0.18 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${DATE}-${CASE}; }
  # Bottom panel (0<depth<1000m)
  coupbot() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -5500 -clrvar $clrvar $CLRMODIF \
              $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.20 0.68 -o gmetabot -nolon -ylat 0.10  -english $CLRMARK $PAL \
              -clrxypal 0.1 0.95 0.08 0.18 -zstep 500; }

#--------
                        if [ $coupes_aus == 1 ] ; then
  listplt=' '
  # get files  gridT, gridU, gridV LSPV in march and september
  t=${CONFCASE}_y${YEAR}_gridT.nc
  u=${CONFCASE}_y${YEAR}_gridU.nc
  v=${CONFCASE}_y${YEAR}_gridV.nc
  fich_pvm3=${CONFCASE}_y${YEAR}m03_LSPV.nc
  fich_pvm9=${CONFCASE}_y${YEAR}m09_LSPV.nc

  PAL="-p $PAL2"

  #Coupes T,S,U and PV
  for sec in DRAK WEDD ATLS AFRS KERG AUST ADEL CAMP ROSS PACS ; do
    for fld in t s u pvm3 pvm9 ; do
      LEV=''
      skip=0
      var=${fld}${sec}
      LATmin=-70
      LATmax=-30
      case $fld in
         t) clrvar=votemper ; CLRDATA="-clrdata $t" ;
            CLRMODIF='' ;
cat << eof > T.lim
0
2.0
5.0
8.0
10.0
eof
cat << eof > T.mark
-0.5
1.0
5.0
8.0
10.0
15.0
eof
            CLRMARK="-clrmark T.mark" ;
            CNTLIM="-cntlim T.lim" ; CNTDATA="-cntdata $t" ;;
         s) clrvar=vosaline ; CLRDATA="-clrdata $t" ;
            CLRMODIF='' ;
cat << eof > S.lim
34.2
34.4
34.7
34.8
35.0
eof
cat << eof > S.mark
34.0
34.2
34.6
35.0
35.3
eof
            CLRMARK="-clrmark S.mark" ;
            CNTLIM="-cntlim S.lim" ; CNTDATA="-cntdata $t" ;;
         u) clrvar=vozocrtx ; CLRDATA="-clrdata $u" ;
            CLRMODIF='-clrmodif deptht=depthu';
cat << eof > u.mark
-0.2
-0.1
0
0.1
0.2
eof
            CLRMARK="-clrmark u.mark" ;
            CNTLIM='' ; CNTDATA='' ;;
      pvm3) clrvar=volspv ; CLRDATA="-clrdata $fich_pvm3" ;
            CLRMODIF='-clrmodif deptht=depthw';
cat << eof > PV.mark
-1
0
1
2
3
4
5
6
eof
            CLRMARK="-clrmark PV.mark" ;
            rapatrie $fich_pvm3 $MEANY $fich_pvm3 ;
            nlev=$( cdfinfo $fich_pvm3 | grep npk | awk '{ print $3}' );
            LEV="-lev 2-$nlev" ;
            CNTLIM='' ; CNTDATA='' ;;
      pvm9) clrvar=volspv ; CLRDATA="-clrdata pvm9" ;
            CLRMODIF='-clrmodif deptht=depthw';
cat << eof > PV.mark
-1
0
1
2
3
4
5
6
eof
            CLRMARK="-clrmark PV.mark" ;
            rapatrie $fich_pvm9 $MEANY $fich_pvm9 ;
            nlev=$( cdfinfo $fich_pvm9 | grep npk | awk '{ print $3}' );
            LEV="-lev 2-$nlev" ;
            CNTLIM='' ; CNTDATA='' ;;
     esac
     case $sec in
        DRAK)  LONmin=-65 ; LONmax=-65 ; SECTION=065W ;;
        WEDD)  LONmin=-42 ; LONmax=-42 ; SECTION=042W ;;
        ATLS)  LONmin=0   ; LONmax=0   ; SECTION=0E ;;
        AFRS)  LONmin=30  ; LONmax=30  ; SECTION=030E ;;
        KERG)  LONmin=70  ; LONmax=70  ; SECTION=070E ;;
        AUST)  LONmin=115 ; LONmax=115 ; SECTION=115E ;;
        ADEL)  LONmin=140 ; LONmax=140 ; SECTION=140E ;;
        CAMP)  LONmin=170 ; LONmax=170 ; SECTION=170E ;;
        ROSS)  LONmin=-170; LONmax=-170; SECTION=170W ;;
        PACS)  LONmin=-90 ; LONmax=-90 ; SECTION=090W ;;
          * )  skip=1 ;;
     esac
# JMM WARNING  hard coded 46 below ; can be avoided with coupe option
     if [ $skip != 1 ] ; then
       filout=${CONFIG}_${var}_${DATE}-${CASE}
       if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
         rapatrie $t $MEANY $t
         rapatrie $u $MEANY $u
         rapatrie $v $MEANY $v
#        rapatrie $fich_pvm3 $MEANY $fich_pvm3
#        nlev=$( ncdump -h $fich_pvm3 | grep depth | head -1 | awk '{ print $3}' )
#        ncks -F -O -d depthw,2,$nlev ${fich_pvm3} pvm3
#        rapatrie $fich_pvm9 $MEANY $fich_pvm9
#        ncks -F -O -d depthw,2,$nlev ${fich_pvm9} pvm9
         couptop ; coupbot
         med -e 'r gmetatop' -e 'r gmetabot' -e '1,2 merge' -e '1 w gmeta'
         mkplt $filout
         rm gmeta* pvm3 pvm9
       fi
     fi
    done
  done
  puttogaya  SECTIONS
                        fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6.4 Coupes sigma as in the WOCE Atlas (in the Southern Ocean)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  ##Generic plot for sigma
  # Top panel (0<depth<1000m zoom)
  coupsig() { $COUPE -clrdata sig0.nc -pts $LONmin $LONmax $LATmin $LATmax -forcexy -pmax -1000 -clrvar vosigma0 \
              -cntdata sig0.nc -cntvar vosigma0 $CNTLIM0 -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.65 0.9 -o gmetatop $CLRMARK0 $PAL -nolon -clrnopal\
              -ylat 0.10 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${DATE}-${CASE} ; \
  # Bottom panel (0<depth<1000m)
              $COUPE -clrdata sig0.nc -pts $LONmin $LONmax $LATmin $LATmax -forcexy -pmax -1000 -clrvar vosigma0 \
              -cntdata sig0.nc -cntvar vosigma0 $CNTLIM0  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.53 0.63 -o gmetabot1 $CLRMARK0 $PAL -zstep 500 -nolon \
              -clrnopal -ylat 0.10 ; \
              $COUPE -clrdata sig2.nc -pts $LONmin $LONmax $LATmin $LATmax -forcexy -pmin -1000 -pmax -3000 -clrvar vosigmai \
              -cntdata sig2.nc -cntvar vosigmai $CNTLIM2  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.34 0.53 -o gmetabot2 $CLRMARK2 $PAL -nolon \
              -clrnopal -ylat 0.10 ; \
              $COUPE -clrdata sig4.nc -pts $LONmin $LONmax $LATmin $LATmax -forcexy -pmin -3000 -clrvar vosigmai \
              -cntdata sig4.nc -cntvar vosigmai $CNTLIM4 -cntrc1 0.06 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.15 0.34 -o gmetabot3 $CLRMARK4 $PAL -nolon \
              -clrnopal -ylat 0.10; }
#--------------
                        if [ $coupes_aus_sigma = 1 ] ; then
  listplt=' '
cat << eof > sig0.lim
27.3
27.6
27.81
eof
cat << eof > sig0.mark
26.2
27.3
27.6
27.81
28.1
eof
cat << eof > sig2.lim
36.4
36.8
37.05
37.14
eof
cat << eof > sig2.mark
34.95
37.05
37.14
37.45
eof
cat << eof > sig4.lim
45.88
46.02
eof
cat << eof > sig4.mark
43.0
45.88
46.02
46.45
eof
  CNTLIM0="-cntlim sig0.lim"
  CLRMARK0="-clrmark sig0.mark"
  CNTLIM2="-cntlim sig2.lim"
  CLRMARK2="-clrmark sig2.mark"
  CNTLIM4="-cntlim sig4.lim"
  CLRMARK4="-clrmark sig4.mark"

  for sec in DRAK WEDD ATLS AFRS KERG AUST ADEL CAMP ROSS PACS ; do
    skip=0
    var=sig${sec}
    LATmin=-70
    LATmax=-30
    case $sec in
      DRAK)  LONmin=-65 ; LONmax=-65 ; SECTION=065W ;;
      WEDD)  LONmin=-42 ; LONmax=-42 ; SECTION=042W ;;
      ATLS)  LONmin=0   ; LONmax=0   ; SECTION=0E ;;
      AFRS)  LONmin=30  ; LONmax=30  ; SECTION=030E ;;
      KERG)  LONmin=70  ; LONmax=70  ; SECTION=070E ;;
      AUST)  LONmin=115 ; LONmax=115 ; SECTION=115E ;;
      ADEL)  LONmin=140 ; LONmax=140 ; SECTION=140E ;;
      CAMP)  LONmin=170 ; LONmax=170 ; SECTION=170E ;;
      ROSS)  LONmin=-170; LONmax=-170; SECTION=170W ;;
      PACS)  LONmin=-90 ; LONmax=-90 ; SECTION=090W ;;
        * )  skip=1 ;;
    esac
    if [ $skip != 1 ] ; then
      filout=${CONFIG}_${var}_${DATE}-${CASE}
      if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
        rapatrie $t $MEANY $t
        cdfsig0 $t
        cdfsigi $t 2000
        mv sigi.nc sig2.nc
        cdfsigi $t 4000
        mv sigi.nc sig4.nc
        coupsig
        med -e 'r gmetatop' -e 'r gmetabot1' -e '1,2 merge' -e '1 w gmeta1'
        med -e 'r gmeta1' -e 'r gmetabot2' -e '1,2 merge' -e '1 w gmeta2'
        med -e 'r gmeta2' -e 'r gmetabot3' -e '1,2 merge' -e '1 w gmeta'
        mkplt $filout
        rm gmeta*
      fi
    fi
  done
  rm sig*

  puttogaya  SECTIONS
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6.5 : Sections in the ACC (designed from PERIANT runs)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $circum == 1 ] ; then
  # Generic function for circumpolar plot
  # Top panel (0<depth<1000m zoom)
  couptopcirc() { $COUPE $CLRDATA -pts $LON1 $LON2 $LATmin $LATmax $LEV -forcexy -pmax -5500 -clrvar $clrvar $CLRMODIF \
                  $CNTDATA -cntvar $clrvar $CNTLIM -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
                  -xyplot 0.1 0.95 0.60 0.95 -o gmetatop -ylon 0.58 -nolat -english -clrmark zclrmark $PAL \
                  -clrxypal 0.1 0.95 0.03 0.13 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${DATE}-${CASE}; }
  # Bottom panel (0<depth<1000m)
  coupbotcirc() { $COUPE $CLRDATA -pts $LON2 $LON3 $LATmin $LATmax $LEV -forcexy -pmax -5500 -clrvar $clrvar $CLRMODIF \
                  $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.03 \
                  -xyplot 0.1 0.44 0.20 0.55 -o gmetabot1 -ylon 0.18 -nolat -english -clrmark zclrmark $PAL \
                  -clrxypal 0.1 0.95 0.03 0.13 ; \
                  $COUPE $CLRDATA -pts $LON3 $LON4 $LATmin $LATmax -forcexy -pmax -5500 -clrvar $clrvar $CLRMODIF \
                  $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.20 0.55 -o gmetabot2 -nozlab -ylon 0.18 -nolat -english -clrmark zclrmark $PAL \
                  -clrxypal 0.1 0.95 0.03 0.13 ; }
#------------

  listplt=' '
  # get files  gridT, gridU, gridV
  t=${CONFCASE}_y${YEAR}_gridT.nc
  fich_pvm3=${CONFCASE}_y${YEAR}m03_LSPV.nc
  fich_pvm9=${CONFCASE}_y${YEAR}m09_LSPV.nc

  PAL="-p $PAL2"

  #Coupes T,S
  for sec in CIRC70 CIRC60 CIRC48 CIRC40 ; do
   for fld in t s pvm3 pvm9 ; do
     LEV=""
     skip=0
     var=${fld}
     LON1=-180 ; LON2=0 ; LON3=72.8 ; LON4=180
     case $fld in
         t) clrvar=votemper ; CLRDATA="-clrdata $t" ;
            CLRMODIF='' ;
cat << eof > T.lim
0
2.0
5.0
8.0
10.0
eof
            CNTLIM="-cntlim T.lim" ; CNTDATA="-cntdata $t" ;;
         s) clrvar=vosaline ; CLRDATA="-clrdata $t" ;
            CLRMODIF='' ;
cat << eof > S.lim
34.2
34.4
34.7
34.8
35.0
eof
            CNTLIM="-cntlim S.lim" ; CNTDATA="-cntdata $t" ;;
      pvm3) clrvar=volspv ; CLRDATA="-clrdata $fich_pvm3" ;
            CLRMODIF='-clrmodif deptht=depthw';
            CNTDATA='' ; CNTLIM='' ;
            rapatrie $fich_pvm3 $MEANY $fich_pvm3 ; 
            nlev=$( cdfinfo $fich_pvm3 | grep npk | awk '{ print $3}' );
            LEV="-lev 2-$nlev" ;
            min=-1.0 ; max=6.0 ; pas=1.0 ;;
      pvm9) clrvar=volspv ; CLRDATA="-clrdata $fich_pvm9" ;
            CLRMODIF='-clrmodif deptht=depthw';
            CNTDATA='' ; CNTLIM='' ;
            rapatrie $fich_pvm9 $MEANY $fich_pvm9 ; 
            nlev=$( cdfinfo $fich_pvm9 | grep npk | awk '{ print $3}' );
            LEV="-lev 2-$nlev" ; 
            min=-1.0 ; max=6.0 ; pas=1.0 ;;
     esac
     case $sec in
        CIRC70)  LATmin=-70; LATmax=-70 ; SECTION=070S ;
                 case $fld in
                   t) min=-1 ; max=2 ; pas=0.5 ;;
                   s) min=33.8 ; max=34.6 ; pas=0.2 ;;
                 esac ;;
        CIRC60)  LATmin=-60; LATmax=-60 ; SECTION=060S ;
                 case $fld in
                   t) min=-1 ; max=5 ; pas=0.5 ;;
                   s) min=33.8 ; max=34.6 ; pas=0.2 ;;
                 esac ;;
        CIRC48)  LATmin=-48; LATmax=-48 ; SECTION=048S ;
                 case $fld in
                   t) min=0 ; max=10 ; pas=1 ;;
                   s) min=33.8 ; max=34.6 ; pas=0.2 ;;
                 esac ;;
        CIRC40)  LATmin=-40; LATmax=-40 ; SECTION=040S ;
                 case $fld in
                   t) min=0 ; max=16 ; pas=2 ;;
                   s) min=33.5 ; max=35 ; pas=0.5 ;;
                 esac ;;
            * )  skip=1 ;;
     esac
     if [ $skip != 1 ] ; then
       filout=${CONFIG}_${var}${sec}_${DATE}-${CASE}
       if [ $( chkfile $PLOTDIR/CIRCUM/$filout.cgm ) == absent ] ; then
         rapatrie $t $MEANY $t
#        rapatrie $fich_pvm3 $MEANY $fich_pvm3
#        nlev=$( ncdump -h $fich_pvm3 | grep depth | head -1 | awk '{ print $3}' )
#        ncks -F -O -d depthw,2,$nlev ${fich_pvm3} pvm3
#        rapatrie $fich_pvm9 $MEANY $fich_pvm9
#        ncks -F -O -d depthw,2,$nlev ${fich_pvm9} pvm9
         mklim $min $max $pas > zclrmark
         couptopcirc ; coupbotcirc
         med -e 'r gmetabot1' -e 'r gmetabot2' -e '1,2 merge' -e '1 w gmetabot'
         med -e 'r gmetatop' -e 'r gmetabot' -e '1,2 merge' -e '1 w gmeta'
         mkplt $filout
#         rm gmeta* pvm3 pvm9
       fi
     fi
   done
  done
  puttogaya  CIRCUM 
                        fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6.6 :  Density sections in the ACC
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  #Coupes sigma as in the WOCE Atlas

  ##Generic plot for circumpolar sigma plots
  # Top panel (0<depth<1000m zoom)
  coupsigcirc() { $COUPE -clrdata sig0.nc -pts $LON1 $LON2 $LATmin $LATmax -forcexy -pmax -1000 -clrvar vosigma0 \
                  -cntdata sig0.nc -cntvar vosigma0 $CNTLIM0 -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
                  -xyplot 0.1 0.95 0.87 0.95 -o gmetatop1 $CLRMARK0 $PAL -clrnopal -ylon 0.53 -nolat -zstep 500 \
                  -ylat 0.10 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${DATE}-${CASE} ; \
                  $COUPE -clrdata sig2.nc -pts $LON1 $LON2 $LATmin $LATmax -forcexy -pmin -1000 -pmax -3000 \
                  -clrvar vosigmai -cntdata sig2.nc -cntvar vosigmai $CNTLIM2 -cntrc1 0.04 -cntrc2 0.07 \
                  -cntlis 1 -cntlls 0.013 \
                  -xyplot 0.1 0.95 0.71 0.87 -o gmetatop2 $CLRMARK2 $PAL -clrnopal -ylon 0.53 -nolat -ylat 0.10 ; \
                  $COUPE -clrdata sig4.nc -pts $LON1 $LON2 $LATmin $LATmax -forcexy -pmin -3000 -pmax -5500 -clrvar vosigmai \
                  -cntdata sig4.nc -cntvar vosigmai $CNTLIM4 -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
                  -xyplot 0.1 0.95 0.55 0.71 -o gmetatop3 $CLRMARK4 $PAL -nolon -clrnopal -ylon 0.53 -nolat \
                  -ylat 0.10 ; \
  # Bottom panel (0<depth<1000m)
                  $COUPE -clrdata sig0.nc -pts $LON2 $LON3 $LATmin $LATmax -forcexy -pmax -1000 -clrvar vosigma0 \
                  -cntdata sig0.nc -cntvar vosigma0 $CNTLIM0  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.03 \
                  -xyplot 0.1 0.44 0.42 0.50 -o gmetabot11 $CLRMARK0 $PAL -zstep 500 -ylon 0.08 -nolat \
                  -clrnopal -ylat 0.10 ; \
                  $COUPE -clrdata sig2.nc -pts $LON2 $LON3 $LATmin $LATmax -forcexy -pmin -1000 -pmax -3000 \
                  -clrvar vosigmai -cntdata sig2.nc -cntvar vosigmai $CNTLIM2  -cntrc1 0.04 -cntrc2 0.07 \
                  -cntlis 1 -cntlls 0.03 \
                  -xyplot 0.1 0.44 0.26 0.42 -o gmetabot12 $CLRMARK2 $PAL -ylon 0.08 -nolat \
                  -clrnopal -ylat 0.10 ; \
                  $COUPE -clrdata sig4.nc -pts $LON2 $LON3 $LATmin $LATmax -forcexy -pmin -3000 -pmax -5500 -clrvar vosigmai \
                  -cntdata sig4.nc -cntvar vosigmai $CNTLIM4 -cntrc1 0.06 -cntrc2 0.07 -cntlis 1 -cntlls 0.03 \
                  -xyplot 0.1 0.44 0.10 0.26 -o gmetabot13 $CLRMARK4 $PAL -nolon -ylon 0.08 -nolat \
                  -clrnopal -ylat 0.10; \
                  $COUPE -clrdata sig0.nc -pts $LON3 $LON4 $LATmin $LATmax -forcexy -pmax -1000 -clrvar vosigma0 \
                  -cntdata sig0.nc -cntvar vosigma0 $CNTLIM0  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.42 0.50 -o gmetabot21 $CLRMARK0 $PAL -zstep 500 -ylon 0.08 -nolat -nozlab \
                  -clrnopal -ylat 0.10 ; \
                  $COUPE -clrdata sig2.nc -pts $LON3 $LON4 $LATmin $LATmax -forcexy -pmin -1000 -pmax -3000 \
                  -clrvar vosigmai -cntdata sig2.nc -cntvar vosigmai $CNTLIM2  -cntrc1 0.04 -cntrc2 0.07 \
                  -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.26 0.42 -o gmetabot22 $CLRMARK2 $PAL -ylon 0.08 -nolat -nozlab \
                  -clrnopal -ylat 0.10 ; \
                  $COUPE -clrdata sig4.nc -pts $LON3 $LON4 $LATmin $LATmax -forcexy -pmin -3000 -pmax -5500 -clrvar vosigmai \
                  -cntdata sig4.nc -cntvar vosigmai $CNTLIM4 -cntrc1 0.06 -cntrc2 0.07 -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.10 0.26 -o gmetabot23 $CLRMARK4 $PAL -ylon 0.08 -nolat -nozlab \
                  -clrnopal -ylat 0.10; }
#--------------------
                        if [ $circum_sigma = 1 ] ; then
  listplt=' '
cat << eof > sig0.lim
27.3
27.6
27.81
eof
cat << eof > sig0.mark
26.2
27.3
27.6
27.81
28.1
eof
cat << eof > sig2.lim
36.4
36.8
37.05
37.14
eof
cat << eof > sig2.mark
34.95
37.05
37.14
37.45
eof
cat << eof > sig4.lim
45.88
46.02
eof
cat << eof > sig4.mark
43.0
45.88
46.02
46.45
eof
  CNTLIM0="-cntlim sig0.lim"
  CLRMARK0="-clrmark sig0.mark"
  CNTLIM2="-cntlim sig2.lim"
  CLRMARK2="-clrmark sig2.mark"
  CNTLIM4="-cntlim sig4.lim"
  CLRMARK4="-clrmark sig4.mark"

  for sec in CIRC70 CIRC60 CIRC48 CIRC40 ; do
    skip=0
    var=sigma
    LON1=-180 ; LON2=0 ; LON3=72.8 ; LON4=180
    case $sec in
      CIRC70)  LATmin=-70; LATmax=-70 ; SECTION=070S ;;
      CIRC60)  LATmin=-60; LATmax=-60 ; SECTION=060S ;;
      CIRC48)  LATmin=-48; LATmax=-48 ; SECTION=048S ;;
      CIRC40)  LATmin=-40; LATmax=-40 ; SECTION=040S ;;
          * )  skip=1 ;;
    esac
    if [ $skip != 1 ] ; then
      filout=${CONFIG}_sig${sec}_${DATE}-${CASE}
      if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
        rapatrie $t $MEANY $t
        cdfsig0 $t
        cdfsigi $t 2000
        mv sigi.nc sig2.nc
        cdfsigi $t 4000
        mv sigi.nc sig4.nc
        coupsigcirc
        med -e 'r gmetatop1' -e 'r gmetatop2' -e '1,2 merge' -e '1 w gmeta1'
        med -e 'r gmeta1' -e 'r gmetatop3' -e '1,2 merge' -e '1 w gmeta2'
        med -e 'r gmeta2' -e 'r gmetabot11' -e '1,2 merge' -e '1 w gmeta3'
        med -e 'r gmeta3' -e 'r gmetabot12' -e '1,2 merge' -e '1 w gmeta4'
        med -e 'r gmeta4' -e 'r gmetabot13' -e '1,2 merge' -e '1 w gmeta5'
        med -e 'r gmeta5' -e 'r gmetabot21' -e '1,2 merge' -e '1 w gmeta6'
        med -e 'r gmeta6' -e 'r gmetabot22' -e '1,2 merge' -e '1 w gmeta7'
        med -e 'r gmeta7' -e 'r gmetabot23' -e '1,2 merge' -e '1 w gmeta'
        mkplt $filout
        rm gmeta*
      fi
    fi
  done
  rm sig*


  puttogaya  CIRCUM 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 7. Ice plots:
#^^^^^^^^^^^^^^
                        if [ $iceplt == 1 ] ; then
  # plot ice thickness for march and september
  listplt=' '
    cat << eof > S_thick.lim
0
0.15
0.30
0.45
0.60
0.75
0.90
1.05
1.20
1.35
1.50
1.65
1.80
1.95
2.10
2.50
3.00
3.50
4.50
eof
   cat << eof > thick.lim
0
0.5
1
1.5
2
2.5
3
3.5
4
4.5
5
5.5
6
6.5
7
7.5
8
8.5
9
eof
  cat << eof > ice_noaa.lim
-0.001
0.001
0.05
0.1
0.15
0.2
0.25
0.3
0.35
0.4
0.45
0.5
0.55
0.6
0.65
0.7
0.75
0.8
0.85
0.9
0.95
1.0
eof

  for mm in 3 9 ; do
    m=$( printf "%02d" $mm )
    ice=${CONFCASE}_y${YEAR}m${m}_icemod.nc

    tmp=${ice#${CONFCASE}_y} ; tag=${tmp%_*}
    year=$( echo $tag | awk -Fm '{print $1}' )
    month=$( echo $tag | awk -Fm '{print $2}' )
    icetxt=${CONFCASE}_y${YEAR}_icemonth.txt
    rapatrie $icetxt $DIAGS $icetxt

    case $month in
      01 ) month_name=Jan ;;
      02 ) month_name=Feb ;;
      03 ) month_name=Mar ;;
      04 ) month_name=Apr ;;
      05 ) month_name=May ;;
      06 ) month_name=Jun ;;
      07 ) month_name=Jul ;;
      08 ) month_name=Aug ;;
      09 ) month_name=Sep ;;
      10 ) month_name=Oct ;;
      11 ) month_name=Nov ;;
      12 ) month_name=Dec ;;
    esac
    listhemi="NORTH SOUTH" 
    if [ $( chconf PERIANT) != 0 ] ; then listhemi="SOUTH" ; fi
    for hemi in $listhemi ; do
      case $hemi in
      SOUTH ) surf=$( grep -A 12 -e "$YEAR $m" $icetxt | grep SExtend  | awk '{printf "%5.1f",  $NF/1000}' )
              volu=$( grep -A 12 -e "$YEAR $m" $icetxt | grep SVolume  | awk '{printf "%5.1f",  $NF/1000}' )
         filout=${CONFIG}_S_ithic_${month}_${YEAR}-${CASE}
        if [ $( chkfile $PLOTDIR/ICE/$filout.cgm ) == absent ] ; then
         rapatrie $ice $MEANY $ice
        $CHART -forcexy -clrdata $ice -clrvar iicethic -clrlim S_thick.lim -p $PALBLUE2RED4 \
        -proj OR -vertpal -rlat -90 -zoom -180 180 -90 -50  -spval 0  -xstep 45 -xgrid -ystep 45 -ygrid \
        -clrexp -2 -format PALETTE I3 -noperim \
        -clrxypal 0.9 1 0.1 0.9 \
        -xyplot 0.01 0.8 0.1 0.9 \
        -string 0 0.97 1.1 -1 " Sea Ice Thicknes" -font SIMPLEX_ROMAN -noteam \
        -string 0 0.94 1.1 -1 " $month_name $year " \
        -string 0.5 0.97 1.2 -1 " $CONFCASE "  \
        -string 0.3 0.05 1.1 -1 " Total area = $surf million sq km" \
        -string 0.3 0.02 1.1 -1 " Volume = $volu cubic km" \
        -string 0.93 0.07 1.1 1 " (cm)    "
         mkplt  $filout
        fi
         filout=${CONFIG}_S_iconc_${month}_${YEAR}-${CASE}
        if [ $( chkfile $PLOTDIR/ICE/$filout.cgm ) == absent ] ; then
         rapatrie $ice $MEANY $ice
        $CHART -forcexy -clrdata $ice -clrvar ileadfra -clrlim ice_noaa.lim -p $PALNOAA \
        -proj OR -vertpal -rlat -90 -zoom -180 180 -90 -50  -spval -1 \
        -clrexp -2 -format PALETTE I3 -noperim \
        -clrxypal 0.9 1 0.1 0.9 \
        -xyplot 0.01 0.8 0.1 0.9 \
        -string 0 0.97 1.1 -1 " Sea Ice Concentration" -font SIMPLEX_ROMAN -noteam \
        -string 0 0.94 1.1 -1 " $month_name $year " \
        -string 0.5 0.97 1.2 -1 " $CONFCASE "  \
        -string 0.3 0.03 1.1 -1 " Total area = $surf million sq km" \
        -string 0.93 0.07 1.1 1 " %    "
         mkplt  $filout
        fi ;;

       NORTH ) surf=$( grep -A 12 -e "$YEAR $m" $icetxt | grep NExtend  | awk '{printf "%5.1f",  $NF/1000}' )
               volu=$( grep -A 12 -e "$YEAR $m" $icetxt | grep NVolume  | awk '{printf "%5.1f",  $NF/1000}' )
         filout=${CONFIG}_N_ithic_${month}_${YEAR}-${CASE}
        if [ $( chkfile $PLOTDIR/ICE/$filout.cgm ) == absent ] ; then
         rapatrie $ice $MEANY $ice
         $CHART -forcexy -clrdata $ice -clrvar iicethic -clrlim thick.lim -p $PALBLUE2RED4 \
         -proj OR -vertpal -rlat 90 -rlon -45 -zoom -180 178.0 40 87  -spval 0 -xstep 45 -xgrid -ystep 45 -ygrid \
         -clrexp -2 -format PALETTE I3 -noperim \
         -clrxypal 0.9 1 0.1 0.9 \
         -xyplot 0.01 0.8 0.1 0.9 \
         -string 0 0.97 1.1 -1 " Sea Ice Thicknes" -font SIMPLEX_ROMAN -noteam \
         -string 0 0.94 1.1 -1 " $month_name $year " \
         -string 0.5 0.97 1.2 -1 " $CONFCASE "  \
         -string 0.3 0.05 1.1 -1 " Total area = $surf million sq km"  \
         -string 0.3 0.02 1.1 -1 " Volume = $volu cubic km" \
         -string 0.93 0.07 1.1 1 " (cm)    "
          mkplt $filout
        fi
         filout=${CONFIG}_N_iconc_${month}_${YEAR}-${CASE}
        if [ $( chkfile $PLOTDIR/ICE/$filout.cgm ) == absent ] ; then
         rapatrie $ice $MEANY $ice
         $CHART -forcexy -clrdata $ice -clrvar ileadfra -clrlim ice_noaa.lim -p $PALNOAA \
         -proj OR -vertpal -rlat 90 -rlon -45 -zoom -180 178.0 40 87  -spval -1 \
         -clrexp -2 -format PALETTE I3 -noperim \
         -clrxypal 0.9 1 0.1 0.9 \
         -xyplot 0.01 0.8 0.1 0.9 \
         -string 0 0.97 1.1 -1 " Sea Ice Concentration" -font SIMPLEX_ROMAN -noteam \
         -string 0 0.94 1.1 -1 " $month_name $year " \
         -string 0.5 0.97 1.2 -1 " $CONFCASE "  \
         -string 0.3 0.03 1.1 -1 " Total area = $surf million sq km"  \
         -string 0.93 0.07 1.1 1 " %    "
         mkplt  $filout
        fi ;;
       esac
     done

#    \rm $ice $tmp
   done

    puttogaya  ICE 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 8. Deep western boundary current
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 8.1 : DWBC in the Atlantic :
#------------------------------
                        if [ $dwbc == 1 ] ; then

  listplt=' '
  # get files   gridU, gridV
  u=${CONFCASE}_y${YEAR}_gridU.nc
  v=${CONFCASE}_y${YEAR}_gridV.nc

  CLRLIM='-clrmark zclrmark'
  PAL="-p $PAL1"

  for sec in  53N 48N 69W 24N 55W eq 11S 20S 30S ; do
    for fld in u v ; do
      skip=0
      var=${fld}${sec}
        case $fld in
           u) CLRDATA="-clrdata $u" ;clrvar=vozocrtx ; CLRMODIF='-clrmodif deptht=depthu';;
           v) CLRDATA="-clrdata $v" ;clrvar=vomecrty ; CLRMODIF='-clrmodif deptht=depthv';;
        esac
        echo SEC $sec
        case $sec in
          53N)  PTS='-pts -52  -48  53  54'
                pmin=0 ; pmax=-4000 ; zstep=500
                case $fld in
                 u) skip=1 ;;
                 v) min=-.28 ; max=-.04 ; pas=.04 ;;
                esac ;;
          48N)  PTS='-pts -45  -40  49   50'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u)  skip=1 ;;
                 v) min=-.1 ; max=-.04 ; pas=.02 ;;
                esac ;;
          69W)  PTS='-pts -70 -67.8 40 36.5'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u) min=-.15 ; max=0.75 ; pas=.15 ;;
                 v)  skip=1 ;;
                esac ;;
          24N)  PTS='-pts -81 -71   24   24'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u)  skip=1 ;;
                 v) min=-.4 ; max=0.3 ; pas=.1 ;;
                esac ;;
          55W)  PTS='-pts -55 -55    4   15'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u) min=-.2 ; max=0.15 ; pas=.05 ;;
                 v)  skip=1 ;;
                esac ;;
          eq)  PTS='-pts  -50 -30    0    0'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u)  skip=1 ;;
                 v) min=-.2; max=0.15 ; pas=.05 ;;
                esac ;;
          11S)  PTS='-pts  -40 -30  -11  -11'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u)  skip=1 ;;
                 v) min=-.2; max=0.15 ; pas=.05 ;;
                esac ;;
          20S)  PTS='-pts  -42 -32  -20  -20'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u)  skip=1 ;;
                 v) min=-.2; max=0.15 ; pas=.05 ;;
                esac ;;
          30S)  PTS='-pts  -52 -42  -30  -30'
                pmin=0 ; pmax=-5000 ; zstep=500
                case $fld in
                 u)  skip=1 ;;
                 v) min=-.2; max=0.15 ; pas=.05 ;;
                esac ;;
        esac

        if [ $skip != 1 ] ; then
          filout=${CONFIG}_${var}_${DATE}-${CASE}
          if [ $( chkfile $PLOTDIR/DWBC/$filout.cgm ) == absent ] ; then
             rapatrie $u $MEANY $u
             rapatrie $v $MEANY $v
             mklim $min $max $pas > zclrmark
             coupplt ; mkplt $filout
          fi
        fi
    done
  done

 puttogaya  DWBC 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 8.2 Deep Western Boundary Current in the Southern Ocean
#-----------------------------------------------------------
                        if [ $dwbc_aus == 1 ] ; then

  listplt=' '
  # get files   gridU, gridV
  v=${CONFCASE}_y${YEAR}_gridV.nc

  PAL="-p $PAL2"

  for sec in 31S ; do
     skip=0
     var=${sec}
     CLRDATA="-clrdata $v" ;clrvar=vomecrty
     CLRMODIF='-clrmodif deptht=depthv'
cat << eof > v.mark
-0.2
-0.1
0
0.1
0.2
eof
     CLRMARK="-clrmark v.mark"
     CNTLIM='' ; CNTDATA=''
     case $sec in
       31S) LATmin=-31 ; LATmax=-31 ;
            LONmin=-52 ; LONmax=-42 ; SECTION='042W-052W' ;;
     esac

     if [ $skip != 1 ] ; then
       filout=${CONFIG}_${var}_${DATE}-${CASE}
       if [ $( chkfile $PLOTDIR/DWBC/$filout.cgm ) == absent ] ; then
         rapatrie $v $MEANY $v
         couptop ; coupbot
         med -e 'r gmetatop' -e 'r gmetabot' -e '1,2 merge' -e '1 w gmeta'
         mkplt $filout
         rm gmeta*
       fi
     fi
  done
  rm *.mark
  puttogaya  DWBC
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 9. Eddy Kinetic Energy
#^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $ekes == 1 ] ; then

  ekeplt() { $CHART $ZOOM -forcexy $XYPLOT $STRING $CLRDATA  -p $PAL1 \
             -clrvar voeke $dep $CLRXYPAL -clrmark zclrmark \
             $OPTIONS -english $FORMAT -scale 1.e4 ; }
#-----------

  # get files  EKE
  eke=${CONFCASE}_y${DATE}_EKE.nc
  # check if input file is observation ( obs in CASE ) (no -dep option in this case )
  echo $CASE | grep -q -i obs 
  if [ $? = 0 ] ; then 
     dep='' 
  else
     dep='-dep 10' 
  fi

  FORMAT=""
  min=0 ; max=2500 ; pas=250   # ORCA CONFIG default
  XYPLOT='' ; CLRXYPAL=''

  if [ $(chconf PERIANT) != 0 ] ; then
    min=0 ; max=1000 ; pas=250 
    XYPLOT='-xyplot 0.1 0.95 0.4 0.8'
    CLRXYPAL='-clrxypal 0.1 0.95 0.2 0.3'
  fi

  mklim $min $max $pas > zclrmark
  CLRDATA=" -clrdata $eke"

  #Global plot
  listplt=' '
  var=EKEgl  ; ZOOM='-ijgrid -noproj -noint' ; OPTIONS='' 
  STRING="-string 0.5 0.95 1.0 0  ${CONFIG}_${var}_${DATE}_${CASE}_DEPTH=@CLR_DEPTH@ "
  filout=${CONFIG}_${var}_${dep}_${DATE}-${CASE}
  if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
     rapatrie $eke $MEANY $eke
     ekeplt ; mkplt $filout
  fi
  
  if [ $(chconf PERIANT) = 0 ] ; then  # EKE in the Atlantic
    var=EKEatl ; ZOOM='-zoom -100 20 -70 70' ; OPTIONS='-proj ME -xstep 15 -ystep 15'
    STRING="-string 0.5 0.95 1.0 0 ${CONFIG}_${var}_${DATE}_${CASE}_DEPTH=@CLR_DEPTH@ "
    filout=${CONFIG}_${var}_${dep}_${DATE}-${CASE}


    if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
       rapatrie $eke $MEANY $eke
       ekeplt ; mkplt $filout
    fi
  fi

  puttogaya  GLOBAL

  #Zoom plots in the Austral area ,take PERIANT settings even for ORCA plots
  min=0 ; max=1000 ; pas=250 
  mklim $min $max $pas > zclrmark

  for BASIN in DRAKE KERGUELEN CAMPBELL ; do
    listplt=' '
    case $BASIN in
         DRAKE)  var=EKEdrak ; ZOOM='-zoom -100 -20 -70 -30' ;;
     KERGUELEN)  var=EKEkerg ; ZOOM='-zoom 30 110 -70 -30' ;;
      CAMPBELL)  var=EKEcamp ; ZOOM='-zoom 100 180 -70 -30' ;;
    esac
    OPTIONS='-proj ME -xstep 10 -ystep 5'
    STRING="-string 0.5 0.95 1.0 0 ${CONFIG}_${var}_${DATE}_${CASE}_DEPTH=@CLR_DEPTH@ "
    filout=${CONFIG}_${var}_${dep}_${DATE}-${CASE}
    XYPLOT='-xyplot 0.1 0.95 0.2 0.9'
    CLRXYPAL='-clrxypal 0.1 0.95 0.05 0.15'
    if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
     rapatrie $eke $MEANY $eke
     ekeplt ; mkplt $filout
    fi
    
    puttogaya  $BASIN
  done
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 10. Mixed Layer depth in march and september NH and SH
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $mxls == 1 ] ; then

  mxlplt() { $CHART -string 0.5 0.97 1.0 0 ${CONFCASE}_${var}_${YEAR} \
             $CLRDATA -noint -ijgrid -noproj -p $PAL2 -english $CLRXYPAL \
             -clrvar $clrvar -clrmark zclrmark  -xyplot $POS -o $1 $FORMAT ;}

  listplt=' '
  # get files  MXL in m03 and m09
  mxl3=${CONFCASE}_y${YEAR}m03_MXL.nc
  mxl9=${CONFCASE}_y${YEAR}m09_MXL.nc

  # this plot have 2 frames on the panel up= march, down=september
  up='.1 .9 .55 .95'
  down=' .1 .9 .14 .54'
  CLRXYPAL=''
  min=0 ; max=1500 ; pas=500

  if [ $(chconf PERIANT) != 0 ] ; then 
   up='.1 .95 .58 .95'
   down=' .1 .95 .20 .57'
   CLRXYPAL='-clrxypal 0.1 0.95 0.05 0.15'
   min=0 ; max=700 ; pas=100 
  fi

  mklim $min $max $pas > zclrmark

  for crit in rho0.01 rho0.03 tem0.20 ; do
    var=m03-m09.MLD${crit}
    var2=MLD${crit} ; filout=${CONFIG}_${var2}_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/MXL/$filout.cgm )  == absent ] ; then
      rapatrie $mxl3 $MEANY $mxl3
      rapatrie $mxl9 $MEANY $mxl9
      case $crit in
        rho0.01 ) clrvar=somxl010 ;;
        rho0.03 ) clrvar=somxl030 ;;
        tem0.20 ) clrvar=somxlt02 ;;
      esac
      # march
      filout1=gm1 ; POS=$up  ; CLRDATA="-clrdata $mxl3"
      mxlplt $filout1
      # september
      filout2=gm2 ; POS=$down  ; CLRDATA="-clrdata $mxl9"
      mxlplt $filout2
      # merge frame
      \rm -f gmeta
      med -e 'r gm1' -e 'r gm2' -e '1,2 merge' -e '1 w gmeta'
      mkplt $filout
    fi
  done
  rm gm*

  puttogaya  MXL
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 11. Mixed Layer in the Atlantic
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $mxlatl == 1 ] ; then
  mxlpltatl() { $CHART -string 0.5 0.97 1.0 0 ${CONFCASE}_${var}_${YEAR} \
        $CLRDATA -proj ME -xstep 20 -ystep 20 -xgrid -ygrid -zoom -100 30 20 80  -p $PAL2 -english \
       -clrvar $clrvar -clrmark zclrmark  -o $1 $FORMAT ;}
#------------
   listplt=' '
  # get files  MXL in m03
  mxl3=${CONFCASE}_y${YEAR}m03_MXL.nc

 # this plot have 2 frames on the panel up= march, down=september

 min=0 ; max=2000 ; pas=500 ;  mklim $min $max $pas > zclrmark

 for crit in rho0.01 rho0.03 tem0.20 ; do
   var=m03.MLD${crit}
   var2=ATL_MLD${crit} ; filout=${CONFIG}_${var2}_${YEAR}-${CASE}
   if [ $( chkfile $PLOTDIR/ATLN/$filout.cgm )  == absent ] ; then
     rapatrie $mxl3 $MEANY $mxl3
    case $crit in
     rho0.01 ) clrvar=somxl010 ;;
     rho0.03 ) clrvar=somxl030 ;;
     tem0.20 ) clrvar=somxlt02 ;;
    esac
    # march
    filout1=gmeta ;  CLRDATA="-clrdata $mxl3"
    mxlpltatl $filout1


    mkplt $filout
   fi
 done

 puttogaya  ATLN 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 12. Air-Sea Fluxes on the Atlantic
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $flxatl == 1 ] ; then
  mxlpltatl() { $CHART -string 0.5 0.97 1.0 0 ${CONFCASE}_${var}_${YEAR} \
        $CLRDATA -proj ME -xstep 20 -ystep 20 -xgrid -ygrid -zoom -100 30 20 80  -p $PALBLUE2RED4 -english \
       -clrvar $clrvar -clrmark zclrmark  -o $1 $FORMAT ;}

   listplt=' '
  # get files gridT in m03
  flx3=${CONFCASE}_y${YEAR}m03_gridT.nc

 # this plot have 2 frames on the panel up= march, down=september

 min=-500 ; max=400 ; pas=100 ;  mklim $min $max $pas > zclrmark

 for crit in  net latent sensible longw shortw ; do
   var=m03.FLX_${crit}
   var2=ATL_FLX_${crit} ; filout=${CONFIG}_${var2}_${YEAR}-${CASE}
   if [ $( chkfile $PLOTDIR/ATLN/$filout.cgm )  == absent ] ; then
     rapatrie $flx3 $MEANY $flx3
    case $crit in
     net ) clrvar=sohefldo ;;
     latent) clrvar=solhflup ;;
     sensible ) clrvar=sosbhfup ;;
     longw ) clrvar=solwfldo ;;
     shortw ) clrvar=soshfldo ;;
    esac
    # march
    filout1=gmeta ;  CLRDATA="-clrdata $flx3"
    mxlpltatl $filout1

    mkplt $filout
   fi
 done

 puttogaya  ATLN 
                        fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 13. TRACER on the website : to be improved
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

 # generic function for global tracer plots
 glotrcplt() { $CHART -forcexy $CLRDATA -xyplot 0.1 0.95 0.4 0.9 $PAL $MEAN $FORMAT $CLRLIM $STRING \
               -lev $lev -cntlw 5:5 -clrexp $exp -clrvar $clrvar -clrxypal 0.1 0.95 0.2 0.3 \
               -english -clrnoint -ijgrid -noproj $CNTTRC -cntlis 1 -cntlls 0.01 \
               -string 0.95 0.2 1 1 "x10|S|$exp" ; }
 
 # Generic function for basin tracer plots
 bastrcplt() { $CHART -forcexy $CLRDATA $zoom $OPTIONS  \
               -clrvar $clrvar -english -clrexp $exp -string 0.95 0.15 1 1 "x10|S|$exp" \
               -lev $lev $DEP $PAL $STRING $CLRLIM $CNTTRC -cntlis 1 -cntlls 0.01  $MEAN $FORMAT; }

 # Generic function for section plots
 couptrcplt() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax -forcexy -clrvar $clrvar \
                $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
                -xyplot 0.1 0.95 0.2 0.95 -o gmeta -nolon -ylat 0.10  -english $CLRMARK $PAL $FORMAT $MEAN \
                -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${DATE}-${CASE}_DEPTH=@CLR_DEPTH@ \
                -clrexp $exp -clrxypal 0.1 0.95 0.08 0.18 -string 0.95 0.1 1 1 "x10|S|$exp" ; }

                        if [ $tracer == 1 ] ; then

 ####### Global plots ##########
 # reset the list of plots that are produced ( list is updated in mkplt )
 listplt=' '

 # Common values for this group of plots
# CLRDATA="-clrdata $trc"
 PAL="-p $PAL1"
 CLRLIM="-clrmark zclrmark"
 MEAN=""
# CNTTRC=" -cntdata $trc -cntvar $clrvar -cntlim trc.lim"
cat << eof > trc.lim
1.e07
1.e08
9.e08
eof

 for mm in 1 2 3 4 5 6 7 8 9 10 11 12 ; do
    m=$( printf "%02d" $mm )
    trc=${CONFCASE}_y${YEAR}m${m}_ptrcT.nc
    tmp=${trc#${CONFCASE}_y} ; tag=${tmp%_*}
    year=$( echo $tag | awk -Fm '{print $1}' )
    month=$( echo $tag | awk -Fm '{print $2}' )                   

    case $month in
      01 ) month_name=Jan ;;
      02 ) month_name=Feb ;;
      03 ) month_name=Mar ;;
      04 ) month_name=Apr ;;
      05 ) month_name=May ;;
      06 ) month_name=Jun ;;
      07 ) month_name=Jul ;;
      08 ) month_name=Aug ;;
      09 ) month_name=Sep ;;
      10 ) month_name=Oct ;;
      11 ) month_name=Nov ;;
      12 ) month_name=Dec ;;
    esac

    for var in CLR01 CLR02 CLR03 CLR04 CLR05 CLR06 CLR07 ; do
      case $var in
        CLR01) clrvar=clr01 ; lev=1 ; exp=7
               min=5.e07 ; max=5.e08 ; pas=1.e$exp ;;
        CLR02) clrvar=clr02 ; lev=24 ; exp=7
               min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
        CLR03) clrvar=clr03 ; lev=24 ; exp=8
               min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
        CLR04) clrvar=clr04 ; lev=30 ; exp=7
               min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
        CLR05) clrvar=clr05 ; lev=30 ; exp=8
               min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
        CLR06) clrvar=clr06 ; lev=34 ; exp=7
               min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
        CLR07) clrvar=clr07 ; lev=34 ; exp=8
               min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
      esac

      CLRDATA="-clrdata $trc"
      CNTTRC=" -cntdata $trc -cntvar $clrvar -cntlim trc.lim"
      filout=${CONFIG}_${var}_${lev}_y${YEAR}m${m}-${CASE}
      mklim $min $max $pas > zclrmark
      STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${month_name}_${YEAR}_DEPTH=@CLR_DEPTH@"
      if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent  ] ; then
        rapatrie $trc $MEANY $trc 
        glotrcplt  ; mkplt $filout
      fi

    done
 done

 puttogaya  GLOBAL

 ######## Basin plots ############
 # reset the list of plots that are produced ( list is updated in mkplt )
 listplt=' '

 # get ptrcT
 trc=${CONFCASE}_y${YEAR}_ptrcT.nc

 # Common values for this group of plots
  OPTIONS='-proj ME -xstep 10 -ystep 5'
  CLRDATA="-clrdata $trc"
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"

  for BASIN in DRAKE KERGUELEN CAMPBELL ; do
    case $BASIN in
      DRAKE)     zoom='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) zoom='-zoom 30 110 -70 -30'   ; bas=KERG ;;
      CAMPBELL)  zoom='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac
    # reset the list of plots
    listplt=' '
    for var in CLR01 CLR02 CLR03 CLR04 CLR05 CLR06 CLR07 ; do
    MEAN=""
cat << eof > trc.lim
1.e04
1.e05
1.e06
1.e07
1.e08
5.e08
9.e08
eof
    for dep in dep0 dep1 ; do
    skip=0
    case $var in
     CLR01) clrvar=clr01 ;
            case $dep in
              dep0) skip=1 ;;
              dep1) lev=1 ; exp=7 ; min=5.e07 ; max=5.e08 ; pas=3.e$exp ;;
            esac ;;
     CLR02) clrvar=clr02 ;
            case $dep in
              dep0) lev=1 ; exp=7 ; min=8.e07 ; max=3.e08 ; pas=1.e$exp ;;
              dep1) lev=24 ; exp=7 ; min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
            esac ;;
     CLR03) clrvar=clr03 ;
            case $dep in
              dep0) lev=1 ; exp=7 ; min=8.e07 ; max=3.e08 ; pas=1.e$exp ;;
              dep1) lev=24 ; exp=8 ; min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
            esac ;;
     CLR04) clrvar=clr04 ;
            case $dep in
              dep0) lev=1 ; exp=7 ; min=1.e07 ; max=1.e08 ; pas=1.e$exp ;;
              dep1) lev=30 ; exp=7 ; min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
            esac ;;
     CLR05) clrvar=clr05 ;
            case $dep in
              dep0) lev=1 ; exp=7 ; min=1.e07 ; max=1.e08 ; pas=1.e$exp ;;
              dep1) lev=30 ; exp=8 ; min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
            esac ;;
     CLR06) clrvar=clr06 ;
            case $dep in
              dep0) lev=1 ; exp=6 ; min=1.e06 ; max=1.e07 ; pas=1.e$exp ;;
              dep1) lev=34 ; exp=7 ; min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
            esac ;;
     CLR07) clrvar=clr07 ;
            case $dep in
              dep0) lev=1 ; exp=6 ; min=1.e06 ; max=1.e07 ; pas=1.e$exp ;;
              dep1) lev=34 ; exp=8 ; min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
            esac ;;
    esac

    CNTTRC=" -cntdata $trc -cntvar $clrvar -cntlim trc.lim"
    STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${DATE}_DEPTH=@CLR_DEPTH@"
    if [ $skip != 1 ] ; then
      filout=${CONFIG}_${var}_${lev}_${bas}_${DATE}-${CASE}
      mklim $min $max $pas > zclrmark
      if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent  ] ; then
        rapatrie $trc $MEANY $trc
        bastrcplt  ; mkplt $filout
      fi
    fi
   done
  done

  puttogaya  $BASIN

 done

 ####### Section plots ##########
 # reset the list of plots that are produced ( list is updated in mkplt )
 listplt='' 

 # get ptrcT
 trc=${CONFCASE}_y${YEAR}_ptrcT.nc

 # Common values for this group of plots
 CLRDATA="-clrdata $trc"
 CLRMARK="-clrmark zclrmark"
 CNTDATA=" -cntdata $trc "

 PAL="-p $PAL2"
 MEAN=""
cat << eof > trc.lim
1.e07
1.e08
9.e08
eof
 CNTLIM="-cntlim trc.lim"

 for sec in DRAK WEDD ATLS AFRS KERG AUST ADEL CAMP ROSS PACS ; do
  case $sec in
        DRAK)  LONmin=-65 ; LONmax=-65 ; SECTION=065W ;;
        WEDD)  LONmin=-42 ; LONmax=-42 ; SECTION=042W ;;
        ATLS)  LONmin=0   ; LONmax=0   ; SECTION=0E ;;
        AFRS)  LONmin=30  ; LONmax=30  ; SECTION=030E ;;
        KERG)  LONmin=70  ; LONmax=70  ; SECTION=070E ;;
        AUST)  LONmin=115 ; LONmax=115 ; SECTION=115E ;;
        ADEL)  LONmin=140 ; LONmax=140 ; SECTION=140E ;;
        CAMP)  LONmin=170 ; LONmax=170 ; SECTION=170E ;;
        ROSS)  LONmin=-170; LONmax=-170; SECTION=170W ;;
        PACS)  LONmin=-90 ; LONmax=-90 ; SECTION=090W ;;
          * )  skip=1 ;;
  esac
    for fld in CLR01 CLR02 CLR03 CLR04 CLR05 CLR06 CLR07 ; do
    skip=0
    var=${fld}
    LATmin=-70
    LATmax=-30
    case $fld in
      CLR01) clrvar=clr01 ; lev=1
             filout=${CONFIG}_${var}_${lev}_${sec}_${DATE}-${CASE} ;
             exp=7
             min=8.e07 ; max=3.e08 ; pas=1.e$exp ;;
      CLR02) clrvar=clr02 ; lev=24
             filout=${CONFIG}_${var}_${lev}_${sec}_${DATE}-${CASE}
             exp=7
             min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR03) clrvar=clr03 ; lev=24
             filout=${CONFIG}_${var}_${lev}_${sec}_${DATE}-${CASE}
             exp=8
             min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR04) clrvar=clr04 ; lev=30
             filout=${CONFIG}_${var}_${lev}_${sec}_${DATE}-${CASE}
             exp=7
             min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR05) clrvar=clr05 ; lev=30
             filout=${CONFIG}_${var}_${lev}_${sec}_${DATE}-${CASE}
             exp=8
             min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR06) clrvar=clr06 ; lev=34
             filout=${CONFIG}_${var}_${lev}_${sec}_${DATE}-${CASE}
             exp=7
             min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR07) clrvar=clr07 ; lev=34
             filout=${CONFIG}_${var}_${lev}_${sec}_${DATE}-${CASE}
             exp=8
             min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
    esac 
    if [ $skip != 1 ] ; then
#      filout=${CONFIG}_${var}_${lev}_${DATE}-${CASE}
      mklim $min $max $pas > zclrmark
      couptrcplt ; mkplt $filout
      rm gmeta* 
    fi
    done     
 done        

 puttogaya  SECTIONS 

                             fi

#------------------------------------------------------------------------------
# 14. MOCSIG = OVT in sigma coordinates on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                       if [ $mocsig  == 1 ] ; then

  #Choose reference depth
  NREF=2

  # generic function for moc plot
  mocsigplt() { $COUPE -clrdata $mocf  -clrmodif 'deptht=sigma' \
               -clrmet 1 -clrmin $clrmin -clrmax $clrmax  -p $PALBLUE2RED4\
               -clrvar $clrvar  -format PALETTE I3 -noteam -pmin $min -pmax $max \
               -cntdata $mocf  -cntmodif 'deptht=sigma' -cntmin -20 -cntint 2.5 -cntmax 20 \
               -cntvar $clrvar  -zstep 1 -zgrid -ystep 5 -ygrid \
               -cntdash -cntlls 0.015 -cntrc1 0.05 -cntrc2 0.2 -cntlis 2 -cntlw 1:3 \
               $PTS $STRING1 $STRING2 ;}

  # get files
  mocf=${CONFCASE}_y${YEAR}_MOCSIG_${NREF}.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # set common min and max (sv) for plots
  if [ $NREF == 2 ] ; then
    clrmin=-10  ;  clrmax=10
    min=-32 ; max=-37.2
  fi

  # Loop for all basins
  for bas in AUSTRAL ; do
    clrvar=zomsfglo
    case $bas in
      AUSTRAL) PTS='-pts 0 0 -70 -30' ;
               STRING1="-string 0.5 0.98 1.3 0 MOCSIG_AUSTRAL_(sv)_${CONFCASE} " ;
               STRING2="-string 0.5 0.93 1.3 0 y${YEAR}_SIGMA_${NREF} " ;;
    esac

    filout=${CONFIG}_OVTSIG_${NREF}_${bas}_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/OVT/$filout.cgm) == absent ] ; then
     rapatrie $mocf $MEANY $mocf
     mocsigplt ; mkplt $filout
    fi
  done

  puttogaya  OVT
                       fi


#------------------------------------------------------------------------------
# 15. CFC = CFC plots on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $cfc == 1 ] ; then

  # generic function for moc plot
  cfcgloplt() { $CHART -forcexy -clrdata $file -clrvar $vari -scale $scale -clrmark zclrmark  \
                -clrmet 1 -clrmin $min -clrmax $max -p $PAL1 -format PALETTE I3 $OPTION $STRING -clrexp $clrexp ;
              }

  # get files
  pendep=${CONFCASE}_y${YEAR}_pendep.nc
  fracinv=${CONFCASE}_y${YEAR}_fracinv.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Set options
  for var in  PEN FRAC ; do
    case $var in
      PEN) vari=pendep ; file=$pendep ; source=$MEANY
           min=0 ; max=1500 ; pas=200
           scale=1 ; clrexp=0 ;;
      FRAC) vari=fracinv ; file=$fracinv ; source=$MEANY
           min=2 ; max=28 ; pas=2
           scale=1.e+06 ; clrexp=0 ;;
    esac
    filout=${CONFIG}_${vari}_global_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/CFC/$filout.cgm) == absent ] ; then
     rapatrie $file $source $file
     STRING="-string 0.5 0.97 1.0 0 ${CONFIG}_${CASE}_${vari}_(10_exp_${clrexp}_picomol)${DATE}"
     mklim $min $max $pas > zclrmark
     cfcgloplt ; mkplt $filout
    fi
  done

  puttogaya  CFC

                               fi

#------------------------------------------------------------------------------
# 16. PISCES = PISCES plots on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_global == 1 ] ; then

  # generic function for moc plot
  glopiplt() { set -x ; $CHART -forcexy $CLRDATA -scale $scale   \
               -clrvar $clrvar -english  \
               $LEV $DEP $PAL $STRING $CLRLIM  $MEAN $FORMAT; }

  # get files
  t=${CONFCASE}_y${YEAR}_ptrcT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""
  CLRDATA="-clrdata $t"

  # Set options
  for var in DIC TALK O2 PO4 Si NO3 Fer DOC ; do
    for dep in 0 150  ; do
      DEP="-dep $dep"
      filout=${CONFIG}_${var}_${dep}_${DATE}-${CASE}
      case $var in
        DIC) clrvar=DIC ; unit="mol-C/L"
          case $dep in
            0)  min=2000 ; max=2200  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
          150)  min=2000 ; max=2200  ; pas=50 ; scale=1.e6 ;;
          esac ;;
       TALK) clrvar=Alkalini ; unit="mol-Alk/L"
          case $dep in
            0)  min=2250 ; max=2400  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
          150)  min=2250 ; max=2400  ; pas=50 ; scale=1.e6 ;;
          esac ;;
         O2) clrvar=O2 ; unit="mol-O2/L"
          case $dep in
            0)  min=200 ; max=350  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
          150)  min=0 ; max=300  ; pas=100 ; scale=1.e6 ;;
          esac ;;
        PO4) clrvar=PO4 ; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=250  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
          150)  min=0 ; max=250  ; pas=50 ; scale=1.e6 ;;
          esac ;;
         Si) clrvar=Si ; unit="mol-Si/L"
          case $dep in
            0)  min=0 ; max=50  ; pas=10 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
          150)  min=0 ; max=100  ; pas=10 ; scale=1.e6 ;;
          esac ;;
        NO3) clrvar=NO3 ; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=220  ; pas=100 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
          150)  min=0 ; max=260  ; pas=100 ; scale=1.e6 ;;
          esac ;;
        Fer) clrvar=Fer ; unit="mol-Fe/L"
          case $dep in
            0)  min=0 ; max=5  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e10 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e10 ;;
          esac ;;
        DOC) clrvar=DOC ; unit="mol-C/L"
          case $dep in
            0)  min=4 ; max=30  ; pas=2 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
          150)  min=4 ; max=26  ; pas=2 ; scale=1.e6 ;;
          esac ;;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${DATE}_DEPTH=@CLR_DEPTH@"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
    done
  done

  puttogaya PISCES/GLOBAL

                             fi

#------------------------------------------------------------------------------
# 17. PISCES_OBS = PISCES comparison to climatologies on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_clim == 1 ] ; then

  # generic function for moc plot
  gloclimplt() { set -x ; $CHART -forcexy $CLRDATA -scale $scale   \
                 -clrvar $clrvar -english -noint -ijgrid -noproj $CLRXYPAL \
                 $LEV -p $PAL2 $STRING -clrmark zclrmark  -xyplot $POS -o $1 $MEAN $FORMAT; }

  # get files
  filerun=${CONFCASE}_y${YEAR}_ptrcT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  up='.1 .95 .58 .95'
  down=' .1 .95 .20 .57'
  CLRXYPAL='-clrxypal 0.1 0.95 0.05 0.15'

  # Set options
  for var in DIC TALK O2 PO4 Si NO3 ; do
    filout=${CONFIG}_diffobs_${var}_${YEAR}-${CASE}
    case $var in
      DIC)  clrvar=DIC ; unit="mol-C/L" ; source=GLODAP
            min=2000 ; max=2200  ; pas=50 ; LEV="-lev 1" ; scale=1.e6  ;;
     TALK)  clrvar=Alkalini ; unit="mol-Alk/L" ; source=GLODAP
            min=2250 ; max=2400  ; pas=30 ; LEV="-lev 1" ; scale=1.e6 ;;
       O2)  clrvar=O2 ; unit="mol-O2/L" ; source=WOA01
            min=200 ; max=350  ; pas=50 ; LEV="-lev 1" ; scale=1.e6 ;;
      PO4)  clrvar=PO4 ; unit="mol-C/L" ; source=WOA01
            min=0 ; max=250  ; pas=50 ; LEV="-lev 1" ; scale=1.e6 ;;
       Si)  clrvar=Si ; unit="mol-Si/L" ; source=WOA01
            min=0 ; max=50  ; pas=10 ; LEV="-lev 1" ; scale=1.e6 ;;
      NO3)  clrvar=NO3 ; unit="mol-C/L" ; source=WOA01
            min=0 ; max=220  ; pas=100 ; LEV="-lev 1" ; scale=1.e6 ;
    esac
    if [ $( chkfile $PLOTDIR/PISCES/CLIM/$filout.cgm) == absent ] ; then
      fileobs=${clrvar}_ORCA05-${CONFIG}_${source}_masked_cunit.nc
      rapatrie $filerun $MEANY $filerun
      rapatrie $fileobs $CLIMY $fileobs
      mklim $min $max $pas > zclrmark
      # obs
      filout1=gm1 ; POS=$up  ; CLRDATA="-clrdata $fileobs"
      STRING=""
      gloclimplt $filout1
      # run
      filout2=gm2 ; POS=$down ; CLRDATA="-clrdata $filerun"
      STRING="-string 0.5 0.97 1.0 0 ${CONFCASE}_${source}_${var}_${unit}*${scale}_${YEAR}"
      gloclimplt $filout2
      # merge frame
      \rm -f gmeta
      med -e 'r gm1' -e 'r gm2' -e '1,2 merge' -e '1 w gmeta'
      mkplt $filout
    fi
  done

  puttogaya PISCES/CLIM

                           fi

#------------------------------------------------------------------------------
# 18. MXL_OBS = MXL comparison to Boyer-Montegut climatology on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $mxls_clim == 1 ] ; then

  # generic function for moc plot
  mxlplt() { $CHART -string 0.5 0.97 1.0 0 ${CONFCASE}_${var}_${month}_${YEAR} \
             $CLRDATA -noint -ijgrid -noproj -p $PAL2 -english $CLRXYPAL \
             -clrvar $clrvar -clrmark zclrmark  -xyplot $POS -o $1 $FORMAT ;}


  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  up='.1 .95 .58 .95'
  down=' .1 .95 .20 .57'
  CLRXYPAL='-clrxypal 0.1 0.95 0.05 0.15'

  # Set options
  for mm in 03 09  ; do
    mxl_run=${CONFCASE}_y${YEAR}m${mm}_MXL.nc
    mxl_obs=somxlt02_BM-${CONFIG}_m${mm}_masked.nc
    var=MLDtem0.20
    filout=${CONFIG}_dif${var}_m${mm}_${YEAR}-${CASE}
    clrvar=somxlt02
    case $mm in
      03) min=0 ; max=500 ; pas=100 ; month='March' ;;
      09) min=0 ; max=800 ; pas=100 ; month='September' ;;
    esac
    if [ $( chkfile $PLOTDIR/MXL/$filout.cgm) == absent ] ; then
      rapatrie $mxl_run $MEANY $mxl_run
      rapatrie $mxl_obs $CLIMY $mxl_obs
      mklim $min $max $pas > zclrmark
      # obs
      filout1=gm1 ; POS=$up  ; CLRDATA="-clrdata $mxl_obs"
      mxlplt $filout1
      # run
      filout2=gm2 ; POS=$down  ; CLRDATA="-clrdata $mxl_run"
      mxlplt $filout2
      # merge frame
      \rm -f gmeta
      med -e 'r gm1' -e 'r gm2' -e '1,2 merge' -e '1 w gmeta'
      mkplt $filout
    fi
   done

   puttogaya MXL

                             fi


#------------------------------------------------------------------------------
# 19. PISCES coupes australes on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_coupes_aus == 1 ] ; then

  # Generic function for coupes_aus plot
  # Top panel (0<depth<1000m zoom)
  coupitop() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -1000 -clrvar $clrvar \
               $CNTDATA -cntvar $clrvar $CNTLIM -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
               -xyplot 0.1 0.95 0.70 0.95 -o gmetatop -nolon -ylat 0.10 -english $CLRMARK $PAL -scale $scale \
               -clrxypal 0.1 0.95 0.08 0.18 -string 0.5 0.98 1.0 0 ${CONFIG}-${CASE}_${var}_${unit}*${scale}_${SECTION}_${DATE} ; }
  # Bottom panel (0<depth<1000m)
  coupibot() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -5500 -clrvar $clrvar \
               $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
               -xyplot 0.1 0.95 0.20 0.68 -o gmetabot -nolon -ylat 0.10  -english $CLRMARK $PAL \
               -clrxypal 0.1 0.95 0.08 0.18 -zstep 500 -scale $scale ; } 


  # get files
  t=${CONFCASE}_y${YEAR}_ptrcT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  PAL="-p $PAL2"
  LATmin=-70 ; LATmax=-30
  LEV=''

  # Set options
  for sec in DRAK WEDD ATLS AFRS KERG AUST ADEL CAMP ROSS PACS ; do
   for fld in DIC TALK O2 PO4 Si NO3 Fer DOC ; do
     skip=0
     var=${fld}_${sec}
     case $fld in
         DIC) clrvar=DIC ; unit="mol-C/L" ; CLRDATA="-clrdata $t" ; scale=1.e6 ;
cat << eof > DIC.lim
2100
2200
2300
eof
cat << eof > DIC.mark
2050
2150
2250
2350
eof
              CLRMARK="-clrmark DIC.mark" ; CNTLIM="-cntlim DIC.lim" ; CNTDATA="-cntdata $t" ;;
        TALK) clrvar=Alkalini ; unit="mol-Alk/L" ;  CLRDATA="-clrdata $t" ; scale=1.e6 ;
cat << eof > TALK.lim
2300
2400
eof
cat << eof > TALK.mark
2300
2350
2400
2450
2500
eof
              CLRMARK="-clrmark TALK.mark" ; CNTLIM="-cntlim TALK.lim" ; CNTDATA="-cntdata $t" ;;
          O2) clrvar=O2 ;  unit="mol-O2/L" ; CLRDATA="-clrdata $t" ; scale=1.e6 ;
cat << eof > O2.lim
150
250
350
eof
cat << eof > O2.mark
150
200
300
400
eof
              CLRMARK="-clrmark O2.mark" ; CNTLIM="-cntlim O2.lim" ; CNTDATA="-cntdata $t" ;;
         PO4) clrvar=PO4;  unit="mol-C/L" ;  CLRDATA="-clrdata $t" ; scale=1.e6 ;
cat << eof > PO4.lim
100
200
300
eof
cat << eof > PO4.mark
0
50
100
150
200
250
300
eof
              CLRMARK="-clrmark PO4.mark" ; CNTLIM="-cntlim PO4.lim" ; CNTDATA="-cntdata $t" ;;
          Si) clrvar=Si; unit="mol-Si/L" ; CLRDATA="-clrdata $t" ; scale=1.e6 ;
cat << eof > Si.lim
50
100
eof
cat << eof > Si.mark
0
50
100
130
eof
              CLRMARK="-clrmark Si.mark" ; CNTLIM="-cntlim Si.lim" ; CNTDATA="-cntdata $t" ;;
         NO3) clrvar=NO3; unit="mol-C/L" ; CLRDATA="-clrdata $t" ; scale=1.e6 ;
cat << eof > NO3.lim
100
200
eof
cat << eof > NO3.mark
0
50
100
150
200
250
eof
              CLRMARK="-clrmark NO3.mark" ; CNTLIM="-cntlim NO3.lim" ; CNTDATA="-cntdata $t" ;;
         Fer) clrvar=Fer; unit="mol-Fe/L" ; CLRDATA="-clrdata $t" ; scale=1.e10 ;
cat << eof > Fer.lim
2
4
6
8
eof
cat << eof > Fer.mark
0
1
2
3
4
5
6
7
8
eof
              CLRMARK="-clrmark Fer.mark" ; CNTLIM="-cntlim Fer.lim" ; CNTDATA="-cntdata $t" ;;
         DOC) clrvar=DOC; unit="mol-C/L" ; CLRDATA="-clrdata $t" ; scale=1.e6 ;
cat << eof > DOC.lim
2
5
10
15
eof
cat << eof > DOC.mark
0
5
10
15
20
eof
              CLRMARK="-clrmark DOC.mark" ; CNTLIM="-cntlim DOC.lim" ; CNTDATA="-cntdata $t" ;;
     esac
     case $sec in
        DRAK)  LONmin=-65 ; LONmax=-65 ; SECTION=065W ;;
        WEDD)  LONmin=-42 ; LONmax=-42 ; SECTION=042W ;;
        ATLS)  LONmin=0   ; LONmax=0   ; SECTION=0E ;;
        AFRS)  LONmin=30  ; LONmax=30  ; SECTION=030E ;;
        KERG)  LONmin=70  ; LONmax=70  ; SECTION=070E ;;
        AUST)  LONmin=115 ; LONmax=115 ; SECTION=115E ;;
        ADEL)  LONmin=140 ; LONmax=140 ; SECTION=140E ;;
        CAMP)  LONmin=170 ; LONmax=170 ; SECTION=170E ;;
        ROSS)  LONmin=-170; LONmax=-170; SECTION=170W ;;
        PACS)  LONmin=-90 ; LONmax=-90 ; SECTION=090W ;;
          * )  skip=1 ;;
     esac
     if [ $( chkfile $PLOTDIR/PISCES/SECTIONS/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        coupitop ; coupibot
        med -e 'r gmetatop' -e 'r gmetabot' -e '1,2 merge' -e '1 w gmeta'
        filout=${CONFIG}_${var}_${DATE}-${CASE}
        mkplt $filout
        rm gmeta*
     fi
    done
  done

  puttogaya PISCES/SECTIONS

                             fi

#------------------------------------------------------------------------------
# 20. PISCES coupes australes compared to climatologies on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_coupes_clim == 1 ] ; then

  # Generic function for coupes_aus plot
  coupicomp() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -clrvar $clrvar \
                $CNTDATA -cntvar $clrvar $CNTLIM -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
                -xyplot $POS -o $1 -english $CLRMARK $PAL -scale $scale \
                -nolon $CLRXYPAL $STRING -pmax -5500 ; }


  # get files
  filerun=${CONFCASE}_y${YEAR}_ptrcT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  up='.1 .95 .58 .95'
  down=' .1 .95 .20 .57'
  CLRXYPAL='-clrxypal 0.1 0.95 0.05 0.15'
  PAL="-p $PAL2"
  LATmin=-70 ; LATmax=-30
  LEV=''

  # Set options
  for sec in ATLS PACS ; do
   for fld in DIC TALK O2 PO4 Si NO3 ; do
     skip=0
     var=${fld}_${sec}
     filout=${CONFIG}_diffobs_${var}_${YEAR}-${CASE}
     case $fld in
         DIC) clrvar=DIC ; unit="mol-C/L" ; source=GLODAP ; scale=1.e6 ;
cat << eof > DIC.lim
2100
2200
2300
eof
cat << eof > DIC.mark
2050
2150
2250
2350
eof
              CLRMARK="-clrmark DIC.mark" ; CNTLIM="-cntlim DIC.lim"  ;;
        TALK) clrvar=Alkalini ; unit="mol-Alk/L" ; source=GLODAP ; scale=1.e6 ;
cat << eof > TALK.lim
2300
2400
eof
cat << eof > TALK.mark
2300
2350
2400
2450
2500
eof
              CLRMARK="-clrmark TALK.mark" ; CNTLIM="-cntlim TALK.lim" ;;
          O2) clrvar=O2 ;  unit="mol-O2/L" ; source=WOA01 ; scale=1.e6 ;
cat << eof > O2.lim
150
250
350
eof
cat << eof > O2.mark
150
200
300
400
eof
              CLRMARK="-clrmark O2.mark" ; CNTLIM="-cntlim O2.lim" ;;
         PO4) clrvar=PO4;  unit="mol-C/L" ;  source=WOA01 ; scale=1.e6 ;
cat << eof > PO4.lim
100
200
300
eof
cat << eof > PO4.mark
0
50
100
150
200
250
300
eof
              CLRMARK="-clrmark PO4.mark" ; CNTLIM="-cntlim PO4.lim"  ;;
          Si) clrvar=Si; unit="mol-Si/L" ; source=WOA01 ; scale=1.e6 ;
cat << eof > Si.lim
50
100
eof
cat << eof > Si.mark
0
50
100
130
eof
              CLRMARK="-clrmark Si.mark" ; CNTLIM="-cntlim Si.lim" ;;
         NO3) clrvar=NO3; unit="mol-C/L" ; source=WOA01 ; scale=1.e6 ;
cat << eof > NO3.lim
100
200
eof
cat << eof > NO3.mark
0
50
100
150
200
250
eof
              CLRMARK="-clrmark NO3.mark" ; CNTLIM="-cntlim NO3.lim" ;;
     esac
     case $sec in
        DRAK)  LONmin=-65 ; LONmax=-65 ; SECTION=065W ;;
        WEDD)  LONmin=-42 ; LONmax=-42 ; SECTION=042W ;;
        ATLS)  LONmin=0   ; LONmax=0   ; SECTION=0E ;;
        AFRS)  LONmin=30  ; LONmax=30  ; SECTION=030E ;;
        KERG)  LONmin=70  ; LONmax=70  ; SECTION=070E ;;
        AUST)  LONmin=115 ; LONmax=115 ; SECTION=115E ;;
        ADEL)  LONmin=140 ; LONmax=140 ; SECTION=140E ;;
        CAMP)  LONmin=170 ; LONmax=170 ; SECTION=170E ;;
        ROSS)  LONmin=-170; LONmax=-170; SECTION=170W ;;
        PACS)  LONmin=-90 ; LONmax=-90 ; SECTION=090W ;;
          * )  skip=1 ;;
     esac
     if [ $( chkfile $PLOTDIR/PISCES/CLIM/$filout.cgm) == absent ] ; then
       fileobs=${clrvar}_ORCA05-${CONFIG}_${source}_masked_cunit.nc
       rapatrie $filerun $MEANY $filerun
       rapatrie $fileobs $CLIMY $fileobs
       # obs
       filout1=gm1 ; POS=$up  ; CLRDATA="-clrdata $fileobs" ; CNTDATA="-cntdata $fileobs"
       STRING=""
       coupicomp $filout1
       # run
       filout2=gm2 ; POS=$down ; CLRDATA="-clrdata $filerun" ; CNTDATA="-cntdata $filerun"
       STRING="-string 0.5 0.97 1.0 0 ${CONFCASE}_${source}_${var}_${unit}*${scale}_${SECTION}_${YEAR}"
       coupicomp $filout2
       # merge frame
       \rm -f gmeta
       med -e 'r gm1' -e 'r gm2' -e '1,2 merge' -e '1 w gmeta'
       mkplt $filout
     fi
    done
  done
   
  puttogaya PISCES/CLIM

                           fi

#------------------------------------------------------------------------------
# 21. PISCES fluxes on website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_fluxes == 1 ] ; then

  # Generic function   
  glopiplt() { set -x ; $CHART -forcexy $CLRDATA \
           -clrvar $clrvar   -english -scale $scale  \
            $LEV $DEP $PAL $STRING $CLRLIM $MEAN $FORMAT; }

  # get files
  t=${CONFCASE}_y${YEAR}_diadT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  CLRLIM="-clrmark zclrmark"
  CLRDATA="-clrdata $t"
  MEAN="" ; DEP="" ; LEV="" ; FORMAT='-format PALETTE I4'

  # Set options
  for var in DICFlx O2Flx DeltaCO2 PPdiatoms PPnano ; do
      filout=${CONFIG}_${var}_${dep}_${DATE}-${CASE}
      filout=${CONFIG}_${var}_${DATE}-${CASE}
      case $var in
          DICFlx) clrvar=Cflx ; unit="molC/m2/s" ;
                  scale=1.e8 ; min=-7 ; max=6  ; pas=1 ; PAL="-p $PALBLUE2RED3" ;;
           O2Flx) clrvar=Oflx ; unit="molC/m2/s" ;
                  scale=1.e8 ; min=-24 ; max=22  ; pas=2 ; PAL="-p $PALBLUE2RED3" ;;
        DeltaCO2) clrvar=Delc ; unit="uatm" ;
                  scale=1    ; min=-50 ; max=45  ; pas=5 ; PAL="-p $PALBLUE2RED3" ;;
       PPdiatoms) clrvar=PPPHY2 ; unit="molC/m3/s" ;
                  scale=1.e10   ; min=0 ; max=40  ; pas=5 ; PAL="-p $PAL2";;
          PPnano) clrvar=PPPHY2 ; unit="molC/m3/s" ;
                  scale=1.e10   ; min=0 ; max=40  ; pas=10 ; PAL="-p $PAL2";;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${DATE}"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
  done

  puttogaya PISCES/GLOBAL

                             fi

#==============================================================================
#==============================================================================
#==============================================================================
#==============================================================================
