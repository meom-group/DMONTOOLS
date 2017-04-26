#!/bin/ksh
set -x
#-------------------------------------------------------------------------------
#  $Date$
#  $Rev$
#  $Id$
#  $History: (02/2009) adapted to PERIANT configuration by C. Dufour $
#-------------------------------------------------------------------------------

if [ $# = 0 ] ; then
   echo 'USAGE: plot_monitor.ksh year'
   exit 1
fi

# define some config dependent variable
. ./config_def.ksh

#  Define some functions to get/put file from/to ergon (can be easily customized)
. ./function_def.ksh
#-----------------------------------------------------------------------------
# take YEAR as the argument to this script
# YEAR can then be used as directory name for files YEARDIR=${XIOS}/$YEAR e.g.: 5d/2000
# YEAR can also be used as a date and for annual mean YEARDAT=$YEAR${xiosid} e.g. 1967.5d
#                                     for monthly mean ${YEAR}m03${xiosid}
YEAR=$1
YEARDAT=$YEAR${xiosid}

# check if computing plots for a climatology or for a sequence of years
single=$( echo $YEAR | awk '{ print index($1,"-") }' )
if [ $single == 0 ] ; then
  clim=0
else
  clim=1
fi

#-----------------------------------------------------------------------------

# PALDIR hold color palettes for these plots
# it is distributed with PLOT_2D scripts, and copied there by the main script
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

    # puttoarch : used to dispose the plots in $listplt on PLOTDIR/specific
    puttoarch() {
       chkdirg  $PLOTDIR ; chkdirg  $PLOTDIR/$1 ; chkdirg  $PLOTDIR/$1/GIFS
       for f in  $listplt  ; do
            for ftyp in cgm  ; do expatrie $f.$ftyp $PLOTDIR/$1  $f.$ftyp  ; done
            for ftyp in $gif  ; do expatrie $f.$ftyp $PLOTDIR/$1/GIFS  $f.$ftyp  ; done
       done ;
                }
    # puttoarchtrc : used to dispose the plots in $listplt on PLOTDIR/TRACER/specific
    puttoarchtrc() {
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
               ctrans -device sun -res 1024x1024 $1.cgm > $1.sun ;
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

# function to decode zoom flag: key letters are D (Drake) K (Kerguelen) C (Campbell) A (Atlantic) or 0 
#            zoom flag is for instance zoomEKE='Dr' or zoomEKE=Ke.Dr or ...
getlist() {

    zoom=$1
    typeset -A zbas
    zbas[Dr]='DRAKE'
    zbas[Ke]='KERGUELEN'
    zbas[Cb]='CAMPBELL'
    zbas[At]='ATLANTIC'
    zbas[Na]='ATLN'
    zbas[Sa]='ATLS'
    list_bas=''

    nz=${#zoom}
    if [ $nz = 0 ] ; then
      echo nothing to do 
    else
      for i in $(seq 0 3 $((nz-1)) ) ; do
        zchk=${zoom:$i:2}
        list_bas="$list_bas ${zbas[$zchk]}"
      done
    fi
    echo  $list_bas
            }


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
 PALBLUE2RED5=ferretblue2red_withland_contour.pal
 PALBLUE2WHITE=globvit_withland_contour.pal

#------------------------------------------------------------------------------
# DIRECTORY NAMES FREQUENTLY USED
#------------------------------------------------------------------------------
 MEANY=$CONFIG/${CONFCASE}-MEAN/$XIOS/$YEAR
 SDIRY=$CONFIG/${CONFCASE}-S/$XIOS/$YEAR
 DIAGS=${CONFIG}/${CONFCASE}-DIAGS
 IDIR=$CONFIG/${CONFIG}-I
 if [ $clim == 0 ] ; then
   PLOTDIR=${CONFIG}/PLOTS/${CONFCASE}
 else
   PLOTDIR=${CONFIG}/CLIM_PLOTS/${CONFCASE}
 fi
 CLIMY=${IDIR}/CLIM_PLOT

 # check for existence
 chkdirg ${CONFIG}/PLOTS/
 chkdirg ${CONFIG}/PLOTS/${CONFCASE} # PLOTDIR
 chkdirg ${CONFIG}/CLIM_PLOTS/
 chkdirg ${CONFIG}/CLIM_PLOTS/${CONFCASE}

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
  mocf=${CONFCASE}_y${YEARDAT}_MOC.nc

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

  puttoarch  OVT 
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
 t=${CONFCASE}_y${YEARDAT}_gridT.nc
 ice=${CONFCASE}_y${YEARDAT}_icemod.nc

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
 var=SSHGLp ;  STRING=" -string $xstring $ystring 1.0 0 ${CONFCASE}_${var}_${YEAR}_DEPTH=@CLR_DEPTH@" ;
 MEAN="-mean 0" ; DEP="" ; LEV="-lev 1" ; dep=0  ;
 min=-2 ; max=1.5  ; pas=.5  # ORCA Default
 if [ $(chconf PERIANT) != 0 ] ; then min=-1.2 ; max=1.2  ; pas=.4  ; fi
 mklim $min $max $pas > zclrmark
 filout=${CONFIG}_${var}_${dep}_${YEAR}-${CASE}

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
     filout=${CONFIG}_${var}_${dep}_${YEAR}-${CASE}
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
       STRING=" -string $xstring $ystring 1.0 0 ${CONFCASE}_${var}_${YEAR}_DEPTH=@CLR_DEPTH@"
       gloplt  ; mkplt $filout
     fi
   done
 done

 puttoarch  GLOBAL
                       fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Difference between model and Levitus at different levels ( In GLOBAL)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                       if [ $glodifmap == 1 ] ; then
  # mkdiff : Usage mkdiff file1 file2 cdfvar level outfile
  mkdiff() { \rm -f $5
            ncks  -F -O -v $3,nav_lon,nav_lat -d deptht,$4,$4 $2  zz2.nc
            ncks  -F -O -v $3,nav_lon,nav_lat -d deptht,$4,$4 $1  zz1.nc
#           ncatted -O -a missing_value,,d,, zz2.nc
            ncatted -O -a _FillValue,,c,f,0. zz2.nc
#           ncatted -O -a missing_value,,d,, zz1.nc
            ncatted -O -a _FillValue,,c,f,0. zz1.nc
            ncbo --op_typ=- -v $3 zz1.nc zz2.nc $5
            ncks  -F -A -v nav_lon,nav_lat,deptht,time_counter -d deptht,$4,$4 $1 $5 ;}


  # mkdiffrey : Usage mkdiffrey file1 file2 cdfvar outfile
  mkdiffrey() { \rm -f $4
            ncks  -F -O -v $3,nav_lon,nav_lat  $2  zz2.nc
            ncks  -F -O -v $3,nav_lon,nav_lat -d deptht,1,1 $1  zz1.nc
            ncatted -O -a missing_value,,d,, zz2.nc  
            ncatted -O -a missing_value,,d,, zz1.nc
            ncatted -O -a _FillValue,,c,f,0. zz1.nc
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
  t=${CONFCASE}_y${YEARDAT}_gridT.nc
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
       STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}_SST_DEPTH=@CLR_DEPTH@"
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
        STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}_DEPTH=@CLR_DEPTH@"
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

    # dispose to archive all plots in the listplt
  puttoarch  GLOBAL 
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
  if [ $XIOS ] ; then
    t=${CONFCASE}_y${YEARDAT}_flxT.nc
  else
    t=${CONFCASE}_y${YEARDAT}_gridT.nc
  fi

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # plot the heat flux (w/m2) and fresh water flux (mm/day) (scaled by 86400 from kg/m2/s)
  MEAN="" ; DEP="" ; LEV="" ; PAL="-p $PALBLUE2RED3" ; CNTICE="" ; FORMAT='-format PALETTE I4'
#   for var in HeatFlx WaterFlx WaterDmp CDWaterFlx  ; do
    for var in HeatFlx WaterFlx WaterDmp   ; do
      STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}_DEPTH=@CLR_DEPTH@"
      filout=${CONFIG}_${var}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
         rapatrie $t    $MEANY $t
         case $var in
           HeatFlx) clrvar=sohefldo ; CLRDATA=" -clrdata $t"; min=-140 ; max=140  ; pas=15 ;;
           WaterFlx) clrvar=sowaflup ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
           WaterDmp) clrvar=sowafldp ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
           CDWaterFlx) clrvar=sowaflcd ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
         esac
         CLRLIM="-clrmin $min -clrmax $max -clrmet 1"
         gloplt  ; mkplt $filout
      fi
    done

    # dispose to archive all plots in the listplt
    puttoarch  GLOBAL 
                         fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.2 Hallberg Salinity Damping : HSD (flx (mm/day) and trend ??)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                if [ $hsd == 1 ] ; then

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
  t=${CONFCASE}_y${YEARDAT}_gridT.nc
  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # plot the corrective Hallberg flux (scaled by 86400 from kg/m2/S) and SSS Damping trends (PSU/day) (scaled by 86400 from PSU/s)
  MEAN="" ; DEP="" ; LEV="" ; PAL="-p $PALBLUE2RED3" ; CNTICE="" ; FORMAT='-format PALETTE I4'
    for var in HsdFlx HsdTrd ; do
      STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}_DEPTH=@CLR_DEPTH@"
      filout=${CONFIG}_${var}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
         rapatrie $t    $MEANY $t
         case $var in
           HsdFlx) clrvar=sohsdflxc ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
           HsdTrd) clrvar=sohsdtrdc ; CLRDATA=" -clrdata $t -scale 345600."; min=-7 ; max=7  ; pas=2 ;
         esac
         CLRLIM="-clrmin $min -clrmax $max -clrmet 1"
         gloplt  ; mkplt $filout
      fi
    done

    # dispose to archive all plots in the listplt
    puttoarch  GLOBAL
                         fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. Local details of the TS fields
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # Generic function for Basin plot
  basplt() { $CHART $CLRDATA -forcexy $ZOOM $OPTIONS  \
             -clrvar $clrvar   -english \
              $LEV $DEP $PAL $STRING $CLRLIM $CNTICE  $MEAN $FORMAT; }
#---

# 5.1 Some details focused on the Atlantic
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 5.1.1 ; Temperature and Salinity
#---------------------------------
#  now done with zoomTS=Na.Sa

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.1.2 : Barotropic Stream Function
#------------------------------------
#  now done with zoomUV=Na.Sa


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.1.3 : Bottom sigma-4
#------------------------------------
                        if [ $botsig4atl == 1 ] ; then
    t=${CONFCASE}_y${YEARDAT}_gridT.nc
    listplt=' '

    var=botsigma4 ;STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}"
    filout=${CONFIG}_${var}_${YEAR}-${CASE}
    ZOOM="-zoom -65 20 45 75"
    if [ $( chkfile $PLOTDIR/ATLN/$filout.cgm ) == absent  ] ; then
       rapatrie $t $MEANY $t
       cdfbottomsig -t $t -r 4000
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

    puttoarch  ATLN
                        fi

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2 : Some details focused on some regions ( Drake, Kerguelen, Campbell, 
#        North-Atlantic, South-Atlantic
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 5.2.1 : Temperature and Salinity
#---------------------------------
                        if [ $zoomTS != 0 ] ; then
  list_bas=$( getlist $zoomTS )

  # get files  gridT, icemod
  t=${CONFCASE}_y${YEARDAT}_gridT.nc
  ice=${CONFCASE}_y${YEARDAT}_icemod.nc

  for BASIN in $list_bas ; do
    case $BASIN in
#                      Regional limit            basin ID       Limit for SSHp plot
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ; min=-1.2 ; max=1.2  ; pas=.2  ;;
      KERGUELEN) ZOOM='-zoom   30 110 -70 -30' ; bas=KERG ; min=-1.2 ; max=1.2  ; pas=.2  ;;
       CAMPBELL) ZOOM='-zoom  100 180 -70 -30' ; bas=CAMP ; min=-1.2 ; max=1.2  ; pas=.2  ;;
           ATLN) ZOOM='-zoom -100  20  -5  70' ; bas=ATLN ; min=-1.5 ; max=.75  ; pas=.25 ;;
           ATLS) ZOOM='-zoom  -70  30 -70   5' ; bas=ATLS ; min=-1.5 ; max=.75  ; pas=.25 ;;
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
  var=SSHp ;  STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${YEAR}_DEPTH=@CLR_DEPTH@" ;
  MEAN="-mean 0" ; DEP="" ; LEV="-lev 1" ; dep=0  ;
  mklim $min $max $pas > zclrmark
  filout=${CONFIG}_${var}_${bas}_${dep}_${YEAR}-${CASE}
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
     filout=${CONFIG}_${var}_${bas}_${dep}_${YEAR}-${CASE}
     if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
        rapatrie $t $MEANY $t
        rapatrie $ice $MEANY $ice
            case $BASIN in 
     ATLN | ATLS )
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
            esac ;;
     * ) 
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
             esac ;;
        esac
        mklim $min $max $pas > zclrmark
        STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${YEAR}_DEPTH=@CLR_DEPTH@"
        basplt  ; mkplt $filout
     fi
   done
 done

  puttoarch  $BASIN 
 done

                fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2.2 : Barotropic Stream Function
#------------------------------------
                        if [ $zoomUV != 0 ] ; then

  list_bas=$( getlist $zoomUV )
  # get files  for PSI
  psi=${CONFCASE}_y${YEARDAT}_PSI.nc


  for BASIN in $list_bas ; do
    case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ; CNTINT="-cntint 20.e+06" ;;
      KERGUELEN) ZOOM='-zoom   30 110 -70 -30' ; bas=KERG ; CNTINT="-cntint 20.e+06" ;;
       CAMPBELL) ZOOM='-zoom  100 179 -70 -30' ; bas=CAMP ; CNTINT="-cntint 20.e+06" ;;
           ATLN) ZOOM='-zoom -100  10  -5  70' ; bas=ATLN ; CNTINT="-cntint 10.e+06" ;;
           ATLS) ZOOM='-zoom  -70  30 -70   5' ; bas=ATLS ; CNTINT="-cntint 20.e+06" ;;
    esac
    case $BASIN in
       DRAKE | KERGUELEN | CAMPBELL ) 
              CNTILOPT="-cntilxy 0.5 0.12 -cntils 0.015 -cntilp 0"
              CNTLIM="-cntmin -300.e+06 -cntmax 300.e+06 $CNTINT"
              CNTLAB="-cntlls 0.015 -cntrc2 0.4 -cntlis 3 -cntlw 1:2" ;;
       ATLN | ATLS)
              CNTILOPT=''
              CNTLIM="-cntmin -300.e+06 -cntmax 220.e+06 $CNTINT"
              CNTLAB="-cntlis 5 -cntlw 1:2" ;;
    esac


    listplt=' '

    var=PSI ;STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${YEAR}"
    filout=${CONFIG}_${var}_${bas}_${YEAR}-${CASE}
    OPTIONS='-proj ME -xstep 10 -ystep 10'

    if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent  ] ; then
       rapatrie $psi $MEANY $psi
       $CHART -cntdata $psi -cntvar sobarstf -clrvar sobarstf -cntexp 6 -cntshade -forcexy -cslab 1.1 $ZOOM $OPTIONS  $CNTLAB \
              $CNTILOPT $CNTLIM $STRING -english 
       mkplt $filout
    fi

    puttoarch  $BASIN 
  done
  rm *.cgm *.jpg
                        fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2.3 : Atmospheric fluxes
#------------------------------------
                       if [ $zoomFLX != 0 ] ; then

  list_bas=$( getlist $zoomFLX )
  # get files  gridT
  if [ $XIOS ] ; then
     t=${CONFCASE}_y${YEARDAT}_flxT.nc
  else
     t=${CONFCASE}_y${YEARDAT}_gridT.nc
  fi

  for BASIN in $list_bas ; do
    case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) ZOOM='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # plot the heat flux (w/m2) and fresh water flux (mm/day) (scaled by 86400 from kg/m2/s)
  OPTIONS=""
  MEAN="" ; DEP="" ; LEV="" ; PAL="-p $PALBLUE2RED3" ; CNTICE="" ; FORMAT='-format PALETTE I4'
    for var in HeatFlx WaterFlx WaterDmp CDWaterFlx ; do
      STRING="-string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}_DEPTH=@CLR_DEPTH@"
      filout=${CONFIG}_${var}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
         rapatrie $t    $MEANY $t
         case $var in
           HeatFlx)    clrvar=sohefldo ; CLRDATA=" -clrdata $t"              ; min=-140 ; max=140  ; pas=15 ;;
           WaterFlx)   clrvar=sowaflup ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
           WaterDmp)   clrvar=sowafldp ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
           CDWaterFlx) clrvar=sowaflcd ; CLRDATA=" -clrdata $t -scale 86400."; min=-7 ; max=7  ; pas=2 ;;
         esac
         CLRLIM="-clrmin $min -clrmax $max -clrmet 1"
         basplt  ; mkplt $filout
      fi
    done

    # dispose to archive all plots in the listplt
    puttoarch  $BASIN
done
                         fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2.4 : MXL 
#------------------------------------
                          if [ $zoomMXL != 0 ] ; then

  list_bas=$( getlist $zoomMXL )

 basmxlplt(){ $CHART $STRING $ZOOM -proj ME -xstep 10 -ystep 5 \
              $CLRDATA -noint -p $PAL2 -english $clrvar -clrmark zclrmark -forcexy -xyplot $POS -o $1 $FORMAT ;}


  # get files  for MXL in m03 and m09
  mxl3=${CONFCASE}_y${YEAR}m03${xiosid}_MXL.nc
  mxl9=${CONFCASE}_y${YEAR}m09${xiosid}_MXL.nc

  FORMAT="-format PALETTE I3"

  # build limits file for clrmark
cat << eof > zclrmark
0
100
200
300
400
500
600
700
800
eof

  # this plot have 2 frames on the panel up= march, down=september
  up='.1 .9 .57 .9'
  down=' .1 .9 .2 .53'

  for BASIN in $list_bas ; do
    case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) ZOOM='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac
  listplt=' '

    for crit in rho0.01 rho0.03 tem0.20 ; do
      var=MLD$crit
      filout=${CONFIG}_${var}_${bas}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent  ] ; then
        rapatrie $mxl3 $MEANY $mxl3
        rapatrie $mxl9 $MEANY $mxl9
         case $crit in
          rho0.01 ) clrvar="-clrvar somxl010" ;;
          rho0.03 ) clrvar="-clrvar somxl030" ;;
          tem0.20 ) clrvar="-clrvar somxlt02" ;;
        esac
        # march
        filout1=gm1 ; POS=$up  ; CLRDATA="-clrdata $mxl3" ; STRING="-string 0.5 0.97 1.0 0 ${CONFCASE}_${var} -string 0.5 0.94 1.0 0 m03-m09_${bas}_${YEAR}"
        basmxlplt $filout1
        # september
        filout2=gm2 ; POS=$down  ; CLRDATA="-clrdata $mxl9"; STRING=""
        basmxlplt $filout2
        # merge frame
        \rm -f gmeta
        med -e 'r gm1' -e 'r gm2' -e '1,2 merge' -e '1 w gmeta'
        \rm -f gm1 gm2
        mkplt $filout
      fi
    done
    puttoarch  $BASIN
  done
  rm gm* *.jpg
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2.5 : Ice
#------------------------------------
                        if [ $zoomICE != 0 ] ; then
  list_bas=$( getlist $zoomICE )

  # plot ice thickness for march and september
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
    ice=${CONFCASE}_y${YEAR}m${m}${xiosid}_icemod.nc

    tmp=${ice#${CONFCASE}_y} ; tag=${tmp%${xiosid}_*}
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

    for BASIN in $list_bas ; do
      case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) ZOOM='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
      esac
      listplt=' '
      filout=${CONFIG}_S_ithic_${bas}_${month}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
        rapatrie $ice $MEANY $ice
        $CHART -forcexy -clrdata $ice -clrvar iicethic -clrlim S_thick.lim -p $PALBLUE2RED4 \
        -proj ME -vertpal $ZOOM  -spval 0  -xstep 10 -xgrid -ystep 5 -ygrid \
        -clrexp -2 -format PALETTE I3  \
        -clrxypal 0.9 1 0.1 0.9 \
        -xyplot 0.01 0.8 0.1 0.9 \
        -string 0 0.97 1.1 -1 " Sea Ice Thickness $bas " -font SIMPLEX_ROMAN -noteam \
        -string 0 0.94 1.1 -1 " $month_name $year " \
        -string 0.5 0.97 1.2 -1 " $CONFCASE "  \
        -string 0.93 0.07 1.1 1 " (cm)    "
         mkplt  $filout
      fi
      filout=${CONFIG}_S_iconc_${bas}_${month}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
        rapatrie $ice $MEANY $ice
        $CHART -forcexy -clrdata $ice -clrvar ileadfra -clrlim ice_noaa.lim -p $PALNOAA \
        -proj ME -vertpal $ZOOM  -spval -1 -xstep 10 -xgrid -ystep 5 -ygrid \
        -clrexp -2 -format PALETTE I3  \
        -clrxypal 0.9 1 0.1 0.9 \
        -xyplot 0.01 0.8 0.1 0.9 \
        -string 0 0.97 1.1 -1 " Sea Ice Concentration $bas" -font SIMPLEX_ROMAN -noteam \
        -string 0 0.94 1.1 -1 " $month_name $year " \
        -string 0.5 0.97 1.2 -1 " $CONFCASE "  \
        -string 0.93 0.07 1.1 1 " %    "
        mkplt  $filout
      fi
      puttoarch  $BASIN

     done

   done

                        fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 5.2.6 : EKE
#------------------------------------
                        if [ $zoomEKE != 0 ] ; then
  list_bas=$( getlist $zoomEKE )


  ekeplt() { $CHART $ZOOM -forcexy $STRING $CLRDATA $XYPLOT -p $PAL1 \
             -clrvar voeke $dep $CLRXYPAL $CLRMARK \
             $OPTIONS -english -scale 1.e4 ; }
#-----------

  # get files  EKE
  eke=${CONFCASE}_y${YEARDAT}_EKE.nc

  echo $CASE | grep -q -i obs
  if [ $? = 0 ] ; then
     dep=''
     ZCONF=''
  else
     dep='-dep 10'
     ZCONF=$CONFIG
  fi

  zdep=$( echo $dep | awk '{print$2}')

  min=0 ; max=1000 ; pas=250
  mklim $min $max $pas > zclrmark
  cat << eof > zclrmarklog   # log scale from 1 to 5000 ( 1 10 100 1000 5000)
0.
1.
2.
3.
3.69897
eof

  CLRDATA=" -clrdata $eke"

  for BASIN in $list_bas ; do
    listplt=' '
    case $BASIN in
         DRAKE)  var=EKEdrak ; ZOOM='-zoom -100 -20 -70 -30' ; OPTIONS='-proj ME -xstep 10 -ystep 10' ;;
     KERGUELEN)  var=EKEkerg ; ZOOM='-zoom   30 110 -70 -30' ; OPTIONS='-proj ME -xstep 10 -ystep 10' ;;
      CAMPBELL)  var=EKEcamp ; ZOOM='-zoom  100 180 -70 -30' ; OPTIONS='-proj ME -xstep 10 -ystep 10' ;;
      ATLANTIC)  var=EKEatl  ; ZOOM='-zoom -100  20 -70  70' ; OPTIONS='-proj ME -xstep 15 -ystep 15' ;;
          ATLN)  var=EKEnatl ; ZOOM='-zoom -100  20  -5  70' ; OPTIONS='-proj ME -xstep 10 -ystep 10' ;;
          ATLS)  var=EKEsatl ; ZOOM='-zoom  -70  30 -70   5' ; OPTIONS='-proj ME -xstep 10 -ystep 10' ;;

    esac
    XYPLOT='-xyplot 0.1 0.95 0.2 0.9'
    CLRXYPAL='-clrxypal 0.1 0.95 0.05 0.15'
    # linear view
    CLRMARK='-clrmark zclrmark'
    STRING="-string 0.5 0.95 1.0 0 ${ZCONF}_${var}_${YEAR}_${CASE}_DEPTH=@CLR_DEPTH@ "
    filout=${CONFIG}_${var}_${zdep}_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
     rapatrie $eke $MEANY $eke
     ekeplt ; mkplt $filout
    fi
    # log view
    OPTIONS="$OPTION -log10" ; CLRMARK='-clrmark zclrmarklog'
    STRING="-string 0.5 0.95 1.0 0 ${ZCONF}_${var}_${YEAR}_${CASE}_DEPTH=@CLR_DEPTH@ "
    filout=${CONFIG}_log${var}_${zdep}_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/$BASIN/$filout.cgm ) == absent ] ; then
     rapatrie $eke $MEANY $eke
     ekeplt ; mkplt $filout
    fi

    puttoarch  $BASIN
  done
                        fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6. Sections in the ocean
#^^^^^^^^^^^^^^^^^^^^^^^^^
# Generic function for coupe plot
  coupplt() {  echo COUPPLT for section $var ;
             $COUPE $CLRDATA $PTS -forcexy -clrnoint \
             -string 0.5 0.95 1.0 0 ${CONFIG}_${var}_${YEAR}-${CASE}  \
             -clrvar $clrvar -pmax $pmax -pmin $pmin -zstep $zstep  -english \
             $PAL  $CLRLIM $CLRMODIF $FORMAT; }
#---
# 6.1 General Section for ORCA configs
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $coupes == 1 ] ; then
  listplt=' '
  # get files  gridT, gridU, gridV
  t=${CONFCASE}_y${YEARDAT}_gridT.nc
  u=${CONFCASE}_y${YEARDAT}_gridU.nc
  v=${CONFCASE}_y${YEARDAT}_gridV.nc

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
         filout=${CONFIG}_${var}_${YEAR}-${CASE}
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
         filout=${CONFIG}_${var}_${YEAR}-${CASE}
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
     filout=${CONFIG}_${var}_ATL_${YEAR}-${CASE}
     title="${CONFCASE} U at Equator ATL $YEAR"
     PTS="-pts -50 10 0 0"
     CLRLIMIT="-clrmin -1.0 -clrmax 1.2"

     if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $u $MEANY $u
            equat_section  ; mkplt $filout
     fi

# Pacific
     filout=${CONFIG}_${var}_PACIF_${YEAR}-${CASE}
     title="${CONFCASE} U at Equator PAC $YEAR"
     PTS="-pts 130 280 0 0 -360 "
     CLRLIMIT="-clrmin -1.0 -clrmax 1.3"

     if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $u $MEANY $u
            equat_section  ; mkplt $filout
     fi

# Indian
     filout=${CONFIG}_${var}_INDIAN_${YEAR}-${CASE}
     title="${CONFCASE} U at Equator IND $YEAR"
     PTS1="-pts 40   73.0  0 0 "  # "-pts 40   72.8  0 0 "
     PTS2="-pts 73.0 105   0 0 "  # "-pts 72.8 105   0 0 "
     CLRLIMIT="-clrmin -0.5 -clrmax 0.5"

     if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
            rapatrie $u $MEANY $u
            equat_section_indian  ; mkplt $filout
     fi

  puttoarch  SECTIONS 
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
     topa=${CONFCASE}_y${YEAR}${m}${xiosid}_gridT.nc

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
            $CDFTOOLS/cdfsig0 -t $t
            mklim $min $max $pas > zclrmark
            coupar7w; mkplt $filout
         fi
       fi
    done
    done
  done
  puttoarch  SECTIONS1 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6.3 : Sections in the Southern Ocean (designed from PERIANT runs)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  # Generic function for coupes_aus plot
  # Top panel (0<depth<1000m zoom)
  couptop() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -1000 -clrvar $clrvar $CLRMODIF \
              $CNTDATA -cntvar $clrvar $CNTLIM -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.70 0.95 -o gmetatop -nolon -ylat 0.10 -english $CLRMARK $PAL \
              -clrxypal 0.1 0.95 0.08 0.18 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${YEAR}-${CASE}; }
  # Bottom panel (0<depth<1000m)
  coupbot() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -5500 -clrvar $clrvar $CLRMODIF \
              $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.20 0.68 -o gmetabot -nolon -ylat 0.10  -english $CLRMARK $PAL \
              -clrxypal 0.1 0.95 0.08 0.18 -zstep 500; }

#--------
                        if [ $coupes_aus == 1 ] ; then
  listplt=' '
  # get files  gridT, gridU, gridV LSPV in march and september
  t=${CONFCASE}_y${YEARDAT}_gridT.nc
  u=${CONFCASE}_y${YEARDAT}_gridU.nc
  v=${CONFCASE}_y${YEARDAT}_gridV.nc
  fich_pvm3=${CONFCASE}_y${YEAR}m03${xiosid}_LSPV.nc
  fich_pvm9=${CONFCASE}_y${YEAR}m09${xiosid}_LSPV.nc

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
            nlev=$( cdfinfo -f $fich_pvm3 | grep npk | awk '{ print $3}' );
         #   LEV="-lev 2-$nlev" ;
            LEV='' ;
            CNTLIM='' ; CNTDATA='' ;;
      pvm9) clrvar=volspv ; CLRDATA="-clrdata $fich_pvm9" ; # CLRDATA="-clrdata pvm9" ;
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
            nlev=$( cdfinfo -f $fich_pvm9 | grep npk | awk '{ print $3}' );
         #   LEV="-lev 2-$nlev" ;
            LEV='' ; 
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
       filout=${CONFIG}_${var}_${YEAR}-${CASE}
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
         rm gmeta* # $fich_pvm3 $fich_pvm9
       fi
     fi
    done
  done
  puttoarch  SECTIONS
                        fi
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 6.4 Coupes sigma as in the WOCE Atlas (in the Southern Ocean)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  ##Generic plot for sigma
  # Top panel (0<depth<1000m zoom)
  coupsig() { $COUPE -clrdata sig0.nc -pts $LONmin $LONmax $LATmin $LATmax -forcexy -pmax -1000 -clrvar vosigma0 \
              -cntdata sig0.nc -cntvar vosigma0 $CNTLIM0 -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
              -xyplot 0.1 0.95 0.65 0.9 -o gmetatop $CLRMARK0 $PAL -nolon -clrnopal\
              -ylat 0.10 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${YEAR}-${CASE} ; \
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

  t=${CONFCASE}_y${YEARDAT}_gridT.nc

  PAL="-p $PAL2"

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
      filout=${CONFIG}_${var}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/SECTIONS/$filout.cgm ) == absent ] ; then
        rapatrie $t $MEANY $t
        cdfsig0 -t $t
        cdfsigi -t $t -r 2000 -o sig2.nc
        cdfsigi -t $t -r 4000 -o sig4.nc
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

  puttoarch  SECTIONS
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
                  -clrxypal 0.1 0.95 0.03 0.13 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${YEAR}-${CASE}; }
  # Bottom panel (0<depth<1000m)
  coupbotcirc() { $COUPE $CLRDATA -pts $LON2 $LON3 $LATmin $LATmax $LEV -forcexy -pmax -5500 -clrvar $clrvar $CLRMODIF \
                  $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.03 \
                  -xyplot 0.1 0.44 0.20 0.55 -o gmetabot1 -ylon 0.18 -nolat -english -clrmark zclrmark $PAL \
                  -clrxypal 0.1 0.95 0.03 0.13 ; \
                  $COUPE $CLRDATA -pts $LON3spec $LON4 $LATmin $LATmax -forcexy -pmax -5500 -clrvar $clrvar $CLRMODIF \
                  $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.20 0.55 -o gmetabot2 -nozlab -ylon 0.18 -nolat -english -clrmark zclrmark $PAL \
                  -clrxypal 0.1 0.95 0.03 0.13 ; }
#------------

  listplt=' '
  # get files  gridT, gridU, gridV
  t=${CONFCASE}_y${YEARDAT}_gridT.nc
  fich_pvm3=${CONFCASE}_y${YEAR}m03${xiosid}_LSPV.nc
  fich_pvm9=${CONFCASE}_y${YEAR}m09${xiosid}_LSPV.nc

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
            nlev=$( cdfinfo -f $fich_pvm3 | grep npk | awk '{ print $3}' );
            LEV='' ; # LEV="-lev 2-$nlev" ;
            min=-1.0 ; max=6.0 ; pas=1.0 ;;
      pvm9) clrvar=volspv ; CLRDATA="-clrdata $fich_pvm9" ;
            CLRMODIF='-clrmodif deptht=depthw';
            CNTDATA='' ; CNTLIM='' ;
            rapatrie $fich_pvm9 $MEANY $fich_pvm9 ; 
            nlev=$( cdfinfo -f $fich_pvm9 | grep npk | awk '{ print $3}' );
            LEV='' ; # LEV="-lev 2-$nlev" ; 
            min=-1.0 ; max=6.0 ; pas=1.0 ;;
     esac
     case $sec in
        CIRC70)  LATmin=-70; LATmax=-70 ; SECTION=070S ; LON3spec=72.9 ;
                 case $fld in
                   t) min=-1 ; max=2 ; pas=0.5 ;;
                   s) min=33.8 ; max=34.6 ; pas=0.2 ;;
                 esac ;;
        CIRC60)  LATmin=-60; LATmax=-60 ; SECTION=060S ; LON3spec=72.9 ;
                 case $fld in
                   t) min=-1 ; max=5 ; pas=0.5 ;;
                   s) min=33.8 ; max=34.6 ; pas=0.2 ;;
                 esac ;;
        CIRC48)  LATmin=-48; LATmax=-48 ; SECTION=048S ; LON3spec=72.9 ;
                 case $fld in
                   t) min=0 ; max=10 ; pas=1 ;;
                   s) min=33.8 ; max=34.6 ; pas=0.2 ;;
                 esac ;;
        CIRC40)  LATmin=-40; LATmax=-40 ; SECTION=040S ; LON3spec=73.0 ;
                 case $fld in
                   t) min=0 ; max=16 ; pas=2 ;;
                   s) min=33.5 ; max=35 ; pas=0.5 ;;
                 esac ;;
            * )  skip=1 ;;
     esac
     if [ $skip != 1 ] ; then
       filout=${CONFIG}_${var}${sec}_${YEAR}-${CASE}
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
  puttoarch  CIRCUM 
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
                  -ylat 0.10 -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${YEAR}-${CASE} ; \
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
                  -clrnopal -ylat 0.10 ; \
                  $COUPE -clrdata sig0.nc -pts $LON3spec $LON4 $LATmin $LATmax -forcexy -pmax -1000 -clrvar vosigma0 \
                  -cntdata sig0.nc -cntvar vosigma0 $CNTLIM0  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.42 0.50 -o gmetabot21 $CLRMARK0 $PAL -zstep 500 -ylon 0.08 -nolat -nozlab \
                  -clrnopal -ylat 0.10 ; \
                  $COUPE -clrdata sig2.nc -pts $LON3spec $LON4 $LATmin $LATmax -forcexy -pmin -1000 -pmax -3000 \
                  -clrvar vosigmai -cntdata sig2.nc -cntvar vosigmai $CNTLIM2  -cntrc1 0.04 -cntrc2 0.07 \
                  -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.26 0.42 -o gmetabot22 $CLRMARK2 $PAL -ylon 0.08 -nolat -nozlab \
                  -clrnopal -ylat 0.10 ; \
                  $COUPE -clrdata sig4.nc -pts $LON3spec $LON4 $LATmin $LATmax -forcexy -pmin -3000 -pmax -5500 -clrvar vosigmai \
                  -cntdata sig4.nc -cntvar vosigmai $CNTLIM4 -cntrc1 0.06 -cntrc2 0.07 -cntlis 1 -cntlls 0.02 \
                  -xyplot 0.44 0.95 0.10 0.26 -o gmetabot23 $CLRMARK4 $PAL -ylon 0.08 -nolat -nozlab \
                  -clrnopal -ylat 0.10 ; }
#--------------------
                        if [ $circum_sigma = 1 ] ; then
  listplt=' '

  # get files  gridT
  t=${CONFCASE}_y${YEARDAT}_gridT.nc

  PAL="-p $PAL2"

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
      CIRC70)  LATmin=-70; LATmax=-70 ; SECTION=070S ; LON3spec=72.9 ;;
      CIRC60)  LATmin=-60; LATmax=-60 ; SECTION=060S ; LON3spec=72.9 ;;
      CIRC48)  LATmin=-48; LATmax=-48 ; SECTION=048S ; LON3spec=72.9 ;;
      CIRC40)  LATmin=-40; LATmax=-40 ; SECTION=040S ; LON3spec=73.0 ;;
          * )  skip=1 ;;
    esac
    if [ $skip != 1 ] ; then
      filout=${CONFIG}_sig${sec}_${YEAR}-${CASE}
      if [ $( chkfile $PLOTDIR/CIRCUM/$filout.cgm ) == absent ] ; then
        rapatrie $t $MEANY $t
        cdfsig0 -t $t
        cdfsigi -t $t -r 2000 -o sig2.nc
        cdfsigi -t $t -r 4000 -o sig4.nc
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


  puttoarch  CIRCUM 
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
    ice=${CONFCASE}_y${YEAR}m${m}${xiosid}_icemod.nc

    tmp=${ice#${CONFCASE}_y} ; tag=${tmp%${xiosid}_*}
    year=$( echo $tag | awk -Fm '{print $1}' )
    month=$( echo $tag | awk -Fm '{print $2}' )
#    icetxt=${CONFCASE}_y${YEAR}_icemonth.txt
#    rapatrie $icetxt $DIAGS/TXT $icetxt
    icenc=${CONFCASE}_y${YEARDAT}_1m_ICEMONTH.nc
    rapatrie $icenc $DIAGS/NC $icenc

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
      SOUTH )
         surf=$( ncks -F -H -v SExnsidc $icenc | fgrep "SExnsidc($mm)" | awk -F=  '{printf "%5.1f",  $NF/1000}' )
         volu=$( ncks -F -H -v SVolume  $icenc | fgrep "SVolume($mm)"  | awk -F=  '{printf "%5.1f",  $NF/1000}' )
         # surf=$( grep -A 12 -e "$YEAR $m" $icetxt | grep SExtend  | awk '{printf "%5.1f",  $NF/1000}' )
         # volu=$( grep -A 12 -e "$YEAR $m" $icetxt | grep SVolume  | awk '{printf "%5.1f",  $NF/1000}' )
         filout=${CONFIG}_S_ithic_${month}_${YEAR}-${CASE}
        if [ $( chkfile $PLOTDIR/ICE/$filout.cgm ) == absent ] ; then
         rapatrie $ice $MEANY $ice
        $CHART -forcexy -clrdata $ice -clrvar iicethic -clrlim S_thick.lim -p $PALBLUE2RED4 \
        -proj OR -vertpal -rlat -90 -zoom -179 179 -90 -50  -spval 0  -xstep 45 -xgrid -ystep 45 -ygrid \
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
        -proj OR -vertpal -rlat -90 -zoom -179 179 -90 -50  -spval -1 \
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

       NORTH )
         surf=$( ncks -F -H -v NExnsidc $icenc | fgrep "NExnsidc($mm)" | awk -F=  '{printf "%5.1f",  $NF/1000}' )
         volu=$( ncks -F -H -v NVolume  $icenc | fgrep "NVolume($mm)"  | awk -F=  '{printf "%5.1f",  $NF/1000}' )
         # surf=$( grep -A 12 -e "$YEAR $m" $icetxt | grep NExtend  | awk '{printf "%5.1f",  $NF/1000}' )
         # volu=$( grep -A 12 -e "$YEAR $m" $icetxt | grep NVolume  | awk '{printf "%5.1f",  $NF/1000}' )
         filout=${CONFIG}_N_ithic_${month}_${YEAR}-${CASE}
        if [ $( chkfile $PLOTDIR/ICE/$filout.cgm ) == absent ] ; then
         rapatrie $ice $MEANY $ice
         $CHART -forcexy -clrdata $ice -clrvar iicethic -clrlim thick.lim -p $PALBLUE2RED4 \
         -proj OR -vertpal -rlat 90 -rlon -45 -zoom -177 177.0 40 87  -spval 0 -xstep 45 -xgrid -ystep 45 -ygrid \
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
         -proj OR -vertpal -rlat 90 -rlon -45 -zoom -177 177.0 40 87  -spval -1 \
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

    puttoarch  ICE 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 8. Deep western boundary current
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 8.1 : DWBC in the Atlantic :
#------------------------------
                        if [ $dwbc == 1 ] ; then

  listplt=' '
  # get files   gridU, gridV
  u=${CONFCASE}_y${YEARDAT}_gridU.nc
  v=${CONFCASE}_y${YEARDAT}_gridV.nc

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
          filout=${CONFIG}_${var}_${YEAR}-${CASE}
          if [ $( chkfile $PLOTDIR/DWBC/$filout.cgm ) == absent ] ; then
             rapatrie $u $MEANY $u
             rapatrie $v $MEANY $v
             mklim $min $max $pas > zclrmark
             coupplt ; mkplt $filout
          fi
        fi
    done
  done

 puttoarch  DWBC 
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 8.2 Deep Western Boundary Current in the Southern Ocean
#-----------------------------------------------------------
                        if [ $dwbc_aus == 1 ] ; then

  listplt=' '
  # get files   gridU, gridV
  v=${CONFCASE}_y${YEARDAT}_gridV.nc

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
       filout=${CONFIG}_${var}_${YEAR}-${CASE}
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
  puttoarch  DWBC
                        fi
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 9. Eddy Kinetic Energy
#^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $ekes == 1 ] ; then

  ekeplt() { $CHART $ZOOM -forcexy $XYPLOT $STRING $CLRDATA  -p $PAL1 \
             -clrvar voeke $dep $CLRXYPAL $CLRMARK  \
             $OPTIONS -english $FORMAT -scale 1.e4 ; }

#-----------

  # get files  EKE
  eke=${CONFCASE}_y${YEARDAT}_EKE.nc
  # check if input file is observation ( obs in CASE )
  # if observation, surface EKE and no CONFIG in the header of the plot
  echo $CASE | grep -q -i obs 
  if [ $? = 0 ] ; then 
     dep='' 
     ZCONF=''
  else
     dep='-dep 10' 
     ZCONF=$CONFIG
  fi

  zdep=$( echo $dep | awk '{print$2}')

  FORMAT=""
  min=0 ; max=2500 ; pas=250   # ORCA CONFIG default
  cat << eof > zclrmarklog   # log scale from 1 to 5000 ( 1 10 100 1000 5000)
0.
1.
2.
3.
3.69897
eof
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
  #linear view
  var=EKEgl  ; ZOOM='-ijgrid -noproj -noint' ; OPTIONS='' ; CLRMARK="-clrmark zclrmark"
  STRING="-string 0.5 0.95 1.0 0  ${ZCONF}_${var}_${YEAR}_${CASE}_DEPTH=@CLR_DEPTH@ "
  filout=${CONFIG}_${var}_${zdep}_${YEAR}-${CASE}
  if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
     rapatrie $eke $MEANY $eke
     if [ $filtering != 0 ] ; then 
       cdfsmooth -f $eke -c $filtering  -k 1-4 
       mv $eke $eke.all
       mv ${eke}L* $eke
     fi
     ekeplt ; mkplt $filout
  fi
  # log view
  var=logEKEgl ; ZOOM='-pixel' ; OPTIONS='-log10' ; CLRMARK="-clrmark zclrmarklog"
  filout=${CONFIG}_${var}_${zdep}_${YEAR}-${CASE}
  if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
     rapatrie $eke $MEANY $eke
     ekeplt ; mkplt $filout
  fi

  # Atlantic zoom 
  
  if [ $(chconf PERIANT) = 0 ] ; then  # EKE in the Atlantic
    # linear view
    var=EKEatl ; ZOOM='-zoom -100 20 -70 70' ; OPTIONS='-proj ME -xstep 15 -ystep 15' ; CLRMARK="-clrmark zclrmark"
    STRING="-string 0.5 0.95 1.0 0 ${ZCONF}_${var}_${YEAR}_${CASE}_DEPTH=@CLR_DEPTH@ "
    filout=${CONFIG}_${var}_${zdep}_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
       rapatrie $eke $MEANY $eke
       ekeplt ; mkplt $filout
    fi
    # logview
    var=logEKEatl ; ZOOM='-zoom -100 20 -70 70' ; OPTIONS='-proj ME -xstep 15 -ystep 15 -log10 ' ; CLRMARK="-clrmark zclrmarklog"
    STRING="-string 0.5 0.95 1.0 0 ${ZCONF}_${var}_${YEAR}_${CASE}_DEPTH=@CLR_DEPTH@ "
    filout=${CONFIG}_${var}_${zdep}_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent ] ; then
       rapatrie $eke $MEANY $eke
       ekeplt ; mkplt $filout
    fi
  fi

  puttoarch  GLOBAL
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
  mxl3=${CONFCASE}_y${YEAR}m03${xiosid}_MXL.nc
  mxl9=${CONFCASE}_y${YEAR}m09${xiosid}_MXL.nc

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

  puttoarch  MXL
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
  mxl3=${CONFCASE}_y${YEAR}m03${xiosid}_MXL.nc

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

 puttoarch  ATLN 
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
  flx3=${CONFCASE}_y${YEAR}m03${xiosid}_gridT.nc

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

 puttoarch  ATLN 
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
 bastrcplt() { $CHART -forcexy $CLRDATA $ZOOM $OPTIONS  \
               -clrvar $clrvar -english -clrexp $exp -string 0.95 0.15 1 1 "x10|S|$exp" \
               -lev $lev $DEP $PAL $STRING $CLRLIM $CNTTRC -cntlis 1 -cntlls 0.01  $MEAN $FORMAT; }

 # Generic function for section plots
 couptrcplt() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax -forcexy -clrvar $clrvar \
                $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
                -xyplot 0.1 0.95 0.2 0.95 -o gmeta -nolon -ylat 0.10  -english $CLRMARK $PAL $FORMAT $MEAN \
                -string 0.5 0.98 1.0 0 ${CONFIG}_${var}_${SECTION}_${YEAR}-${CASE}_DEPTH=@CLR_DEPTH@ \
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
    trc=${CONFCASE}_y${YEAR}m${m}${xiosid}_ptrcT.nc
    tmp=${trc#${CONFCASE}_y} ; tag=${tmp%${xiosid}_*}
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

 puttoarch  GLOBAL

 ######## Basin plots ############
 # reset the list of plots that are produced ( list is updated in mkplt )
 listplt=' '

 # get ptrcT
 trc=${CONFCASE}_y${YEARDAT}_ptrcT.nc

 # Common values for this group of plots
  OPTIONS='-proj ME -xstep 10 -ystep 5'
  CLRDATA="-clrdata $trc"
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"

  for BASIN in DRAKE KERGUELEN CAMPBELL ; do
    case $BASIN in
      DRAKE)     ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
      CAMPBELL)  ZOOM='-zoom 100 179 -70 -30'  ; bas=CAMP ;;
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
    STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${bas}_${YEAR}_DEPTH=@CLR_DEPTH@"
    if [ $skip != 1 ] ; then
      filout=${CONFIG}_${var}_${lev}_${bas}_${YEAR}-${CASE}
      mklim $min $max $pas > zclrmark
      if [ $( chkfile $PLOTDIR/GLOBAL/$filout.cgm ) == absent  ] ; then
        rapatrie $trc $MEANY $trc
        bastrcplt  ; mkplt $filout
      fi
    fi
   done
  done

  puttoarch  $BASIN

 done

 ####### Section plots ##########
 # reset the list of plots that are produced ( list is updated in mkplt )
 listplt='' 

 # get ptrcT
 trc=${CONFCASE}_y${YEARDAT}_ptrcT.nc

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
             filout=${CONFIG}_${var}_${lev}_${sec}_${YEAR}-${CASE} ;
             exp=7
             min=8.e07 ; max=3.e08 ; pas=1.e$exp ;;
      CLR02) clrvar=clr02 ; lev=24
             filout=${CONFIG}_${var}_${lev}_${sec}_${YEAR}-${CASE}
             exp=7
             min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR03) clrvar=clr03 ; lev=24
             filout=${CONFIG}_${var}_${lev}_${sec}_${YEAR}-${CASE}
             exp=8
             min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR04) clrvar=clr04 ; lev=30
             filout=${CONFIG}_${var}_${lev}_${sec}_${YEAR}-${CASE}
             exp=7
             min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR05) clrvar=clr05 ; lev=30
             filout=${CONFIG}_${var}_${lev}_${sec}_${YEAR}-${CASE}
             exp=8
             min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR06) clrvar=clr06 ; lev=34
             filout=${CONFIG}_${var}_${lev}_${sec}_${YEAR}-${CASE}
             exp=7
             min=9.e08 ; max=1.e09 ; pas=1.e$exp ;;
      CLR07) clrvar=clr07 ; lev=34
             filout=${CONFIG}_${var}_${lev}_${sec}_${YEAR}-${CASE}
             exp=8
             min=1.e08 ; max=1.e09 ; pas=1.e$exp ;;
    esac 
    if [ $skip != 1 ] ; then
#      filout=${CONFIG}_${var}_${lev}_${YEAR}-${CASE}
      mklim $min $max $pas > zclrmark
      couptrcplt ; mkplt $filout
      rm gmeta* 
    fi
    done     
 done        

 puttoarch  SECTIONS 

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
  mocf=${CONFCASE}_y${YEARDAT}_MOCSIG_${NREF}.nc

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

  puttoarch  OVT
                       fi


#------------------------------------------------------------------------------
# 15. CFC = CFC plots on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $cfc == 1 ] ; then

  # generic function for moc plot
#  cfcgloplt() { $CHART -forcexy -clrdata $file -clrvar $vari -scale $scale -clrmark zclrmark  \
  cfcgloplt() { $CHART -pixel -clrdata $file -clrvar $vari -scale $scale -clrmark zclrmark  \
                -clrmet 1  -p $PAL1 $FORMAT $OPTION $STRING -clrexp $clrexp ;
              }

  # get files
  pendep=${CONFCASE}_y${YEARDAT}_pendep.nc
  fracinv=${CONFCASE}_y${YEARDAT}_fracinv.nc
  diadT=${CONFCASE}_y${YEARDAT}_diadT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Set options
  for var in  INV PEN FRAC ; do
    case $var in
      INV) vari=INVCFC ; file=$diadT ; source=$MEANY
           min=0 ; max=1.8 ; pas=0.2
           scale=1.e6 ; clrexp=0 
           STRING="-string 0.5 0.97 1.0 0 ${CONFIG}_${CASE}_${vari}_(10|S|-6|N|.mol/m2)_${YEAR}" ;;
      PEN) vari=pendep ; file=$pendep ; source=$MEANY
           min=0 ; max=1500 ; pas=200
           scale=1 ; clrexp=0 
           STRING="-string 0.5 0.97 1.0 0 ${CONFIG}_${CASE}_${vari}_(m)_${YEAR}" ;;
      FRAC) vari=fracinv ; file=$fracinv ; source=$MEANY
           min=0 ; max=1.8 ; pas=0.2
           scale=1.e+06 ; clrexp=0 
           STRING="-string 0.5 0.97 1.0 0 ${CONFIG}_${CASE}_${vari}_(x_10|S|-6|N|)_${YEAR}" ;;
    esac
    filout=${CONFIG}_${vari}_global_${YEAR}-${CASE}
    if [ $( chkfile $PLOTDIR/CFC/$filout.cgm) == absent ] ; then
     rapatrie $file $source $file
     mklim $min $max $pas > zclrmark
     cfcgloplt ; mkplt $filout
    fi
  done

  puttoarch  CFC

                               fi

#------------------------------------------------------------------------------
# 16. PISCES = PISCES plots on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 16.1 : Global Plots
#--------------------

  # generic function for PISCES global plot

  glopiplt() { set -x ; $CHART -forcexy $CLRDATA -scale $scale   \
               -clrvar $clrvar -english  \
               $LEV $DEP $PAL $STRING $CLRLIM  $MEAN $FORMAT; }
  add_var() { ncap -O -s "$4=$2+$3" $1 $5 ;}

#------------------------------------------------------------------
# 16.1.1 : Global Plots of surface and at 150m depth concentrations
#------------------------------------------------------------------


                             if [ $pisces_global == 1 ] ; then

  # get files
  t=${CONFCASE}_y${YEARDAT}_ptrcT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for var in DIC TALK O2 PO4 Si NO3 Fer DOC CHL NCHL DCHL POC PHY PHYnano PHYdiatoms ZOO ZOOmeso ZOOmicro GOC SFe; do
    for dep in 0 150  ; do
      if [ $dep == 0 ]; then
        DEP=""
        LEV="-lev 1"
      else
        DEP="-dep $dep"
        LEV=""
      fi
      filout=${CONFIG}_${var}_${dep}_${YEAR}-${CASE}
      case $var in
        DIC) clrvar=DIC ; unit="mol-C/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=2000 ; max=2200  ; pas=50 ; scale=1.e6 ;;
          150)  min=2000 ; max=2300  ; pas=50 ; scale=1.e6 ;;
          esac ;;
       TALK) clrvar=Alkalini ; unit="mol-Alk/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=2300 ; max=2400  ; pas=50 ; scale=1.e6 ;;
          150)  min=2300 ; max=2400  ; pas=50 ; scale=1.e6 ;;
          esac ;;
         O2) clrvar=O2 ; unit="mol-O2/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=200 ; max=350  ; pas=50 ; scale=1.e6 ;;
          150)  min=200 ; max=300  ; pas=50 ; scale=1.e6 ;;
          esac ;;
        PO4) clrvar=PO4 ; unit="mol-C/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=050  ; pas=50 ; scale=1.e6 ;;
          150)  min=0 ; max=250  ; pas=50 ; scale=1.e6 ;;
          esac ;;
         Si) clrvar=Si ; unit="mol-Si/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=70  ; pas=10 ; scale=1.e6 ;;
          150)  min=0 ; max=100  ; pas=10 ; scale=1.e6 ;;
          esac ;;
        NO3) clrvar=NO3 ; unit="mol-C/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=150  ; pas=50 ; scale=1.e6 ;;
          150)  min=0 ; max=200  ; pas=50 ; scale=1.e6 ;;
          esac ;;
        Fer) clrvar=Fer ; unit="mol-Fe/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=5  ; pas=1 ; scale=1.e10 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e10 ;;
          esac ;;
        DOC) clrvar=DOC ; unit="mol-C/L"; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=6 ; max=26  ; pas=2 ; scale=1.e6 ;;
          150)  min=4 ; max=22  ; pas=2 ; scale=1.e6 ;;
          esac ;;
        CHL) clrvar=CHL ; unit="mgCHL/m3*10" ; CLRDATA="-clrdata CHL2.nc"
          case $dep in
            0)  min=0 ; max=9  ; pas=1 ; scale=1.e7 ;;
          150)  min=0 ; max=1  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
       NCHL) clrvar=NCHL ; unit="mgCHL/m3*10"  ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=9  ; pas=1 ; scale=1.e7 ;;
          150)  min=0 ; max=1  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
       DCHL) clrvar=DCHL ; unit="mgCHL/m3*10"  ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=9  ; pas=1 ; scale=1.e7 ;;
          150)  min=0 ; max=1  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
        POC) clrvar=POC; unit="mol-C/L" ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=2 ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=1 ; pas=0.5 ; scale=1.e6 ;;
          esac ;;
        PHY) clrvar=PHY; unit="mol-C/L" ; CLRDATA="-clrdata PHY2.nc"
          case $dep in
            0)  min=0 ; max=4 ; pas=1 ; scale=1.e6 ;;
          150)  min=0 ; max=1 ; pas=0.5 ; scale=1.e6 ;;
          esac ;;
    PHYnano) clrvar=PHY; unit="mol-C/L" ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=4 ; pas=1 ; scale=1.e6 ;;
          150)  min=0 ; max=1 ; pas=0.5 ; scale=1.e6 ;;
          esac ;;
 PHYdiatoms) clrvar=PHY2; unit="mol-C/L" ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=4 ; pas=1 ; scale=1.e6 ;;
          150)  min=0 ; max=1 ; pas=0.5 ; scale=1.e6 ;;
          esac ;;
        ZOO) clrvar=ZOO; unit="mol-C/L" ; CLRDATA="-clrdata ZOO2.nc"
          case $dep in
            0)  min=0 ; max=2  ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e7 ;;
          esac ;;
    ZOOmeso) clrvar=ZOO2; unit="mol-C/L" ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=2  ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e7 ;;
          esac ;;
   ZOOmicro) clrvar=ZOO; unit="mol-C/L" ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=2  ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e7 ;;
          esac ;;
        GOC) clrvar=GOC; unit="mol-C/L"  ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=3  ; pas=0.5 ; scale=1.e7 ;;
          150)  min=0 ; max=2  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
        SFe) clrvar=SFe; unit="mol-Fe/L"  ; CLRDATA="-clrdata $t"
          case $dep in
            0)  min=0 ; max=2  ; pas=0.5 ; scale=1.e11 ;;
          150)  min=0 ; max=2  ; pas=0.5 ; scale=1.e11 ;;
          esac ;;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        # Compute total CHL if necessary
        case $var in
          CHL) add_var $t NCHL DCHL CHL CHL2.nc ;;
          PHY) add_var $t PHY PHY2 PHY PHY2.nc ;;
          ZOO) add_var $t ZOO ZOO2 ZOO ZOO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING1=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}"
        STRING2=" -string 0.5 0.80 1.0 0 ${unit}*${scale}_DEPTH=@CLR_DEPTH@"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
    done
  done

  puttoarchtrc PISCES GLOBAL

                             fi

#----------------------------------------------
# 16.1.2 : Global Plots of diagnostics variable
#----------------------------------------------


                             if [ $pisces_diags == 1 ] ; then

  # get files
  t=${CONFCASE}_y${YEARDAT}_diadT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for var in PPdiatoms PPnano PPtot PNEWnano PNEWdiatoms PNEW  PH PAR ; do
    for dep in 0 150  ; do
      if [ $dep == 0 ]; then
        DEP=""
        LEV="-lev 1"
      else
        DEP="-dep $dep"
        LEV=""
      fi
      filout=${CONFIG}_${var}_${dep}_${YEAR}-${CASE}
      case $var in
        PPdiatoms) clrvar=PPPHY2 ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";
          case $dep in
            0) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          150) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          esac ;;
     PPnano) clrvar=PPPHY ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";
          case $dep in
            0) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          150) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          esac ;;
      PPtot) clrvar=PPPHY ; unit="molC/m3/s" ;CLRDATA="-clrdata PPPHY2.nc" ;
          case $dep in
            0) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          150) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          esac ;;
 PNEWnano) clrvar=PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";
          case $dep in
            0) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          150) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          esac ;;
PNEWdiatoms) clrvar=PPNEWD ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";
          case $dep in
            0) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          150) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          esac ;;
       PNEW) clrvar=PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata PPNEW2.nc" ;
          case $dep in
            0) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          150) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          esac ;;
         PH) clrvar=PH ; unit="" ;CLRDATA="-clrdata $t";
          case $dep in
            0) scale=1.e9   ; min=6 ; max=8  ; pas=1 ;;
          150) scale=1.e9   ; min=6 ; max=8  ; pas=1 ;;
          esac ;;
        PAR) clrvar=PAR ; unit="W/m2" ;CLRDATA="-clrdata $t";
          case $dep in
             0) scale=1   ; min=0 ; max=50  ; pas=10 ;;
           150) scale=1   ; min=0 ; max=50  ; pas=10 ;;
           esac ;;
#        EXPsmall) clrvar=PMO ; unit="molC/m2/s" ;CLRDATA="-clrdata $t";
#                  case $dep in  
#                   0) scale=   ; min= ; max=  ; pas= ;;
#                 150) scale=   ; min= ; max=  ; pas= ;;
#                  esac ;;
#          EXPbig) clrvar=PMO2 ; unit="molC/m2/s" ;CLRDATA="-clrdata $t";
#                  case $dep in  
#                   0) scale=   ; min= ; max=  ; pas= ;;
#                 150) scale=   ; min= ; max=  ; pas= ;;
#                  esac ;;
#          EXPtot) clrvar=PMO ; unit="molC/m2/s" ; CLRDATA="-clrdata PMO2.nc" ;
#                  case $dep in  
#                   0) scale=   ; min= ; max=  ; pas= ;;
#                 150) scale=   ; min= ; max=  ; pas= ;;
#                  esac ;;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        # Compute total CHL if necessary
        case $var in
        PPtot) add_var $s PPPHY2 PPPHY PPPHY PPPHY2.nc ;;
         PNEW) add_var $s PPNEWN PPNEWD PPNEW PPNEW2.nc ;;
#       EXPtot) add_var $t PMO PMO2 PMO PMO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING1=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${YEAR}"
        STRING2=" -string 0.5 0.80 1.0 0 ${unit}*${scale}_DEPTH=@CLR_DEPTH@"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
    done
  done

  for var in DICFlx O2Flx DeltaCO2 Fedep; do
      filout=${CONFIG}_${var}_${YEAR}-${CASE}
      case $var in
          DICFlx) clrvar=Cflx ; unit="molC/m2/s" ;
                  scale=1.e8 ; min=-7 ; max=6  ; pas=1 ; PAL="-p $PALBLUE2RED3" ;;
           O2Flx) clrvar=Oflx ; unit="molC/m2/s" ;
                  scale=1.e8 ; min=-24 ; max=22  ; pas=2 ; PAL="-p $PALBLUE2RED3" ;;
        DeltaCO2) clrvar=Delc ; unit="uatm" ;
                  scale=1    ; min=-50 ; max=45  ; pas=5 ; PAL="-p $PALBLUE2RED3" ;;
           Fedep) clrvar=Fedep ; unit="molFe/m2/s" ;
                  scale=1.e14   ; min=0 ; max=16  ; pas=2 ; PAL="-p $PAL2";;
        esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${YEAR}"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
  done

  puttoarchtrc PISCES GLOBAL

                             fi
#----------------------------------------------------------------------------
# 16.1.3 : Global Plots of concentrations integrated between surface and 150m
#----------------------------------------------------------------------------

                             if [ $pisces_global_int == 1 ] ; then

  # get files
  t=${CONFCASE}_y${YEARDAT}_biovertmean.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for var in DIC TALK O2 PO4 Si NO3 Fer DOC CHL NCHL DCHL POC PHY PHYnano PHYdiatoms ZOO ZOOmeso ZOOmicro GOC SFe; do
      CLRDATA="-clrdata $t"
      filout=${CONFIG}_${var}_int0-150m_${YEAR}-${CASE}
      case $var in
        DIC) clrvar=vertmean_DIC ; unit="mol-C/L";CLRDATA="-clrdata $t";  min=2000 ; max=2200  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
       TALK) clrvar=vertmean_Alkalini ; unit="mol-Alk/L" ;CLRDATA="-clrdata $t";  min=2300 ; max=2400  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
         O2) clrvar=vertmean_O2 ; unit="mol-O2/L" ;CLRDATA="-clrdata $t";  min=200 ; max=300  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        PO4) clrvar=vertmean_PO4 ; unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=0 ; max=250  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
         Si) clrvar=vertmean_Si ; unit="mol-Si/L" ;CLRDATA="-clrdata $t";  min=0 ; max=80  ; pas=20 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        NO3) clrvar=vertmean_NO3 ; unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=0 ; max=250  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        Fer) clrvar=vertmean_Fer ; unit="mol-Fe/L" ;CLRDATA="-clrdata $t";  min=0 ; max=5  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e10 ;;
        DOC) clrvar=vertmean_DOC ; unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=4 ; max=26  ; pas=2 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        CHL) clrvar=vertmean_CHL ; unit="mgCHL/m3*10" ; CLRDATA="-clrdata CHL2.nc"; min=0 ; max=9  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
       NCHL) clrvar=vertmean_NCHL ; unit="mgCHL/m3*10"  ;CLRDATA="-clrdata $t";  min=0 ; max=9  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
       DCHL) clrvar=vertmean_DCHL ; unit="mgCHL/m3*10"  ;CLRDATA="-clrdata $t";  min=0 ; max=9  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
        POC) clrvar=vertmean_POC; unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=0 ; max=2 ; pas=0.5 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        PHY) clrvar=vertmean_PHY; unit="mol-C/L" ; CLRDATA="-clrdata PHY2.nc"; min=0 ; max=4 ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
    PHYnano) clrvar=vertmean_PHY; unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=0 ; max=2 ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
 PHYdiatoms) clrvar=vertmean_PHY2 unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=0 ; max=2 ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        ZOO) clrvar=vertmean_ZOO; unit="mol-C/L" ; CLRDATA="-clrdata ZOO2.nc"; min=0 ; max=3  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
   ZOOmicro) clrvar=vertmean_ZOO; unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=0 ; max=3  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
    ZOOmeso) clrvar=vertmean_ZOO2 unit="mol-C/L" ;CLRDATA="-clrdata $t";  min=0 ; max=3  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        GOC) clrvar=vertmean_GOC; unit="mol-C/L"  ;CLRDATA="-clrdata $t";  min=0 ; max=3  ; pas=0.5 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
        SFe) clrvar=vertmean_SFe; unit="mol-Fe/L"  ;CLRDATA="-clrdata $t";  min=0 ; max=0.5  ; pas=0.25 ; LEV="-lev 1" ; DEP="" ; scale=1.e10 ;;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $DIAGS $t
        # Compute total CHL if necessary
        case $var in
          CHL) add_var $t vertmean_NCHL vertmean_DCHL vertmean_CHL CHL2.nc ;;
          PHY) add_var $t vertmean_PHY vertmean_PHY2 vertmean_PHY PHY2.nc ;;
          ZOO) add_var $t vertmean_ZOO vertmean_ZOO2 vertmean_ZOO ZOO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${YEAR}_int0-150m"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
  done
  puttoarchtrc PISCES GLOBAL

                             fi
#----------------------------------------------------------------------------
# 16.1.4 : Global Plots of diagnostics integrated between surface and 150m
#----------------------------------------------------------------------------

                             if [ $pisces_diags_int == 1 ] ; then
  # get files
  t=${CONFCASE}_y${YEARDAT}_biovertmean.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for var in PPdiatoms PPnano PPtot PNEWnano PNEWdiatoms PNEW  PH PAR; do
      CLRDATA="-clrdata $t"
      filout=${CONFIG}_${var}_int0-150m_${YEAR}-${CASE}
      case $var in
  PPdiatoms) clrvar=vertmean_PPPHY2 ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
     PPnano) clrvar=vertmean_PPPHY ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
   PNEWnano) clrvar=vertmean_PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e9   ; min=0 ; max=1  ; pas=0.25 ;;
PNEWdiatoms) clrvar=vertmean_PPNEWD ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e9   ; min=0 ; max=1  ; pas=0.25 ;;
       PNEW) clrvar=vertmean_PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata PPNEW2.nc" ; scale=1.e9   ; min=0 ; max=1  ; pas=0.25 ;;
         PH) clrvar=vertmean_PH ; unit="" ;CLRDATA="-clrdata $t";  scale=1.e9   ; min=6 ; max=7  ; pas=0.25 ;;
        PAR) clrvar=vertmean_PAR ;CLRDATA="-clrdata $t"; unit="W/m2" ; scale=1   ; min=0 ; max=10  ; pas=1 ;;
#  EXPsmall) clrvar=vertmean_PMO ; unit="molC/m2/s" ; scale=   ; min= ; max=  ; pas= ;;
#    EXPbig) clrvar=vertmean_PMO2 ; unit="molC/m2/s" ; scale=   ; min= ; max=  ; pas= ;;
#    EXPtot) clrvar=vertmean_PMO ; unit="molC/m2/s" ; CLRDATA="-clrdata PMO2.nc" ; scale=   ; min= ; max=  ; pas= ;;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        # Compute total CHL if necessary
        case $var in
        PPtot) add_var $t vertmean_PPPHY2 vertmean_PPPHY vertmean_PPPHY PPPHY2.nc ;;
         PNEW) add_var $t vertmean_PPNEWN vertmean_PPNEWD vertmean_PPNEW PPNEW2.nc ;;
#          EXPtot) add_var $t vertmean_PMO vertmean_PMO2 vertmean_PMO PMO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${YEAR}_int0-150m"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
  done

  puttoarchtrc PISCES GLOBAL

                             fi


#----------------------------------------------------------------
# 16.1.5 PISCES global comparison to climatologies on the website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_global_clim == 1 ] ; then

  # generic function for moc plot
  gloclimplt() { set -x ; $CHART -forcexy $CLRDATA -scale $scale   \
                 -clrvar $clrvar -english -noint -ijgrid -noproj $CLRXYPAL \
                 $LEV -p $PAL2 $STRING -clrmark zclrmark  -xyplot $POS -o $1 $MEAN $FORMAT; }

  # get files
  filerun=${CONFCASE}_y${YEARDAT}_ptrcT.nc

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

  puttoarch PISCES/CLIM

                           fi


#-------------------
# 16.2  PISCES zooms 
#-------------------

  # generic function for PISCES zooms plot
  baspiplt() { set -x ; $CHART -forcexy $ZOOM $CLRDATA -scale $scale   \
               -clrvar $clrvar -english  \
               $LEV $DEP $PAL $STRING $CLRLIM  $MEAN $FORMAT; }

  add_var() { ncap -O -s "$4=$2+$3" $1 $5 ;}

#-------------------------------------------------------------------------------------------------------------------------
# 16.2.1 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots of surface and at 150m depth concentrations
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                            if [ $pisces_zooms == 1 ] ; then
  # get files
  t=${CONFCASE}_y${YEARDAT}_ptrcT.nc
  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for BASIN in KERGUELEN ; do
    case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) ZOOM='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac
  listplt=' '
  for var in DIC TALK O2 PO4 Si NO3 Fer DOC CHL NCHL DCHL POC PHY PHYnano PHYdiatoms ZOO ZOOmeso ZOOmicro GOC SFe; do
    CLRDATA="-clrdata $t"
    for dep in 0 150  ; do
      if [ $dep == 0 ]; then
        DEP=""
        LEV="-lev 1"
      else
        DEP="-dep $dep"
        LEV=""
      fi
      filout=${CONFIG}_${var}_${dep}_${bas}_${YEAR}-${CASE}
      case $var in
        DIC) clrvar=DIC ; unit="mol-C/L"
          case $dep in
            0)  min=2000 ; max=2200  ; pas=50 ; scale=1.e6 ;;
          150)  min=2050 ; max=2250  ; pas=100 ; scale=1.e6 ;;
          esac ;;
       TALK) clrvar=Alkalini ; unit="mol-Alk/L"
          case $dep in
            0)  min=2300 ; max=2400  ; pas=50 ; scale=1.e6 ;;
          150)  min=2300 ; max=2400  ; pas=50 ; scale=1.e6 ;;
          esac ;;
         O2) clrvar=O2 ; unit="mol-O2/L"
          case $dep in
            0)  min=200 ; max=400  ; pas=50 ; scale=1.e6 ;;
          150)  min=200 ; max=300  ; pas=50 ; scale=1.e6 ;;
          esac ;;
        PO4) clrvar=PO4 ; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=200  ; pas=50 ; scale=1.e6 ;;
          150)  min=0 ; max=300  ; pas=50 ; scale=1.e6 ;;
          esac ;;
         Si) clrvar=Si ; unit="mol-Si/L"
          case $dep in
            0)  min=0 ; max=50  ; pas=10 ; scale=1.e6 ;;
          150)  min=0 ; max=100  ; pas=10 ; scale=1.e6 ;;
          esac ;;
        NO3) clrvar=NO3 ; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=150  ; pas=50 ; scale=1.e6 ;;
          150)  min=0 ; max=200  ; pas=50 ; scale=1.e6 ;;
          esac ;;
        Fer) clrvar=Fer ; unit="mol-Fe/L"
          case $dep in
            0)  min=0 ; max=5  ; pas=1 ; scale=1.e10 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e10 ;;
          esac ;;
        DOC) clrvar=DOC ; unit="mol-C/L"
          case $dep in
            0)  min=6 ; max=26  ; pas=2 ; scale=1.e6 ;;
          150)  min=6 ; max=18  ; pas=2 ; scale=1.e6 ;;
          esac ;;
        CHL) clrvar=CHL ; unit="mgCHL/m3*10" ; CLRDATA="-clrdata CHL2.nc"
          case $dep in
            0)  min=0 ; max=9  ; pas=1 ; scale=1.e7 ;;
          150)  min=0 ; max=2  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
       NCHL) clrvar=NCHL ; unit="mgCHL/m3*10"
          case $dep in
            0)  min=0 ; max=4 ; pas=1 ; scale=1.e7 ;;
          150)  min=0 ; max=1  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
       DCHL) clrvar=DCHL ; unit="mgCHL/m3*10"
          case $dep in
            0)  min=0 ; max=4  ; pas=1 ; scale=1.e7 ;;
          150)  min=0 ; max=1  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
        POC) clrvar=POC; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=1 ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=1 ; pas=0.5 ; scale=1.e6 ;;
          esac ;;
    PHYnano) clrvar=PHY; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=4 ; pas=1 ; scale=1.e6 ;;
          150)  min=0 ; max=0.5 ; pas=0.25 ; scale=1.e6 ;;
          esac ;;
 PHYdiatoms) clrvar=PHY2; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=4 ; pas=1 ; scale=1.e6 ;;
          150)  min=0 ; max=0.5 ; pas=0.25 ; scale=1.e6 ;;
          esac ;;
        PHY) clrvar=PHY; unit="mol-C/L" ; CLRDATA="-clrdata PHY2.nc"
          case $dep in
            0)  min=0 ; max=5 ; pas=1 ; scale=1.e6 ;;
          150)  min=0 ; max=1 ; pas=0.5 ; scale=1.e6 ;;
          esac ;;
        ZOO) clrvar=ZOO; unit="mol-C/L" ; CLRDATA="-clrdata ZOO2.nc"
          case $dep in
            0)  min=0 ; max=2  ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e7 ;;
          esac ;;
    ZOOmeso) clrvar=ZOO2; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=2  ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e7 ;;
          esac ;;
   ZOOmicro) clrvar=ZOO; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=2  ; pas=0.5 ; scale=1.e6 ;;
          150)  min=0 ; max=5  ; pas=1 ; scale=1.e7 ;;
          esac ;;
        GOC) clrvar=GOC; unit="mol-C/L"
          case $dep in
            0)  min=0 ; max=1  ; pas=0.5 ; scale=1.e7 ;;
          150)  min=0 ; max=1  ; pas=0.5 ; scale=1.e7 ;;
          esac ;;
        SFe) clrvar=SFe; unit="mol-Fe/L"
          case $dep in
            0)  min=0 ; max=1  ; pas=0.25 ; scale=1.e11 ;;
          150)  min=0 ; max=1  ; pas=0.25 ; scale=1.e11 ;;
          esac ;;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        # Compute total CHL if necessary
        case $var in
          CHL) add_var $t NCHL DCHL CHL CHL2.nc ;;
          PHY) add_var $t PHY PHY2 PHY PHY2.nc ;;
          ZOO) add_var $t ZOO ZOO2 ZOO ZOO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${bas}_${YEAR}_DEPTH=@CLR_DEPTH@"
        mklim $min $max $pas > zclrmark
        baspiplt  ; mkplt $filout
      fi
    done
  done

  puttoarchtrc PISCES $BASIN

done

                             fi
#--------------------------------------------------------------------------------------------
# 16.2.2 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots of diagnostics
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_diags_zooms == 1 ] ; then
  # get files
  t=${CONFCASE}_y${YEARDAT}_diadT.nc
 # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for BASIN in KERGUELEN ; do
    case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) ZOOM='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac
  listplt=' '

  for var in PPdiatoms PPnano PPtot PNEWnano PNEWdiatoms PNEW  PH PAR; do
    CLRDATA="-clrdata $t"
    for dep in 0 150  ; do
      if [ $dep == 0 ]; then
        DEP=""
        LEV="-lev 1"
      else
        DEP="-dep $dep"
        LEV=""
      fi
      filout=${CONFIG}_${var}_${dep}_${bas}_${YEAR}-${CASE}
      case $var in
  PPdiatoms) clrvar=PPPHY2 ; unit="molC/m3/s" ;CLRDATA="-clrdata $s";
          case $dep in
            0) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          150) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          esac ;;
     PPnano) clrvar=PPPHY ; unit="molC/m3/s" ;CLRDATA="-clrdata $s";
          case $dep in
            0) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          150) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          esac ;;
      PPtot) clrvar=PPPHY ; unit="molC/m3/s" ;CLRDATA="-clrdata PPPHY2.nc" ;
          case $dep in
            0) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          150) scale=1.e10   ; min=0 ; max=40  ; pas=10 ;;
          esac ;;
   PNEWnano) clrvar=PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata $s";
          case $dep in
            0) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          150) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          esac ;;
PNEWdiatoms) clrvar=PPNEWD ; unit="molC/m3/s" ;CLRDATA="-clrdata $s";
          case $dep in
            0) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          150) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          esac ;;
       PNEW) clrvar=PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata PPNEW2.nc" ;
          case $dep in
            0) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          150) scale=1.e9   ; min=0 ; max=10  ; pas=1 ;;
          esac ;;
         PH) clrvar=PH ; unit="" ;CLRDATA="-clrdata $s";
          case $dep in
            0) scale=1.e9   ; min=6 ; max=8  ; pas=1 ;;
          150) scale=1.e9   ; min=6 ; max=8  ; pas=1 ;;
          esac ;;
        PAR) clrvar=PAR ; unit="W/m2" ;CLRDATA="-clrdata $s";
          case $dep in
             0) scale=1   ; min=0 ; max=50  ; pas=10 ;;
           150) scale=1   ; min=0 ; max=50  ; pas=10 ;;
           esac ;;
#        EXPsmall) clrvar=PMO ; unit="molC/m2/s" ;CLRDATA="-clrdata $s";
#                  case $dep in  
#                   0) scale=   ; min= ; max=  ; pas= ;;
#                 150) scale=   ; min= ; max=  ; pas= ;;
#                  esac ;;
#          EXPbig) clrvar=PMO2 ; unit="molC/m2/s" ;CLRDATA="-clrdata $s";
#                  case $dep in  
#                   0) scale=   ; min= ; max=  ; pas= ;;
#                 150) scale=   ; min= ; max=  ; pas= ;;
#                  esac ;;
#          EXPtot) clrvar=PMO ; unit="molC/m2/s" ; CLRDATA="-clrdata PMO2.nc" ;
#                  case $dep in  
#                   0) scale=   ; min= ; max=  ; pas= ;;
#                 150) scale=   ; min= ; max=  ; pas= ;;
#                  esac ;;

      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        # Compute total CHL if necessary
        case $var in
        PPtot) add_var $s PPPHY2 PPPHY PPPHY PPPHY2.nc ;;
         PNEW) add_var $s PPNEWN PPNEWD PPNEW PPNEW2.nc ;;
#          EXPtot) add_var $s PMO PMO2 PMO PMO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${bas}_${YEAR}_DEPTH=@CLR_DEPTH@"
        mklim $min $max $pas > zclrmark
        baspiplt  ; mkplt $filout
      fi
    done
  done

  puttoarchtrc PISCES $BASIN

done

                             fi

#--------------------------------------------------------------------------------------------------------------------------------
# 16.2.3 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots concentrations integrated between surface and 150m
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                              if [ $pisces_zooms_int == 1 ] ; then
  # get files
  t=${CONFCASE}_y${YEARDAT}_biovertmean.nc


  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for BASIN in KERGUELEN ; do
    case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) ZOOM='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac
  listplt=' '

  for var in DIC TALK O2 PO4 Si NO3 Fer DOC CHL NCHL DCHL POC PHY ZOO GOC SFe; do
      CLRDATA="-clrdata $t"
      filout=${CONFIG}_${var}_int0-150m_${bas}_${YEAR}-${CASE}
      case $var in
        DIC) clrvar=vertmean_DIC ; unit="mol-C/L";CLRDATA="-clrdata $t"; min=2000 ; max=2300  ; pas=100 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
       TALK) clrvar=vertmean_Alkalini ; unit="mol-Alk/L";CLRDATA="-clrdata $t";  min=2300 ; max=2350  ; pas=25 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
         O2) clrvar=vertmean_O2 ; unit="mol-O2/L";CLRDATA="-clrdata $t";  min=200 ; max=300  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        PO4) clrvar=vertmean_PO4 ; unit="mol-C/L";CLRDATA="-clrdata $t";  min=0 ; max=250  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
         Si) clrvar=vertmean_Si ; unit="mol-Si/L";CLRDATA="-clrdata $t";  min=0 ; max=50  ; pas=10 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        NO3) clrvar=vertmean_NO3 ; unit="mol-C/L";CLRDATA="-clrdata $t";  min=0 ; max=250  ; pas=50 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        Fer) clrvar=vertmean_Fer ; unit="mol-Fe/L";CLRDATA="-clrdata $t";   min=0 ; max=10  ; pas=2 ; LEV="-lev 1" ; DEP="" ; scale=1.e10 ;;
        DOC) clrvar=vertmean_DOC ; unit="mol-C/L";CLRDATA="-clrdata $t";  min=4 ; max=20  ; pas=2 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        CHL) clrvar=vertmean_CHL ; unit="mgCHL/m3*10" ; CLRDATA="-clrdata CHL2.nc"; min=0 ; max=9  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
       NCHL) clrvar=vertmean_NCHL ; unit="mgCHL/m3*10";CLRDATA="-clrdata $t";  min=0 ; max=5  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
       DCHL) clrvar=vertmean_DCHL ; unit="mgCHL/m3*10";CLRDATA="-clrdata $t";  min=0 ; max=5  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
        POC) clrvar=vertmean_POC; unit="mol-C/L";CLRDATA="-clrdata $t";  min=0 ; max=1 ; pas=0.5 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        PHY) clrvar=vertmean_PHY; unit="mol-C/L" ; CLRDATA="-clrdata PHY2.nc"; min=0 ; max=2 ; pas=0.5 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
    PHYnano) clrvar=vertmean_PHY; unit="mol-C/L" ;CLRDATA="-clrdata $t";   min=0 ; max=10 ; pas=2 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
 PHYdiatoms) clrvar=vertmean_PHY2; unit="mol-C/L" ;CLRDATA="-clrdata $t";   min=0 ; max=10 ; pas=2 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        ZOO) clrvar=vertmean_ZOO; unit="mol-C/L" ; CLRDATA="-clrdata ZOO2.nc"; min=0 ; max=2  ; pas=0.5 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
    ZOOmeso) clrvar=vertmean_ZOO2; unit="mol-C/L" ;CLRDATA="-clrdata $t";   min=0 ; max=5  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
   ZOOmicro) clrvar=vertmean_ZOO; unit="mol-C/L" ;CLRDATA="-clrdata $t";   min=0 ; max=5  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e6 ;;
        GOC) clrvar=vertmean_GOC; unit="mol-C/L";CLRDATA="-clrdata $t";  min=0 ; max=3  ; pas=1 ; LEV="-lev 1" ; DEP="" ; scale=1.e7 ;;
        SFe) clrvar=vertmean_SFe; unit="mol-Fe/L";CLRDATA="-clrdata $t";  min=0 ; max=1  ; pas=0.25 ; LEV="-lev 1" ; DEP="" ; scale=1.e11 ;;
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        ##ncks -a -d x,1,721 $t $t
        # Compute total CHL if necessary
        case $var in
          CHL) add_var $t vertmean_NCHL vertmean_DCHL vertmean_CHL CHL2.nc ;;
          PHY) add_var $t vertmean_PHY vertmean_PHY2 vertmean_PHY PHY2.nc ;;
          ZOO) add_var $t vertmean_ZOO vertmean_ZOO2 vertmean_ZOO ZOO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${bas}_${YEAR}_int0-150m"
        mklim $min $max $pas > zclrmark
        baspiplt  ; mkplt $filout
      fi
  done

  puttoarchtrc PISCES $BASIN

done

                             fi
#--------------------------------------------------------------------------------------------------------------------------------
# 16.2.4 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots diagnostics integrated between surface and 150m
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                              if [ $pisces_zooms_diags_int == 1 ] ; then
  # get files
  t=${CONFCASE}_y${YEARDAT}_biovertmean.nc


  # Common values for this group of plots
  PAL="-p $PAL1"
  CLRLIM="-clrmark zclrmark"
  MEAN=""

  # Set options
  for BASIN in KERGUELEN ; do
    case $BASIN in
          DRAKE) ZOOM='-zoom -100 -20 -70 -30' ; bas=DRAK ;;
      KERGUELEN) ZOOM='-zoom 30 110 -70 -30'   ; bas=KERG ;;
       CAMPBELL) ZOOM='-zoom 100 180 -70 -30'  ; bas=CAMP ;;
    esac
  listplt=' '

  for var in PPdiatoms PPnano PPtot PNEWnano PNEWdiatoms PNEW  PH PAR; do
      CLRDATA="-clrdata $t"
      filout=${CONFIG}_${var}_int0-150m_${bas}_${YEAR}-${CASE}
      case $var in
  PPdiatoms) clrvar=vertmean_PPPHY2 ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e10   ; min=0 ; max=20  ; pas=10 ;;
     PPnano) clrvar=vertmean_PPPHY ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e10   ; min=0 ; max=20  ; pas=10 ;;
      PPtot) clrvar=vertmean_PPPHY ; unit="molC/m3/s" ;CLRDATA="-clrdata PPPHY2.nc" ; scale=1.e10   ; min=0 ; max=30  ; pas=10 ;;
   PNEWnano) clrvar=vertmean_PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e9   ; min=0 ; max=1  ; pas=0.5 ;;
PNEWdiatoms) clrvar=vertmean_PPNEWD ; unit="molC/m3/s" ;CLRDATA="-clrdata $t";  scale=1.e9   ; min=0 ; max=1  ; pas=0.5 ;;
       PNEW) clrvar=vertmean_PPNEWN ; unit="molC/m3/s" ;CLRDATA="-clrdata PPNEW2.nc" ; scale=1.e9   ; min=0 ; max=3  ; pas=1 ;;
         PH) clrvar=vertmean_PH ; unit="" ;CLRDATA="-clrdata $t";  scale=1.e9   ; min=6 ; max=7  ; pas=0.25 ;;
        PAR) clrvar=vertmean_PAR ; unit="W/m2" ;CLRDATA="-clrdata $t";  scale=1   ; min=0 ; max=10  ; pas=1 ;;
#        EXPsmall) clrvar=vertmean_PMO ; unit="molC/m2/s" ; scale=   ; min= ; max=  ; pas= ;;
#          EXPbig) clrvar=vertmean_PMO2 ; unit="molC/m2/s" ; scale=   ; min= ; max=  ; pas= ;;
#          EXPtot) clrvar=vertmean_PMO ; unit="molC/m2/s" ; CLRDATA="-clrdata PMO2.nc" ; scale=   ; min= ; max=  ; pas= ;;
#        
      esac
      if [ $( chkfile $PLOTDIR/PISCES/GLOBAL/$filout.cgm) == absent ] ; then
        rapatrie $t $MEANY $t
        ##ncks -a -d x,1,721 $t $t
        # Compute total CHL if necessary
        case $var in
        PPtot) add_var $t vertmean_PPPHY2 vertmean_PPPHY vertmean_PPPHY PPPHY2.nc ;;
         PNEW) add_var $t vertmean_PPNEWN vertmean_PPNEWD vertmean_PPNEW PPNEW2.nc ;;
#          EXPtot) add_var $s vertmean_PMO vertmean_PMO2 vertmean_PMO PMO2.nc ;;
           * ) skip=1 ;;
        esac
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${bas}_${YEAR}_int0-150m"
        mklim $min $max $pas > zclrmark
        baspiplt  ; mkplt $filout
      fi
  done

  puttoarchtrc PISCES $BASIN

done

                             fi



#--------------------
# 16.3  PISCES coupes 
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# 16.3.1 PISCES coupes australes 
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_coupes_aus == 1 ] ; then

  # Generic function for coupes_aus plot
  # Top panel (0<depth<1000m zoom)
  coupitop() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -1000 -clrvar $clrvar \
               $CNTDATA -cntvar $clrvar $CNTLIM -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
               -xyplot 0.1 0.95 0.70 0.95 -o gmetatop -nolon -ylat 0.10 -english $CLRMARK $PAL -scale $scale \
               -clrxypal 0.1 0.95 0.08 0.18 -string 0.5 0.98 1.0 0 ${CONFIG}-${CASE}_${var}_${unit}*${scale}_${SECTION}_${YEAR} ; }
  # Bottom panel (0<depth<1000m)
  coupibot() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -pmax -5500 -clrvar $clrvar \
               $CNTDATA -cntvar $clrvar $CNTLIM  -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
               -xyplot 0.1 0.95 0.20 0.68 -o gmetabot -nolon -ylat 0.10  -english $CLRMARK $PAL \
               -clrxypal 0.1 0.95 0.08 0.18 -zstep 500 -scale $scale ; } 


  # get files
  t=${CONFCASE}_y${YEARDAT}_ptrcT.nc

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
        filout=${CONFIG}_${var}_${YEAR}-${CASE}
        mkplt $filout
        rm gmeta*
     fi
    done
  done

  puttoarch PISCES/SECTIONS

                             fi

#---------------------------------------------------------
# 16.3.2 PISCES coupes australes compared to climatologies 
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_coupes_clim == 1 ] ; then

  # Generic function for coupes_aus plot
  coupicomp() { $COUPE $CLRDATA -pts $LONmin $LONmax $LATmin $LATmax $LEV -forcexy -clrvar $clrvar \
                $CNTDATA -cntvar $clrvar $CNTLIM -cntrc1 0.04 -cntrc2 0.07 -cntlis 1 -cntlls 0.013 \
                -xyplot $POS -o $1 -english $CLRMARK $PAL -scale $scale \
                -nolon $CLRXYPAL $STRING -pmax -5500 ; }


  # get files
  filerun=${CONFCASE}_y${YEARDAT}_ptrcT.nc

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
   
  puttoarch PISCES/CLIM

                           fi

#------------------------------------------------------------------------------
# 16.4.1. PISCES fluxes on website
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                             if [ $pisces_fluxes == 1 ] ; then

  # Generic function   
  glopiplt() { set -x ; $CHART -forcexy $CLRDATA \
           -clrvar $clrvar   -english -scale $scale  \
            $LEV $DEP $PAL $STRING $CLRLIM $MEAN $FORMAT; }

  # get files
  t=${CONFCASE}_y${YEARDAT}_diadT.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=' '

  # Common values for this group of plots
  CLRLIM="-clrmark zclrmark"
  CLRDATA="-clrdata $t"
  MEAN="" ; DEP="" ; LEV="" ; FORMAT='-format PALETTE I4'

  # Set options
  for var in DICFlx O2Flx DeltaCO2 PPdiatoms PPnano ; do
      filout=${CONFIG}_${var}_${dep}_${YEAR}-${CASE}
      filout=${CONFIG}_${var}_${YEAR}-${CASE}
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
        STRING=" -string 0.5 0.95 1.0 0 ${CONFCASE}_${var}_${unit}*${scale}_${YEAR}"
        mklim $min $max $pas > zclrmark
        glopiplt  ; mkplt $filout
      fi
  done

  puttoarch PISCES/GLOBAL

                             fi
#------------------------------------------------------------------------------
# 17. MXL_OBS = MXL comparison to Boyer-Montegut climatology on the website
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
    mxl_run=${CONFCASE}_y${YEAR}m${mm}${xiosid}_MXL.nc
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

   puttoarch MXL

                             fi
#------------------------------------------------------------------------------
# 18. Contour tool
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                  if [ $contour == 1 ] ; then

  # Generic function to create the contour file  
  mkcnt() { set -x
            $CHART $CNTDATA -cntvar $cntvar $ZOOM $PROJ $DEP $CNTLIM $CNTSAV > infolayer.txt
            LAYER=`cat infolayer.txt | grep Layer | uniq | awk '{print $10}'`
            PROF=`ncks -F -d deptht,$LAYER,$LAYER -v deptht $t | tail -2 | head -1 | awk -F= '{print $2}' | awk '{print $1}'`
            echo "# AREA : $AREA" > $fileout.txt
            echo "# CONFCASE : $CONFCASE" >> $fileout.txt
            echo "# PERIOD : $YEAR" >> $fileout.txt
            echo "# LAYER : $LAYER" >> $fileout.txt
            echo "# DEPTH : $PROF m" >> $fileout.txt
            echo "# VARIABLE : $cntvar" >> $fileout.txt
            echo "# ISOTHERM : $ISOTHERM" >> $fileout.txt
            cat cntlimit.txt >> $fileout.txt
            \rm cntlimit.txt infolayer.txt gmeta
          }

  # Generic function to create the plot with contour + SSH/SPEED/STD(T)/MXL
  mkcntplt() { set -x
               $CHART $CLRDATA -clrvar $clrvar $ZOOM $PROJ $CLRMARK -xyplot 0.1 0.95 0.24 0.86 $OPTIONS -overdata $fileout.txt -overlw 2 $PAL -string 0.5 0.95 1 0 $TITLE1 -string 0.5 0.9 1 0 $TITLE2 -string 0.5 0.15 1 0 $STRING  
             }

  # get files
  t=${CONFCASE}_y${YEARDAT}_gridT.nc
  u=${CONFCASE}_y${YEARDAT}_gridU.nc
  v=${CONFCASE}_y${YEARDAT}_gridV.nc
  stdt=${CONFCASE}_y${YEARDAT}_gridT_STD.nc

  # reset the list of plots that are produced ( list is updated in mkplt )
  listplt=''

  cntvar=votemper
  AREA="GULFSTREAM"
  case $AREA in
    GULFSTREAM) ZOOM="-zoom -82 -50 24 43" ;
                ISOTHERM=17 ;
                NEAREST_DEPTH=180 ;;
    KUROSHIO)   ZOOM="-zoom 110 140 15 35" ;
                ISOTHERM=17 ;
                NEAREST_DEPTH=180 ;;
    ACC)        ZOOM="-zoom -180 178 -90 -40" ;
                ISOTHERM=3 ; # to be defined 3 5 7 9 ?
                NEAREST_DEPTH=380 ;;
  esac

  DEP="-ndep $NEAREST_DEPTH"
  PROJ="-proj ME -noint"
  CNTDATA="-cntdata $t"
  CNTLIM="-cntlim limit.dat"
  CNTSAV="-cntsav cntlimit.txt"
cat << eof > limit.dat
$ISOTHERM
eof
  fileout=${CONFCASE}_y${YEAR}_cnt_${cntvar}_${ISOTHERM}_D${NEAREST_DEPTH}_${AREA}

cat << eof > sshclrmark
-0.9
-0.6
-0.3
0
0.3
0.6
0.9
eof
cat << eof > speedclrmark
-2
-1.69897
-1.52288
-1.30103
-1
-0.69897
-0.52288
-0.30103
0
0.17609
eof
cat << eof > stdtclrmark
0
0.3
0.6
0.9
1.2
1.5
1.8
eof

    rapatrie $t $MEANY $t
    rapatrie $u $MEANY $u
    rapatrie $v $MEANY $v

    CLRDATA="-clrdata $t"
    mkcnt
    
    CLRDATA="-clrdata $t"
    clrvar=sossheig
    CLRMARK="-clrmark sshclrmark"
    OPTIONS="-clrmean 0"
    PAL="-format PALETTE f5.2 -p $PALBLUE2RED5"
    TITLE1="${cntvar}:${ISOTHERM}_(${PROF}m)"
    TITLE2="${CONFCASE}_${YEAR}"
    STRING="SSH_in_m_(-clrmean_0_option)"
    mkcntplt
    sshfilout=${CONFIG}_${cntvar}_${ISOTHERM}_D${NEAREST_DEPTH}_SSH_${AREA}_${YEAR}-${CASE}
    mkplt $sshfilout

	cdfvita -u $u -v $v -t $t -lev $LAYER
    CLRDATA="-clrdata vita.nc"
    clrvar=sovitmod
    CLRMARK="-clrmark speedclrmark"
    OPTIONS="-log10"
    PAL="-format PALETTE f5.2 -p $PALBLUE2WHITE"
    STRING="Horizontal_velocity_at_@CLR_DEPTH@m_in_m/s"
    mkcntplt
    \rm vita.nc
    speedfilout=${CONFIG}_${cntvar}_${ISOTHERM}_D${NEAREST_DEPTH}_SPEED_${AREA}_${YEAR}-${CASE}
    mkplt $speedfilout

    if [ $clim == 1 ] ; then
      rapatrie $stdt $MEANY $stdt
      CLRDATA="-clrdata $stdt"
      clrvar=votemper_std
      CLRMARK="-clrmark stdtclrmark"
      OPTIONS="-lev $LAYER"
      PAL="-format PALETTE f5.2 -p $PALBLUE2RED5"
      STRING="Interannual_Std(T)_at_@CLR_DEPTH@m_in_deg"
      mkcntplt
      stdtfilout=${CONFIG}_${cntvar}_${ISOTHERM}_D${NEAREST_DEPTH}_STDT_${AREA}_${YEAR}-${CASE}
      mkplt $stdtfilout
    fi

  chkdirg $DIAGS/CONTOURS
  expatrie $fileout.txt $DIAGS/CONTOURS $fileout.txt
  puttoarch CONTOURS
                  fi

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
# 19. Surface circulation in the Alboran sea + salinity
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        if [ $alboran == 1 ] ; then
  alboran() { $CHART -string 0.5 0.94 1.0 0 "${CONFCASE}_${var}_${YEAR} @CLR_DEPTH@ m "\
        -proj ME -xstep 2 -ystep 2 -xgrid -ygrid -zoom -8 0 33 38  -p $PAL2 -english \
       -clrdata $t -clrvar vosaline -clrmark zclrmark   $FORMAT  \
       -vecdatax $u -vecdatay $v -vecvarx vozocrtx -vecvary vomecrty -Cgrid \
       -vecvfr 0.3 -vecvrl 250 -lev 3 ;}
#------------
listplt=' '
t=${CONFCASE}_y${YEARDAT}_gridT.nc
u=${CONFCASE}_y${YEARDAT}_gridU.nc
v=${CONFCASE}_y${YEARDAT}_gridV.nc


min=35.8 ; max=36.4 ; pas=0.1 ;  mklim $min $max $pas > zclrmark
var=ALBORAN
filout=${CONFIG}_${var}_${YEAR}-${CASE}

if [ $( chkfile $PLOTDIR/MEDSEA/$filout.cgm )  == absent ] ; then
   rapatrie $u $MEANY $u
   rapatrie $v $MEANY $v
   rapatrie $t $MEANY $t
   alboran 

   mkplt $filout
fi

 puttoarch  MEDSEA
                        fi
#==============================================================================
#==============================================================================
#==============================================================================
#==============================================================================
