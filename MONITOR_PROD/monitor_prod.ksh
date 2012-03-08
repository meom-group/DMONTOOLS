#!/bin/ksh
set -x
# This script is intended to be sourced from a main script. Not Stand Alone
# Basically it runs on the production machine, once the MEAN fields 
# have been computed (monthly, annual) and disposed on the respective 
# CONFIG-CASE-MEAN/YEAR/ directory.

# Each block corresponds to a particular monitoring task. Each block is supposed
# to be independant from the other (in particular, required file are downloaded
# via the rapatrie function, which does the job only if necessary.

# The different tasks are performed with the cdftools programs. CDFTOOLS is 
# added to the PATH.

#--------------------------------------------------------------
#   $Rev$
#   $Date$
#   $Id$
#--------------------------------------------------------------

if [ $# = 0 ] ; then
   echo 'USAGE : monitor_prod.ksh year'
   exit 1
fi

# take YEAR as the argument to this script
YEAR=$1

# define some config dependent variable 
. ./config_def.ksh    # can be a link

#  Define some functions to get/put file from/to gaya (can be easily customized)
. ./function_def.ksh  # can be a link

# monitor_prod shoud be run in individual directory for each year to
# allow parallel processing
chkdir $YEAR

# prepare the section.dat and dens_section.dat
  cp drakkar_sections_table.txt drakkar_trpsig_table.txt $YEAR/

# Function to create taglist for frequency of diagnostics (monthly, annual, or both)
mktaglist(){
   taglist=''
   case $1 in
      1) taglist="${taglist} y${YEAR}" ;;
      2) for m in $(seq 1 12); do
            mm=$( printf "%02d" $m )
            taglist="${taglist} y${YEAR}m${mm}"
         done ;;
      3) taglist="${taglist} y${YEAR}"
         for m in $(seq 1 12); do
            mm=$( printf "%02d" $m )
            taglist="${taglist} y${YEAR}m${mm}"
         done ;;
   esac
   echo "$taglist"
            }

   # Function to check if you are in monthly or yearly case and define corresponding DIAGS directory
define_diags_dir(){
   monthly=$( echo $TAG | awk '{ print index($1,"m") }' )
   if [ $monthly == 0 ] ; then
     DIAGSTXT=$DIAGS/TXT
     DIAGSNC=$DIAGS/NC
   else
     DIAGSTXT=$DIAGS/MONTHLY/TXT
     DIAGSNC=$DIAGS/MONTHLY/NC
   fi
                  }

cd $YEAR
#------------------------------------------------------------------------------
# directory name frequently used:
#------------------------------------------------------------------------------
  # on the storage machine : path relative to the root of S-machine
  MEANY=$CONFIG/${CONFCASE}-MEAN/$YEAR
  SDIRY=$CONFIG/${CONFCASE}-S/$YEAR
  DIAGS=${CONFIG}/${CONFCASE}-DIAGS
  IDIR=$CONFIG/${CONFIG}-I

  # on the production machine
  P_CTL=$HOME/RUN_${CONFIG}/${CONFCASE}/CTL

  # check existence of some required directories
  # ... on gaya
  chkdirg $CONFIG
  chkdirg $DIAGS
  chkdirg $DIAGS/TXT    # for ASCII diag files (to become obsolete)
  chkdirg $DIAGS/NC     # for NetCdf diag files
  chkdirg $DIAGS/MONTHLY
  chkdirg $DIAGS/MONTHLY/TXT
  chkdirg $DIAGS/MONTHLY/NC

#------------------------------------------------------------------------------
# PATH:
#-----------------------------------------------------------------------------
  export PATH=$CDFTOOLS/:$PATH

# check if required cdftools are available, exit if missing
  err=0
  for cdfprog in cdfeke cdfmean cdfrmsssh cdfstdevw cdficediags cdftransport\
                  cdfmhst cdfhflx cdfmoc cdfmaxmoc  cdfpsi  cdfsigtrp cdfmxl \
                  cdfzonalmean cdfzonalsum cdfzonalout cdfvita cdffindij cdfprofile ; do
     if [ ! -x $CDFTOOLS/$cdfprog ] ; then
       err=$(( err + 1 ))
       echo $cdfprog executable missing. Check your $CDFTOOLS installation
     fi
  done

  if [ $err != 0 ] ; then 
     echo " monitoring cannot be performed, sorry !" ; exit 1 
  fi

#=============================================================================
#  PART I: Derived quantities, to be added to the -MEAN/YEAR directory
#=============================================================================
   # check if we have an ORCA config or otherconfig (to be improved ....) orca=0 if not
   orca=$( echo 1 | awk '{ ii=index (config,"ORCA") ; print ii  }' config=$CONFIG )

# EKE : Eddy Kinetic Energy: Input files gridU, gridV gridU2, gridV2 
#^^^^^^^^^^^^^^^^^^^^^^^^^^
   for TAG in $(mktaglist $EKE) ; do
   
   # retrieve U and V ANNUAL mean files and squared mean
     rapatrie  ${CONFCASE}_${TAG}_gridU.nc  $MEANY ${CONFCASE}_${TAG}_gridU.nc
     rapatrie  ${CONFCASE}_${TAG}_gridU2.nc $MEANY ${CONFCASE}_${TAG}_gridU2.nc
     rapatrie  ${CONFCASE}_${TAG}_gridV.nc  $MEANY ${CONFCASE}_${TAG}_gridV.nc
     rapatrie  ${CONFCASE}_${TAG}_gridV2.nc $MEANY ${CONFCASE}_${TAG}_gridV2.nc
   
   # retrieve a T file needed for headers only (EKE is computed on the T-point)
     rapatrie  ${CONFCASE}_${TAG}_gridT2.nc $MEANY  ${CONFCASE}_${TAG}_gridT2.nc

   cdfeke ${CONFCASE}_${TAG}_gridU.nc \
     ${CONFCASE}_${TAG}_gridU2.nc \
     ${CONFCASE}_${TAG}_gridV.nc \
     ${CONFCASE}_${TAG}_gridV2.nc \
     ${CONFCASE}_${TAG}_gridT2.nc

   # dispose file on the MEAN directory
   expatrie eke.nc $MEANY  ${CONFCASE}_${TAG}_EKE.nc
   \rm eke.nc
  done 

# RMS SSH and StdDev W : Input files : gridT, gridT2  gridW, gridW2
#^^^^^^^^^^^^^^^^^^^^^^^
  for TAG in $(mktaglist $RMSSSH) ; do
   
   # RMSSSH :get gridT gridT2
   rapatrie  ${CONFCASE}_${TAG}_gridT.nc $MEANY  ${CONFCASE}_${TAG}_gridT.nc
   rapatrie  ${CONFCASE}_${TAG}_gridT2.nc $MEANY  ${CONFCASE}_${TAG}_gridT2.nc
   cdfrmsssh  ${CONFCASE}_${TAG}_gridT.nc ${CONFCASE}_${TAG}_gridT2.nc

   # dispose file on the MEAN directory
   expatrie rms.nc $MEANY  ${CONFCASE}_${TAG}_RMSSSH.nc
   \rm rms.nc

   # StdDev W :get gridW and gridW2 files
   rapatrie ${CONFCASE}_${TAG}_gridW.nc $MEANY ${CONFCASE}_${TAG}_gridW.nc
   rapatrie ${CONFCASE}_${TAG}_gridW2.nc $MEANY ${CONFCASE}_${TAG}_gridW2.nc

   cdfstdevw  ${CONFCASE}_${TAG}_gridW.nc ${CONFCASE}_${TAG}_gridW2.nc

   # dispose file on the MEAN directory
   expatrie rmsw.nc $MEANY ${CONFCASE}_${TAG}_STDEVW.nc
   \rm rmsw.nc
  done

# Barotropic Transport: Input file: gridU, gridV mesh mask
#^^^^^^^^^^^^^^^^^^^^^
  for TAG in $(mktaglist $BSF) ; do 
   
   # get gridU gridV files
   rapatrie ${CONFCASE}_${TAG}_gridU.nc $MEANY ${CONFCASE}_${TAG}_gridU.nc
   rapatrie ${CONFCASE}_${TAG}_gridV.nc $MEANY ${CONFCASE}_${TAG}_gridV.nc
 
   # get mesh mask files 
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
 
   cdfpsi ${CONFCASE}_${TAG}_gridU.nc ${CONFCASE}_${TAG}_gridV.nc 
 
   # dispose and rename on the MEAN directory
   expatrie psi.nc  $MEANY ${CONFCASE}_${TAG}_PSI.nc
   \rm psi.nc
  done

# MOC Meridional Overturning Circulation:  Input file: gridV, mesh mask, mask_glo
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  for TAG in $(mktaglist $MOC) ; do 
   # get gridV  files
   rapatrie ${CONFCASE}_${TAG}_gridV.nc $MEANY ${CONFCASE}_${TAG}_gridV.nc
 
   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi
 
   cdfmoc ${CONFCASE}_${TAG}_gridV.nc
 
   # dispose on gaya MEAN/YEAR directory
   expatrie moc.nc $MEANY ${CONFCASE}_${TAG}_MOC.nc
   \rm moc.nc
  done

# MOCSIG Meridional Overturning Circulation on sigma coordinates:  Input file: gridV, gridT, mesh mask, mask_glo
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

mkmocsig(){
     # get annual mean gridV and gridT files
     rapatrie ${CONFCASE}_${TAG}_gridV.nc $MEANY ${CONFCASE}_${TAG}_gridV.nc
     rapatrie ${CONFCASE}_${TAG}_gridT.nc $MEANY ${CONFCASE}_${TAG}_gridT.nc
     cdfmocsig ${CONFCASE}_${TAG}_gridV.nc ${CONFCASE}_${TAG}_gridT.nc $DREF
     expatrie mocsig.nc $MEANY ${CONFCASE}_${TAG}_MOCSIG_${NREF}.nc
           }

mkmocsig5d(){
     # get 5-day averaged gridV and gridT files
     rapatrie_5d gridV $SDIRY $TAG
     rapatrie_5d gridT $SDIRY $TAG
     for fileV in ${CONFCASE}_y${YEAR}m${MONTH}*d??_gridV.nc ; do
       fileT=$( echo $fileV | sed -e 's/gridV/gridT/'  )
       fileM=$( echo $fileV | sed -e 's/gridV/MOCSIG/'  )
       cdfmocsig $fileV $fileT $DREF
       mv mocsig.nc $fileM
     done
     cdfmoy ${CONFCASE}_y${YEAR}m${MONTH}*d??_MOCSIG.nc
     mv cdfmoy.nc mocsig.nc
     expatrie mocsig.nc $MEANY ${CONFCASE}_${TAG}_MOCSIG_5d_${NREF}.nc
             }

  for TAG in $(mktaglist $MOCSIG) ; do
   # Compute NREF
   if [ $DREF == 0 ]; then NREF=0 ; fi
   if [ $DREF == 1000 ]; then NREF=1 ; fi
   if [ $DREF == 2000 ]; then NREF=2 ; fi

   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
   if (( $orca == 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi

   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`

   case $mocsig_5d in
     0) mkmocsig ;;
     1) mkmocsig5d ;;
     2) mkmocsig ; mkmocsig5d ;;
   esac

  done

# Mixed Layer Diagnostics : Input file : gridT, mesh_hgr, mesh_zgr
#^^^^^^^^^^^^^^^^^^^^^^^^

mkmxl(){
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   listfiles=''
   for m in $*  ; do
     mm=$(printf "%02d" $m)
     f=${CONFCASE}_y${YEAR}m${mm}_gridT.nc
     g=$(echo $f | sed -e 's/gridT/MXL/')
     rapatrie $f $MEANY $f
     cdfmxl  $f
     # dispose on gaya, MEAN/YEAR directory
     mv mxl.nc $g
     expatrie $g $MEANY $g
     listfiles="$listfiles $g"
   done
        }

  case $MXL in
     1) taglist="3 9" ; mkmxl $taglist ;;
     2) taglist=$(seq 1 12) ; mkmxl $taglist ;;
     3) taglist=$(seq 1 12) ; mkmxl $taglist
        cdfmoy_weighted $listfiles
        h=${CONFCASE}_y${YEAR}_MXL.nc
        expatrie cdfmoy_weighted.nc $MEANY $h ;;
  esac

# Large scale potential vorticity: input file : gridT, and mesh_mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

mklspv(){
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
   listfiles=''
   for m in $* ; do
     mm=$(printf "%02d" $m)
     f=${CONFCASE}_y${YEAR}m${mm}_gridT.nc
     g=$(echo $f | sed -e 's/gridT/LSPV/')
     rapatrie $f $MEANY $f
     # compute LSPV
     cdfpvor  $f -lspv
     # dispose on gaya, MEAN/YEAR directory
     mv lspv.nc $g
     expatrie $g $MEANY $g
     listfiles="$listfiles $g"
   done
         }

   case $LSPV in
     1) taglist="3 9" ; mklspv $taglist ;;
     2) taglist=$(seq 1 12) ; mklspv $taglist ;;
     3) taglist=$(seq 1 12) ; mklspv $taglist
        cdfmoy_weighted $listfiles
        h=${CONFCASE}_y${YEAR}_LSPV.nc
        expatrie cdfmoy_weighted.nc $MEANY $h ;;
   esac

#=============================================================================
#  PART II: Time series: compute some integral quantities relevant for monitor
#           the ocean variability, and the behaviour of the on going run. 
#           Output is basically a small ASCII file, from which a matlab
#           suitable input file  (.mtl) is derived.
#=============================================================================
# Global MEANS: T S SSH Input files: gridT , mesh_hgr, mesh_zgr, mask
#^^^^^^^^^^^^^^

  mkannuallevitus(){
   if [ $(chkfile $DIAGS/NC/LEVITUS_y0000_TMEAN.nc ) == absent ] ; then
    # first time : Create header with Levitus equivalent
    # requires  LEVITUS 'same' diags (from the ANNUAL mean )
    #  !!! NEW !!!
    # get non-masked levitus then mask it with the same mask as the model
    levitus=${TSCLIM:=Levitus_p2.1}_1y_TS_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
    rapatrie $levitus $IDIR $levitus
    cdfmltmask $levitus  mask.nc votemper T             # votemper --> $levitus_masked
    cdfmltmask ${levitus}_masked  mask.nc vosaline T    # vosaline --> $levitus_masked_masked
    mv ${levitus}_masked_masked ${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
    levitus=${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
    #  
    cdfmean $levitus  votemper T  >  LEVITUS_y0000_TMEAN.txt  ; mv cdfmean.nc LEVITUS_y0000_TMEAN.nc
    cdfmean $levitus  vosaline T  >  LEVITUS_y0000_SMEAN.txt  ; mv cdfmean.nc LEVITUS_y0000_SMEAN.nc

    expatrie  LEVITUS_y0000_TMEAN.txt $DIAGS/TXT  LEVITUS_y0000_TMEAN.txt
    expatrie  LEVITUS_y0000_SMEAN.txt $DIAGS/TXT  LEVITUS_y0000_SMEAN.txt
    expatrie  LEVITUS_y0000_TMEAN.nc $DIAGS/NC  LEVITUS_y0000_TMEAN.nc
    expatrie  LEVITUS_y0000_SMEAN.nc $DIAGS/NC  LEVITUS_y0000_SMEAN.nc
   fi
                    }

  mkmonthlylevitus(){
   if [ $MONTH ] ; then
    if [ $(chkfile $DIAGS/MONTHLY/NC/LEVITUS_y0000m${MONTH}_TMEAN.nc ) == absent ] ; then
      # first time : Create header with Levitus equivalent
      # requires  LEVITUS 'same' diags (from the MONTHLY mean )
      # Contrary to ANNUAL means, for MONTHLY means we must have two distinct files (T and S): file size issue
      #  !!! NEW !!!
      # get non-masked levitus then mask it with the same mask as the model
      tlevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
      slevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
      rapatrie $tlevitus $IDIR $tlevitus
      rapatrie $slevitus $IDIR $slevitus
      tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
      smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
      ncks -F -d time_counter,${MONTH},${MONTH} $tlevitus $tmlevitus
      ncks -F -d time_counter,${MONTH},${MONTH} $slevitus $smlevitus
      cdfmltmask $tmlevitus  mask.nc votemper T      # votemper --> $levitus_masked
      cdfmltmask $smlevitus  mask.nc vosaline T      # vosaline --> $levitus_masked
      mv ${tmlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
      mv ${smlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
      tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
      smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
      #  
      cdfmean $tmlevitus  votemper T  >  LEVITUS_y0000m${MONTH}_TMEAN.txt  ; mv cdfmean.nc LEVITUS_y0000m${MONTH}_TMEAN.nc
      cdfmean $smlevitus  vosaline T  >  LEVITUS_y0000m${MONTH}_SMEAN.txt  ; mv cdfmean.nc LEVITUS_y0000m${MONTH}_SMEAN.nc

      expatrie  LEVITUS_y0000m${MONTH}_TMEAN.txt $DIAGS/MONTHLY/TXT  LEVITUS_y0000m${MONTH}_TMEAN.txt
      expatrie  LEVITUS_y0000m${MONTH}_SMEAN.txt $DIAGS/MONTHLY/TXT  LEVITUS_y0000m${MONTH}_SMEAN.txt
      expatrie  LEVITUS_y0000m${MONTH}_TMEAN.nc $DIAGS/MONTHLY/NC  LEVITUS_y0000m${MONTH}_TMEAN.nc
      expatrie  LEVITUS_y0000m${MONTH}_SMEAN.nc $DIAGS/MONTHLY/NC  LEVITUS_y0000m${MONTH}_SMEAN.nc
     fi
    fi
                    }

  for TAG in $(mktaglist $TSMEAN) ; do
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
  
   # get gridT files
   rapatrie ${CONFCASE}_${TAG}_gridT.nc $MEANY ${CONFCASE}_${TAG}_gridT.nc

   # output file name ascii and nc
   fsshmean=${CONFCASE}_${TAG}_SSHMEAN.txt  ; fsshmean_nc=${CONFCASE}_${TAG}_SSHMEAN.nc
   ftmean=${CONFCASE}_${TAG}_TMEAN.txt      ; ftmean_nc=${CONFCASE}_${TAG}_TMEAN.nc
   fsmean=${CONFCASE}_${TAG}_SMEAN.txt      ; fsmean_nc=${CONFCASE}_${TAG}_SMEAN.nc
   # set header on the output file (ASCII)
   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
   echo $YEAR $MONTH >  $fsshmean ; echo $YEAR $MONTH >  $ftmean ;  echo $YEAR $MONTH >  $fsmean

   # 3D means
   cdfmean  ${CONFCASE}_${TAG}_gridT.nc sossheig T >> $fsshmean ; mv cdfmean.nc $fsshmean_nc
   cdfmean  ${CONFCASE}_${TAG}_gridT.nc votemper T >> $ftmean   ; mv cdfmean.nc $ftmean_nc
   cdfmean  ${CONFCASE}_${TAG}_gridT.nc vosaline T >> $fsmean   ; mv cdfmean.nc $fsmean_nc
 
   # check if you are in monthly or yearly case and define corresponding diags directory
   define_diags_dir

   # dispose ASCII file in the -DIAGS directory
   expatrie  $fsshmean $DIAGSTXT $fsshmean
   expatrie  $ftmean   $DIAGSTXT $ftmean
   expatrie  $fsmean   $DIAGSTXT $fsmean

   # dispose ASCII file in the -DIAGS/NC directory
   expatrie  $fsshmean_nc $DIAGSNC $fsshmean_nc
   expatrie  $ftmean_nc   $DIAGSNC $ftmean_nc
   expatrie  $fsmean_nc   $DIAGSNC $fsmean_nc

   case $TSMEAN in
      1) mkannuallevitus ;;
      2) mkmonthlylevitus ;;
      3) mkannuallevitus
         mkmonthlylevitus ;;
   esac

  done

# Ice Volume area and extent for all months: input file : icemod, and mesh_mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $ICEMONTH == 1 ] ; then
   TAG=y${YEAR}
   # get icemod files
   for m in $(seq 1 12) ; do
    mm=$( printf "%02d" $m )
    rapatrie  ${CONFCASE}_${TAG}m${mm}_icemod.nc $MEANY ${CONFCASE}_${TAG}m${mm}_icemod.nc
   done
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
 
   # Ascii/nc output file:
   fice=${CONFCASE}_${TAG}_icemonth.txt
   fice_nc=${CONFCASE}_${TAG}_icemonth.nc

   # special function for concatenation of Netcdf output
   cdfmean_concat_ice() { case $mm in
      01)  mv icediags.nc $1 ;;
      *)   ncatted -O -a missing_value,,d,, $1  
           ncatted -O -a missing_value,,d,, icediags.nc  
           ncrcat $1 icediags.nc -o tmp.nc ; mv tmp.nc $1 ;;
      esac        ; }

 
   for m in $(seq 1 12) ; do 
    mm=$( printf "%02d" $m )

    case $mm in 
    01) echo '###' $YEAR $mm > $fice ;;
    *)  echo '###' $YEAR $mm >> $fice ;;
    esac

    cdficediags ${CONFCASE}_${TAG}m${mm}_icemod.nc  >> $fice 
    cdfmean_concat_ice $fice_nc

   done
 
   expatrie $fice    $DIAGS/TXT $fice 
   expatrie $fice_nc $DIAGS/NC  $fice_nc
 
  fi

# Vertical T-S profiles off the coast of Portugal for Gib monitoring: input file: gridT, mesh_mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  mkannuallevitusgib(){
   if [ $(chkfile $DIAGS/NC/LEVITUS_y0000_TGIB.nc ) == absent ] ; then
    # first time : Create header with Levitus equivalent
    # requires  LEVITUS 'same' diags (from the ANNUAL mean )
    levitus=${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
    if [ ! -f $levitus ] ; then
     # need to build a masked LEvitus with proper mask
     levitus=${TSCLIM:=Levitus_p2.1}_1y_TS_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
     rapatrie $levitus $IDIR $levitus
     cdfmltmask $levitus  mask.nc votemper T             # votemper --> $levitus_masked
     cdfmltmask ${levitus}_masked  mask.nc vosaline T    # vosaline --> $levitus_masked_masked
     mv ${levitus}_masked_masked ${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
     levitus=${TSCLIM:=Levitus_p2.1}_1y_TS_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG
    fi
    cdfmean $levitus  votemper T $GIBWIN  0 0  >  LEVITUS_y0000_TGIB.txt ; mv cdfmean.nc LEVITUS_y0000_TGIB.nc
    cdfmean $levitus  vosaline T $GIBWIN  0 0  >  LEVITUS_y0000_SGIB.txt ; mv cdfmean.nc LEVITUS_y0000_SGIB.nc
    expatrie  LEVITUS_y0000_TGIB.txt $DIAGS/TXT LEVITUS_y0000_TGIB.txt
    expatrie  LEVITUS_y0000_SGIB.txt $DIAGS/TXT LEVITUS_y0000_SGIB.txt
    expatrie  LEVITUS_y0000_TGIB.nc $DIAGS/NC   LEVITUS_y0000_TGIB.nc
    expatrie  LEVITUS_y0000_SGIB.nc $DIAGS/NC   LEVITUS_y0000_SGIB.nc
   fi
                      }

  mkmonthlylevitusgib(){
  if [ $MONTH ] ; then
    if [ $(chkfile $DIAGS/MONTHLY/NC/LEVITUS_y0000m${MONTH}_TGIB.nc ) == absent ] ; then
      # first time : Create header with Levitus equivalent
      # requires  LEVITUS 'same' diags (from the MONTHLY mean )
      tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  
      smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc 
      if [ ! -f $tmlevitus -o ! -f $smlevitus ] ; then
        tlevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
        slevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
        rapatrie $tlevitus $IDIR $tlevitus
        rapatrie $slevitus $IDIR $slevitus
        tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
        smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
        ncks -F -d time_counter,${MONTH},${MONTH} $tlevitus $tmlevitus
        ncks -F -d time_counter,${MONTH},${MONTH} $slevitus $smlevitus
        cdfmltmask $tmlevitus  mask.nc votemper T      # votemper --> $levitus_masked
        cdfmltmask $smlevitus  mask.nc vosaline T      # vosaline --> $levitus_masked
        mv ${tmlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
        mv ${smlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
        tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
        smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
      fi  
      cdfmean $tmlevitus  votemper T $GIBWIN  0 0  >  LEVITUS_y0000m${MONTH}_TGIB.txt  ; mv cdfmean.nc LEVITUS_y0000m${MONTH}_TGIB.nc
      cdfmean $smlevitus  vosaline T $GIBWIN  0 0  >  LEVITUS_y0000m${MONTH}_SGIB.txt  ; mv cdfmean.nc LEVITUS_y0000m${MONTH}_SGIB.nc

      expatrie  LEVITUS_y0000m${MONTH}_TGIB.txt $DIAGS/MONTHLY/TXT  LEVITUS_y0000m${MONTH}_TGIB.txt
      expatrie  LEVITUS_y0000m${MONTH}_SGIB.txt $DIAGS/MONTHLY/TXT  LEVITUS_y0000m${MONTH}_SGIB.txt
      expatrie  LEVITUS_y0000m${MONTH}_TGIB.nc $DIAGS/MONTHLY/NC  LEVITUS_y0000m${MONTH}_TGIB.nc
      expatrie  LEVITUS_y0000m${MONTH}_SGIB.nc $DIAGS/MONTHLY/NC  LEVITUS_y0000m${MONTH}_SGIB.nc
    fi
  fi
                    }

  for TAG in $(mktaglist $GIB) ; do
   # get gridT file
   rapatrie ${CONFCASE}_${TAG}_gridT.nc $MEANY ${CONFCASE}_${TAG}_gridT.nc
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii output files:
   ftgib=${CONFCASE}_${TAG}_TGIB.txt
   fsgib=${CONFCASE}_${TAG}_SGIB.txt
   # nc output files
   ftgib_nc=${CONFCASE}_${TAG}_TGIB.nc
   fsgib_nc=${CONFCASE}_${TAG}_SGIB.nc

   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
 
   echo $YEAR $MONTH > $ftgib
   cdfmean ${CONFCASE}_${TAG}_gridT.nc  votemper T $GIBWIN  0 0  >>  $ftgib ; mv cdfmean.nc $ftgib_nc
   echo $YEAR $MONTH > $fsgib
   cdfmean ${CONFCASE}_${TAG}_gridT.nc  vosaline T $GIBWIN  0 0  >>  $fsgib ; mv cdfmean.nc $fsgib_nc

   # check if you are in monthly or yearly case and define corresponding diags directory
   define_diags_dir

   expatrie $ftgib    $DIAGSTXT $ftgib
   expatrie $fsgib    $DIAGSTXT $fsgib
   expatrie $ftgib_nc $DIAGSNC  $ftgib_nc
   expatrie $fsgib_nc $DIAGSNC  $fsgib_nc

   case $GIB in
      1) mkannuallevitusgib ;;
      2) mkmonthlylevitusgib ;;
      3) mkannuallevitusgib
         mkmonthlylevitusgib ;;
   esac

  done

# El nino indexes : Input files : monthly gridT,  mesh mask
#^^^^^^^^^^^^^^^^^^
  if [ $ELNINO == 1 -o $ELNINO == 2 ] ; then
   TAG=y${YEAR}
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii outputfile
   fnino=${CONFCASE}_${TAG}_NINO.txt
   # nc outputfile
   fnino12_nc=${CONFCASE}_${TAG}_NINO12.nc
   fnino3_nc=${CONFCASE}_${TAG}_NINO3.nc
   fnino4_nc=${CONFCASE}_${TAG}_NINO4.nc
   fnino34_nc=${CONFCASE}_${TAG}_NINO34.nc

   # special function for concatenation of Netcdf output
   cdfmean_concat() { case $mm in
      01)  mv cdfmean.nc $1 ;;
      *)   ncatted -O -a missing_value,,d,, $1  
           ncatted -O -a missing_value,,d,, cdfmean.nc  
           ncrcat $1 cdfmean.nc -o tmp.nc ; mv tmp.nc $1 ;;
      esac        ; }
 
   # get monthly mean gridT files and compute mean SST on each NINO box
   \rm $fnino
   for m in $(seq 1 12) ; do
     mm=$(printf "%02d" $m)
     f=${CONFCASE}_${TAG}m${mm}_gridT.nc 

     rapatrie $f $MEANY  $f
 
     #  header
     printf "%04d %02d" $TAG $m >>   $fnino
 
    # nino 1+2   [ -90 W -- -80 W, -10 S -- 10 N ]
    cdfmean  $f votemper T $NINO12 1 1 | tail -1 | awk '{ printf " %8.5f 0.00", $6 }'  >> $fnino 
    cdfmean_concat  $fnino12_nc

    # nino 3     [ -150 W -- -90 W, -5 S -- 5 N ]
    cdfmean  $f votemper T $NINO3 1 1  | tail -1 | awk '{ printf " %8.5f 0.00", $6 }'  >> $fnino 
    cdfmean_concat  $fnino3_nc

    # nino 4     [ -200 W -- -150 W, -5 S -- 5 N ]
    cdfmean  $f votemper T $NINO4 1 1 | tail -1 | awk '{ printf " %8.5f 0.00", $6 }'  >> $fnino  
    cdfmean_concat  $fnino4_nc

    # nino 3.4   [ -170 W -- -120 W, -% S -- % N ]
    cdfmean  $f votemper T $NINO34 1 1 | tail -1 | awk '{ printf " %8.5f 0.00\n", $6 }'  >> $fnino 
    cdfmean_concat  $fnino34_nc
 
   done
 
   expatrie $fnino      $DIAGS/TXT $fnino
   expatrie $fnino12_nc $DIAGS/NC  $fnino12_nc
   expatrie $fnino3_nc  $DIAGS/NC  $fnino3_nc
   expatrie $fnino4_nc  $DIAGS/NC  $fnino4_nc
   expatrie $fnino34_nc $DIAGS/NC  $fnino34_nc
  fi

# Transport: Input files: VT, gridU, gridV, mesh mask, section.dat
#^^^^^^^^^^^

cdf_trpconcatexpatrie(){
   if [ $MONTH ] ; then
     cat $1 >> ${CONFCASE}_y${YEAR}_section_monitor_concat.txt
     if [ $MONTH == 12 ] ; then
       mv ${CONFCASE}_y${YEAR}_section_monitor_concat.txt ${CONFCASE}_y${YEAR}_MONTHLY_section_monitor.txt
       expatrie ${CONFCASE}_y${YEAR}_MONTHLY_section_monitor.txt $DIAGS/MONTHLY/TXT ${CONFCASE}_y${YEAR}_MONTHLY_section_monitor.txt
     fi
   else
     expatrie  $1 $DIAGS/TXT $1
   fi
               }

cdf_trpncconcatexpatrie(){
   if [ $MONTH ] ; then
     case $MONTH in
       01 ) mv $1 $1_concat ;;
       *  ) ncrcat $1_concat $1 -o tmp.nc ; mv tmp.nc $1_concat ;
            if [ $MONTH == 12 ] ; then 
              mv $1_concat ${CONFCASE}_y${YEAR}_MONTHLY_$1
              expatrie ${CONFCASE}_y${YEAR}_MONTHLY_$1 $DIAGS/MONTHLY/NC ${CONFCASE}_y${YEAR}_MONTHLY_$1
            fi ;;
     esac
   else
     mv $1 ${CONFCASE}_${TAG}_$1
     expatrie ${CONFCASE}_${TAG}_$1 $DIAGS/NC ${CONFCASE}_${TAG}_$1
   fi
                 }


  for TAG in $(mktaglist $TRP) ; do
   # section.dat describes the position (I,J) of the sections to monitor
   # ./create_sections_list.ksh ${CONFIG%.*}   # to skip .Lxx part of the config name
   ../create_sections_list.ksh ${CONFIG} 
 
   # get VT , gridU, gridV files
   rapatrie ${CONFCASE}_${TAG}_VT.nc $MEANY ${CONFCASE}_${TAG}_VT.nc
   rapatrie ${CONFCASE}_${TAG}_gridU.nc $MEANY ${CONFCASE}_${TAG}_gridU.nc
   rapatrie ${CONFCASE}_${TAG}_gridV.nc $MEANY ${CONFCASE}_${TAG}_gridV.nc
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii output file:
   fsection=${CONFCASE}_${TAG}_section_monitor.txt
 
   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
   echo $YEAR $MONTH > $fsection
 
   # clean eventually old x_transport.nc files in this current directory
   \rm -f *transports.nc

   cdftransport   ${CONFCASE}_${TAG}_VT.nc \
                  ${CONFCASE}_${TAG}_gridU.nc \
                  ${CONFCASE}_${TAG}_gridV.nc  < section.dat >> $fsection
 
   # eliminate garbage from txt file ...
   grep -v Give $fsection | grep -v level | grep -v IMAX | grep -v FROM > tmp
   mv -f tmp $fsection
 
   cdf_trpconcatexpatrie $fsection
 
   # save x_transports.nc file
   listfiles=$( ls | grep transports.nc )

   for file in $listfiles ; do
     cdf_trpncconcatexpatrie $file
   done

  done
 
# Heat and Salt Meridional Transport : Input files : VT, mesh mask, new_maskglo
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  for TAG in $(mktaglist $MHT) ; do
# (a) From advection:
#--------------------
   # get VT  files
   rapatrie ${CONFCASE}_${TAG}_VT.nc $MEANY ${CONFCASE}_${TAG}_VT.nc
 
   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi
 
   # Ascii output files:
   fheat=${CONFCASE}_${TAG}_heattrp.dat
   fsalt=${CONFCASE}_${TAG}_salttrp.dat
   # Netcdf output files: (both head and salt in 2 separated variables)
 
   cdfmhst  ${CONFCASE}_${TAG}_VT.nc MST   # Save Meridional salt transport as well
   mv mhst.nc ${CONFCASE}_${TAG}_mhst.nc

   # check if you are in monthly or yearly case and define corresponding diags directory
   define_diags_dir
 
   expatrie zonal_heat_trp.dat $DIAGSTXT $fheat
   expatrie zonal_salt_trp.dat $DIAGSTXT $fsalt
   expatrie ${CONFCASE}_${TAG}_mhst.nc $DIAGSNC ${CONFCASE}_${TAG}_mhst.nc
 
   # needed below with the correct name
   cp zonal_heat_trp.dat ${CONFCASE}_${TAG}_heattrp.dat
 
# (b) from Surface Heat fluxes
#-----------------------------
    rapatrie ${CONFCASE}_${TAG}_gridT.nc $MEANY  ${CONFCASE}_${TAG}_gridT.nc
    cdfhflx  ${CONFCASE}_${TAG}_gridT.nc
 
    expatrie hflx.out $DIAGSTXT ${CONFCASE}_${TAG}_hflx.dat
    expatrie cdfhflx.nc $DIAGSNC ${CONFCASE}_${TAG}_hflx.nc
    \rm hflx.out cdfhflx.nc
  done

# MAX and MIN of MOC: requires that MOC files already exists
#^^^^^^^^^^^^^^^^^^^^
  for TAG in $(mktaglist $MAXMOC) ; do
   f=moc.nc
   rapatrie ${CONFCASE}_${TAG}_MOC.nc $MEANY $f

   # check if you are in monthly or yearly case and define corresponding diags directory
   define_diags_dir

   # Ascii output file
   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
   fmaxmoc=${CONFCASE}_${TAG}_minmaxmoc.txt
   echo $YEAR $MONTH > $fmaxmoc
   fmaxmoc40=${CONFIG}-${CASE}_${TAG}_maxmoc40.txt
   echo $YEAR $MONTH > $fmaxmoc40

   case $CONFIG in
        PERIANT05 | PERIANT025 | PERIANT8 )

   #AUS
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 0 2000   | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_maxmoc.nc
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 2000 5500  | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_minmoc.nc 
   
   expatrie $fmaxmoc $DIAGSTXT $fmaxmoc ;;

        ORCA12 | ORCA12.L46 | ORCA12.L75 | ORCA025 | ORCA025.L75 | ORCA05 | ORCA2 | ORCA246 )
   # GLO
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo 20 60 500 2000 | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_maxmoc.nc
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo -40 30 2000 5500 | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_minmoc.nc
   # ATL
   printf "%s" 'Atl ' >>  $fmaxmoc ; cdfmaxmoc $f atl 0 60 500 2000 | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Atl_maxmoc.nc
   printf "%s" 'Atl ' >>  $fmaxmoc ; cdfmaxmoc $f atl -20 40 2000 5500 | grep Minimum  >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Atl_minmoc.nc
   #INP
   printf "%s" 'Inp ' >>  $fmaxmoc ; cdfmaxmoc $f inp 15 50 100 1000 | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Inp_minmoc.nc
   printf "%s" 'Inp ' >>  $fmaxmoc ; cdfmaxmoc $f inp -30 20 1000 5500  | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Inp_minmoc2.nc
   #AUS
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 0 2000   | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Aus_maxmoc.nc
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 2000 5500  | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Aus_minmoc.nc
   
   expatrie $fmaxmoc $DIAGSTXT $fmaxmoc

   # Max and Min of MOC at some specific latitudes
   # GLO  MAX at 40 N and 30S
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo 40 40 500 2000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_maxmoc40N.nc
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -30 -30 500  5500 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_maxmoc30S.nc
   # ATL  MAX at 40N and 30S
   printf "%s" 'Atl ' >>  $fmaxmoc40 ; cdfmaxmoc $f atl 40 40 500 2000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Atl_maxmoc40N.nc
   printf "%s" 'Atl ' >>  $fmaxmoc40 ; cdfmaxmoc $f atl -30 -30  500 5000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Atl_maxmoc30S.nc
   #INP  Min at 30 S
   printf "%s" 'Inp ' >>  $fmaxmoc40 ; cdfmaxmoc $f inp -30 -30 1000 5500  | grep Minimum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Inp_minmoc30S.nc
   #AUS  MAX at 50 S
   printf "%s" 'Aus ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -50 -50 0 2000   | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Aus_maxmoc50S.nc

   expatrie $fmaxmoc40 $DIAGSTXT $fmaxmoc40 ;;

       # NATL configuration
        NATL025 | NATL4 | NATL12 )
   # GLO
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo 20 60 500 2000 | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_maxmoc.nc
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo -40 30 2000 5500 | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_minmoc.nc
   expatrie $fmaxmoc $DIAGSTXT $fmaxmoc

   # Max and Min of MOC at some specific latitudes
   # GLO  MAX at 40 N and 30S
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo 40 40 500 2000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_maxmoc40N.nc
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -15 -15 500  5500 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGSNC ${CONFIG}-${CASE}_${TAG}_Glo_maxmoc15S.nc

   expatrie $fmaxmoc40 $DIAGSTXT $fmaxmoc40 ;;

   esac
   # clean for next year 
   \rm moc.nc 
  done


# DCT :Density Class transport: Input files : gridT, gridU gridV, mesh mask, dens_section.dat
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  if [ $DCT == 1 ] ; then
   TAG=y${YEAR}
  # dens_section.dat describe the sections (either zonal or meridional) 
  # where the DCT is computed: it is buildt by a  DMONTOOLS script
  #./create_sections_list.ksh ${CONFIG%.*}   # to skip .Lxx part of the config name
  ../create_sections_list.ksh ${CONFIG}   

  # get mesh mask files
  rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
  rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
  rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc

  # due to the large amount of files that are produced by this diags, 
  # we prefer to keep them on a separate directory
  chkdirg ${CONFIG}/${CONFCASE}-TRPSIG/
  chkdirg ${CONFIG}/${CONFCASE}-TRPSIG/$YEAR/
  chkdirg $DIAGS/TXT/TRPSIG/

  # also need temporary directories in the actual directory:
  chkdir ${CONFIG}
  chkdir ${CONFIG}/${CONFCASE}-TRPSIG
  chkdir ${CONFIG}/${CONFCASE}-TRPSIG/$YEAR/

  TRPSIGY=${CONFIG}/${CONFCASE}-TRPSIG/$YEAR/

  for m in $(seq 1 12) ; do
    mm=$(printf "%02d" $m)
    tfich=${CONFCASE}_${TAG}m${mm}_gridT.nc 
    ufich=$(echo  $tfich | sed -e 's/gridT/gridU/' )
    vfich=$(echo  $tfich | sed -e 's/gridT/gridV/' )

    #get files on gaya
    rapatrie  $tfich $MEANY  $tfich
    rapatrie  $ufich $MEANY  $ufich
    rapatrie  $vfich $MEANY  $vfich
    
    #retrieve tag time from file name
    tag=$(echo $tfich | sed -e "s/${CONFCASE}_//" -e 's/_gridT.nc//')

    #echo $tag > ${CONFCASE}_y${tag}_trpsig_monitor.lst

    cdfsigtrp $tfich $ufich $vfich 21 30 180 # -print  >>  ${CONFCASE}_y${tag}_trpsig_monitor.lst
    # save netcdf files
    listfiles=$( ls | grep trpsig.nc  )

    for file in $listfiles ; do
        expatrie $file $DIAGS/NC ${CONFCASE}_${tag}_$file
        mv $file $TRPSIGY/${CONFCASE}_${tag}_$file
    done

    # and create a mirror on the local tmpdir
    #mv ${CONFCASE}_y${tag}_trpsig_monitor.lst  $TRPSIGY

  # end of month loop
  done

  here=$(pwd)  # save the actual directory path
  cd $TRPSIGY

  # compute mean nc files : all sections are mixed in this dir with 12 months each 
  # isolate sections from m01 files whose name is similar to 
  #  NATL025-GRD83_y1999m01_01_Denmark_strait_trpsig.nc
  section_list=''
  for f in *m01_*trpsig.nc ; do
   section=${f%_trpsig.nc} ; section=${section#*m01_}
   section_list="$section_list $section"
  done

  for section in $section_list ; do
   cdfmoy_weighted ${CONFCASE}_${TAG}m??_${section}_trpsig.nc 
   froot=${CONFCASE}_${TAG}_${section}_trpsig
   mv cdfmoy_weighted.nc  $froot.nc
   expatrie $froot.nc $DIAGS/NC $froot.nc
   ncks -v sigtrp  $froot.nc | \
       sed -e 's/=/ /g' | grep -e '^time_counter\[' | grep -e lev | \
       awk '{printf " %07.4f %+14.9e\n", $4, $8}' > $froot.txt
   expatrie $froot.txt $DIAGS/TXT/TRPSIG/  $froot.txt
  done

   # return to tmpdir
   cd $here
   # Erase the TRPSIG tree for this current year
   \rm -rf ${CONFIG} ${CONFCASE}_y*_trpsig.txt *.mtl
  fi

# TRACER DIAGS (31/12 of each year) : Input files : ptrcT, mesh mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $TRACER == 1 ] ; then
   TAG=y${YEAR}
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi
 
   # get tracer file from gaya: note that this is from -S dir (5 day average ... to discuss ...)
   rapatrie ${CONFCASE}_${TAG}m12d31_ptrcT.nc $SDIRY ${CONFCASE}_${TAG}m12d31_ptrcT.nc
   rapatrie ${CONFCASE}_${TAG}m12d31_diadT.nc $SDIRY ${CONFCASE}_${TAG}m12d31_diadT.nc
   rapatrie ${CONFCASE}_${TAG}_ptrcT.nc $MEANY ${CONFCASE}_${TAG}_ptrcT.nc
 
   # Ascii output file:
   ftrc=${CONFCASE}_${TAG}_TRCmean.dat
   ftrc_nc=${CONFCASE}_${TAG}_TRCmean.nc
 
   # Number of mol in the ocean ...
   printf "%04d "  $YEAR   >  $ftrc
 
   # CFC11
   \rm -f tmp1
   cdfmean  ${CONFCASE}_${TAG}m12d31_diadT.nc  INVCFC T > tmp1
   area=$(cat tmp1 |  grep -e 'Mean value at level' | awk ' {print $12}')
   mean=$(cat tmp1 |  grep -e 'Mean value over the ocean' | awk ' {print $6}')
   total=$(echo $mean $area |  awk '{print $1 * $2 }' )
   printf "%s "  $total  >> $ftrc
   mv cdfmean.nc $ftrc_nc
 
   # B-C14
#   \rm -f tmp1
#   cdfmean  ${CONFCASE}_y${YEAR}m12d31_ptrcT.nc  invc14 T > tmp1
#   area=$(cat tmp1 |  grep -e 'Mean value at level' | awk ' {print $12}')
#   mean=$(cat tmp1 |  grep -e 'Mean value over the ocean' | awk ' {print $6}')
#   total=$(echo $mean $area |  awk '{print $1 * $2 }' )
#   printf "%s \n"  $total  >> $ftrc
#   # append cdfmean.nc variable to the already existing nc file
#   ncks -A cdfmean.nc $ftrc_nc
 
   expatrie $ftrc $DIAGS/TXT $ftrc
   expatrie $ftrc_nc $DIAGS/NC $ftrc_nc
 
   # zonal integral of inventories
   cdfzonalsum  ${CONFCASE}_${TAG}m12d31_ptrcT.nc  T
   expatrie zonalsum.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalsum_conc.nc
   cdfzonalsum  ${CONFCASE}_${TAG}m12d31_diadT.nc  T
 
   # zonal means
   cdfzonalmean  ${CONFCASE}_${TAG}m12d31_ptrcT.nc  T
   expatrie zonalmean.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalmean_conc.nc
   cdfzonalmean  ${CONFCASE}_${TAG}m12d31_diadT.nc  T
 
   # ncks is required on the prod machine ... !! not standard !!
   # it is used to take only the interesting variables from the results
#   ncks -F -d deptht,1,1 -v zocfc11_glo,zobc14_glo,nav_lon,nav_lat zonalmean.nc zonalsurf.nc
   ncks -F -d deptht,1,1 -v zoVCFC_glo,zoRCFC_glo,zoNTCFC_glo,nav_lon,nav_lat zonalmean.nc zonalsurf.nc
 
   # put in ascii format the 1D profiles
   cdfzonalout zonalmean.nc > zonalmean.dat
   cdfzonalout zonalsum.nc >  zonalsum.dat
   cdfzonalout zonalsurf.nc >  zonalsurf.dat
 
   expatrie zonalmean.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalmean_flx.nc
   expatrie zonalsum.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalsum_flx.nc
 
   expatrie zonalmean.dat $DIAGS/TXT ${CONFCASE}_${TAG}_TRCzonalmean_flx.dat
   expatrie zonalsum.dat $DIAGS/TXT ${CONFCASE}_${TAG}_TRCzonalsum_flx.dat
   expatrie zonalsurf.dat $DIAGS/TXT ${CONFCASE}_${TAG}_TRCzonalsurf_flx.dat
   \rm zonalsurf.nc

   # penetration depth
   cdfpendep ${CONFCASE}_${TAG}m12d31_ptrcT.nc ${CONFCASE}_${TAG}m12d31_diadT.nc -inv INVCFC -trc CFC11
   expatrie pendep.nc $MEANY ${CONFCASE}_${TAG}_pendep.nc

   # Fraction of inventory
   cdffracinv ${CONFCASE}_${TAG}m12d31_diadT.nc -inv INVCFC
   expatrie fracinv.nc $MEANY ${CONFCASE}_${TAG}_fracinv.nc
 
  fi

# Compare zonal current with TAO moorings: Input file: gridU, gridV, gridT2, coordinates
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  for TAG in $(mktaglist $TAO) ; do
    
   # check if you are in monthly or yearly case and define corresponding diags directory
   define_diags_dir

     LAT=0
     u=${CONFCASE}_${TAG}_gridU.nc
     v=${CONFCASE}_${TAG}_gridV.nc
     t2=${CONFCASE}_${TAG}_gridT2.nc 

     rapatrie $u  $MEANY $u
     rapatrie $v  $MEANY $v
     rapatrie $t2 $MEANY $t2

     rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc

     cdfvita $u $v $t2
     \mv vita.nc ${CONFCASE}_${TAG}_vita.nc

     for LON in 156 165 -110 -140 -170 ; do

        I=$( cdffindij $LON $LON $LAT $LAT -c mesh_hgr.nc -p T | tail -2 | head -1  | awk '{print $1 }' )
        J=$( cdffindij $LON $LON $LAT $LAT -c mesh_hgr.nc -p T | tail -2 | head -1  | awk '{print $3 }' )
        LONG=${LON}e
        LATI=${LAT}n
        if (( $LON < 0 )) ; then LONG=${LON}w ; fi
        if (( $LAT < 0 )) ; then LATI=${LAT}s ; fi
        LONG=$( echo $LONG | sed -e 's/-//' )
        LATI=$( echo $LATI | sed -e 's/-//' )
        file=${CONFCASE}_${TAG}_vita.nc
        varu=sovitua
        cdfprofile $I $J $file $varu  # extraire les profils aux coordonnees desirees
        mv profile.nc ${CONFCASE}_${TAG}_VELOCITY_${LATI}${LONG}.nc
        ncrename -O -v sovitua,u_$LONG ${CONFCASE}_${TAG}_VELOCITY_${LATI}${LONG}.nc 
        expatrie ${CONFCASE}_${TAG}_VELOCITY_${LATI}${LONG}.nc $DIAGSNC  ${CONFCASE}_${TAG}_VELOCITY_${LATI}${LONG}.nc

     done
     	
  uclev=$( find_model_level depth 80 ${CONFCASE}_${TAG}_VELOCITY_0n110w.nc )
  ncks -F -O -v u_110w -d depth,$uclev,$uclev ${CONFCASE}_${TAG}_VELOCITY_0n110w.nc \
                     -o ${CONFCASE}_${TAG}_VELOCITY_0n110w_UC.nc
  newval=$( interpolation_vertical depth 80 u_110w ${CONFCASE}_${TAG}_VELOCITY_0n110w.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_110w[time_counter] = float($newval) " ${CONFCASE}_${TAG}_VELOCITY_0n110w_UC.nc ${CONFCASE}_${TAG}_VELOCITY_0n110w_UC.nc
  ncrename -O -v u_110w,u_110w_UC ${CONFCASE}_${TAG}_VELOCITY_0n110w_UC.nc 

  uclev=$( find_model_level depth 120 ${CONFCASE}_${TAG}_VELOCITY_0n140w.nc )
  ncks -F -O -v u_140w -d depth,$uclev,$uclev ${CONFCASE}_${TAG}_VELOCITY_0n140w.nc \
                     -o ${CONFCASE}_${TAG}_VELOCITY_0n140w_UC.nc
  newval=$( interpolation_vertical depth 120 u_140w ${CONFCASE}_${TAG}_VELOCITY_0n140w.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_140w[time_counter] = float($newval) " ${CONFCASE}_${TAG}_VELOCITY_0n140w_UC.nc ${CONFCASE}_${TAG}_VELOCITY_0n140w_UC.nc
  ncrename -O -v u_140w,u_140w_UC ${CONFCASE}_${TAG}_VELOCITY_0n140w_UC.nc 
	
  uclev=$( find_model_level depth 150 ${CONFCASE}_${TAG}_VELOCITY_0n170w.nc ) 
  ncks -F -O -v u_170w -d depth,$uclev,$uclev ${CONFCASE}_${TAG}_VELOCITY_0n170w.nc \
                     -o ${CONFCASE}_${TAG}_VELOCITY_0n170w_UC.nc
  newval=$( interpolation_vertical depth 150 u_170w ${CONFCASE}_${TAG}_VELOCITY_0n170w.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_170w[time_counter] = float($newval) " ${CONFCASE}_${TAG}_VELOCITY_0n170w_UC.nc ${CONFCASE}_${TAG}_VELOCITY_0n170w_UC.nc
  ncrename -O -v u_170w,u_170w_UC ${CONFCASE}_${TAG}_VELOCITY_0n170w_UC.nc 

  uclev=$( find_model_level depth 200 ${CONFCASE}_${TAG}_VELOCITY_0n156e.nc )
  ncks -F -O -v u_156e -d depth,$uclev,$uclev ${CONFCASE}_${TAG}_VELOCITY_0n156e.nc \
                     -o ${CONFCASE}_${TAG}_VELOCITY_0n156e_UC.nc
  newval=$( interpolation_vertical depth 200 u_156e ${CONFCASE}_${TAG}_VELOCITY_0n156e.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_156e[time_counter] = float($newval) " ${CONFCASE}_${TAG}_VELOCITY_0n156e_UC.nc ${CONFCASE}_${TAG}_VELOCITY_0n156e_UC.nc
  ncrename -O -v u_156e,u_156e_UC ${CONFCASE}_${TAG}_VELOCITY_0n156e_UC.nc 

  uclev=$( find_model_level depth 200 ${CONFCASE}_${TAG}_VELOCITY_0n165e.nc )
  ncks -F -O -v u_165e -d depth,$uclev,$uclev ${CONFCASE}_${TAG}_VELOCITY_0n165e.nc \
                     -o ${CONFCASE}_${TAG}_VELOCITY_0n165e_UC.nc
  newval=$( interpolation_vertical depth 200 u_165e ${CONFCASE}_${TAG}_VELOCITY_0n165e.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_165e[time_counter] = float($newval) " ${CONFCASE}_${TAG}_VELOCITY_0n165e_UC.nc ${CONFCASE}_${TAG}_VELOCITY_0n165e_UC.nc
  ncrename -O -v u_165e,u_165e_UC ${CONFCASE}_${TAG}_VELOCITY_0n165e_UC.nc 


  for file in ${CONFCASE}_${TAG}_VELOCITY_*_UC.nc ; do

     ncwa -F -O -a depth $file -o $file     # remove depth dim
     ncks -F -O -x -v depth $file -o $file  # remove depth var
     expatrie $file $DIAGSNC  $file

  done

done

# PISCES PROFILES : Input files : ptrcT
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

if [ $BIO_PROFILE == 1 ] ; then
   TAG=y${YEAR}
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc

   # get tracer file from gaya: 
   rapatrie ${CONFCASE}_${TAG}_ptrcT.nc $MEANY ${CONFCASE}_${TAG}_ptrcT.nc

   # Parameters for integration
   latmin=$( cdffindij -180 180 -75 -45 -c mesh_hgr.nc -p T | tail -2 | head -1 | awk '{ print $4 }' )
   
   # compute vertical profiles
   jc=0
   for var in DIC Alkalini O2 PO4 Si NO3 Fer DOC ; do 
     cdfmean ${CONFCASE}_${TAG}_ptrcT.nc $var T 0 180 1 $latmin 0 46 
     if [ $jc == 0 ] ; then
       mv cdfmean.nc ${CONFCASE}_${TAG}_bioprofile.nc
     else
       ncks -A -v mean_$var cdfmean.nc ${CONFCASE}_${TAG}_bioprofile.nc
     fi
     jc=$((jc+1))
   done

   expatrie ${CONFCASE}_${TAG}_bioprofile.nc $MEANY  ${CONFCASE}_${TAG}_bioprofile.nc 

fi

######################################################################
### Clean the working directory :
######################################################################

if [ $RNDTMPDIR == 1 ] ; then

   cd $TMPDIR/$YEAR

else

   cd $R_MONITOR/$YEAR

fi

\rm -f *gridT.nc *gridU.nc *gridV.nc *gridW.nc *icemod.nc *VT.nc
\rm -f mesh_hgr.nc mesh_zgr.nc mask.nc

