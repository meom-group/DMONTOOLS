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

#define a variable for new_maskglo.nc for easy change later on
fbasinmask=new_maskglo.nc

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

# Function getsuffix returns _1m or eventually _1y or '' according to case in use. Argument is TAG
getsuffix() {
   monthly=$( echo $1 | awk '{ print index($1,"m") }' )
   if [ $monthly == 0 ] ; then
      echo '_1y'
   else
      echo '_1m'
   fi
            }

# Function to check if you are in monthly or yearly case and define corresponding DIAGS directory
#  eg : define_diags_dir ORCA2_y2000_1m_TSMEAN.nc
define_diags_dir() {
   txt=$( echo $1 | awk '{ print index($1,".txt") }' )
   if [ $txt == 0 ] ; then
       DIAGSOUT=$DIAGS/NC
   else
       DIAGSOUT=$DIAGS/TXT
   fi
                   }

# Function that transfert/concatenate cdftool file to target file
#  eg : concat_file $TAG cdftool_file  target_file
concat_file() {
   # look for month
   month=$( echo $1 | awk -Fm '{ print $2 }' )
   if [ $month ] ; then  # monthly diags
     case $month in
     01) cp $2 $3 ;;     # first month initialize output file
      *)  ncrcat -h $3 $2 -o tmp.nc  ; mv tmp.nc $3 ;;
     esac
   else                  # no month
     mv $2 $3            # just rename
   fi
              }

# Function to merge a bunch of files into a single one. All variables
# must have different names !
#  eg : merge_files  merged_file.nc   list_of_files...
merge_files () {
   if (( $# > 1 )) ; then   # performs merge only if files to merge ...
     mfile=$1 ; shift         # merge file is the first argument
     cp $1 $mfile  ; shift  # copy first file to merge file
     for f in $* ; do       # loop on files
       ncks -A -h $f $mfile # merge
     done
   fi
                }

# Function that return a list without duplicate items
#  eg : filter_list $list
filter_list() {
   for ll in $* ; do echo $ll ; done | sort -u
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
  chkdirg $DIAGS/NC     # for NetCdf diag files
  chkdirg $DIAGS/TXT    # for NetCdf diag files

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
#  keyword : EKE
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
#  keyword : RMSSSH
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
#  keyword : BSF
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
#  keyword : MOC
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  for TAG in $(mktaglist $MOC) ; do 
   # get gridV  files
   rapatrie ${CONFCASE}_${TAG}_gridV.nc $MEANY ${CONFCASE}_${TAG}_gridV.nc
 
   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  $fbasinmask $IDIR $fbasinmask ; fi
 
   cdfmoc ${CONFCASE}_${TAG}_gridV.nc
 
   # dispose on gaya MEAN/YEAR directory
   expatrie moc.nc $MEANY ${CONFCASE}_${TAG}_MOC.nc
   \rm moc.nc
  done

# MOCSIG Meridional Overturning Circulation on sigma coordinates:  Input file: gridV, gridT, mesh mask, mask_glo
#  keyword : MOCSIG
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
   if [ $DREF == 0    ]; then NREF=0 ; fi
   if [ $DREF == 1000 ]; then NREF=1 ; fi
   if [ $DREF == 2000 ]; then NREF=2 ; fi

   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
   if (( $orca == 0 )) ; then rapatrie  $fbasinmask $IDIR $fbasinmask ; fi

   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`

   case $mocsig_5d in
     0) mkmocsig ;;
     1) mkmocsig5d ;;
     2) mkmocsig ; mkmocsig5d ;;
   esac

  done

# Mixed Layer Diagnostics : Input file : gridT, mesh_hgr, mesh_zgr
#  keyword : MXL
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
#  keyword : LSPV
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
     1) taglist="3 9"       ; mklspv $taglist ;;
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
#  keyword : TSMEAN  file_id :  TMEAN / SMEAN / SSHMEAN ==> TSMEAN
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [   $TSMEAN != 0 ] ; then

  mkannuallevitus(){
   if [ $(chkfile $DIAGS/NC/LEVITUS_y0000_1y_TSMEAN.nc ) == absent ] ; then
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
    cdfmean $levitus votemper T > LEVITUS_y0000_1y_TMEAN.txt ; concat_file y0000 cdfmean.nc LEVITUS_y0000_1y_TMEAN.nc
    cdfmean $levitus vosaline T > LEVITUS_y0000_1y_SMEAN.txt ; concat_file y0000 cdfmean.nc LEVITUS_y0000_1y_SMEAN.nc

    #
    define_diags_dir LEVITUS_y0000_1y_TMEAN.txt
    expatrie  LEVITUS_y0000_1y_TMEAN.txt $DIAGSOUT  LEVITUS_y0000_1y_TMEAN.txt
    expatrie  LEVITUS_y0000_1y_SMEAN.txt $DIAGSOUT  LEVITUS_y0000_1y_SMEAN.txt

    merge_files LEVITUS_y0000_1y_TSMEAN.nc LEVITUS_y0000_1y_TMEAN.nc LEVITUS_y0000_1y_SMEAN.nc
    define_diags_dir LEVITUS_y0000_1y_TSMEAN.nc
    expatrie  LEVITUS_y0000_1y_TSMEAN.nc $DIAGSOUT  LEVITUS_y0000_1y_TSMEAN.nc
   fi
                    }

  mkmonthlylevitus(){
    if [ $(chkfile $DIAGS/NC/LEVITUS_y0000_1m_TSMEAN.nc ) == absent ] ; then
      # first time : Create header with Levitus equivalent
      # requires  LEVITUS 'same' diags (from the MONTHLY mean )
      # Contrary to ANNUAL means, for MONTHLY means we must have two distinct files (T and S): file size issue
      #  !!! NEW !!!
      # get non-masked levitus then mask it with the same mask as the model
      tlevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
      slevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
      rapatrie $tlevitus $IDIR $tlevitus
      rapatrie $slevitus $IDIR $slevitus
      for m in $(seq 1 12 ) ; do 
         MONTH=$(printf "%02d" $m )
         tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
         smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc

         ncks -h -F -d time_counter,${MONTH},${MONTH} $tlevitus $tmlevitus
         ncks -h -F -d time_counter,${MONTH},${MONTH} $slevitus $smlevitus
         cdfmltmask $tmlevitus  mask.nc votemper T      # votemper --> $levitus_masked
         cdfmltmask $smlevitus  mask.nc vosaline T      # vosaline --> $levitus_masked
         mv ${tmlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
         mv ${smlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
         tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
         smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
         #  
         cdfmean $tmlevitus votemper T >> LEVITUS_y0000_1m_TMEAN.txt ; concat_file y0000m${MONTH} cdfmean.nc LEVITUS_y0000_1m_TMEAN.nc
         cdfmean $smlevitus vosaline T >> LEVITUS_y0000_1m_SMEAN.txt ; concat_file y0000m${MONTH} cdfmean.nc LEVITUS_y0000_1m_SMEAN.nc
      done
    
      define_diags_dir LEVITUS_y0000_1m_TMEAN.txt
      expatrie  LEVITUS_y0000_1m_TMEAN.txt $DIAGSOUT  LEVITUS_y0000_1m_TMEAN.txt
      expatrie  LEVITUS_y0000_1m_SMEAN.txt $DIAGSOUT  LEVITUS_y0000_1m_SMEAN.txt

      merge_files LEVITUS_y0000_1m_TSMEAN.nc LEVITUS_y0000_1m_TMEAN.nc LEVITUS_y0000_1m_SMEAN.nc
      define_diags_dir LEVITUS_y0000_1m_TSMEAN.nc
      expatrie  LEVITUS_y0000_1m_TSMEAN.nc $DIAGSOUT  LEVITUS_y0000_1m_TSMEAN.nc
     fi
                    }

  file_lst_txt=''
  file_lst=''
  for TAG in $(mktaglist $TSMEAN) ; do
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
  
   # get gridT files
   rapatrie ${CONFCASE}_${TAG}_gridT.nc $MEANY ${CONFCASE}_${TAG}_gridT.nc


   # output file name ascii and nc
   suf=$( getsuffix $TAG )
   fbase=${CONFCASE}_y${YEAR}${suf}
   fsshmean=${fbase}_SSHMEAN.txt  ; fsshmean_nc=${fbase}_SSHMEAN.nc
   ftmean=${fbase}_TMEAN.txt      ; ftmean_nc=${fbase}_TMEAN.nc
   fsmean=${fbase}_SMEAN.txt      ; fsmean_nc=${fbase}_SMEAN.nc

   # save file list ( to be sorted later to eliminate duplicate entries )
   file_lst_txt=$( filter_list $file_lst_txt $fsshmean $ftmean $fsmean  )
   file_lst=$( filter_list $file_lst $fsshmean_nc $ftmean_nc $fsmean_nc )

   # set header on the output file (ASCII)
   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
   echo $YEAR $MONTH >>  $fsshmean ; echo $YEAR $MONTH >>  $ftmean ;  echo $YEAR $MONTH >>  $fsmean

   # 3D means
   cdfmean  ${CONFCASE}_${TAG}_gridT.nc sossheig T >> $fsshmean ; concat_file $TAG cdfmean.nc $fsshmean_nc
   cdfmean  ${CONFCASE}_${TAG}_gridT.nc votemper T >> $ftmean   ; concat_file $TAG cdfmean.nc $ftmean_nc
   cdfmean  ${CONFCASE}_${TAG}_gridT.nc vosaline T >> $fsmean   ; concat_file $TAG cdfmean.nc $fsmean_nc
  done

  # dispose TXT file in the ad-hoc -DIAGS/xxx directory
  for f in $file_lst_txt ; do
   define_diags_dir $f
   expatrie  $f $DIAGSOUT $f
  done

  # merge matching nc files
  for suf in _1m _1y ; do 
    fbase=${CONFCASE}_y${YEAR}${suf}
    ftsmean_nc=${fbase}_TSMEAN.nc
    tmplst=''
    for f in $file_lst ; do
      tmplst="$tmplst $(echo $f | grep $fbase )"
    done
    merge_files $ftsmean_nc $tmplst 
    if [ -f $ftsmean_nc ] ; then
      define_diags_dir $ftsmean_nc
      expatrie  $ftsmean_nc $DIAGSOUT $ftsmean_nc
    fi
  done

  case $TSMEAN in
     1) mkannuallevitus  ;;
     2) mkmonthlylevitus ;;
     3) mkannuallevitus
        mkmonthlylevitus ;;
  esac
fi

# Vertical T-S profiles off the coast of Portugal for Gib monitoring: input file: gridT, mesh_mask
#  keyword : GIB       file_id :  TGIB / SGIB
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [        $GIB  !=  0    ] ; then
  mkannuallevitusgib(){
   if [ $(chkfile $DIAGS/NC/LEVITUS_y0000_1y_TSGIB.nc ) == absent ] ; then
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
    #
    cdfmean $levitus  votemper T $GIBWIN  0 0 > LEVITUS_y0000_1y_TGIB.txt ; concat_file y0000 cdfmean.nc LEVITUS_y0000_1y_TGIB.nc
    cdfmean $levitus  vosaline T $GIBWIN  0 0 > LEVITUS_y0000_1y_SGIB.txt ; concat_file y0000 cdfmean.nc LEVITUS_y0000_1y_SGIB.nc

    define_diags_dir LEVITUS_y0000_1y_TGIB.txt
    expatrie  LEVITUS_y0000_1y_TGIB.txt $DIAGSOUT  LEVITUS_y0000_1y_TGIB.txt
    expatrie  LEVITUS_y0000_1y_SGIB.txt $DIAGSOUT  LEVITUS_y0000_1y_SGIB.txt

    merge_files LEVITUS_y0000_1y_TSGIB.nc LEVITUS_y0000_1y_TGIB.nc LEVITUS_y0000_1y_SGIB.nc
    define_diags_dir LEVITUS_y0000_1y_TSGIB.nc
    expatrie  LEVITUS_y0000_1y_TSGIB.nc $DIAGSOUT  LEVITUS_y0000_1y_TSGIB.nc
   fi
                      }

  mkmonthlylevitusgib(){
    if [ $(chkfile $DIAGS/NC/LEVITUS_y0000_1m_TSGIB.nc ) == absent ] ; then
      # first time : Create header with Levitus equivalent
      # requires  LEVITUS 'same' diags (from the MONTHLY mean )
      for m in $(seq 1 12 ) ; do
         MONTH=$(printf "%02d" $m )
         tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  
         smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc 
         if [ ! -f $tmlevitus -o ! -f $smlevitus ] ; then
           tlevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
           slevitus=${TSCLIM:=Levitus_p2.1}_1m_01_12_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
           rapatrie $tlevitus $IDIR $tlevitus
           rapatrie $slevitus $IDIR $slevitus
      
           tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
           smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_$( echo $CONFIG | tr 'A-Z' 'a-z').nc
           ncks -h -F -d time_counter,${MONTH},${MONTH} $tlevitus $tmlevitus
           ncks -h -F -d time_counter,${MONTH},${MONTH} $slevitus $smlevitus
           cdfmltmask $tmlevitus  mask.nc votemper T      # votemper --> $levitus_masked
           cdfmltmask $smlevitus  mask.nc vosaline T      # vosaline --> $levitus_masked
           mv ${tmlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
           mv ${smlevitus}_masked ${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc  # simplify name
           tmlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_T_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
           smlevitus=${TSCLIM:=Levitus_p2.1}_1m_${MONTH}_S_masked_$( echo $CONFIG | tr 'A-Z' 'a-z').nc # will be ready for GIB DIAG 
         fi  
         #
         cdfmean $tmlevitus votemper T $GIBWIN 0 0 >> LEVITUS_y0000_1m_TGIB.txt ; concat_file y0000m${MONTH} cdfmean.nc LEVITUS_y0000_1m_TGIB.nc
         cdfmean $smlevitus vosaline T $GIBWIN 0 0 >> LEVITUS_y0000_1m_SGIB.txt ; concat_file y0000m${MONTH} cdfmean.nc LEVITUS_y0000_1m_SGIB.nc
      done

      define_diags_dir LEVITUS_y0000_1m_TGIB.txt
      expatrie  LEVITUS_y0000_1m_TGIB.txt $DIAGSOUT  LEVITUS_y0000_1m_TGIB.txt
      expatrie  LEVITUS_y0000_1m_SGIB.txt $DIAGSOUT  LEVITUS_y0000_1m_SGIB.txt

      merge_files LEVITUS_y0000_1m_TSGIB.nc LEVITUS_y0000_1m_TGIB.nc LEVITUS_y0000_1m_SGIB.nc
      define_diags_dir LEVITUS_y0000_1m_TSGIB.nc
      expatrie  LEVITUS_y0000_1m_TSGIB.nc $DIAGSOUT  LEVITUS_y0000_1m_TSGIB.nc
    fi
                    }

  file_lst_txt=''
  file_lst=''
  for TAG in $(mktaglist $GIB) ; do
   # get gridT file
   rapatrie ${CONFCASE}_${TAG}_gridT.nc $MEANY ${CONFCASE}_${TAG}_gridT.nc
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   #
   suf=$( getsuffix $TAG )
   fbase=${CONFCASE}_y${YEAR}${suf}
   
   #  output files:
   ftgib=${fbase}_TGIB.txt
   fsgib=${fbase}_SGIB.txt
   # 
   ftgib_nc=${fbase}_TGIB.nc
   fsgib_nc=${fbase}_SGIB.nc

   # save file list ( to be sorted later to eliminate duplicate entries )
   file_lst_txt=$( filter_list $file_lst_txt $ftgib             )
   file_lst=$( filter_list $file_lst $fsgib $ftgib_nc $fsgib_nc )

   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
   echo $YEAR $MONTH >> $ftgib ;  echo $YEAR $MONTH >> $fsgib

   cdfmean ${CONFCASE}_${TAG}_gridT.nc votemper T $GIBWIN 0 0 >> $ftgib ; concat_file ${TAG} cdfmean.nc $ftgib_nc
   cdfmean ${CONFCASE}_${TAG}_gridT.nc vosaline T $GIBWIN 0 0 >> $fsgib ; concat_file ${TAG} cdfmean.nc $fsgib_nc
  done

  # dispose TXT file in the ad-hoc -DIAGS/xxx directory
  for f in $file_lst_txt ; do
   define_diags_dir $f
   expatrie $f $DIAGSOUT $f
  done

  # merge matching nc files
  for suf in _1m _1y ; do
    fbase=${CONFCASE}_y${YEAR}${suf}
    ftsgib_nc=${fbase}_TSGIB.nc
    tmplst=''
    for f in $file_lst ; do
      tmplst="$tmplst $(echo $f | grep $fbase )"
    done
    merge_files $ftsgib_nc $tmplst
    if [ -f $ftsgib_nc ] ; then
      define_diags_dir $ftsgib_nc
      expatrie  $ftsgib_nc $DIAGSOUT $ftsgib_nc
    fi
  done

  # compute and dispose climatological equivalent, if necessary
  case $GIB in
    1) mkannuallevitusgib ;;
    2) mkmonthlylevitusgib ;;
    3) mkannuallevitusgib
       mkmonthlylevitusgib ;;
  esac
fi
  
# Ice Volume area and extent for all months: input file : icemod, and mesh_mask
#  keyword : ICEMONTH file_id :  icemonth
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [    $ICEMONTH != 0    ] ; then
   TAG=y${YEAR}
   # get icemod files
   file_lst=''
   for m in $(seq 1 12) ; do
    mm=$( printf "%02d" $m )
    rapatrie  ${CONFCASE}_${TAG}m${mm}_icemod.nc $MEANY ${CONFCASE}_${TAG}m${mm}_icemod.nc
   done
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
 
   # output files:
   fbase=${CONFCASE}_${TAG}_1m
   fice=${fbase}_icemonth.txt
   fice_nc=${fbase}_ICEMONTH.nc

   for m in $(seq 1 12) ; do 
    mm=$( printf "%02d" $m )

    case $mm in 
    01) echo '###' $YEAR $mm > $fice ;;
    *)  echo '###' $YEAR $mm >> $fice ;;
    esac

    cdficediags ${CONFCASE}_${TAG}m${mm}_icemod.nc  >> $fice 
    concat_file ${TAG}m${mm} icediags.nc $fice_nc

   done
 
   expatrie $fice_nc $DIAGS/NC  $fice_nc
 
fi

# El nino indexes : Input files : monthly gridT,  mesh mask
#  keyword : ELNINO  file_id: NINO12 / NINO3 / NINO4 / NINO34
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [    $ELNINO != 0    ] ; then  # always monthly diags
   TAG=y${YEAR}
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # outputfile
   fbase=${CONFCASE}_${TAG}_1m
   fnino=${fbase}_NINO.txt

   fnino12_nc=${fbase}_NINO12.nc
   fnino3_nc=${fbase}_NINO3.nc
   fnino4_nc=${fbase}_NINO4.nc
   fnino34_nc=${fbase}_NINO34.nc
   fnino_nc=${fbase}_NINO.nc   # merged file

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
    ncrename -h -v mean_votemper,mean_votemper_NINO12 cdfmean.nc
    concat_file ${TAG}m${mm} cdfmean.nc $fnino12_nc

    # nino 3     [ -150 W -- -90 W, -5 S -- 5 N ]
    cdfmean  $f votemper T $NINO3 1 1  | tail -1 | awk '{ printf " %8.5f 0.00", $6 }'  >> $fnino 
    ncrename -h -v mean_votemper,mean_votemper_NINO3 cdfmean.nc
    concat_file ${TAG}m${mm} cdfmean.nc $fnino3_nc

    # nino 4     [ -200 W -- -150 W, -5 S -- 5 N ]
    cdfmean  $f votemper T $NINO4 1 1 | tail -1 | awk '{ printf " %8.5f 0.00", $6 }'  >> $fnino  
    ncrename -h -v mean_votemper,mean_votemper_NINO4 cdfmean.nc
    concat_file ${TAG}m${mm} cdfmean.nc $fnino4_nc

    # nino 3.4   [ -170 W -- -120 W, -% S -- % N ]
    cdfmean  $f votemper T $NINO34 1 1 | tail -1 | awk '{ printf " %8.5f 0.00\n", $6 }'  >> $fnino 
    ncrename -h -v mean_votemper,mean_votemper_NINO34 cdfmean.nc
    concat_file ${TAG}m${mm} cdfmean.nc $fnino34_nc
 
   done
 
   merge_files $fnino_nc $fnino12_nc $fnino3_nc $fnino4_nc $fnino34_nc
   ncks -h -x -v mean_3Dvotemper $fnino_nc ztmp.nc ; mv ztmp.nc $fnino_nc
   expatrie $fnino_nc $DIAGS/NC  $fnino_nc
fi

# Transport: Input files: VT, gridU, gridV, mesh mask, section.dat
#  keyword : TRP    file_id : transport
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [            $TRP != 0    ] ; then
  file_lst_txt=''
  file_lst=''
  # clean eventually old x_transport.nc files in this current directory
  \rm -f *transports.nc
   # section.dat describes the position (I,J) of the sections to monitor
   # ./create_sections_list.ksh ${CONFIG%.*}   # to skip .Lxx part of the config name
   ../create_sections_list.ksh ${CONFIG} 

  for TAG in $(mktaglist $TRP) ; do
 
   # get VT , gridU, gridV files
   rapatrie ${CONFCASE}_${TAG}_VT.nc $MEANY ${CONFCASE}_${TAG}_VT.nc
   rapatrie ${CONFCASE}_${TAG}_gridU.nc $MEANY ${CONFCASE}_${TAG}_gridU.nc
   rapatrie ${CONFCASE}_${TAG}_gridV.nc $MEANY ${CONFCASE}_${TAG}_gridV.nc
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii output file:
   suf=$( getsuffix $TAG )
   fbase=${CONFCASE}_y${YEAR}${suf}
   fsection=${fbase}_section_monitor.txt
 
   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
   echo $YEAR $MONTH >> $fsection

   cdftransport   ${CONFCASE}_${TAG}_VT.nc \
                  ${CONFCASE}_${TAG}_gridU.nc \
                  ${CONFCASE}_${TAG}_gridV.nc  < section.dat >> $fsection
 
   # eliminate garbage from txt file ...
   grep -v Give $fsection | grep -v level | grep -v IMAX | grep -v FROM > tmp
   mv -f tmp $fsection

   file_lst_txt=$( filter_list $file_lst_txt $fsection )

   listfiles=$( ls | grep -e "^[0-9]" | grep transports.nc )

   for file in $listfiles ; do
       concat_file ${TAG} $file ${fbase}_$file  
       file_lst=$( filter_list $file_lst ${fbase}_$file )
       \rm -f $file
   done
  done  # loop on tags
  
  # dispose TXT files
  for f in $file_lst_txt ; do
     define_diags_dir $f
     expatrie $f $DIAGSOUT $f
  done

  # merge matching nc files
  for suf in _1m _1y ; do
    fbase=${CONFCASE}_y${YEAR}${suf}
    ftrp_nc=${fbase}_TRANSPORTS.nc
    tmplst=''
    for f in $file_lst ; do
      tmplst="$tmplst $(echo $f | grep $fbase )"
    done
    merge_files $ftrp_nc $tmplst
    if [ -f $ftrp_nc ] ; then
      define_diags_dir $ftrp_nc
      expatrie  $ftrp_nc $DIAGSOUT $ftrp_nc
    fi
  done

fi
 
# DCT :Density Class transport: Input files : gridT, gridU gridV, mesh mask, dens_section.dat
#  keyword : DCT   file_id :  trpsig
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [ $DCT != 0 ] ; then
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

  file_lst=''
  for m in $(seq 1 12) ; do
    mm=$(printf "%02d" $m)
    suf=$( getsuffix ${TAG}m${mm} )
    fbase=${CONFCASE}_${TAG}${suf}

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
    # save netcdf files ( one per section )
    listfiles=$( ls | grep -e "^[0-9]" | grep trpsig.nc  )

    for file in $listfiles ; do
       concat_file ${TAG}m${mm} $file ${fbase}_$file  # this is one monthly file to be archived
       file_lst=$( filter_list $file_lst ${fbase}_$file )
       mv $file $TRPSIGY/${CONFCASE}_${tag}_$file     # we need individual monthly file for cdfmoyweighted
    done

  done  # month loop

  # merge matching nc files
    fbase=${CONFCASE}_y${YEAR}_1m
    ftrpsig_nc=${fbase}_TRPSIG.nc
    tmplst=''
    for f in $file_lst ; do
      tmplst="$tmplst $(echo $f | grep $fbase )"
    done
    merge_files $ftrpsig_nc $tmplst
    if [ -f $ftrpsig_nc ] ; then
      define_diags_dir $ftrpsig_nc
      expatrie  $ftrpsig_nc $DIAGSOUT $ftrpsig_nc
    fi

  here=$(pwd)  # save the actual directory path
  cd $TRPSIGY
  file_lst=''
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
   froot=${CONFCASE}_${TAG}_1y_${section}_trpsig
   concat_file ${TAG} cdfmoy_weighted.nc  $froot.nc
   file_lst=$( filter_list $file_lst $froot.nc )

   ncks -h -v sigtrp  $froot.nc | \
       sed -e 's/=/ /g' | grep -e '^time_counter\[' | grep -e lev | \
       awk '{printf " %07.4f %+14.9e\n", $4, $8}' > $froot.txt
   file_lst=$( filter_list $file_lst $froot.txt )
  done
  
  # save yearly files
  # merge matching nc files
  fbase=${CONFCASE}_y${YEAR}_1y
  ftrpsig_nc=${fbase}_TRPSIG.nc
  tmplst=''
  for f in $file_lst ; do
    tmplst="$tmplst $(echo $f | grep $fbase )"
  done
  merge_files $ftrpsig_nc $tmplst
  if [ -f $ftrpsig_nc ] ; then
    define_diags_dir $ftrpsig_nc
    expatrie  $ftrpsig_nc $DIAGSOUT $ftrpsig_nc
  fi

   # return to tmpdir
   cd $here
   # Erase the TRPSIG tree for this current year
   \rm -rf ${CONFIG} ${CONFCASE}_y*_trpsig.txt *.mtl
fi

# Heat and Salt Meridional Transport : Input files : VT, mesh mask, new_maskglo
#  keyword : MHT   file_id :  hflx
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [            $MHT  !=  0   ] ; then
  file_lst=''
  for TAG in $(mktaglist $MHT) ; do
# (a) From advection:
#--------------------
   # get VT  files
   rapatrie ${CONFCASE}_${TAG}_VT.nc $MEANY ${CONFCASE}_${TAG}_VT.nc
 
   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  $fbasinmask $IDIR $fbasinmask ; fi
 
   # output files:
   suf=$( getsuffix $TAG )
   fbase=${CONFCASE}_y${YEAR}${suf}
   fmhst=${fbase}_mhst.nc
   
   # Netcdf output files: (both head and salt in 2 separated variables)
   cdfmhst  ${CONFCASE}_${TAG}_VT.nc MST   # Save Meridional salt transport as well
   concat_file $TAG mhst.nc $fmhst

   # save file list ( to be sorted later to eliminate duplicate entries )
   file_lst=$( filter_list $file_lst $fmhst )
 
# (b) from Surface Heat fluxes
#-----------------------------
   fhflx=${fbase}_hflx.nc
   rapatrie ${CONFCASE}_${TAG}_gridT.nc $MEANY  ${CONFCASE}_${TAG}_gridT.nc
   cdfhflx  ${CONFCASE}_${TAG}_gridT.nc
   concat_file $TAG cdfhflx.nc $fhflx
   file_lst=$( filter_list $file_lst $fhflx )
 
    \rm hflx.out cdfhflx.nc
  done
  
  # dispose files in the ad-hoc -DIAGS/xxx directory
  # merge matching nc files
  for suf in _1m _1y ; do
    fbase=${CONFCASE}_y${YEAR}${suf}
    fmht_nc=${fbase}_MHT.nc
    tmplst=''
    for f in $file_lst ; do
      tmplst="$tmplst $(echo $f | grep $fbase )"
    done
    merge_files $fmht_nc $tmplst
    if [ -f $fmht_nc ] ; then
      define_diags_dir $fmht_nc
      expatrie  $fmht_nc $DIAGSOUT $fmht_nc
    fi
  done

fi

# MAX and MIN of MOC: requires that MOC files already exists
#  keyword : MAXMOC   file_id : Glo_maxmoc Glo_minmoc   Atl_maxmoc Atl_minmoc 
#                               Inp_minmoc Inp_minmoc2  Aus_maxmoc Aus_minmoc
#                               Glo_maxmoc40N Glo_maxmoc30S Atl_maxmoc40N Atl_maxmoc30S
#                               Inp_minmoc30S  Aus_maxmoc50S Glo_maxmoc15S
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [            $MAXMOC  !=  0    ] ; then

# function rename_maxmoc : append id to variable name
# eg : rename_maxmoc  maxmoc.nc Atl_maxmoc30S
rename_maxmoc()   {
   for var in maxmoc minmoc latmaxmoc latminmoc depthmaxmoc depthminmoc ; do
      ncrename -h -v $var,${var}_$2 $1
   done
                  }

  file_lst_txt=''
  file_lst=''
  for TAG in $(mktaglist $MAXMOC) ; do
   f=moc.nc
   rapatrie ${CONFCASE}_${TAG}_MOC.nc $MEANY $f

   # output file
   MONTH=`echo ${TAG} | awk -Fm '{print $2}'`
   suf=$( getsuffix $TAG )

   fbase=${CONFCASE}_y${YEAR}${suf}
   fmaxmoc=${fbase}_minmaxmoc.txt
   echo $YEAR $MONTH > $fmaxmoc
   fmaxmoc40=${fbase}_maxmoc40.txt
   echo $YEAR $MONTH > $fmaxmoc40
   file_lst_txt=$( filter_list $file_lst_txt $fmaxmoc $fmaxmoc40 )

   fglomaxmoc=${fbase}_Glo_maxmoc.nc
   fglominmoc=${fbase}_Glo_minmoc.nc
   fatlmaxmoc=${fbase}_Atl_maxmoc.nc
   fatlminmoc=${fbase}_Atl_minmoc.nc
   finpminmoc=${fbase}_Inp_minmoc.nc
   finpminmoc2=${fbase}_Inp_minmoc2.nc
   fausmaxmoc=${fbase}_Aus_maxmoc.nc
   fausminmoc=${fbase}_Aus_minmoc.nc
   # for maxmoc at specified lon/lat
   fglomaxmoc40n=${fbase}_Glo_maxmoc40N.nc
   fglomaxmoc30s=${fbase}_Glo_maxmoc30S.nc
   fglomaxmoc15s=${fbase}_Glo_maxmoc15S.nc
   fatlmaxmoc40n=${fbase}_Atl_maxmoc40N.nc
   fatlmaxmoc30s=${fbase}_Atl_maxmoc30S.nc
   finpminmoc30s=${fbase}_Inp_minmoc30S.nc
   fausmaxmoc50s=${fbase}_Aus_maxmoc50S.nc

   case $CONFIG in
        PERIANT05 | PERIANT025 | PERIANT8 )

   #AUS
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 0 2000   | grep Maximum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Glo_maxmoc
   concat_file $TAG maxmoc.nc $fglomaxmoc
   file_lst=$( filter_list $file_lst $fglomaxmoc )

   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 2000 5500  | grep Minimum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Glo_minmoc
   concat_file $TAG maxmoc.nc $fglominmoc
   file_lst=$( filter_list $file_lst $fglominmoc )  ;;

        ORCA12 | ORCA12.L46 | ORCA12.L75 | ORCA025 | ORCA025.L75 | ORCA05 | ORCA2 | ORCA246 )
   # GLO
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo 20 60 500 2000 | grep Maximum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Glo_maxmoc
   concat_file $TAG maxmoc.nc $fglomaxmoc
   file_lst=$( filter_list $file_lst $fglomaxmoc )

   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo -40 30 2000 5500 | grep Minimum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Glo_minmoc
   concat_file $TAG maxmoc.nc $fglominmoc
   file_lst=$( filter_list $file_lst $fglominmoc )


   # ATL
   printf "%s" 'Atl ' >>  $fmaxmoc ; cdfmaxmoc $f atl 0 60 500 2000 | grep Maximum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Atl_maxmoc
   concat_file $TAG maxmoc.nc $fatlmaxmoc
   file_lst=$( filter_list $file_lst $fatlmaxmoc )

   printf "%s" 'Atl ' >>  $fmaxmoc ; cdfmaxmoc $f atl -20 40 2000 5500 | grep Minimum  >> $fmaxmoc
   rename_maxmoc maxmoc.nc Atl_minmoc
   concat_file $TAG maxmoc.nc $fatlminmoc
   file_lst=$( filter_list $file_lst $fatlminmoc )


   #INP
   printf "%s" 'Inp ' >>  $fmaxmoc ; cdfmaxmoc $f inp 15 50 100 1000 | grep Minimum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Inp_minmoc
   concat_file $TAG maxmoc.nc $finpminmoc
   file_lst=$( filter_list $file_lst $finpminmoc )

   printf "%s" 'Inp ' >>  $fmaxmoc ; cdfmaxmoc $f inp -30 20 1000 5500  | grep Minimum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Inp_minmoc2
   concat_file $TAG maxmoc.nc $finpminmoc2
   file_lst=$( filter_list $file_lst $finpminmoc2 )


   #AUS
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 0 2000   | grep Maximum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Aus_maxmoc
   concat_file $TAG maxmoc.nc $fausmaxmoc
   file_lst=$( filter_list $file_lst $fausmaxmoc )

   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 2000 5500  | grep Minimum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Aus_minmoc
   concat_file $TAG maxmoc.nc $fausminmoc
   file_lst=$( filter_list $file_lst $fausminmoc )

   # Max and Min of MOC at some specific latitudes
   # GLO  MAX at 40 N and 30S
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo 40 40 500 2000 | grep Maximum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Glo_maxmoc40N
   concat_file $TAG maxmoc.nc $fglomaxmoc40n
   file_lst=$( filter_list $file_lst $fglomaxmoc40n )

   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -30 -30 500  5500 | grep Maximum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Glo_maxmoc30S
   concat_file $TAG maxmoc.nc $fglomaxmoc30s
   file_lst=$( filter_list $file_lst $fglomaxmoc30s )


   # ATL  MAX at 40N and 30S
   printf "%s" 'Atl ' >>  $fmaxmoc40 ; cdfmaxmoc $f atl 40 40 500 2000 | grep Maximum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Atl_maxmoc40N
   concat_file $TAG maxmoc.nc $fatlmaxmoc40n
   file_lst=$( filter_list $file_lst $fatlmaxmoc40n )

   printf "%s" 'Atl ' >>  $fmaxmoc40 ; cdfmaxmoc $f atl -30 -30  500 5000 | grep Maximum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Atl_maxmoc30S
   concat_file $TAG maxmoc.nc $fatlmaxmoc30s
   file_lst=$( filter_list $file_lst $fatlmaxmoc30s )


   #INP  Min at 30 S
   printf "%s" 'Inp ' >>  $fmaxmoc40 ; cdfmaxmoc $f inp -30 -30 1000 5500  | grep Minimum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Inp_minmoc30S
   concat_file $TAG maxmoc.nc $finpminmoc30s
   file_lst=$( filter_list $file_lst $finpminmoc30s )


   #AUS  MAX at 50 S
   printf "%s" 'Aus ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -50 -50 0 2000   | grep Maximum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Aus_maxmoc50S
   concat_file $TAG maxmoc.nc $fausmaxmoc50s
   file_lst=$( filter_list $file_lst $fausmaxmoc50s ) ;;

       # NATL configuration
        NATL025 | NATL4 | NATL12 )
   # GLO
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo 20 60 500 2000 | grep Maximum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Glo_maxmoc
   concat_file $TAG maxmoc.nc $fglomaxmoc
   file_lst=$( filter_list $file_lst $fglomaxmoc )

   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo -40 30 2000 5500 | grep Minimum >> $fmaxmoc
   rename_maxmoc maxmoc.nc Glo_minmoc
   concat_file $TAG maxmoc.nc $fglominmoc
   file_lst=$( filter_list $file_lst $fglominmoc )


   # Max and Min of MOC at some specific latitudes
   # GLO  MAX at 40 N and 30S
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo 40 40 500 2000 | grep Maximum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Glo_maxmoc40N
   concat_file $TAG maxmoc.nc $fglomaxmoc40n
   file_lst=$( filter_list $file_lst $fglomaxmoc40n )

   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -15 -15 500  5500 | grep Maximum >> $fmaxmoc40
   rename_maxmoc maxmoc.nc Glo_maxmoc15S
   concat_file $TAG maxmoc.nc $fglomaxmoc15s
   file_lst=$( filter_list $file_lst $fglomaxmoc15s ) ;;

   esac
   # clean for next year 
   \rm moc.nc 
  done  # loop on TAG
  
  # dispose txt file first
  for f in $file_lst_txt ; do 
    define_diags_dir $f
    expatrie $f $DIAGSOUT $f
  done

  # dispose files on ad-hoc DIAG/xxx directory
  fmaxmocall=${fbase}_MAXMOC.nc

  # Concatenate all files into  a single one
  fileout_lst=''
  for f in $file_lst ; do
    suf=$( echo $f | awk -F_ '{ print $3 }' )  # suf is then 1m or 1y
    fmaxmocall=${CONFCASE}_y${YEAR}_${suf}_MAXMOC.nc
    ncks -h -A $f $fmaxmocall
    fileout_lst=$( filter_list $fileout_lst $fmaxmocall )
    # clean temporary files
    \rm -f $f 
  done
     
  # dispose file
  for f in $fileout_lst ; do
   define_diags_dir $f 
   expatrie $f $DIAGSOUT $f
  done
fi


# Compare zonal current with TAO moorings: Input file: gridU, gridV, gridT2, coordinates
#  keyword : TAO  file_id : VELOCITY_LATLON VELOCITY_LATLON_UC
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [             $TAO !=  0   ] ; then
  file_lst=''
  for TAG in $(mktaglist $TAO) ; do
    
     suf=$( getsuffix $TAG )
     fbase=${CONFCASE}_y${YEAR}${suf}_VELOCITY

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
        fvel=${fbase}_${LATI}${LONG}.nc
        varu=sovitua
        cdfprofile $I $J $file $varu  # extract profile at given position for given variable
        ncrename -h -O -v sovitua,u_$LONG profile.nc

        concat_file $TAG profile.nc $fvel
        file_lst=$( filter_list $file_lst $fvel )
     done
  done

  # under current time series, from vertical profiles files  ( only based on nco tools)
  file_lst_uc=''  # reset file list for UC files
  for suf in _1m _1y ; do   	
     fbase=${CONFCASE}_y${YEAR}${suf}_VELOCITY
     if [ -f ${fbase}_0n110w.nc ] ; then  # assume that if the 1rst file exist for this suffix, all exist
       # 110W  depth = 80 m
       fvel=${fbase}_0n110w ; dep=80
       cdfprofile 1 1 $fvel.nc u_110w -dep $dep
       mv profile.nc ${fvel}_UC.nc
       ncrename -h -O -v u_110w,u_110w_UC ${fvel}_UC.nc 
       file_lst_uc=$( filter_list $file_lst_uc ${fvel}_UC.nc )

     # 140W  depth = 120m
       fvel=${fbase}_0n140w  ; dep=120
       cdfprofile 1 1 $fvel.nc u_140w -dep $dep
       mv profile.nc ${fvel}_UC.nc
       ncrename -h -O -v u_140w,u_140w_UC ${fvel}_UC.nc 
       file_lst_uc=$( filter_list $file_lst_uc ${fvel}_UC.nc )

     # 170W  depth = 150m
       fvel=${fbase}_0n170w  ; dep=150
       cdfprofile 1 1 $fvel.nc u_170w -dep $dep
       mv profile.nc ${fvel}_UC.nc
       ncrename -h -O -v u_170w,u_170w_UC ${fvel}_UC.nc
       file_lst_uc=$( filter_list $file_lst_uc ${fvel}_UC.nc )

     # 156E  depth = 200m
       fvel=${fbase}_0n156e  ; dep=200
       cdfprofile 1 1 $fvel.nc u_156e -dep $dep
       mv profile.nc ${fvel}_UC.nc
       ncrename -h -O -v u_156e,u_156e_UC ${fvel}_UC.nc
       file_lst_uc=$( filter_list $file_lst_uc ${fvel}_UC.nc )

     # 165E  depth = 200m
       fvel=${fbase}_0n165e  ; dep=200
       cdfprofile 1 1 $fvel.nc u_165e -dep $dep
       mv profile.nc ${fvel}_UC.nc
       ncrename -h -O -v u_165e,u_165e_UC ${fvel}_UC.nc
       file_lst_uc=$( filter_list $file_lst_uc ${fvel}_UC.nc )
     fi
  done

  # remove useless var
  for f in $file_lst_uc ; do
    ncwa -h -F -O -a depth $f -o $f     # remove depth dim
    ncks -h -F -O -x -v depth $f -o $f  # remove depth var
  done

  # merge matching nc files
  file_lst="$file_lst $file_lst_uc"
  for suf in _1m _1y ; do
    fbase=${CONFCASE}_y${YEAR}${suf}
    ftao_nc=${fbase}_TAO.nc
    tmplst=''
    for f in $file_lst ; do
      tmplst="$tmplst $(echo $f | grep $fbase )"
    done
    merge_files $ftao_nc $tmplst
    if [ -f $ftao_nc ] ; then
      define_diags_dir $ftao_nc
      expatrie  $ftao_nc $DIAGSOUT $ftao_nc
    fi
  done
fi

#                        #####################################
#                        ### T O P   B A S E D   D I A G S ###
#                        #####################################
# TRACER DIAGS  : Input files : ptrcT, mesh mask
#  keyword : TRACER   file_id : TRCmean  TRCzonalmean_conc TRCzonalmean_flx TRCzonalsum_flx pendep fracinv
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [ $TRACER != 0 ] ; then
   file_lst=''
 for TAG in $(mktaglist $TRACER) ; do
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  $fbasinmask $IDIR $fbasinmask ; fi
 
   # get tracer file from gaya: note that this is from -S dir (5 day average ... to discuss ...)
   rapatrie ${CONFCASE}_${TAG}_ptrcT.nc $MEANY ${CONFCASE}_${TAG}_ptrcT.nc
   rapatrie ${CONFCASE}_${TAG}_diadT.nc $MEANY ${CONFCASE}_${TAG}_diadT.nc
   rapatrie ${CONFCASE}_${TAG}_ptrcT.nc $MEANY ${CONFCASE}_${TAG}_ptrcT.nc
 
   # output files:
   suf=$(getsuffix $TAG )
   fbase=${CONFCASE}_y${YEAR}${suf}
   ftrc=${fbase}_TRCmean.txt
   ftrc_zomean=${fbase}_TRCzonalmean_flx.txt
   ftrc_zosum=${fbase}_TRCzonalsum_flx.txt
   ftrc_zosurf=${fbase}_TRCzonalsurf_flx.txt

   ftrc_nc=${fbase}_TRCmean.nc
#  ftrc_zomean_nc=${fbase}_TRCzonalmean_flx.nc
#  ftrc_zosum_nc=${fbase}_TRCzonalsum_flx.nc
#  ftrc_zosurf_nc=${fbase}_TRCzonalsurf_flx.nc
 
   # Number of mol in the ocean ...
   printf "%04d "  $YEAR   >>  $ftrc
 
   # CFC11
   \rm -f tmp1
   cdfmean  ${CONFCASE}_${TAG}_diadT.nc  INVCFC T > tmp1
   area=$(cat tmp1 |  grep -e 'Mean value at level' | awk ' {print $12}')
   mean=$(cat tmp1 |  grep -e 'Mean value over the ocean' | awk ' {print $6}')
   total=$(echo $mean $area |  awk '{print $1 * $2 }' )
   printf "%s "  $total  >> $ftrc
   concat_file $TAG  cdfmean.nc $ftrc_nc
   file_lst=$( filter_list $file_lst $ftrc_nc)
 
   if [ $TRACER_BC14 != 0 ] ; then
     # B-C14
     \rm -f tmp1
     cdfmean  ${CONFCASE}_${TAG}_ptrcT.nc  invc14 T > tmp1
     area=$(cat tmp1 |  grep -e 'Mean value at level' | awk ' {print $12}')
     mean=$(cat tmp1 |  grep -e 'Mean value over the ocean' | awk ' {print $6}')
     total=$(echo $mean $area |  awk '{print $1 * $2 }' )
     printf "%s \n"  $total  >> $ftrc
     # append cdfmean.nc variable to the already existing nc file
     concat_file $TAG  cdfmean.nc $ftrc_nc
     file_lst=$( filter_list $file_lst $ftrc_nc)
   fi
 
   # zonal integral of inventories
   cdfzonalsum  ${CONFCASE}_${TAG}_ptrcT.nc  T
   expatrie zonalsum.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalsum_conc.nc  # in MEANDIR, not a time-series
   cdfzonalsum  ${CONFCASE}_${TAG}_diadT.nc  T
   expatrie zonalsum.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalsum_flx.nc
 
   # zonal means
   cdfzonalmean  ${CONFCASE}_${TAG}_ptrcT.nc  T
   expatrie zonalmean.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalmean_conc.nc
   cdfzonalmean  ${CONFCASE}_${TAG}_diadT.nc  T
   expatrie zonalmean.nc $MEANY ${CONFCASE}_${TAG}_TRCzonalmean_flx.nc
 
   # it is used to take only the interesting variables from the results
#   ncks -F -d deptht,1,1 -v zocfc11_glo,zobc14_glo,nav_lon,nav_lat zonalmean.nc zonalsurf.nc
   ncks -h -F -d deptht,1,1 -v zoVCFC_glo,zoRCFC_glo,zoNTCFC_glo,nav_lon,nav_lat zonalmean.nc zonalsurf.nc
 
   # put in ascii format the 1D profiles
   cdfzonalout zonalmean.nc >> $ftrc_zomean
   cdfzonalout zonalsum.nc >>  $ftrc_zosum
   cdfzonalout zonalsurf.nc >>  $ftrc_zosurf
   file_lst=$( filter_list $file_lst $ftrc_zomean $ftrc_zosum $ftrc_zosurf )
 
   # penetration depth
   cdfpendep ${CONFCASE}_${TAG}_ptrcT.nc ${CONFCASE}_${TAG}_diadT.nc -inv INVCFC -trc CFC11
   expatrie pendep.nc $MEANY ${CONFCASE}_${TAG}_pendep.nc

   # Fraction of inventory
   cdffracinv ${CONFCASE}_${TAG}_diadT.nc -inv INVCFC
   expatrie fracinv.nc $MEANY ${CONFCASE}_${TAG}_fracinv.nc
 done

 # dispose time-series files
 for f in $file_lst ; do
   define_diags_dir $f
   expatrie $f $DIAGSOUT $f
 done
 
fi
# PISCES PROFILES : Input files : ptrcT
#  keyword : BIO_PROFILE   file_id: bioprofile
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [ $BIO_PROFILE != 0 ] ; then

   file_lst=''
   for TAG in $( mktaglist $BIO_PROFILE ) ; do
     # get mesh mask files
     rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
     rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
     rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc

     # get tracer file from gaya: 
     rapatrie ${CONFCASE}_${TAG}_ptrcT.nc $MEANY ${CONFCASE}_${TAG}_ptrcT.nc

     # Parameters for integration
     latmin=$( cdffindij -180 180 -75 -45 -c mesh_hgr.nc -p T | tail -2 | head -1 | awk '{ print $4 }' )
   
     # compute vertical profiles
     suf=$(getsuffix $TAG )
     fbioprofile=${CONFCASE}_y${YEAR}${suf}_bioprofile.nc

     jc=0
     for var in DIC Alkalini O2 PO4 Si NO3 Fer DOC ; do 
       cdfmean ${CONFCASE}_${TAG}_ptrcT.nc $var T 0 180 1 $latmin 0 46 
       if [ $jc == 0 ] ; then
         ncks -h -A -v nav_lon,nav_lat,mean_$var cdfmean.nc tmpbioprofile.nc
       else
         ncks -h -A -v mean_$var cdfmean.nc tmpbioprofile.nc
       fi
       jc=$((jc+1))
     done
     concat_file $TAG tmpbioprofile.nc $fbioprofile
     \rm tmpbioprofile.nc
     file_lst=$( filter_list $file_lst $fbioprofile )
   done
   
   # dispose files
   for f in $file_lst ; do
      define_diags_dir $f
      expatrie $f $DIAGSOUT $f
   done

fi

# PISCES integrated values : Input files : ptrcT, diadT
#  keyword : PISCES_INT  file_id : biovertmean
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
if [ $PISCES_INT != 0 ] ; then
   file_lst=''
   for TAG in $(mktaglist $PISCES_INT ) ; do
     # get mesh mask files
     rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
     rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc

     # get tracer file from gaya:
     rapatrie ${CONFCASE}_${TAG}_ptrcT.nc $MEANY ${CONFCASE}_${TAG}_ptrcT.nc
     rapatrie ${CONFCASE}_${TAG}_diadT.nc $MEANY ${CONFCASE}_${TAG}_diadT.nc
   
     # output files
     suf=$(getsuffix $TAG )
     fbiovert=${CONFCASE}_y${YEAR}${suf}_biovertmean.nc

     jc=0
     # compute vertically integrated mean
     for var in DIC Alkalini O2 PO4 Si NO3 Fer DOC NCHL DCHL POC PHY PHY2 ZOO ZOO2 GOC SFe CFC11; do
       cdfvertmean ${CONFCASE}_${TAG}_ptrcT.nc $var T 0 150
       ncrename -h -v sovertmean,vertmean_${var} vertmean.nc
       if [ $jc == 0 ] ; then
         ncks -h -A -v nav_lon,nav_lat,vertmean_$var vertmean.nc tmpvertmean.nc
       else
         ncks -h -A -v vertmean_$var vertmean.nc tmpvertmean.nc
       fi
       jc=$((jc+1))
     done

     for var in PPPHY2 PPPHY PPNEWN PPNEWD PMO PMO2 PAR PH; do
       cdfvertmean ${CONFCASE}_${TAG}_diadT.nc $var T 0 150
       ncrename -h -v sovertmean,vertmean_${var} vertmean.nc
       ncks -A -v vertmean_$var vertmean.nc tmpvertmean.nc
     done

     concat_file $TAG tmpvertmean.nc $fbiovert
     \rm tmpvertmean.nc
     file_lst=$( filter_list $file_lst $fbiovert )
   done

   # dispose files
   for f in $file_lst ; do
      define_diags_dir $f
      expatrie $f $DIAGSOUT $f
   done

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

