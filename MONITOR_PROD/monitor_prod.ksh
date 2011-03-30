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

#------------------------------------------------------------------------------
# PATH:
#-----------------------------------------------------------------------------
  export PATH=$CDFTOOLS/:$PATH

# check if required cdftools are available, exit if missing
  err=0
  for cdfprog in cdfeke cdfmean cdfrmsssh cdfstdevw cdficediags cdftransportiz\
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
  if [ $EKE == 1 ] ; then
   # retrieve U and V ANNUAL mean files and squared mean
     rapatrie  ${CONFCASE}_y${YEAR}_gridU.nc  $MEANY ${CONFCASE}_y${YEAR}_gridU.nc
     rapatrie  ${CONFCASE}_y${YEAR}_gridU2.nc $MEANY ${CONFCASE}_y${YEAR}_gridU2.nc
     rapatrie  ${CONFCASE}_y${YEAR}_gridV.nc  $MEANY ${CONFCASE}_y${YEAR}_gridV.nc
     rapatrie  ${CONFCASE}_y${YEAR}_gridV2.nc $MEANY ${CONFCASE}_y${YEAR}_gridV2.nc
   
   # retrieve a T file needed for headers only (EKE is computed on the T-point)
     rapatrie  ${CONFCASE}_y${YEAR}_gridT2.nc $MEANY  ${CONFCASE}_y${YEAR}_gridT2.nc

   cdfeke ${CONFCASE}_y${YEAR}_gridU.nc \
     ${CONFCASE}_y${YEAR}_gridU2.nc \
     ${CONFCASE}_y${YEAR}_gridV.nc \
     ${CONFCASE}_y${YEAR}_gridV2.nc \
     ${CONFCASE}_y${YEAR}_gridT2.nc

   # dispose file on the MEAN directory
   expatrie eke.nc $MEANY  ${CONFCASE}_y${YEAR}_EKE.nc
   \rm eke.nc
  fi

# RMS SSH and StdDev W : Input files : gridT, gridT2  gridW, gridW2
#^^^^^^^^^^^^^^^^^^^^^^^
  if [ $RMSSSH == 1 ] ; then 
   # RMSSSH :get gridT gridT2
   rapatrie  ${CONFCASE}_y${YEAR}_gridT.nc $MEANY  ${CONFCASE}_y${YEAR}_gridT.nc
   rapatrie  ${CONFCASE}_y${YEAR}_gridT2.nc $MEANY  ${CONFCASE}_y${YEAR}_gridT2.nc
   cdfrmsssh  ${CONFCASE}_y${YEAR}_gridT.nc ${CONFCASE}_y${YEAR}_gridT2.nc

   # dispose file on the MEAN directory
   expatrie rms.nc $MEANY  ${CONFCASE}_y${YEAR}_RMSSSH.nc
   \rm rms.nc

   # StdDev W :get gridW and gridW2 files
   rapatrie ${CONFCASE}_y${YEAR}_gridW.nc $MEANY ${CONFCASE}_y${YEAR}_gridW.nc
   rapatrie ${CONFCASE}_y${YEAR}_gridW2.nc $MEANY ${CONFCASE}_y${YEAR}_gridW2.nc

   cdfstdevw  ${CONFCASE}_y${YEAR}_gridW.nc ${CONFCASE}_y${YEAR}_gridW2.nc

   # dispose file on the MEAN directory
   expatrie rmsw.nc $MEANY ${CONFCASE}_y${YEAR}_STDEVW.nc
  \rm rmsw.nc
  fi

# Barotropic Transport: Input file: gridU, gridV mesh mask
#^^^^^^^^^^^^^^^^^^^^^
  if [ $BSF == 1 ] ; then
   # get gridU gridV files
   rapatrie ${CONFCASE}_y${YEAR}_gridU.nc $MEANY ${CONFCASE}_y${YEAR}_gridU.nc
   rapatrie ${CONFCASE}_y${YEAR}_gridV.nc $MEANY ${CONFCASE}_y${YEAR}_gridV.nc
 
   # get mesh mask files 
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
 
   cdfpsi ${CONFCASE}_y${YEAR}_gridU.nc ${CONFCASE}_y${YEAR}_gridV.nc 
 
   # dispose and rename on the MEAN directory
   expatrie psi.nc  $MEANY ${CONFCASE}_y${YEAR}_PSI.nc
  fi

# MOC Meridional Overturning Circulation:  Input file: gridV, mesh mask, mask_glo
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $MOC == 1 ] ; then
   # get gridV  files
   rapatrie ${CONFCASE}_y${YEAR}_gridV.nc $MEANY ${CONFCASE}_y${YEAR}_gridV.nc
 
   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi
 
   cdfmoc ${CONFCASE}_y${YEAR}_gridV.nc
 
   # dispose on gaya MEAN/YEAR directory
   expatrie moc.nc $MEANY ${CONFCASE}_y${YEAR}_MOC.nc
  fi

# MOCSIG Meridional Overturning Circulation on sigma coordinates:  Input file: gridV, gridT, mesh mask, mask_glo
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  if [ $MOCSIG == 1 ] ; then
   # Compute NREF
   if [ $DREF == 0 ]; then NREF=0 ; fi
   if [ $DREF == 1000 ]; then NREF=1 ; fi
   if [ $DREF == 2000 ]; then NREF=2 ; fi

   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
   if (( $orca == 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi

   if [ $mocsig_annual == 1 ] ; then
     # get annual mean gridV and gridT files
     rapatrie ${CONFCASE}_y${YEAR}_gridV.nc $MEANY ${CONFCASE}_y${YEAR}_gridV.nc
     rapatrie ${CONFCASE}_y${YEAR}_gridT.nc $MEANY ${CONFCASE}_y${YEAR}_gridT.nc
     cdfmocsig ${CONFCASE}_y${YEAR}_gridV.nc ${CONFCASE}_y${YEAR}_gridT.nc $DREF
   else
     # get 5-day averaged gridV and gridT files
     rapatrie_5d gridV $SDIRY $YEAR
     rapatrie_5d gridT $SDIRY $YEAR
     for fileV in ${CONFCASE}_y${YEAR}m*d*_gridV.nc ; do
       fileT=$( echo $fileV | sed -e 's/gridV/gridT/'  )
       fileM=$( echo $fileV | sed -e 's/gridV/MOCSIG/'  )
       cdfmocsig $fileV $fileT $DREF
       mv mocsig.nc $fileM
     done
     cdfmoy ${CONFCASE}_y${YEAR}*MOCSIG.nc
     mv cdfmoy.nc mocsig.nc
   fi

   # dispose on gaya MEAN/YEAR directory
   expatrie mocsig.nc $MEANY ${CONFCASE}_y${YEAR}_MOCSIG_${NREF}.nc
  fi


# Mixed Layer Diagnostics : Input file : gridT for month 03 and 09 mesh_hgr, mesh_zgr
#^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $MXL == 1 ] ; then
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   for m in 3 9  ; do
     f=${CONFCASE}_y${YEAR}m0${m}_gridT.nc 
     g=$(echo $f | sed -e 's/gridT/MXL/')

     rapatrie $f $MEANY $f
 
     cdfmxl  $f
 
     # dispose on gaya, MEAN/YEAR directory
     expatrie mxl.nc $MEANY $g
   done
  fi

# Large scale potential vorticity for m03 m09: input file : gridT, and mesh_mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $LSPV == 1 ] ; then
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc

   for m in 3 9  ; do
     f=${CONFCASE}_y${YEAR}m0${m}_gridT.nc
     g=$(echo $f | sed -e 's/gridT/LSPV/')

     rapatrie $f $MEANY $f
     # compute LSPV
     cdflspv  $f
     # dispose on gaya, MEAN/YEAR directory
     expatrie lspv.nc $MEANY $g
   done
  fi

#=============================================================================
#  PART II: Time series: compute some integral quantities relevant for monitor
#           the ocean variability, and the behaviour of the on going run. 
#           Output is basically a small ASCII file, from which a matlab
#           suitable input file  (.mtl) is derived.
#=============================================================================
# Global MEANS: T S SSH Input files: gridT , mesh_hgr, mesh_zgr, mask
#^^^^^^^^^^^^^^
  if [ $TSMEAN == 1 ] ; then
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
  
   # get gridT files
   rapatrie ${CONFCASE}_y${YEAR}_gridT.nc $MEANY ${CONFCASE}_y${YEAR}_gridT.nc

   # output file name ascii and nc
   fsshmean=${CONFCASE}_y${YEAR}_SSHMEAN.txt  ; fsshmean_nc=${CONFCASE}_y${YEAR}_SSHMEAN.nc
   ftmean=${CONFCASE}_y${YEAR}_TMEAN.txt      ; ftmean_nc=${CONFCASE}_y${YEAR}_TMEAN.nc
   fsmean=${CONFCASE}_y${YEAR}_SMEAN.txt      ; fsmean_nc=${CONFCASE}_y${YEAR}_SMEAN.nc
   # set header on the output file (ASCII) 
   echo $YEAR >  $fsshmean ; echo $YEAR >  $ftmean ;  echo $YEAR >  $fsmean

   # 3D means
   cdfmean  ${CONFCASE}_y${YEAR}_gridT.nc sossheig T >> $fsshmean ; mv cdfmean.nc $fsshmean_nc
   cdfmean  ${CONFCASE}_y${YEAR}_gridT.nc votemper T >> $ftmean   ; mv cdfmean.nc $ftmean_nc
   cdfmean  ${CONFCASE}_y${YEAR}_gridT.nc vosaline T >> $fsmean   ; mv cdfmean.nc $fsmean_nc
 
   # dispose ASCII file in the -DIAGS directory
   expatrie  $fsshmean $DIAGS/TXT $fsshmean
   expatrie  $ftmean   $DIAGS/TXT $ftmean
   expatrie  $fsmean   $DIAGS/TXT $fsmean

   # dispose ASCII file in the -DIAGS/NC directory
   expatrie  $fsshmean_nc $DIAGS/NC $fsshmean_nc
   expatrie  $ftmean_nc   $DIAGS/NC $ftmean_nc
   expatrie  $fsmean_nc   $DIAGS/NC $fsmean_nc

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
  fi

# Ice Volume area and extent for m02 m03   m08 m09: input file : icemod, and mesh_mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $ICE == 1 ] ; then
   # get icemod file for the month 02 03 and 08  09
   rapatrie  ${CONFCASE}_y${YEAR}m02_icemod.nc $MEANY ${CONFCASE}_y${YEAR}m02_icemod.nc
   rapatrie  ${CONFCASE}_y${YEAR}m03_icemod.nc $MEANY ${CONFCASE}_y${YEAR}m03_icemod.nc
   rapatrie  ${CONFCASE}_y${YEAR}m08_icemod.nc $MEANY ${CONFCASE}_y${YEAR}m08_icemod.nc
   rapatrie  ${CONFCASE}_y${YEAR}m09_icemod.nc $MEANY ${CONFCASE}_y${YEAR}m09_icemod.nc
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii/nc output file:
   fice=${CONFCASE}_y${YEAR}_ice.txt
   fice_nc=${CONFCASE}_y${YEAR}_ice.nc
 
   echo '###' $YEAR 02 > $fice
   cdficediags ${CONFCASE}_y${YEAR}m02_icemod.nc  >> $fice ; mv icediags.nc $fice_nc
   echo '###' $YEAR 03 >> $fice
   cdficediags ${CONFCASE}_y${YEAR}m03_icemod.nc  >> $fice ; ncrcat -A $fice_nc icediags.nc -o $fice_nc
   echo '###' $YEAR 08 >> $fice
   cdficediags ${CONFCASE}_y${YEAR}m08_icemod.nc  >> $fice ; ncrcat -A $fice_nc icediags.nc -o $fice_nc
   echo '###' $YEAR 09 >> $fice
   cdficediags ${CONFCASE}_y${YEAR}m09_icemod.nc  >> $fice ; ncrcat -A $fice_nc icediags.nc -o $fice_nc
 
   expatrie $fice    $DIAGS/TXT $fice 
   expatrie $fice_nc $DIAGS/NC  $fice_nc
  fi

# Ice Volume area and extent for all months: input file : icemod, and mesh_mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $ICEMONTH == 1 ] ; then
   # get icemod files
   m=1
   while (( $m <= 12 )) ; do
    mm=$( printf "%02d" $m )
    rapatrie  ${CONFCASE}_y${YEAR}m${mm}_icemod.nc $MEANY ${CONFCASE}_y${YEAR}m${mm}_icemod.nc
    m=$(( m + 1 ))
   done
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc  $IDIR mesh_zgr.nc
 
   # Ascii/nc output file:
   fice=${CONFCASE}_y${YEAR}_icemonth.txt
   fice_nc=${CONFCASE}_y${YEAR}_icemonth.nc
 
   m=1
   while (( $m <= 12 )) ; do
    mm=$( printf "%02d" $m )

    case $mm in 
    01) echo '###' $YEAR $mm > $fice ;;
    *)  echo '###' $YEAR $mm >> $fice ;;
    esac

    cdficediags ${CONFCASE}_y${YEAR}m${mm}_icemod.nc  >> $fice 

    case $mm in
    01) mv icediags.nc $fice_nc ;;
    *)  ncrcat -A $fice_nc icediags.nc -o $fice_nc ;;
    esac

    m=$(( m + 1 ))
   done
 
   expatrie $fice    $DIAGS/TXT $fice 
   expatrie $fice_nc $DIAGS/NC  $fice_nc
 
  fi

# Vertical T-S profiles off the coast of Portugal for Gib monitoring: input file: gridT, mesh_mask
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $GIB == 1 ] ; then
   # get gridT file
   rapatrie ${CONFCASE}_y${YEAR}_gridT.nc $MEANY ${CONFCASE}_y${YEAR}_gridT.nc
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii output files:
   ftgib=${CONFCASE}_y${YEAR}_TGIB.txt
   fsgib=${CONFCASE}_y${YEAR}_SGIB.txt
   # nc output files
   ftgib_nc=${CONFCASE}_y${YEAR}_TGIB.nc
   fsgib_nc=${CONFCASE}_y${YEAR}_SGIB.nc
 
   echo $YEAR > $ftgib
   cdfmean ${CONFCASE}_y${YEAR}_gridT.nc  votemper T $GIBWIN  0 0  >>  $ftgib ; mv cdfmean.nc $ftgib_nc
   echo $YEAR > $fsgib
   cdfmean ${CONFCASE}_y${YEAR}_gridT.nc  vosaline T $GIBWIN  0 0  >>  $fsgib ; mv cdfmean.nc $fsgib_nc
 
   expatrie $ftgib    $DIAGS/TXT $ftgib
   expatrie $fsgib    $DIAGS/TXT $fsgib
   expatrie $ftgib_nc $DIAGS/NC  $ftgib_nc
   expatrie $fsgib_nc $DIAGS/NC  $fsgib_nc

   if [ $(chkfile $DIAGS/TXT/LEVITUS_y0000_TGIB.txt ) == absent ] ; then
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
  fi

# El nino indexes : Input files : monthly gridT,  mesh mask
#^^^^^^^^^^^^^^^^^^
  if [ $ELNINO == 1 ] ; then
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii outputfile
   fnino=${CONFCASE}_y${YEAR}_NINO.txt
   # nc outputfile
   fnino12_nc=${CONFCASE}_y${YEAR}_NINO12.nc
   fnino3_nc=${CONFCASE}_y${YEAR}_NINO3.nc
   fnino4_nc=${CONFCASE}_y${YEAR}_NINO4.nc
   fnino34_nc=${CONFCASE}_y${YEAR}_NINO34.nc

   # special function for concatenation of Netcdf output
   cdfmean_concat() { case $mm in
      01)  mv cdfmean.nc $1 ;;
      *)   ncrcat -A $1 cdfmean.nc -o $1 ;;
      esac        ; }
 
   # get monthly mean gridT files and compute mean SST on each NINO box
   \rm $fnino
   for  m in  1 2 3 4 5 6 7 8 9 10 11 12 ; do
     mm=$(printf "%02d" $m)
     f=${CONFCASE}_y${YEAR}m${mm}_gridT.nc 

     rapatrie $f $MEANY  $f
 
     #  header
     printf "%04d %02d" $YEAR $m >>   $fnino
 
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
  if [ $TRP == 1 ] ; then
   # section.dat describes the position (I,J) of the sections to monitor
   # ./create_sections_list.ksh ${CONFIG%.*}   # to skip .Lxx part of the config name
   ../create_sections_list.ksh ${CONFIG} 
 
   # get VT , gridU, gridV files
   rapatrie ${CONFCASE}_y${YEAR}_VT.nc $MEANY ${CONFCASE}_y${YEAR}_VT.nc
   rapatrie ${CONFCASE}_y${YEAR}_gridU.nc $MEANY ${CONFCASE}_y${YEAR}_gridU.nc
   rapatrie ${CONFCASE}_y${YEAR}_gridV.nc $MEANY ${CONFCASE}_y${YEAR}_gridV.nc
 
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
 
   # Ascii output file:
   fsection=${CONFCASE}_y${YEAR}_section_monitor.txt
 
   echo $YEAR > $fsection
 
   # clean eventually old x_transport.nc files in this current directory
   \rm -f *transports.nc

   cdftransportiz ${CONFCASE}_y${YEAR}_VT.nc \
                  ${CONFCASE}_y${YEAR}_gridU.nc \
                  ${CONFCASE}_y${YEAR}_gridV.nc  < section.dat >> $fsection
 
   # eliminate garbage from txt file ...
   grep -v Give $fsection | grep -v level | grep -v IMAX | grep -v FROM > tmp
   mv -f tmp $fsection
   
   expatrie  $fsection $DIAGS/TXT $fsection
   
   # save x_transports.nc file
   listfiles=$( ls | grep transports.nc )

   for file in $listfiles ; do
       mv $file ${CONFCASE}_y${YEAR}_$file
       expatrie ${CONFCASE}_y${YEAR}_$file $DIAGS/NC ${CONFCASE}_y${YEAR}_$file
   done

  fi
 
# Heat and Salt Meridional Transport : Input files : VT, mesh mask, new_maskglo
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $MHT == 1 ] ; then
# (a) From advection:
#--------------------
   # get VT  files
   rapatrie ${CONFCASE}_y${YEAR}_VT.nc $MEANY ${CONFCASE}_y${YEAR}_VT.nc
 
   # get mesh mask files + new_maskglo
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi
 
   # Ascii output files:
   fheat=${CONFCASE}_y${YEAR}_heattrp.dat
   fsalt=${CONFCASE}_y${YEAR}_salttrp.dat
   # Netcdf output files: (both head and salt in 2 separated variables)
 
   cdfmhst  ${CONFCASE}_y${YEAR}_VT.nc MST   # Save Meridional salt transport as well
   mv mhst.nc ${CONFCASE}_y${YEAR}_mhst.nc
 
   expatrie zonal_heat_trp.dat $DIAGS/TXT $fheat
   expatrie zonal_salt_trp.dat $DIAGS/TXT $fsalt
   expatrie ${CONFCASE}_y${YEAR}_mhst.nc $DIAGS/NC ${CONFCASE}_y${YEAR}_mhst.nc
 
   # needed below with the correct name
   cp zonal_heat_trp.dat ${CONFCASE}_y${YEAR}_heattrp.dat
 
# (b) from Surface Heat fluxes
#-----------------------------
    rapatrie ${CONFCASE}_y${YEAR}_gridT.nc $MEANY  ${CONFCASE}_y${YEAR}_gridT.nc
    cdfhflx  ${CONFCASE}_y${YEAR}_gridT.nc
 
    expatrie hflx.out $DIAGS/TXT ${CONFCASE}_y${YEAR}_hflx.dat
    expatrie cdfhflx.nc $DIAGS/NC ${CONFCASE}_y${YEAR}_hflx.nc
  fi


# MAX and MIN of MOC: requires that MOC files already exists
#^^^^^^^^^^^^^^^^^^^^
  if [ $MAXMOC == 1  ] ; then
   f=moc.nc
   rapatrie ${CONFCASE}_y${YEAR}_MOC.nc $MEANY $f

   # Ascii output file
   fmaxmoc=${CONFCASE}_y${YEAR}_minmaxmoc.txt
   echo $YEAR > $fmaxmoc
   fmaxmoc40=${CONFIG}-${CASE}_y${YEAR}_maxmoc40.txt
   echo $YEAR > $fmaxmoc40

   case $CONFIG in
        PERIANT05 | PERIANT025 | PERIANT8 )

   #AUS
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 0 2000   | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_maxmoc.nc
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 2000 5500  | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_minmoc.nc 
   
   expatrie $fmaxmoc $DIAGS/TXT $fmaxmoc ;;

        ORCA12 | ORCA12.L46 | ORCA025 | ORCA025.L75 | ORCA05 | ORCA2 | ORCA246 )
   # GLO
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo 20 60 500 2000 | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_maxmoc.nc
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo -40 30 2000 5500 | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_minmoc.nc
   # ATL
   printf "%s" 'Atl ' >>  $fmaxmoc ; cdfmaxmoc $f atl 0 60 500 2000 | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Atl_maxmoc.nc
   printf "%s" 'Atl ' >>  $fmaxmoc ; cdfmaxmoc $f atl -20 40 2000 5500 | grep Minimum  >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Atl_minmoc.nc
   #INP
   printf "%s" 'Inp ' >>  $fmaxmoc ; cdfmaxmoc $f inp 15 50 100 1000 | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Inp_minmoc.nc
   printf "%s" 'Inp ' >>  $fmaxmoc ; cdfmaxmoc $f inp -30 20 1000 5500  | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Inp_minmoc2.nc
   #AUS
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 0 2000   | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Aus_maxmoc.nc
   printf "%s" 'Aus ' >>  $fmaxmoc ; cdfmaxmoc $f glo -70 0 2000 5500  | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Aus_minmoc.nc
   
   expatrie $fmaxmoc $DIAGS/TXT $fmaxmoc

   # Max and Min of MOC at some specific latitudes
   # GLO  MAX at 40 N and 30S
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo 40 40 500 2000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_maxmoc40N.nc
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -30 -30 500  5500 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_maxmoc30S.nc
   # ATL  MAX at 40N and 30S
   printf "%s" 'Atl ' >>  $fmaxmoc40 ; cdfmaxmoc $f atl 40 40 500 2000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Atl_maxmoc40N.nc
   printf "%s" 'Atl ' >>  $fmaxmoc40 ; cdfmaxmoc $f atl -30 -30  500 5000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Atl_maxmoc30S.nc
   #INP  Min at 30 S
   printf "%s" 'Inp ' >>  $fmaxmoc40 ; cdfmaxmoc $f inp -30 -30 1000 5500  | grep Minimum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Inp_minmoc30S.nc
   #AUS  MAX at 50 S
   printf "%s" 'Aus ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -50 -50 0 2000   | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Aus_maxmoc50S.nc

   expatrie $fmaxmoc40 $DIAGS/TXT $fmaxmoc40 ;;

       # NATL configuration
        NATL025 | NATL4 | NATL12 )
   # GLO
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo 20 60 500 2000 | grep Maximum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_maxmoc.nc
   printf "%s" 'Glo ' >>  $fmaxmoc ; cdfmaxmoc $f glo -40 30 2000 5500 | grep Minimum >> $fmaxmoc
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_minmoc.nc
   expatrie $fmaxmoc $DIAGS/TXT $fmaxmoc

   # Max and Min of MOC at some specific latitudes
   # GLO  MAX at 40 N and 30S
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo 40 40 500 2000 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_maxmoc40N.nc
   printf "%s" 'Glo ' >>  $fmaxmoc40 ; cdfmaxmoc $f glo -15 -15 500  5500 | grep Maximum >> $fmaxmoc40
     expatrie maxmoc.nc $DIAGS/NC ${CONFIG}-${CASE}_y${YEAR}_Glo_maxmoc15S.nc

   expatrie $fmaxmoc40 $DIAGS/TXT $fmaxmoc40 ;;

   esac
   # clean for next year 
   \rm moc.nc 
  fi


# DCT :Density Class transport: Input files : gridT, gridU gridV, mesh mask, dens_section.dat
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $DCT == 1 ] ; then
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

  for  m in  1 2 3 4 5 6 7 8 9 10 11 12 ; do
    mm=$(printf "%02d" $m)
    tfich=${CONFCASE}_y${YEAR}m${mm}_gridT.nc 
    ufich=$(echo  $tfich | sed -e 's/gridT/gridU/' )
    vfich=$(echo  $tfich | sed -e 's/gridT/gridV/' )

    #get files on gaya
    rapatrie  $tfich $MEANY  $tfich
    rapatrie  $ufich $MEANY  $ufich
    rapatrie  $vfich $MEANY  $vfich
    
    #retrieve tag time from file name
    tag=$(echo $tfich | sed -e "s/${CONFCASE}_//" -e 's/_gridT.nc//')

    echo $tag > ${CONFCASE}_y${tag}_trpsig_monitor.lst

    cdfsigtrp $tfich $ufich $vfich 21 30 180 -print  >>  ${CONFCASE}_y${tag}_trpsig_monitor.lst
    # save netcdf files
    listfiles=$( ls | grep trpsig.nc  )

    for file in $listfiles ; do
        expatrie $file $DIAGS/NC ${CONFCASE}_${tag}_$file
        mv $file $TRPSIGY/${CONFCASE}_${tag}_$file
    done

    # and create a mirror on the local tmpdir
    mv ${CONFCASE}_y${tag}_trpsig_monitor.lst  $TRPSIGY

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
   cdfmoy_weighted ${CONFCASE}_y${YEAR}m??_${section}_trpsig.nc 
   froot=${CONFCASE}_y${YEAR}_${section}_trpsig
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
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc
   if (( $orca != 0 )) ; then rapatrie  new_maskglo.nc $IDIR new_maskglo.nc ; fi
 
   # get tracer file from gaya: note that this is from -S dir (5 day average ... to discuss ...)
   rapatrie ${CONFCASE}_y${YEAR}m12d31_ptrcT.nc $SDIRY ${CONFCASE}_y${YEAR}m12d31_ptrcT.nc
   rapatrie ${CONFCASE}_y${YEAR}m12d31_diadT.nc $SDIRY ${CONFCASE}_y${YEAR}m12d31_diadT.nc
   rapatrie ${CONFCASE}_y${YEAR}_ptrcT.nc $MEANY ${CONFCASE}_y${YEAR}_ptrcT.nc
 
   # Ascii output file:
   ftrc=${CONFCASE}_y${YEAR}_TRCmean.dat
   ftrc_nc=${CONFCASE}_y${YEAR}_TRCmean.nc
 
   # Number of mol in the ocean ...
   printf "%04d "  $YEAR   >  $ftrc
 
   # CFC11
   \rm -f tmp1
   cdfmean  ${CONFCASE}_y${YEAR}m12d31_diadT.nc  INVCFC T > tmp1
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
   cdfzonalsum  ${CONFCASE}_y${YEAR}m12d31_ptrcT.nc  T
   expatrie zonalsum.nc $MEANY ${CONFCASE}_y${YEAR}_TRCzonalsum_conc.nc
   cdfzonalsum  ${CONFCASE}_y${YEAR}m12d31_diadT.nc  T
 
   # zonal means
   cdfzonalmean  ${CONFCASE}_y${YEAR}m12d31_ptrcT.nc  T
   expatrie zonalmean.nc $MEANY ${CONFCASE}_y${YEAR}_TRCzonalmean_conc.nc
   cdfzonalmean  ${CONFCASE}_y${YEAR}m12d31_diadT.nc  T
 
   # ncks is required on the prod machine ... !! not standard !!
   # it is used to take only the interesting variables from the results
#   ncks -F -d deptht,1,1 -v zocfc11_glo,zobc14_glo,nav_lon,nav_lat zonalmean.nc zonalsurf.nc
   ncks -F -d deptht,1,1 -v zoVCFC_glo,zoRCFC_glo,zoNTCFC_glo,nav_lon,nav_lat zonalmean.nc zonalsurf.nc
 
   # put in ascii format the 1D profiles
   cdfzonalout zonalmean.nc > zonalmean.dat
   cdfzonalout zonalsum.nc >  zonalsum.dat
   cdfzonalout zonalsurf.nc >  zonalsurf.dat
 
   expatrie zonalmean.nc $MEANY ${CONFCASE}_y${YEAR}_TRCzonalmean_flx.nc
   expatrie zonalsum.nc $MEANY ${CONFCASE}_y${YEAR}_TRCzonalsum_flx.nc
 
   expatrie zonalmean.dat $DIAGS/TXT ${CONFCASE}_y${YEAR}_TRCzonalmean_flx.dat
   expatrie zonalsum.dat $DIAGS/TXT ${CONFCASE}_y${YEAR}_TRCzonalsum_flx.dat
   expatrie zonalsurf.dat $DIAGS/TXT ${CONFCASE}_y${YEAR}_TRCzonalsurf_flx.dat
   \rm zonalsurf.nc

   # penetration depth
   cdfpendep ${CONFCASE}_y${YEAR}m12d31_ptrcT.nc ${CONFCASE}_y${YEAR}m12d31_diadT.nc -inv INVCFC -trc CFC11
   expatrie pendep.nc $MEANY ${CONFCASE}_y${YEAR}_pendep.nc

   # Fraction of inventory
   cdffracinv ${CONFCASE}_y${YEAR}m12d31_diadT.nc -inv INVCFC
   expatrie fracinv.nc $MEANY ${CONFCASE}_y${YEAR}_fracinv.nc
 
  fi

# Compare zonal current with TAO moorings: Input file: gridU, gridV, gridT2, coordinates
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  if [ $TAO == 1 ] ; then
     
     LAT=0
     u=${CONFCASE}_y${YEAR}_gridU.nc
     v=${CONFCASE}_y${YEAR}_gridV.nc
     t2=${CONFCASE}_y${YEAR}_gridT2.nc 

     rapatrie $u  $MEANY $u
     rapatrie $v  $MEANY $v
     rapatrie $t2 $MEANY $t2

     rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc  $IDIR mesh_hgr.nc

     cdfvita $u $v $t2
     \mv vita.nc ${CONFCASE}_y${YEAR}_vita.nc

     for LON in 156 165 -110 -140 -170 ; do

        I=$( cdffindij $LON $LON $LAT $LAT mesh_hgr.nc T | tail -2 | head -1  | awk '{print $1 }' )
        J=$( cdffindij $LON $LON $LAT $LAT mesh_hgr.nc T | tail -2 | head -1  | awk '{print $3 }' )
        LONG=${LON}e
        LATI=${LAT}n
        if (( $LON < 0 )) ; then LONG=${LON}w ; fi
        if (( $LAT < 0 )) ; then LATI=${LAT}s ; fi
        LONG=$( echo $LONG | sed -e 's/-//' )
        LATI=$( echo $LATI | sed -e 's/-//' )
        file=${CONFCASE}_y${YEAR}_vita.nc
        varu=sovitua
        cdfprofile $I $J $file $varu  # extraire les profils aux coordonnees desirees
        mv profile.nc ${CONFCASE}_y${YEAR}_VELOCITY_${LATI}${LONG}.nc
        ncrename -O -v sovitua,u_$LONG ${CONFCASE}_y${YEAR}_VELOCITY_${LATI}${LONG}.nc 
        expatrie ${CONFCASE}_y${YEAR}_VELOCITY_${LATI}${LONG}.nc $DIAGS/NC  ${CONFCASE}_y${YEAR}_VELOCITY_${LATI}${LONG}.nc

     done
     	
  uclev=$( find_model_level depth 80 ${CONFCASE}_y${YEAR}_VELOCITY_0n110w.nc )
  ncks -F -O -v u_110w -d depth,$uclev,$uclev ${CONFCASE}_y${YEAR}_VELOCITY_0n110w.nc \
                     -o ${CONFCASE}_y${YEAR}_VELOCITY_0n110w_UC.nc
  newval=$( interpolation_vertical depth 80 u_110w ${CONFCASE}_y${YEAR}_VELOCITY_0n110w.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_110w[time_counter] = float($newval) " ${CONFCASE}_y${YEAR}_VELOCITY_0n110w_UC.nc ${CONFCASE}_y${YEAR}_VELOCITY_0n110w_UC.nc
  ncrename -O -v u_110w,u_110w_UC ${CONFCASE}_y${YEAR}_VELOCITY_0n110w_UC.nc 

  uclev=$( find_model_level depth 120 ${CONFCASE}_y${YEAR}_VELOCITY_0n140w.nc )
  ncks -F -O -v u_140w -d depth,$uclev,$uclev ${CONFCASE}_y${YEAR}_VELOCITY_0n140w.nc \
                     -o ${CONFCASE}_y${YEAR}_VELOCITY_0n140w_UC.nc
  newval=$( interpolation_vertical depth 120 u_140w ${CONFCASE}_y${YEAR}_VELOCITY_0n140w.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_140w[time_counter] = float($newval) " ${CONFCASE}_y${YEAR}_VELOCITY_0n140w_UC.nc ${CONFCASE}_y${YEAR}_VELOCITY_0n140w_UC.nc
  ncrename -O -v u_140w,u_140w_UC ${CONFCASE}_y${YEAR}_VELOCITY_0n140w_UC.nc 
	
  uclev=$( find_model_level depth 150 ${CONFCASE}_y${YEAR}_VELOCITY_0n170w.nc ) 
  ncks -F -O -v u_170w -d depth,$uclev,$uclev ${CONFCASE}_y${YEAR}_VELOCITY_0n170w.nc \
                     -o ${CONFCASE}_y${YEAR}_VELOCITY_0n170w_UC.nc
  newval=$( interpolation_vertical depth 150 u_170w ${CONFCASE}_y${YEAR}_VELOCITY_0n170w.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_170w[time_counter] = float($newval) " ${CONFCASE}_y${YEAR}_VELOCITY_0n170w_UC.nc ${CONFCASE}_y${YEAR}_VELOCITY_0n170w_UC.nc
  ncrename -O -v u_170w,u_170w_UC ${CONFCASE}_y${YEAR}_VELOCITY_0n170w_UC.nc 

  uclev=$( find_model_level depth 200 ${CONFCASE}_y${YEAR}_VELOCITY_0n156e.nc )
  ncks -F -O -v u_156e -d depth,$uclev,$uclev ${CONFCASE}_y${YEAR}_VELOCITY_0n156e.nc \
                     -o ${CONFCASE}_y${YEAR}_VELOCITY_0n156e_UC.nc
  newval=$( interpolation_vertical depth 200 u_156e ${CONFCASE}_y${YEAR}_VELOCITY_0n156e.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_156e[time_counter] = float($newval) " ${CONFCASE}_y${YEAR}_VELOCITY_0n156e_UC.nc ${CONFCASE}_y${YEAR}_VELOCITY_0n156e_UC.nc
  ncrename -O -v u_156e,u_156e_UC ${CONFCASE}_y${YEAR}_VELOCITY_0n156e_UC.nc 

  uclev=$( find_model_level depth 200 ${CONFCASE}_y${YEAR}_VELOCITY_0n165e.nc )
  ncks -F -O -v u_165e -d depth,$uclev,$uclev ${CONFCASE}_y${YEAR}_VELOCITY_0n165e.nc \
                     -o ${CONFCASE}_y${YEAR}_VELOCITY_0n165e_UC.nc
  newval=$( interpolation_vertical depth 200 u_165e ${CONFCASE}_y${YEAR}_VELOCITY_0n165e.nc )
  echo Value interpolated to $newval
  ncap -F -O -s "u_165e[time_counter] = float($newval) " ${CONFCASE}_y${YEAR}_VELOCITY_0n165e_UC.nc ${CONFCASE}_y${YEAR}_VELOCITY_0n165e_UC.nc
  ncrename -O -v u_165e,u_165e_UC ${CONFCASE}_y${YEAR}_VELOCITY_0n165e_UC.nc 


  for file in ${CONFCASE}_*UC.nc ; do

     ncwa -F -O -a depth $file -o $file     # remove depth dim
     ncks -F -O -x -v depth $file -o $file  # remove depth var
     expatrie $file $DIAGS/NC  $file

  done

fi

# PISCES PROFILES : Input files : ptrcT
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

if [ $BIO_PROFILE == 1 ] ; then
   # get mesh mask files
   rapatrie  ${MESH_MASK_ID}_byte_mask.nc $IDIR mask.nc
   rapatrie  ${MESH_MASK_ID}_mesh_hgr.nc $IDIR mesh_hgr.nc
   rapatrie  ${MESH_MASK_ID}_mesh_zgr.nc $IDIR mesh_zgr.nc

   # get tracer file from gaya: 
   rapatrie ${CONFCASE}_y${YEAR}_ptrcT.nc $MEANY ${CONFCASE}_y${YEAR}_ptrcT.nc

   # Parameters for integration
   latmin=$( cdffindij -180 180 -75 -45 mesh_hgr.nc T | tail -2 | head -1 | awk '{ print $4 }' )
   
   # compute vertical profiles
   jc=0
   for var in DIC Alkalini O2 PO4 Si NO3 Fer DOC ; do 
     cdfmean ${CONFCASE}_y${YEAR}_ptrcT.nc $var T 0 180 1 $latmin 0 46 
     if [ $jc == 0 ] ; then
       mv cdfmean.nc ${CONFCASE}_y${YEAR}_bioprofile.nc
     else
       ncks -A -v mean_$var cdfmean.nc ${CONFCASE}_y${YEAR}_bioprofile.nc
     fi
     jc=$((jc+1))
   done

   expatrie ${CONFCASE}_y${YEAR}_bioprofile.nc $MEANY  ${CONFCASE}_y${YEAR}_bioprofile.nc 

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

