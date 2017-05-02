#!/bin/ksh
##     ***  script  config_def.ksh  ***
##  Set environment variables for DMONTOOLS scripts
## =====================================================================
## History : 1.0  !  2008     J.M. Molines      Original  code
##           2.0  !  2012     all contrib       Rationalization
## ----------------------------------------------------------------------
##  DMONTOOLS_2.0 , MEOM 2012
##  $Id: config_def.ksh 540 2012-12-19 14:08:40Z molines $
##  Copyright (c) 2012, J.-M. Molines
##  Software governed by the CeCILL licence (Licence/DMONTOOLSCeCILL.txt)
## ----------------------------------------------------------------------

# this is the config_def.ksh file
# it must be customized for each configuration
# Name of CONFIG and CASE

CONFIG=<CONFIG>
CASE=<CASE>
MACHINE=<MACHINE>
NODETYPE=BDW28
ulimit -s unlimited  # needed for occigen 

MAIL=<MAILTO>      ## to be edited ##

# parallel monitoring (1: yes / 0: no)
useMPI=1 

# XIOS output from model ? (set XIOS=   if not used
XIOS=5d
NC4=1        # if set to 1 use netcdf4 capabilities of the cdftools
VVL=1        # if set to 1 use vvl capabilities of the cdftools
FULL=0       # if set to other thing than 0, means you are using z-steps, no partial-steps.
LIM3=0       # if set to 1 use LIM3 ice model and its std output in icemod3. If set to 0 use LIM2 (icemod)
FLXT=0       # if set to 1 means that fluxes are in a separate fllxT file ( not in gridT)

if [ $XIOS ] ; then
   xiosid="."$XIOS
else
   xiosid=
fi

CONFCASE=${CONFIG}-${CASE}
MESH_MASK_ID=$CONFIG     # root part of the mesh-mask files (likely  to be edited !!)
                         # (they must be in the -I directory ( $CONFIG/${CONFIG}-I)
                         #  Standard name is thus : ${MESH_MASK_ID}_byte_mask.nc
                         #                          ${MESH_MASK_ID}_mesh_hgr.nc
                         #                          ${MESH_MASK_ID}_mesh_zgr.nc
#TSCLIM=Gouretski        # if TSCLIM is not defined here it takes Levitus_p2.1 as default
#TSCLIM=WOA2009          # if TSCLIM is not defined here it takes Levitus_p2.1 as default


######################################################################
### modules

######################################################################
### -1- MONITOR PROD Menu :
### for ICEMONTH, BIOPROFILE, TRACER, EL NINO, DCT: set to
###    - 1 if you want it
###    - anything else if you do not
### for EKE, RMSSSH, TSMEAN, TSLAT, GIB, TRP, BSF, MOC, MAXMOC, TAO, MHT, MOCSIG: set to 
###    - 1 if you want annual means
###    - 2 if you want monthly means
###    - 3 if you want both
###    - anything else if you don't want anything
### particularity for MOCSIG diagnostics, set mocsig_5d to:
###    - 0 if you want to compute annual/monthly MOCSIG means from annual/monthly V/T means
###    - 1 if you want to compute annual/monthly MOCSIG means from 5day V/T means
###    - 2 if you want both
### for MXL and LSPV: set to
###    - 1 if you want 3 (march) and 9 (september) monthly means
###    - 2 if you want monthly means (every months of the year)
###    - 3 if you want monthly means and annual mean (computed from monthly mean)
###    - anything else if you don't want anything
######################################################################

EKE=0                    # compute EKE
RMSSSH=0                 # compute RMS ssh and w
TSMEAN=0                 # compute TSMEAN and ssh drift
TSLAT=0                  # compute TSMEAN and ssh drift for latitude bands 60S - 60N
ICEMONTH=0               # compute monthly ice volume, area and extent
GIB=0                    # compute Gibraltar diags (restoring zone)
KERG=0                   # compute Kerguelen diags (restoring zone)
KERGb=0                  # compute Kerguelen diags for PISCES (restoring zone)
ELNINO=0                 # compute El Nino monitoring SSTs
TRP=0                    # compute barotropic transport accross section as given in section.dat (CTL dir)
MHT=0                    # compute Meridional Heat Transport (advective and from surface fluxes)
MOC=0                    # compute MOC ( need a sub basin mask file called new_maskglo.nc)
MOCSIG=0                 # compute MOC in sigma coordinates
# To be set if MOCSIG=1/2/3
mocsig_5d=0              # compute MOC in sigma coordinates from:
                         #   5-day averaged files (1); or annual files (0); or both (2)
DREF=2000                # potential density referenced at $DREF m is the sigma coordinate used
#
MAXMOC=0                 # diagnose the min and max of MOC
BSF=0                    # compute the BSF (psi) from U and V
DCT=0                    # compute density class transports for section given in dens_section.dat (CTL dir)
MXL=0                    # compute mixed layer depth from 3 criteria for month 03 and 09
TRACER=0                 # compute passive Tracer statistics
LSPV=0                   # compute large scale potential vorticity
TAO=0                    # compute profiles at TAO moorings location
BIO_PROFILE=0            # compute PISCES mean vertical profiles
PISCES_INT=0             # compute PISCES vertical integration

######################################################################
### -2- PLOT 2D Menu :
### (set to 1 if you want it, to anything else if you do not !)
######################################################################

create_gif=1             #    Create gif (1) or not (0)

moc=0                    # 1.0 Meridional Overturning Circulation
global=0                 # 2.0 Global maps
glodifmap=0              # 3.0 Plot of differences with reynolds and Levitus
fluxes=0                 # 4.0 Air Sea fluxes
hsd=0                    # 4.2 Hallberg Salinity Damping
atlTS=0                  # 5.1.1 Details in North and South Atlantic TS
atlUV=0                  # 5.1.2 Details in North and South Atlantic UV (PSI)
botsig4atl=0             # 5.1.3 Details on bottom sigma4 in the North atlantic

                         # 5.2 Zoomed area. zoomXX variables can take the following values:
##########################
# Code for Regional maps #
##########################    Example of valid zoomXXX variable :
#  0   #   no plot       #      zoomFLX=Ke   or zoomFLX=Dr.Ke.Cb
#  Dr  #   DRAKE         #
#  Ke  #   KERGUELEN     #
#  Cb  #   CAMPBELL      #
#  At  #   ATLANTIC      #
#  Na  #   ATLN          #
#  Sa  #   ATLS          #
##########################
zoomTS=0 #Dr.Ke.Cb.Na.Sa     # 5.2.1 Regional details for temperature and salinity at various depths ( 0 200 1000 2000 3000 4000 5000 ) 
zoomUV=0 #Dr.Ke.Cb.Na.Sa     # 5.2.2 Regional details for Barotropic Stream function (PSI)
zoomFLX=0 #Dr.Ke.Cb          # 5.2.3 Regional details for Air-sea fluxes
zoomMXL=0 #Dr.Ke.Cb          # 5.2.4 Regional details for Mixed Layer depths
zoomICE=0 #Dr.Ke.Cb          # 5.2.5 Regional details for Ice concentration and Ice thickness in march and september
zoomEKE=0 #Dr.Ke.Cb.At.Na.Sa # 5.2.6 Regional details for Eddy Kinetic Energy

coupes=0                 # 6.1 Sections in the global ocean
ar7w=0                   # 6.2 AR7W in the Labrador sea in march and october + Ovide
coupes_aus=0             # 6.3 Sections in the Southern Oceans
coupes_aus_sigma=0       # 6.4 Sections of density in the Southern Ocean (WOCE like)
circum=0                 # 6.5 Sections along the ACC
circum_sigma=0           # 6.6 Density Sections along the ACC
iceplt=0                 # 7.0 Polar plots of ice thickness and concentration
dwbc=0                   # 8.1 Deep Western Boundary Current
dwbc_aus=0               # 8.2 Deep Western Boundary Current at 31 S
ekes=0                   # 9.0 Plot of EKE
mxls=0                   # 10. Mixed Layer depth
mxlatl=0                 # 11.  Mixed Layer in the Atlantic
flxatl=0                 # 12. Air Sea fluxes on the Atlantic
tracer=0                 # 13. Passive tracers
mocsig=0                 # 14. Meridional Overturning Circulation in sigma coordinates
cfc=0                    # 15. CFC plots
pisces_global=0          # 16.1.1 PISCES global plots of surface and at 150m depth concentrations
pisces_diags=0           # 16.1.2 PISCES global plots of diagnostics variables 
pisces_global_int=0      # 16.1.3 PISCES global plots of concentrations integrated between surface and 150m
pisces_diags_int=0       # 16.1.4 PISCES global plots of diagnostics integrated between surface and 150m
pisces_global_clim=0     # 16.1.5 PISCES global plots of comparison to climatologies plots
pisces_zooms=0           # 16.2.1 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots of surface and at 150m depth concentrations
pisces_diags_zooms=0     # 16.2.2 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots of diagnostics
pisces_zooms_int=0       # 16.2.3 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots of concentrations integrated between surface and 150m
pisces_zooms_diags_int=0 # 16.2.4 PISCES Details on Drake(2), Kerguelen(3), Campbell(4) or all(1) plots of concentrations integrated between surface and 150m
pisces_coupes_aus=0      # 16.3.1 PISCES coupes australes
pisces_coupes_clim=0     # 16.3.2 PISCES coupes australes compared to climatologies
pisces_fluxes=0          # 16.4.1 PISCES fluxes
mxls_clim=0              # 17. MXL comparison to Boyer-Montegut climatology
contour=0                # 18. CONTOUR tool
alboran=0                # 19. Surface circulation in the Alboran Sea + salinity

######################################################################
### -3- TIME SERIES Menu :
### (set to 1 if you want it, to anything else if you do not !)
######################################################################

ts_cable=0               #  1. timeserie for cable transport (needs TRP = 1)
ts_gib=0                 #  2. timeserie for gibraltar (needs GIB =1)
ts_icemonth=0            #  3. timeserie for ice monthly area, volume, extent (needs ICEMONTH = 1)
ts_icenoaa=0             #  4. timeserie for ice compared with NOAA data (needs ICEMONTH = 1)
ts_icetrd=0              #  5. timeserie for ice trends (needs ICEMONTH = 1)
ts_maxmoc=0              #  6. timeserie for maximum and minimum MOC (needs MAXMOC = 1 and MHT = 1)
ts_maxmoc40=0            #  7. timeserie for maximum and minimum MOC at 40 N (needs MAXMOC = 1)
ts_mht1=0                #  8. timeserie for meridionnal heat transport (needs MHT = 1)
ts_nino=0                #  9. timeserie for el nino diags (needs ELNINO = 1)
ts_tao=0                 # 10. timeserie for TAO diags (needs TAO = 1)
ts_transports1=0         # 11. timeserie for transports (needs TRP = 1)
ts_trpsig=0              # 12. timeserie for transports in sigma classes (needs DCT = 1)
ts_tsmean=0              # 13. timeserie for T-S drifts (needs TSMEAN = 1)
ts_tsmean_lev=0          # 14. timeserie for T-S drifts compared with Levitus (needs TSMEAN = 1)
ts_mld_kerg=0            # 15. timeserie for MXL in Kerfix, Plume and SINDIAN boxes (needs KERG = 1)
ts_ice_kerg=0            # 16. timeserie for Ice in SINDIAN boxes (needs KERG = 1)
ts_hov_kerg=0            # 17. hovmullers of temperature, salinity and density in Kerfix and Plume  (needs KERG = 1)
ts_bio_kerg=0            # 18. timeserie for nutrients, primary production, delta CO2 and chlorophyll concentration in Kerfix and Plume (needs KERGb = 1)


######################################################################
### Definition for El nino and Gibraltar boxes
######################################################################

# define the I-J window for GIB diags and El NINO DIAG
if [ $CONFIG = 'NATL025' ] ; then
  GIBWIN='338 353 239 260'
  ELNINO=0 ; ts_nino=0 # reset to suitable value
elif [ $CONFIG = 'ORCA05' ] ; then
  # define the I-J window for GIB diags
  GIBWIN='548 555 327 337 '
  # define the I-J windows for EL NINO diags
  NINO12='395 415 230 250'
  NINO3='275 395 240 260'
  NINO4='175 275 240 260'
  NINO34='235 335 240 260'
elif [ $CONFIG = 'ORCA025' -o  $CONFIG = 'ORCA025.L75' -o $CONFIG = 'ORCA025.L300' ] ; then
  # define the I-J window for GIB diags
  GIBWIN='1094 1109 653 674 '
  # define the I-J windows for EL NINO diags
  NINO12='790 830 459 499'
  NINO3='550 790 479 519 '
  NINO4='350 550 479 519 '
  NINO34='470 670 479 519 '
elif [ $CONFIG = 'EORCA12' -o  $CONFIG = 'EORCA12.L46' -o $CONFIG = 'EORCA12.L75' ] ; then
  # define the I-J window for GIB diags
  GIBWIN='3282 3327 2046 2110'
  # define the I-J windows for EL NINO diags
  NINO12='2370 2490 1465 1585'
  NINO3='1650  2370 1525 1645'
  NINO4='1050  1650 1525 1645'
  NINO34='1410 2010 1525 1645'
elif [ $CONFIG = 'ORCA12' -o  $CONFIG = 'ORCA12.L46' -o $CONFIG = 'ORCA12.L75' ] ; then
  # define the I-J window for GIB diags
  GIBWIN='3282 3327 1959 2022 '
  # define the I-J windows for EL NINO diags
  NINO12='2370 2490 1377 1497'
  NINO3='1650 2370 1437 1557 '
  NINO4='1050 1650 1437 1557 '
  NINO34='1410 2010 1437 1557 '
elif [ $CONFIG = 'ORCA2' -o $CONFIG = 'ORCA246' ] ; then
  # define the I-J window for GIB diags
  GIBWIN='135 137 101 104'
  # define the I-J windows for EL NINO diags
  NINO12='97 102 62 74'
  NINO3='67 97 66 82'
  NINO4='42 67 66 82'
  NINO34='57 82 66 82'
elif [ $CONFIG = 'NATL12' ] ; then
  GIBWIN='1014 1059 717 780'
  ELNINO=0 ; ts_nino=0 # reset to suitable value
else
  echo GIBWIN and NINO boxes not defined for config $CONFIG
  GIB=0    ; ts_gib=0  # reset to suitable value
  ELNINO=0 ; ts_nino=0 
fi

# define the I-J window for KERG diags 
if [ $CONFIG = 'SINDIAN05' ] ; then
  KERGWIN1='97 100 82 85' # Kerfix (68-69E 50-51S)
  KERGWIN2='3 6 46 50' # West frontier (21-22E 60-61S)
  KERGWIN3='121 126 79 85' # Plume3 (80-82E 50-52S)
  KERGBOX1='1 202 1 50' # Ice Box (20-120E 60-60S)
  KERGBOX2='1 202 50 79' # Summer max MLD Box (20-120E 52-60S) 
  KERGBOX3='1 202 91 114' # Winter mx MLD Box (20-120E 40-48S)
elif [ $CONFIG = 'ORCA05' ] ; then
  KERGWIN1='711 714 131 134'
  KERGWIN2='617 620 95 99'
  KERGWIN3='15 20 128 134'
  KERGBOX1='615 96 51 99'
  KERGBOX2='615 96 99 128'
  KERGBOX3='615 96 140 163'
elif [ $CONFIG = 'PERIANT05' -o $CONFIG = 'BIOPERIANT05' ] ; then
  KERGWIN1='711 714 145 148'
  KERGWIN2='617 620 109 113'
  KERGWIN3='15 20 141 148'
  KERGBOX1='615 96 65 113'
  KERGBOX2='615 96 113 141'
  KERGBOX3='615 96 154 177'
else
  echo KERGUELEN box not defined for config $CONFIG
  KERG=0
  KERGb=0
fi


    ### : Directory of the DMONTOOLS scripts (generic)
    PRODTOOLS=$DMON_ROOTDIR/MONITOR_PROD    # MONITOR PROD
    PLOTTOOLS=$DMON_ROOTDIR/PLOT_2D         # PLOT 2D
    TIMESERIES=$DMON_ROOTDIR/TIME_SERIES    # TIME SERIES
    DATAOBSDIR=$TIMESERIES/python/pydmontools/data/NC
    DDIR=${DDIR:=$WORKDIR}  # if DDIR not defined use WORKDIR instead

case $MACHINE in
    ( jade | occigen | occigen2 | curie | ada | vayu) 

    ### 1. User informations
    USER=`whoami` ;   REMOTE_USER=`whoami` ; SDIR=$DDIR
    ### 2. Path to several tools
    ### 2.1 : CDFTOOLS executables
    CDFTOOLS=$WORKDIR/bin/
    ### 2.2 : CHART/COUPE executables
    CHARTTOOLS=$WORKDIR/bin/
    CHART=chart # name of CHART executable
    COUPE=coupe # name of COUPE executable
    ### 2.3 : Directory of the MPI_TOOLS executables
    MPITOOLS=$WORKDIR/bin/
    ### 3. Working and storage directories
    ### 3.1 : Root of working directory for the monitor_prod 
    R_MONITOR=$DDIR/MONITOR_${CONFCASE}/
    ### whether we use a random TMPDIR (for R_MONITOR and P_MONITOR) or not (1/0)
    RNDTMPDIR=0
    ### 3.2 : Storage directory for diags (output of monitor_prod)
    DIAGS=$SDIR/${CONFIG}/${CONFCASE}-DIAGS/NC
    ### 3.3 : Storage directory for monitor (output of make_ncdf_timeseries)
    MONITOR=$SDIR/${CONFIG}/${CONFCASE}-MONITOR
    ### 3.4 : Root of working directory for plot_monitor 
    P_MONITOR=$DDIR/TMPDIR_PLT_${CONFCASE}
    ### 3.5 : Root of storage for timeseries plots 
    ### (full path is $PLOTDIR/$CONFIG/PLOTS/$CONFCASE/TIME_SERIES)
    PLOTDIR=$SDIR      

    ### 4 : hardware/batch
    QUEUE='standard'
    ACCOUNT='gen0727'
    PLOT_WALLTIME=05:00:00
    WALLTIME=03:00:00
     if [ $MACHINE = 'jade'     ] ; then MPIPROC=8   ; fi 
     if [ $MACHINE = 'occigen'  ] ; then MPIPROC=24  ; fi 
     if [ $MACHINE = 'occigen2' ] ; then 
        if [ $NODETYPE = BDW28 ] ; then
           MPIPROC=28  
        else
           MPIPROC=24  
        fi 
     fi
     if [ $MACHINE = 'ada'      ] ; then MPIPROC=32  ; fi 
     if [ $MACHINE = 'curie'    ] ; then MPIPROC=16  ; fi  ;;

    'meolkerg') 
    ### 1. User informations
    USER=`whoami` ;   REMOTE_USER=`whoami` ; SDIR=$WORKDIR
    ### 2. Path to several tools
    ### 2.1 : CDFTOOLS executables
    CDFTOOLS=$HOME/CDFTOOLS_3.0/bin/
    ### 2.2 : CHART/COUPE executables
    CHARTTOOLS=$HOME/bin/
    CHART=chart # name of CHART executable
    COUPE=coupe # name of COUPE executable
    ### 2.4 : Directory of the MPI_TOOLS executables
    MPITOOLS=$WORKDIR/bin/
    ### 3. Working and storage directories
    ### 3.1 : Root of working directory for the monitor_prod 
    R_MONITOR=$WORKDIR/MONITOR_${CONFCASE}/
    ### whether we use a random TMPDIR (for R_MONITOR and P_MONITOR) or not (1/0)
    RNDTMPDIR=0
    ### 3.2 : Storage directory for diags (output of monitor_prod)
    DIAGS=$SDIR/${CONFIG}/${CONFCASE}-DIAGS/NC
    ### 3.3 : Storage directory for monitor (output of make_ncdf_timeseries)
    MONITOR=$SDIR/${CONFIG}/${CONFCASE}-MONITOR
    ### 3.4 : Root of working directory for plot_monitor 
    P_MONITOR=$WORKDIR/TMPDIR_PLT_${CONFCASE}
    ### 3.5 : Root of storage for timeseries plots 
    ### (full path is $PLOTDIR/$CONFIG/PLOTS/$CONFCASE/TIME_SERIES)
    PLOTDIR=$SDIR      

    ### 4 : hardware/batch
    WALLTIME=03:00:00
    MPIPROC=8 ;;

    'desktop' )
    ### 1. User informations
    USER=`whoami` ;   REMOTE_USER=`whoami` ; SDIR=${SDIR}
    ### 2. Path to several tools
    ### 2.1 : CDFTOOLS executables
    CDFTOOLS=$WORKDIR/CDFTOOLS_forge/
    ### 2.2 : CHART/COUPE executables
    CHARTTOOLS=$HOME/bin/
    CHART=chart # name of CHART executable
    COUPE=coupe # name of COUPE executable
    ### 2.4 : Directory of the MPI_TOOLS executables
    MPITOOLS=$HOME/DMONTOOLS/MPI_TOOLS
    ### 3. Working and storage directories
    ### 3.1 : Root of working directory for the monitor_prod 
    R_MONITOR=$WORKDIR/MONITOR_${CONFCASE}/
    ### whether we use a random TMPDIR (for R_MONITOR and P_MONITOR) or not (1/0)
    RNDTMPDIR=0
    ### 3.2 : Storage directory for diags (output of monitor_prod)
    DIAGS=$SDIR/${CONFIG}/${CONFCASE}-DIAGS/NC
    ### 3.3 : Storage directory for monitor (output of make_ncdf_timeseries)
    MONITOR=$SDIR/${CONFIG}/${CONFCASE}-MONITOR
    ### 3.4 : Root of working directory for plot_monitor 
    P_MONITOR=$WORKDIR/TMPDIR_PLT_${CONFCASE}
    ### 3.5 : Root of storage for timeseries plots 
    ### (full path is $PLOTDIR/$CONFIG/PLOTS/$CONFCASE/TIME_SERIES)
    PLOTDIR=$SDIR      

    ### 4 : hardware/batch
    WALLTIME=01:00:00
    MPIPROC=1 ;;

    *) 
    echo available machines are jade ada vargas ulam meolkerg desktop  ; exit 1 ;;
esac

#

