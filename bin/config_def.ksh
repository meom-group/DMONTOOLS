#!/bin/ksh

# this is the config_def.ksh file
# it must be customized for each configuration

#  $Rev: 101 $
#  $Date: 2007-10-02 16:23:47 +0200 (Tue, 02 Oct 2007) $
#  $Id: config_def_ORCA025_zahir.ksh 101 2007-10-02 14:23:47Z molines $

# Name of CONFIG and CASE
CONFIG=<CONFIG>
CASE=<CASE>
MACHINE=<MACHINE>
MAIL=user@machine   ## to be edited ##

# parallel monitoring (1: yes / 0: no)
useMPI=1 

CONFCASE=${CONFIG}-${CASE}
MESH_MASK_ID=$CONFCASE   # root part of the mesh-mask files (likely  to be edited !!)
                         # (they must be in the -I directory ( $CONFIG/${CONFIG}-I)
                         #  Standard name is thus : ${MESH_MASK_ID}_byte_mask.nc
                         #                          ${MESH_MASK_ID}_mesh_hgr.nc
                         #                          ${MESH_MASK_ID}_mesh_zgr.nc
#TSCLIM=Gouretski         # if TSCLIM is not defined here it takes Levitus_p2.1 as default

######################################################################
### -1- MONITOR PROD Menu :
### (set to 1 if you want it, to anything else if you do not !)
######################################################################

EKE=1                    # compute EKE
RMSSSH=1                 # compute RMS ssh and w
TSMEAN=1                 # compute TSMEAN and ssh drift
ICE=0                    # compute ice volume, area and extent
ICEMONTH=0               # compute ice volume, area and extent
GIB=1                    # compute Gibraltar diags (restoring zone)
ELNINO=0                 # compute El Nino monitoring SSTs
TRP=1                    # compute barotropic transport accross section as given in section.dat (CTL dir)
MHT=1                    # compute Meridional Heat Transport (advective and from surface fluxes)
MOC=1                    # compute MOC ( need a sub basin mask file called new_maskglo.nc)
MOCSIG=0                 # compute MOC in sigma coordinates
# To be set if MOCSIG=1
mocsig_annual=0          # compute MOC in sigma coordinates from 5-day averaged files (0) or annual files (1)
DREF=2000                # potential density referenced at $DREF m is the sigma coordinate used
#
MAXMOC=1                 # diagnose the min and max of MOC
BSF=1                    # compute the BSF (psi) from U and V
DCT=1                    # compute density class transports for section given in dens_section.dat (CTL dir)
MXL=1                    # compute mixed layer depth from 3 criteria for month 03 and 09
TRACER=0                 # compute passive Tracer statistics
LSPV=0                   # compute large scale potential vorticity
TAO=0                    # compute profiles at TAO moorings location
BIO_PROFILE=1            # compute PISCES mean vertical profiles

######################################################################
### -2- PLOT 2D Menu :
### (set to 1 if you want it, to anything else if you do not !)
######################################################################

create_gif=0             #    Create gif (1) or not (0)

moc=1                    # 1. Meridional Overturning Circulation
global=1                 # 2. Global maps
glodifmap=0              # 3. Plot of differences with reynolds and Levitus
fluxes=0                 # 4. Air Sea fluxes
atlTS=1                  # 5.1.1 Details in North and South Atlantic TS
atlUV=1                  # 5.1.2 Details in North and South Atlantic UV (PSI)
zoomTS=1                 # 5.2.1 Details on Drake, Kerguelen, Campbell plateau TS
zoomUV=1                 # 5.2.2 Details on Drake, Kerguelen, Campbell plateau UV (PSI)
coupes=1                 # 6.1 Sections the global ocean
ar7w=0                   # 6.2 AR7W in the Labrador sea in march and october + Ovide
coupes_aus=1             # 6.3 Sections in the Southern Oceans
coupes_aus_sigma=0       # 6.4 Sections of density in the Southern Ocean (WOCE like)
circum=1                 # 6.5 Sections along the ACC
circum_sigma=0           # 6.6 Density Sections along the ACC
iceplt=1                 # 7.  Polar plots of ice thickness and concentration
dwbc=1                   # 8.1 Deep Western Boundary Current
dwbc_aus=0               # 8.2 Deep Western Boundary Current at 31 S
ekes=1                   # 9. Plot of EKE
mxls=1                   # 10. Mixed Layer depth
mxlatl=1                 # 11.  Mixed Layer in the Atlantic
flxatl=1                 # 12. Air Sea fluxes on the Atlantic
tracer=0                 # 13. Passive tracers
mocsig=0                 # 14. Meridional Overturning Circulation in sigma coordinates
cfc=0                    # 15. CFC plots
pisces_global=0          # 16. PISCES global plots
pisces_clim=0            # 17. PISCES comparison to climatologies plots
mxls_clim=1              # 18. MXL comparison to Boyer-Montegut climatology
pisces_coupes_aus=1      # 19. PISCES coupes australes
pisces_coupes_clim=1     # 20. PISCES coupes australes compared to climatologies
pisces_fluxes=1          # 21. PISCES fluxes 

######################################################################
### -3- TIME SERIES Menu :
### (set to 1 if you want it, to anything else if you do not !)
######################################################################

ts_cable=0               #  1. timeserie for cable transport (needs TRP = 1)
ts_gib=0                 #  2. timeserie for gibraltar (needs GIB =1)
ts_icemonth=0            #  3. timeserie for ice monthly area, volume, extent (needs ICEMONTH = 1)
ts_icenoaa=1             #  4. timeserie for ice compared with NOAA data (needs ICEMONTH = 1)
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

######################################################################
### Definition for El nino and Gibraltar boxes
######################################################################

# define the I-J window for GIB diags and El NINO DIAG
if [ $CONFIG = 'NATL025' ] ; then
  GIBWIN='338 353 239 260'
  ELNINO=0 ; ts_nino=0 # reset to suitable value
elif [ $CONFIG = 'ORCA025' -o  $CONFIG = 'ORCA025.L75' ] ; then
  # define the I-J window for GIB diags
  GIBWIN='1094 1109 653 674 '
  # define the I-J windows for EL NINO diags
  NINO12='790 830 459 499'
  NINO3='550 790 479 519 '
  NINO4='350 550 479 519 '
  NINO34='470 670 479 519 '
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



case $MACHINE in
    'jade') 
    ### 1. User informations
    USER=molines ;   REMOTE_USER=molines ; SDIR=$WORKDIR
    ### 2. Path to several tools
    ### 2.1 : CDFTOOLS executables
    CDFTOOLS=$WORKDIR/bin/
    ### 2.2 : CHART/COUPE executables
    CHARTTOOLS=$WORKDIR/bin/
    CHART=chart # name of CHART executable
    COUPE=coupe # name of COUPE executable
    ### 2.3 : Directory of the DMONTOOLS scripts
    PRODTOOLS=$HOME/DMONTOOLS/MONITOR_PROD    # MONITOR PROD
    PLOTTOOLS=$HOME/DMONTOOLS/PLOT_2D         # PLOT 2D
    TIMESERIES=$HOME/DMONTOOLS/TIME_SERIES    # TIME SERIES
    DATAOBSDIR=$TIMESERIES/data/NC
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
    MPIPROC=8
    login_node=service2
    SUB=qsub ;;


    'vargas' )
    ### 1. User informations
    USER=`whoami` ;   REMOTE_USER=`whoami` ; SDIR=${SDIR}
    ### 2. Path to several tools
    ### 2.1 : CDFTOOLS executables
    CDFTOOLS=$WORKDIR/CDFTOOLS_forge/
    ### 2.2 : CHART/COUPE executables
    CHARTTOOLS=$HOME/bin/
    CHART=chart # name of CHART executable
    COUPE=coupe # name of COUPE executable
    ### 2.3 : Directory of the DMONTOOLS scripts
    PRODTOOLS=$HOME/DMONTOOLS/MONITOR_PROD       # MONITOR PROD
    PLOTTOOLS=$HOME/DMONTOOLS/PLOT_2D            # PLOT 2D
    TIMESERIES=$HOME/DMONTOOLS/TIME_SERIES/python/pydmontools     # TIME SERIES
    DATAOBSDIR=$TIMESERIES/data/NC
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
    MPIPROC=2
    SUB=llsubmit ;;

    'ulam' )
    ### 1. User informations
    USER=`whoami` ;   REMOTE_USER=`whoami` ; SDIR=${SDIR}
    ### 2. Path to several tools
    ### 2.1 : CDFTOOLS executables
    CDFTOOLS=$WORKDIR/CDFTOOLS_forge/
    ### 2.2 : CHART/COUPE executables
    CHARTTOOLS=$HOME/bin/
    CHART=$CHARTTOOLS/chart # name of CHART executable
    COUPE=$CHARTTOOLS/coupe # name of COUPE executable
    ### 2.3 : Directory of the DMONTOOLS scripts
    PRODTOOLS=$HOME/DMONTOOLS/MONITOR_PROD                    # MONITOR PROD
    PLOTTOOLS=$HOME/DMONTOOLS/PLOT_2D                         # PLOT 2D
    TIMESERIES=$HOME/DMONTOOLS/TIME_SERIES/python/pydmontools # TIME SERIES
    DATAOBSDIR=$TIMESERIES/data/NC
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
    MPIPROC=4
    SUB=llsubmit ;;

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
    ### 2.3 : Directory of the DMONTOOLS scripts
    PRODTOOLS=$HOME/DMONTOOLS/MONITOR_PROD       # MONITOR PROD
    PLOTTOOLS=$HOME/DMONTOOLS/PLOT_2D            # PLOT 2D
    TIMESERIES=$HOME/DMONTOOLS/TIME_SERIES/python/pydmontools  # TIME SERIES
    DATAOBSDIR=$TIMESERIES/data/NC
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
    MPIPROC=1
    SUB='' ;;

    *) 
    echo available machines are jade vargas ulam desktop  ; exit 1 ;;
esac

#

