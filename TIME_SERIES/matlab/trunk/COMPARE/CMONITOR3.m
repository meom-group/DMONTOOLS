%%%%%%%%%%%%%%%%%%%%%%% MONITORING OF DRAKKAR OUTPUTS
%%%%%%%%%%%%%%%%%%%%%%% MAIN PLOTTING ROUTINE
%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------

clear;
	%%%%%%%%%%%%%%%%%%%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%% DECLARATIONS
	%%%%%%%%%%%%%%%%%%%%%%% 

run1         =  getenv ('drakkar_config1')
run2         =  getenv ('drakkar_config2')
run3         =  getenv ('drakkar_config3')
maxyear      =  getenv ('MAXYEAR') ;

global plotdir dataobsdir ydeb
plotdir     = getenv ('PLOTDIR') ;
dataobsdir  = getenv ('DATAOBSDIR')  ;
here        = getenv ('HERE' );
datadirpth  = getenv ('DATADIR') ;

maxyear=num2str(maxyear) ; [tmp,bidon]=size(maxyear) ; if tmp == 0 ; maxyear = '0' ; end ; maxyear =  str2num(maxyear) ;

name         = strcat('green: ',run1,' black: ',run2,' red: ',run3) ;
figname      = strcat(run1,'_',run2,'_',run3) ;
style1   = 'g.-' ;
style2   = 'k.-' ;
style3   = 'r.-' ;

%%%%% plots by category. 1 for yes, 0 for no

transports   = 1 ; %% timeseries of mass, heat, and salt transports at choke points
cable        = 1 ; %% timeseries Florida-Bahamas trp with cable data superimposed
sea_icemonth = 1 ; %% timeseries of spring/fall/annual north/south ice volume/area
sea_icenoaa  = 0 ; %% timeseries of spring/fall/annual north/south ice volume/area
sea_icetrd   = 1 ; %% timeseries of ice area extent compared with noaa data
nino         = 1 ; %% timeseries of maxs/mins/depth/lat of atl/indopac/global overturnings
max_mins     = 1 ; %% timeseries of maxs/mins/depth/lat of atl/indopac/global overturnings
max_mins40   = 1 ; %% timeseries of maxs/mins/depth/lat of atl/indopac/global overturnings at fixed lat
overflows    = 1 ; %% 2D timeseries of transports by density classes at and downstream of sills 
tsmean       = 1 ; %% timeseries of SSHmean, T/Smean and T/S profiles
tsmean_lev   = 0 ; %% timeseries of SSHmean, T/Smean and T/S profiles
gib	     = 1 ; %% Gib TS profile
%  following are not relevant for comparing plots
sea_ice     = 0  ; %% timeseries of spring/fall/annual north/south ice volume/area
mhts	    = 0	; %% 2D timeseries of meridional heat transports by basin
% determine the shortest time serie
tiret       =findstr(run1,'-') - 1 ;  config      = strcat(run1(1:tiret),'/') ;
datadir=strcat(datadirpth,'/',config,run1,'/DATA/') ;
tmean1 = strcat(datadir,run1,'_TMEAN.mtl') ;
TMP=load(tmean1) ; [nb(1),dum] = size ( TMP(2:end,1) ) ; ydeb1=TMP(2,1) ;

tiret       =findstr(run2,'-') - 1 ; config      = strcat(run2(1:tiret),'/') ;
datadir=strcat(datadirpth,'/',config,run2,'/DATA/') ;
tmean2 = strcat(datadir,run2,'_TMEAN.mtl') ;
TMP=load(tmean2) ; [nb(2),dum] = size ( TMP(2:end,1) ) ; ydeb2=TMP(2,1) ;

tiret       =findstr(run3,'-') - 1 ; config      = strcat(run3(1:tiret),'/') ;
datadir=strcat(datadirpth,'/',config,run3,'/DATA/') ;
tmean3 = strcat(datadir,run3,'_TMEAN.mtl') ;
TMP=load(tmean3) ; [nb(3),dum] = size ( TMP(2:end,1) ) ; ydeb3=TMP(2,1) ;

nbymin=min(nb) ;
ydeb=max([ydeb1,ydeb2,ydeb3 ]) ;


for irun = 1:3
  run=run1 ; style=style1 ;
  if irun == 2 ; run=run2 ; style=style2 ; end
  if irun == 3 ; run=run3 ; style=style3 ; end
  tiret       =findstr(run,'-') - 1 ;
  config      = strcat(run(1:tiret),'/') ;
  save_plots  = 1    ; %% 1 to save plots in ps and jpeg
  if irun == 1 ; save_plots = 0 ; end
 
  
%%%%% name of ascii files
datadir=strcat(datadirpth,'/',config,run,'/DATA/')

tsIN1            = strcat(datadir,run,'_TMEAN.mtl') ;
tsIN2            = strcat(datadir,run,'_SMEAN.mtl') ;
tslevIN1         = strcat(datadir,run,'_TMEAN_lev.mtl') ;
tslevIN2         = strcat(datadir,run,'_SMEAN_lev.mtl') ;
transportsIN     = strcat(datadir,run,'_matrix.mtl');
sea_iceIN        = strcat(datadir,run,'_ice.mtl');
sea_icemonthIN   = strcat(datadir,run,'_icemonth.mtl');
ninoIN           = strcat(datadir,run,'_nino.mtl') ;
mhtsIN           = strcat(datadir,run,'_heat.mtl') ;
gibIN            = strcat(datadir,run,'_gib.mtl') ;
max_minsIN       = strcat(datadir,run,'_maxmoc.mtl') ;
max_40IN         = strcat(datadir,run,'_maxmoc40.mtl') ;

overflowsIN   = strcat(datadir,run,'_TRPSIG.mtl') ;

	%%%%%%%%%%%%%%%%%%%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%% PLOTTING CALLS
	%%%%%%%%%%%%%%%%%%%%%%% 

fig=1 ; %% SET TO 1 TO START AT FIGURE NUMBER 1




%%%%%%%%%%%%%%%%%%%%%%% TRANSPORTS
if transports == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_transports(fig,transportsIN,name,figname,save_plots, style, maxyear );
end

%%%%%%%%%%%%%%%%%%%%%%% CABLE florida bahamas
if cable == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_cable(fig,transportsIN,name,figname,save_plots, style, maxyear );
end

%%%%%%%%%%%%%%%%%%%%%%% SEA_ICEMONTH
if sea_icemonth    == 1;
 figure (fig)
 style='k-';
 if irun == 1 ; clf ; style='g-'; end
 if irun == 3 ;  style='r-'; end
fig=plot_sea_icemonth(fig,sea_icemonthIN,name,figname,save_plots, style, maxyear );
 if irun == 1 ; style=style1 ; end
 if irun == 2 ; style=style2 ; end
 if irun == 3 ; style=style3 ; end
end

%%%%%%%%%%%%%%%%%%%%%%% SEA_ICENOAA
if sea_icenoaa    == 1;
 figure (fig)
 style='k-';
 if irun == 1 ; clf ; style='g-'; end
 if irun == 3 ;  style='r-'; end
fig=plot_sea_icenoaa(fig,sea_icemonthIN,name,figname,save_plots, style, maxyear );
 if irun == 1 ; style=style1 ; end
 if irun == 2 ; style=style2 ; end
 if irun == 3 ; style=style3 ; end
end

%%%%%%%%%%%%%%%%%%%%%%% SEA_ICE_TRD
if sea_icetrd == 1;
 figure (fig)
 if irun == 1 ; clf ; end
 fig=plot_icetrd(fig,run,sea_icemonthIN,name,figname,save_plots, style, maxyear );
 figure (fig)
 if irun == 1 ; clf ; end
 fig=plot_icetrd_mini(fig,run,sea_icemonthIN,name,figname,save_plots, style, maxyear );
end

%%%%%%%%%%%%%%%%%%%%%%% NINO
if nino       == 1;
 figure (fig)
 style='k-';
 if irun == 1 ;  style='g-' ; clf ; end
 if irun == 3 ;  style='r-' ;  end
fig=plot_nino(fig,ninoIN,name,figname,save_plots, style, maxyear );
 if irun == 1 ; style=style1 ; end
 if irun == 2 ; style=style2 ; end
 if irun == 3 ; style=style3 ; end
end

%%%%%%%%%%%%%%%%%%%%%%% MAX_MINS
if  max_mins  == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_maxmoc(fig,max_minsIN,name,figname,save_plots, style, maxyear );
end

if  max_mins40  == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_maxmoc40(fig,max_40IN,name,figname,save_plots, style, maxyear );
end

%%%%%%%%%%%%%%%%%%%%%%% OVERFLOWS
if overflows == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_trpsig(fig,overflowsIN,name,figname,save_plots, style, maxyear );
end
%%%%%%%%%%%%%%%%%%%%%%% TSMEAN
if tsmean == 1;
 figure (fig)
 if irun == 1 ; clf ; end
 fig=plot_tsmean(fig,tsIN1,tsIN2,nbymin,name,figname,save_plots, style, maxyear );
end

%%%%%%%%%%%%%%%%%%%%%%% TSMEAN levitus
if tsmean_lev == 1;
 figure (fig)
 if irun == 1 ; clf ; end
 fig=plot_tsmean_lev(fig,tslevIN1,tslevIN2,nbymin,name,figname,save_plots, style, maxyear );
end

%%%%%%%%%%%%%%%%%%%%%%% SEA_ICE
if sea_ice    == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_sea_ice(fig,sea_iceIN,name,figname,save_plots, style, maxyear );
end
%%%%%%%%%%%%%%%%%%%%%%% MHTS
if mhts       == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_mhts(fig,mhtsIN,name,figname,save_plots, style, maxyear );
end
%%%%%%%%%%%%%%%%%%%%%%%%GIB
if gib         == 1;
 figure (fig)
 if irun == 1 ; clf ; end
fig=plot_gib(fig,gibIN,name,figname,save_plots, style, maxyear );
end
%%%%%%%%%
%%%%%%%%%
%%%%%%%%%
%display('!lpr nino.ps sea_ice.ps transports1.ps');
%display('!lpr -Pdj990  tsmean1.ps MHT1.ps');


end
quit;
