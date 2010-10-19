%%%%%%%%%%%%%%%%%%%%%%% MONITORING OF DRAKKAR OUTPUTS
%%%%%%%%%%%%%%%%%%%%%%% MAIN PLOTTING ROUTINE
clear;


	%%%%%%%%%%%%%%%%%%%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%% DECLARATIONS
	%%%%%%%%%%%%%%%%%%%%%%% 

%run         =  getenv ('drakkar_config')
run         =  'ORCA025-KAB001'
tiret       =findstr(run,'-') - 1 ;
config      = strcat(run(1:tiret),'/') ;
save_plots  = 1    ; %% 1 to save plots in ps and jpeg

%%%%% plots by category. 1 for yes, 0 for no

tsmean      = 0 ; %% timeseries of SSHmean, T/Smean and T/S profiles
transports  = 0 ; %% timeseries of mass, heat, and salt transports at choke points
sea_ice     = 0  ; %% timeseries of spring/fall/annual north/south ice volume/area
sea_ice_m     = 1  ; %% timeseries of spring/fall/annual north/south ice volume/area
nino        = 0 ; %% timeseries of maxs/mins/depth/lat of atl/indopac/global overturnings
mhts	    = 0	; %% 2D timeseries of meridional heat transports by basin
gib	    = 0	; %% Gib TS profile
max_mins    = 0	; %% timeseries of maxs/mins/depth/lat of atl/indopac/global overturnings
max_mins40  = 0	; %% timeseries of maxs/mins/depth/lat of atl/indopac/global overturnings at fixed lat
overflows   = 0	; %% 2D timeseries of transports by density classes at and downstream of sills 


%%%%% name of ascii files
cd /home/molines/MONITOR/
datadir=strcat('/net/meolipc/home/www/web/DRAKKAR/',config,run,'/DATA/')

tsIN1         = strcat(datadir,run,'_TMEAN.mtl') ;
tsIN2         = strcat(datadir,run,'_SMEAN.mtl') ;
transportsIN  = strcat(datadir,run,'_matrix.mtl');
sea_iceIN     = strcat(datadir,run,'_ice.mtl');
sea_ice_m_IN     = strcat(datadir,run,'_icemonth.mtl');
ninoIN        = strcat(datadir,run,'_nino.mtl') ;
mhtsIN        = strcat(datadir,run,'_heat.mtl') ;
gibIN        = strcat(datadir,run,'_gib.mtl') ;
max_minsIN    = strcat(datadir,run,'_maxmoc.mtl') ;
max_40IN    = strcat(datadir,run,'_maxmoc40.mtl') ;

overflowsIN   = strcat(datadir,run,'_TRPSIG.mtl') ;



	%%%%%%%%%%%%%%%%%%%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%% FIND start_year,end_year
	%%%%%%%%%%%%%%%%%%%%%%% 
tmp=load(sea_ice_m_IN) ; 
start_year  = tmp(1  ,1) ;
end_year    = tmp(end,1) ;
clear tmp



	%%%%%%%%%%%%%%%%%%%%%%% 
	%%%%%%%%%%%%%%%%%%%%%%% PLOTTING CALLS
	%%%%%%%%%%%%%%%%%%%%%%% 

fig=1 ; %% SET TO 1 TO START AT FIGURE NUMBER 1



%%%%%%%%%%%%%%%%%%%%%%% TSMEAN
if tsmean == 1;
fig=plot_tsmean(fig,tsIN1,tsIN2,run,start_year,end_year,save_plots);
end

%%%%%%%%%%%%%%%%%%%%%%% TRANSPORTS
if transports == 1;
fig=plot_transports(fig,transportsIN,run,start_year,end_year,save_plots);
end

%%%%%%%%%%%%%%%%%%%%%%% SEA_ICE
if sea_ice    == 1;
fig=plot_sea_ice(fig,sea_iceIN,run,start_year,end_year,save_plots);
end

if sea_ice_m    == 1;
fig=plot_sea_ice_month(fig,sea_ice_m_IN,run,start_year,end_year,save_plots);
end

%%%%%%%%%%%%%%%%%%%%%%% NINO
if nino       == 1;
fig=plot_nino(fig,ninoIN,run,start_year,end_year,save_plots);
end

%%%%%%%%%%%%%%%%%%%%%%% MHTS
if mhts       == 1;
fig=plot_mhts(fig,mhtsIN,run,start_year,end_year,save_plots);
end

%%%%%%%%%%%%%%%%%%%%%%%%GIB
if gib         == 1;
fig=plot_gib(fig,gibIN,run,start_year,end_year,save_plots);
end

%%%%%%%%%%%%%%%%%%%%%%% MAX_MINS
if  max_mins  == 1;
fig=plot_maxmoc(fig,max_minsIN,run,start_year,end_year,save_plots);
end

if  max_mins40  == 1;
fig=plot_maxmoc40(fig,max_40IN,run,start_year,end_year,save_plots);
end

%%%%%%%%%%%%%%%%%%%%%%% OVERFLOWS
if overflows == 1;
fig=plot_trpsig(fig,overflowsIN,run,start_year,end_year,save_plots);
end

%%%%%%%%%
%%%%%%%%%
%%%%%%%%%
%display('!lpr nino.ps sea_ice.ps transports1.ps');
%display('!lpr -Pdj990  tsmean1.ps MHT1.ps');



%quit;
