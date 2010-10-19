function [fig1]=plot_gib(fig,filename1,name,figname,print_fig,style,maxyear);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR TRANSPORTS
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global plotdir ydeb

%%%%% Declarations
font=8;
	variable = { ...
	'Last Tgib  profile 14W-10W, 36N-40N (blue is Levitus)' ;...
	'Last Sgib profile 14W-10W, 36N-40N (blue is Levitus)' ;...
	'Temperature  anomaly' ;...
	'Salinity anomaly' ;...
	'Temperature Anomaly level  14W-10W, 36N-40N' ;...
	'Salinity Anomaly level  14W-10W, 36N-40N' ;...
	} ; 

nhead = 3 ;  % 3 header lines 
%%%%% READ
X=load(filename1) ;
yr1 = X(nhead+1, 1 ) ;
[yr2, dum] = size(X(nhead+1:end, 1 )) ;
yr2=yr2/2 ;
if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; end ; end
lastline=2*yr2+nhead ;

dep=X(1,2:end);yr =X(4:2:lastline,1); ldat=length(yr) ;
for y=1:ldat
  TS(:,:,y)=X(4+(y-1)*2:5+(y-1)*2,2:end) ;
end
  Tlev=X(2,2:end) ;
  Slev=X(3,2:end) ;
  T(:,:)=TS(1,:,:) ;
  S(:,:)=TS(2,:,:) ;

% index of frst index > 1100 m
looklev=find ( dep > 1100 ,1 ,'first') ;
lookdep=dep (looklev);

if yr1 < 1900 ; yr = yr +ydeb -yr1 ; end


%%%%%%%% PLOTS

%-----------------------------------------------
splt1 = 3 ;splt2 = 2 ;

subplot(splt1,splt2,1); hold on
plot(T(:,end),-dep,style);hold on;set(gca,'fontsize',font);grid;title(variable(1)    ,'fontsize',font) ;axis tight;ylabel('Depth','fontsize',font); plot(Tlev,-dep,'b')

subplot(splt1,splt2,2); hold on
plot(S(:,end),-dep,style);hold on;set(gca,'fontsize',font);grid;title(variable(2)    ,'fontsize',font) ;axis tight;ylabel('Depth','fontsize',font); plot(Slev,-dep,'b')

%tmp=T;[I J]=size(tmp);for j=1:J;tmp(:,j)=tmp(:,j)-Tlev';end;subplot(splt1,splt2,2);contourf(yr,-dep,tmp,50);colorbar;set(gca,'fontsize',font);shading flat;grid;ylabel(variable(3),'fontsize',font) ;%axis tight;

%tmp=S;[I J]=size(tmp);for j=1:J;tmp(:,j)=tmp(:,j)-Slev';end;subplot(splt1,splt2,4);contourf(yr,-dep,tmp,50);colorbar;set(gca,'fontsize',font);shading flat;grid;ylabel(variable(4),'fontsize',font) ;%axis tight;

subplot(6,1,3); hold on
plot(yr,T(looklev,:)-Tlev(looklev),style) ;hold on;set(gca,'fontsize',font);grid;
title( strcat( variable(5),' level ', num2str(looklev), ' (' , num2str(lookdep) ,' m)' ) ,'fontsize',font);
axis tight;ylabel('T anom (1000m)','fontsize',font)

subplot(6,1,4); hold on
 plot(yr,S(looklev,:)-Slev(looklev),style) ;hold on;set(gca,'fontsize',font);grid;
title( strcat( variable(6),' level ', num2str(looklev), ' (' , num2str(lookdep) ,' m)' ) ,'fontsize',font);
axis tight;ylabel('S anom (1000m)','fontsize',font)

xlabel (name,'fontsize',14) ;
	
	
                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s%s ','print','-dpsc','gib.',figname,'.ps') ;
%               orient tall;eval(cmd)

     if maxyear ~= 0
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100', plotdir,'/','beg_gib.',figname,'.jpg') ;
     else
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100', plotdir,'/','gib.',figname,'.jpg') ;
     end
                orient tall;eval(cmd)
                end
		
		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
