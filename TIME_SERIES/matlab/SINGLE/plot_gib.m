function [fig1]=plot_gib(fig,filename1,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR Gibraltar
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------

global plotdir

%%%%% Declarations
font=8;
	variable = { ...
	'Last Tgib  profile 14W-10W, 36N-40N (blue is Levitus)' ;...
	'Last Sgib profile 14W-10W, 36N-40N (blue is Levitus)' ;...
	'Temperature  anomaly' ;...
	'Salinity anomaly' ;...
	'Temperature Anomaly  14W-10W, 36N-40N' ;...
	'Salinity Anomaly  14W-10W, 36N-40N' ;...
	} ; 

nhead = 3 ;  % 3 header lines 
%%%%% READ
X=load(filename1) ;
yr1 = X(nhead+1, 1 ) ;
yr2 = X(end, 1 ) ;

dep=X(1,2:end);yr =X(4:2:end,1); ldat=length(yr) ;
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
%looklev=48
%looklev=25


%%%%%%%% PLOTS

%-----------------------------------------------
splt1 = 3 ;splt2 = 2 ;
figure(fig);clf;


subplot(splt1,splt2,1);plot(T(:,end),-dep,'r');hold on;set(gca,'fontsize',font);grid;title(variable(1)    ,'fontsize',font) ;axis tight;ylabel('Depth','fontsize',font); plot(Tlev,-dep,'b')

subplot(splt1,splt2,3);plot(S(:,end),-dep,'r');hold on;set(gca,'fontsize',font);grid;title(variable(2)    ,'fontsize',font) ;axis tight;ylabel('Depth','fontsize',font); plot(Slev,-dep,'b')

tmp=T;[I J]=size(tmp);for j=1:J;tmp(:,j)=tmp(:,j)-Tlev';end;subplot(splt1,splt2,2);contourf(yr,-dep,tmp,50);colorbar;set(gca,'fontsize',font);shading flat;grid;ylabel(variable(3),'fontsize',font) ;%axis tight;

tmp=S;[I J]=size(tmp);for j=1:J;tmp(:,j)=tmp(:,j)-Slev';end;subplot(splt1,splt2,4);contourf(yr,-dep,tmp,50);colorbar;set(gca,'fontsize',font);shading flat;grid;ylabel(variable(4),'fontsize',font) ;%axis tight;

subplot(6,1,5); plot(yr,T(looklev,:)-Tlev(looklev),'r.-') ;hold on;set(gca,'fontsize',font);grid;
  title( strcat( variable(5),' level ', num2str(looklev), ' (' , num2str(lookdep) ,' m)' ) ,'fontsize',font) ;
  axis tight;ylabel('T anom (1000m)','fontsize',font)

subplot(6,1,6); plot(yr,S(looklev,:)-Slev(looklev),'r.-') ;hold on;set(gca,'fontsize',font);grid;
  title( strcat(variable(6),' level ',num2str(looklev),' (',num2str(lookdep),' m)' ) ,'fontsize',font) ;
  axis tight;ylabel('S anom (1000m)','fontsize',font)

xlabel (name,'fontsize',14) ;
	
	
                if print_fig == 1
%                cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_gib.ps') ;
%                orient tall;eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/',name,'_gib.jpg') ;
                orient tall;eval(cmd)
                end
		
		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
