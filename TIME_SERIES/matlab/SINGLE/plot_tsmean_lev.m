function [fig1]=plot_tsmean(fig,filename1,filename2,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR TRANSPORTS
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global plotdir

%%%%% Declarations
font=8;
	variable = { ...
	'3D Global T mean' ;...
	'3D Global S mean' ;...
	'Global SSH mean' ;...
	'Global change in T (dif to LEVITUS) ' ;...
	'Global change in S (dif to LEVITUS) ' ;...
	'Global T anomaly' ;...
	'Global S anomaly' ;...
	} ; 

nhead= 2 ;   % 1 header line
%%%%% READ
T=load(filename1) ;
yr1 = T(nhead+1, 1 ) ;
yr2 = T(end, 1 ) ;

S=load(filename2) ;
dep=T(1,4:end);yr =T(nhead+1:end,1);


%%%%%%%% PLOTS

%-----------------------------------------------
splt1 = 3 ;splt2 = 3 ;
figure(fig);clf;

subplot(splt1,splt2,1);hold on;plot(yr,T(nhead+1:end,3),'r.-');set(gca,'fontsize',font);grid;title(variable(1)    ,'fontsize',font) ;
  axis tight 
subplot(splt1,splt2,2);hold on;plot(yr,S(nhead+1:end,3),'r.-');set(gca,'fontsize',font);grid;title(variable(2)    ,'fontsize',font) ;
  axis tight 
subplot(splt1,splt2,3);hold on;plot(yr,T(nhead+1:end,2),'r.-');set(gca,'fontsize',font);grid;title(variable(3)    ,'fontsize',font) ;
  axis tight 

subplot(splt1,2,3);semilogy(T(end,4:end)'-T(2,4:end)',-dep,'r');hold on;set(gca,'fontsize',font);grid;
  title(variable(4)    ,'fontsize',font) ;axis tight;ylabel('Log Depth','fontsize',font)
subplot(splt1,2,5);semilogy(S(end,4:end)'-S(2,4:end)',-dep,'r');hold on;set(gca,'fontsize',font);grid;
  title(variable(5)    ,'fontsize',font) ;axis tight;ylabel('Log Depth','fontsize',font)

tmp=T(nhead+1:end,4:end)';[I J]=size(tmp) 
tlev=T(nhead,4:end)';
  for j=2:J;tmp(:,j)=tmp(:,j)-tlev;end;
  tmp(:,1)=0;subplot(splt1,2,4);contourf(yr,-dep,tmp,50);set(gca,'YScale','log');
  colorbar;set(gca,'fontsize',font);shading flat;grid;ylabel(variable(6),'fontsize',font) ;%axis tight;
tmp=S(nhead+1:end,4:end)';[I J]=size(tmp);
slev=S(nhead,4:end)';
  for j=2:J;tmp(:,j)=tmp(:,j)-slev;end;
  tmp(:,1)=0;subplot(splt1,2,6);contourf(yr,-dep,tmp,50);set(gca,'YScale','log');
  colorbar;set(gca,'fontsize',font);shading flat;grid;ylabel(variable(7),'fontsize',font) ;%axis tight

xlabel (name,'fontsize',14) ;
	
	
                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_tsmean1_lev.ps') ;
%               orient tall;eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/',name,'_tsmean1_lev.jpg') ;
                orient tall;eval(cmd)
                end
		
		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
