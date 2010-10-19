function [fig1]=plot_sea_icenoaa(fig,filename,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR SEA-ICE
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global dataobsdir plotdir

%%%%% Declarations
font=12;
	variable = { ...
	'Ice Volume (10^9 m^3)' ;...
	'Ice Area   (10^9 m^2)' ;...
	'Ice Extent (10^9 m^2)' ;...
	} ; 

	zone = { ...
	'Northern' ;...
	'Southern' ;...
	} ; 

	season = { ...
	'Fev';...
	'Mar';...
	'Aug';...
	'Sep';...
	} ; 

nhead=1 ; % 1 header line
%%%%% READ
X=load(filename) ; yrs=X(2:end,1) ; mnths=X(1,2:13) ; yr1=X(2,1) ; X=X(2:end,2:end); 
yr2 = X(end, 1 ) ;

noaaN=load(strcat(dataobsdir,'/','North.mtl')) ;
noaaS=load(strcat(dataobsdir,'/','South.mtl')) ;

nyrs=length(yrs);

%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;



%-----------------------------------------------
date=yrs*ones(1,12)+ones(nyrs,1)*mnths./12 ; [I J]=size(date);

% 2 hemispheres ; 3 variables ( vol, area, extent )
splt1 = 2 ;
splt2 = 3 ;
figure(fig);clf;

datx=reshape(date',I*J,1) ;

i1=1 ; i2=1 ;
tmp = X(:,1+2*length(mnths)*(i2-1)+length(mnths)*(i1-1):length(mnths)+2*length(mnths)*(i2-1)+length(mnths)*(i1-1));
volN=reshape(tmp',I*J,1)/1000. ;
i1=1 ; i2=3 ;
tmp = X(:,1+2*length(mnths)*(i2-1)+length(mnths)*(i1-1):length(mnths)+2*length(mnths)*(i2-1)+length(mnths)*(i1-1));
extN=reshape(tmp',I*J,1)/1000. ;
i1=1 ; i2=2 ;
tmp = X(:,1+2*length(mnths)*(i2-1)+length(mnths)*(i1-1):length(mnths)+2*length(mnths)*(i2-1)+length(mnths)*(i1-1));
areaN=reshape(tmp',I*J,1)/1000. ; 
% Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
noaaN(1:92,4)=noaaN(1:92,4)+1.19 ; noaaN(93:end,4)=noaaN(93:end,4)+0.31 ;

i1=2 ; i2=1 ;
tmp = X(:,1+2*length(mnths)*(i2-1)+length(mnths)*(i1-1):length(mnths)+2*length(mnths)*(i2-1)+length(mnths)*(i1-1));
volS=reshape(tmp',I*J,1)/1000. ;
i1=2 ; i2=3 ;
tmp = X(:,1+2*length(mnths)*(i2-1)+length(mnths)*(i1-1):length(mnths)+2*length(mnths)*(i2-1)+length(mnths)*(i1-1));
extS=reshape(tmp',I*J,1)/1000. ;
i1=2 ; i2=2 ;
tmp = X(:,1+2*length(mnths)*(i2-1)+length(mnths)*(i1-1):length(mnths)+2*length(mnths)*(i2-1)+length(mnths)*(i1-1));
areaS=reshape(tmp',I*J,1)/1000. ;

subplot(4,1,1)  ; hold on
    plot(datx,extN,'k-') ; 
    if yr1 > 1900 ; plot (noaaN(:,1)+noaaN(:,2)/12., noaaN(:,3),'b-' ) ;end 
     grid on ; title( strcat(name,'  Million sq km'),'fontsize',14 ) ;
    ylabel (' Arctic Extent','fontsize',10 ) ;
subplot(4,1,2)  ; hold on
    plot(datx,areaN,'k-') ; 
    if yr1 > 1900 ;  plot (noaaN(:,1)+noaaN(:,2)/12., noaaN(:,4) ,'b-' ) ;end 
      grid on ; 
     ylabel (' Arctic Area','fontsize',10 ) ;
subplot(4,1,3)  ; hold on
    plot(datx,extS,'k-') ; 
    if yr1 > 1900 ;  plot (noaaS(:,1)+noaaS(:,2)/12., noaaS(:,3) ,'b-' ) ; end 
    grid on ; 
     ylabel (' Antarctic Extent','fontsize',10 ) ;
subplot(4,1,4)  ; hold on
    plot(datx,areaS,'k-') ; 
    if yr1 > 1900 ;  plot (noaaS(:,1)+noaaS(:,2)/12., noaaS(:,4) ,'b-' ) ; end 
    grid on ;
     ylabel (' Antarctic Area','fontsize',10 ) ;
     xlabel (' blue : Data from NSIDC (Fetterer at al. ) Based on SSMR and SSM/I')

%Fetterer, F., and K. Knowles. 2002, updated 2004. Sea ice index. Boulder, CO: National Snow and Ice Data Center. Digital media



ii=0;

                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_sea_icenoaa.ps') ;
%               eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/',name,'_sea_icenoaa.jpg') ;
                eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
