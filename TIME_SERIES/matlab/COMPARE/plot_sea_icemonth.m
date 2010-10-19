function [fig1]=plot_sea_icemonth(fig,filename,name,figname,print_fig,style,maxyear);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR SEA-ICE
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%-------------------------------------------------------------
global plotdir ydeb

%%%%% Declarations
font=12;
	variable = { ...
	'Ice Volume (10^9 m^3) N' ;...
	'Ice Volume (10^9 m^3) S' ;...
	'Ice Area   (10^9 m^2) N' ;...
	'Ice Area   (10^9 m^2) S' ;...
	'Ice Extent (10^9 m^2) N' ;...
	'Ice Extent (10^9 m^2) S' ;...
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
X=load(filename) ;

yrs=X(2:end,1) ; mnths=X(1,2:13) ; X=X(2:end,2:end); 
yr1 = yrs(1) ;
nyrs=length(yrs);
yr2 = nyrs ;

if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; nyrs = yr2; yrs=yrs(1:nyrs) ; end ; end


%%%%%%%% PLOTS
%splt=length(zone)*length(variable) ;
splt=6;



%-----------------------------------------------
date=yrs*ones(1,12)+ones(nyrs,1)*mnths./12 ; [I J]=size(date);

% 2 hemispheres ; 3 variables ( vol, area, extent )
splt1 = 3 ;
splt2 = 2 ;
nmonth = length(mnths);
xdate=reshape(date',I*J,1) ;
if yr1 < 1900 ; xdate = xdate + ydeb - yr1 ; end

X1=min(xdate) ; X2=max(xdate) ;

ii=0;
	for i1 = 1:splt1
	for i2 = 1:splt2
	if i1*i2 <= splt
	ii=ii+1;
subplot(splt1,splt2,ii);hold on

XX=X(1:yr2,1+(ii-1)*nmonth: ii*nmonth ) ;


plot(xdate,reshape(XX',I*J,1),style) ; axis tight ;AA=axis; axis([X1 X2 AA(3) AA(4)]); grid on
%p=plot(date,XX,style);set(p,'MarkerSize',1);set(gca,'fontsize',font);grid;
%p=plot(date,XX,style);set(gca,'fontsize',font);grid on;
%axis tight
if i1==3 ; if i2==2 ; xlabel (name,'fontsize',10,'HorizontalAlignment','right' ) ; end ; end
%AA=axis;axis tight;BB=axis;axis([BB(1) BB(2) 0 AA(4)])

%tit=strcat(zone(i1), ' Ice ', variable(i2));
%if i2==1 ; ylabel(zone(i1)    ,'fontsize',font) ; end
%if i1==1 ; title (variable(i1),'fontsize',font) ; end
title (variable(ii),'fontsize',font) 
%if i1==splt1 ; if i2==2 ; xlabel(name,'fontsize',14) ; end ; end
	end
	end
	end
	


                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s%s ','print','-dpsc','sea_icemonth.',figname,'.ps') ;
%               %orient landscape; orient rotated ;eval(cmd)
%               eval(cmd)
    if maxyear ~= 0
                cmd=sprintf('%s  %s   %s%s%s%s%s ','print','-djpeg100 ',plotdir,'/','beg_sea_icemonth.',figname,'.jpg') ;
    else
                cmd=sprintf('%s  %s   %s%s%s%s%s ','print','-djpeg100 ',plotdir,'/','sea_icemonth.',figname,'.jpg') ;
    end
                %orient landscape;eval(cmd)
                eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
