function [fig1]=plot_maxmoc(fig,filename,name,figname,print_fig,style,maxyear);
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
	'Max Overturning (20N 60N 500m-2000m)' ;...
	'Min Overturning (-40S 30N 2000m-5500m)' ;...
	'MHT at 20 N ' ;...
	'Max Overturning (0N 60N 500m-2000m)' ;...
	'Min Overturning (-20S 40N 2000m-5500m)' ;...
	'MHT at 20 N ' ;...
	'Min Overturning (15N 50N 100m-1000m)' ;...
	'Min Overturning (-30S 20N 1000m-5500m)' ;...
	'MHT at 20 N ' ;...
	'Max Overturning (-70S 0S 0m-2000m)' ;...
	'Min Overturning (-70S 0S 2000m-5500m)' ;...
	'MHT at 20 N ' ;...
	} ; 

	zone = { ...
	'Global '          ;...
	'Atlantique '            ;...
	'Indo-Pacif '          ;...
	'Austral '         ;...
	} ; 

nhead=0 ;   % 0 header lines

%%%%% READ
X=load(filename) ; 
ncol=length(X(1,:) ) ;
yr1 = X(nhead+1, 1 ) ;
[yr2,dum] = size( X(nhead+1:end, 1 ) ) ;
if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; end ; end

xdate=X(1:yr2,1) ; 
if yr1 < 1900 ; xdate = xdate + ydeb - yr1 ; end

%%%%%%%% PLOTS
%splt=length(zone)*length(variable) ;
splt=ncol -1 ;

%-----------------------------------------------
splt1 = splt/3 ;
splt2 = 3 ;

ii=0;
	for i1 = 1:splt1
	for i2 = 1:splt2
	if i1*i2 <= splt-1
	ii=ii+1;
        if ii ~= 9
subplot(4,splt2,ii);hold on
plot(xdate,X(1:yr2,1+ii),style);set(gca,'fontsize',font);grid on ;
axis tight
title (variable(ii),'fontsize',font)
if i2==1 ; ylabel(zone(i1)    ,'fontsize',font) ; end
%if i1==1 ; title (variable(i2),'fontsize',font) ; end
if i2==2 ; if i1==splt1 ; xlabel (name,'fontsize',14) ; end ; end
	end
	end
	end
	end
	
                if print_fig == 1
%                cmd=sprintf('%s  %s  %s%s%s ','print','-dpsc', 'maxmoc.',figname,'.ps') ;
%                orient tall;eval(cmd)
     if maxyear ~= 0
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100', plotdir,'/','beg_maxmoc.',figname,'.jpg') ;
     else
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100', plotdir,'/','maxmoc.',figname,'.jpg') ;
     end
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
