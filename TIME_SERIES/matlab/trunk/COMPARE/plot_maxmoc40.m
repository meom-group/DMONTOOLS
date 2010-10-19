function [fig1]=plot_maxmoc40(fig,filename,name,figname,print_fig,style,maxyear);
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
	'MOC at 40N ' ;...
	'MOC at 30S ' ;...
	'MOC at 40N ' ;...
	'MOC at 30S ' ;...
	'MOC at 30S ' ;...
	'MOC at 50S ' ;...
	} ; 

	zone = { ...
	'Global '          ;...
	'Global '          ;...
	'Atlantique '            ;...
	'Atlantique '            ;...
	'Indo-Pacif '          ;...
	'Austral '         ;...
	} ; 


nhead = 0 ;  % 0 header lines

%%%%% READ
X=load(filename) ;
ncol=length( X(1,:) ) ; 
yr1 = X(nhead+1, 1 ) ;
[yr2,dum] = size(X(nhead+1:end, 1 )) ;
if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; end ; end
xdate=X(1:yr2,1) ; 
if yr1 < 1900 ; xdate=xdate + ydeb - yr1 ; end

%%%%%%%% PLOTS
%splt=length(zone)*length(variable) ;
%splt=6;
splt=ncol -1 ; 



%-----------------------------------------------
splt1 = splt/2 ;
splt2 = 2 ;

ii=0;
	for i1 = 1:splt1
	for i2 = 1:splt2
	if i1*i2 <= splt
	ii=ii+1;
subplot(3,splt2,ii);hold on
plot(xdate,X(1:yr2,1+ii),style);set(gca,'fontsize',font);grid on;
axis tight
 ylabel(zone(ii)    ,'fontsize',font) 
 title (variable(ii),'fontsize',font)
if i2==1 ; if i1==splt1 ; xlabel (name,'fontsize',10) ; end ; end
	end
	end
	end
	
                if print_fig == 1
%                cmd=sprintf('%s  %s  %s%s%s ','print','-dpsc','maxmoc40.',figname,'.ps') ;
%                orient tall;eval(cmd)
     if maxyear ~= 0
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100',plotdir,'/','beg_maxmoc40.',figname,'.jpg') ;
     else
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100',plotdir,'/','maxmoc40.',figname,'.jpg') ;
     end
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
