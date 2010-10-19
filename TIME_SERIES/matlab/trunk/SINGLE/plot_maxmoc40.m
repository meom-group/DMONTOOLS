function [fig1]=plot_maxmoc40(fig,filename,name,print_fig);
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
ncol=length(X(1,:)) ;
yr1 = X(nhead+1, 1 ) ;
yr2 = X(end, 1 ) ;


%%%%%%%% PLOTS
%splt=length(zone)*length(variable) ;
%splt=6;
splt=ncol -1 ; 



%-----------------------------------------------
splt1 = splt/2 ;
splt2 = 2 ;
figure(fig);clf;

ii=0;
	for i1 = 1:splt1
	for i2 = 1:splt2
	if i1*i2 <= splt
	ii=ii+1;
subplot(3,splt2,ii);hold on
plot(X(:,1),X(:,1+ii),'r.-');set(gca,'fontsize',font);grid;
axis tight
 ylabel(zone(ii)    ,'fontsize',font) 
 title (variable(ii),'fontsize',font)
if i2==2 ; if i1==splt1 ; xlabel (name,'fontsize',14) ; end ; end
	end
	end
	end
	
                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_maxmoc40.ps') ;
%               orient tall;eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100',plotdir,'/', name,'_maxmoc40.jpg') ;
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
