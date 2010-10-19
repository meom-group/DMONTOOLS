function [fig1]=plot_transports(fig,filename,name,figname,print_fig,style, maxyear);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR TRANSPORTS
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global dataobsdir plotdir ydeb

%%%%% Declarations
font=8;
	variable = { ...
	'Mass transport ' ;...
	'Heat transport ' ;...
	'Salt transport ' ;...
	} ; 

	zone = { ...
	'Bering '          ;...
	'Fram '            ;...
	'Baffin '          ;...
	'Denmark '         ;...
	'Icel.-Scotl. ';...
	'Florida-Cuba '    ;...
	'Florida-Bah. ' ;...
	'Drake '           ;...
	'Austr. Antar. ';...
	'Indo. TFlow ';...
	'Moz. Ch. '  ;...
	} ; 

	sens = [ ...
	1          ;...
	-1         ;...
	-1         ;...
	-1         ;...
	-1         ;...
	1          ;...
	1          ;...
	1          ;...
	1          ;...
	-1         ;...
	-1         ;...
	] ; 

nhead = 0 ;  % 0 header line
%%%%% READ
X=load(filename) ;
yr1 = X(nhead+1, 1 ) ;
[yr2, dum] = size(X(nhead+1:end, 1 ))  ;
if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; end ; end

%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;

%-----------------------------------------------
splt1 = 5 ;
splt2 = 2 ;

subplot(splt1,splt2,1);hold on
i1=7 ; i2=1 ;
Y=X(1:yr2,2+(i1-1)+length(zone)*(i2-1)) ;
if i2 == 1 
moyenne=mean(Y) ;
sens(i1)=sign(sens(i1)*moyenne) ;
end
Y(1:yr2)=Y(1:yr2)*sens(i1) ;
xdate=X(1:yr2,1);
if yr1 < 1900 ; xdate=xdate+ydeb-yr1 ; end


plot(xdate,Y(1:yr2),style);set(gca,'fontsize',font);grid on;
axis tight
if i2==1 ; ylabel(zone(i1)    ,'fontsize',font) ; end
if i1==7 ; title (variable(i2),'fontsize',font) ; end
if i2==1 ; if i1==7 ; xlabel (name,'fontsize',10) ; end ; end

if  yr1 > 1900 
tab=load(strcat(dataobsdir,'/','cable.mtl')) ;
year=tab(:,1) ; trp=tab(:,2) ;
plot(year,trp,'b.-') ; axis tight
end
	
                if print_fig == 1
%                cmd=sprintf('%s  %s%s%s ','print','-dpsc cable.',figname,'.ps') ;
%                orient tall;eval(cmd)
      if maxyear ~= 0 
                cmd=sprintf('%s  %s %s%s%s%s ','print','-djpeg100 ',plotdir,'/beg_cable.',figname,'.jpg') ;
      else
                cmd=sprintf('%s  %s %s%s%s%s ','print','-djpeg100 ',plotdir,'/cable.',figname,'.jpg') ;
      end
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
