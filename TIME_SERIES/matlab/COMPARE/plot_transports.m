function [fig1]=plot_transports(fig,filename,name,figname,print_fig, style, maxyear);
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
[yr2,dum] = size( X(nhead+1:end, 1 )) ;
if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; end ; end

xdate=X(nhead+1:yr2,1) ;
if yr1 < 1900 ; xdate = xdate + ydeb -yr1 ; end

%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;

%-----------------------------------------------
splt1 = 11 ;
splt2 = 3 ;

ii=0;
	for i1 = 1:splt1
	for i2 = 1:splt2
	if i1*i2 <= splt
	ii=ii+1;
subplot(splt1,splt2,ii);hold on
Y=X(nhead+1:yr2,2+(i1-1)+length(zone)*(i2-1));
if i2 == 1 
moyenne=mean(Y) ;
sens(i1)=sign(sens(i1)*moyenne) ;
end
Y=Y*sens(i1) ;

%plot(X(:,1),X(:,2+(i1-1)+length(zone)*(i2-1)),'r.-');set(gca,'fontsize',font);grid;
plot(xdate,Y,style);set(gca,'fontsize',font);grid on;
axis tight
if i2==1 ; ylabel(zone(i1)    ,'fontsize',font) ; end
if i1==1 ; title (variable(i2),'fontsize',font) ; end
if i2==2 ; if i1==splt1 ; xlabel (name,'fontsize',14) ; end ; end
	end
	end
	end
	
                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s%s ','print','-dpsc', 'transports.',figname,'.ps') ;
%               orient tall;eval(cmd)
   if maxyear ~= 0
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100', plotdir,'/','beg_transports.',figname','.jpg') ;
   else
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100', plotdir,'/','transports.',figname','.jpg') ;
   end
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
