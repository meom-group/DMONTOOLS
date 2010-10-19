function [fig1]=plot_cable(fig,filename,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR CABLE Florida Bahamas
%%%%%%%%%%%%%%%%%%%%%%%
%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------

%%%%% Declarations
global dataobsdir plotdir
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
yr2 = X(end, 1 ) ;


%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;



%-----------------------------------------------
splt1 = 5 ;
splt2 = 2 ;
figure(fig);clf;

subplot(splt1,splt2,1);hold on
i1=7 ; i2=1
Y=X(:,2+(i1-1)+length(zone)*(i2-1));
if i2 == 1 
moyenne=mean(Y) ;
sens(i1)=sign(sens(i1)*moyenne) ;
end
Y=Y*sens(i1) ;

plot(X(:,1),Y,'r.-');set(gca,'fontsize',font);grid;
axis tight
if i2==1 ; ylabel(zone(i1)    ,'fontsize',font) ; end
if i1==7 ; title (variable(i2),'fontsize',font) ; end
if i2==1 ; if i1==7 ; xlabel (name,'fontsize',14) ; end ; end

if yr1 > 1900 
tab=load(strcat(dataobsdir,'/cable.mtl')) ;
year=tab(:,1) ; trp=tab(:,2) ;
plot(year,trp,'b.-') ; axis tight
end
	
                if print_fig == 1
%                cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_cable.ps') ;
%                orient tall;eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/',name,'_cable.jpg') ;
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
