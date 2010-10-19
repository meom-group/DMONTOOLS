function [fig1]=plot_transports(fig,filename,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR TRANSPORTS
%%%%%%%%%%%%%%%%%%%%%%%
%01_BERING 02_FRAM 03_DENMARK_STRAIT 04_ICELAND_FAROES 05_FAROES_SCOTLAND 06_GIBRALTAR 07_AZORES 08_TORRES 09_ITF 10_MOZAMBIQUE_CHANNEL

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global plotdir

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
	'Denmark '         ;...
	'Icel.-Faroes '    ;...
	'Faroes-Sctoland '    ;...
	'Gibraltar '           ;...
	'Azores ';...
	'Torres ';...
	'ITF  ';...
	'Moz. Ch. '  ;...
	} ; 

	sens = [ ...
	1          ;...
	-1         ;...
	-1         ;...
	+1         ;...
	+1         ;...
	1          ;...
	1          ;...
	1          ;...
	-1          ;...
	-1         ;...
	] ; 



nhead = 0 ;  % 0 header line
%%%%% READ
X=load(filename) ;
yr1 = X(nhead+1, 1 ) ;
yr2 = X(end, 1 ) ;
years =X(nhead+1:end, 1 ) ;


%%%%%%%% PLOTS
nbz=length(zone);
nbvar=length(variable) +2 ; % add 2 for trp+ and trp-

splt=nbz*length(variable) ;



%-----------------------------------------------
splt1 = 11 ;
splt2 = 3 ;
figure(fig);clf;

ii=0;
	for i1 = 1:nbz
	for i2 = 1:splt2
	if i1*i2 <= splt
	ii=ii+1;
   subplot(splt1,splt2,ii);hold on

  if i2 == 1 
    trp=X(nhead+1:end,(i1+1)) ;
    trpp=X(nhead+1:end,(i1+1)+nbz);
    trpm=X(nhead+1:end,(i1+1)+2*nbz);
    moyenne=mean(trp) ;
    sens(i1)=sign(sens(i1)*moyenne) ;
    trp=trp*sens(i1) ;
    trpp=trpp*sens(i1) ;
    trpm=trpm*sens(i1) ;
    plot(years,trp,'r.-');set(gca,'fontsize',font);grid;
    plot(years,trpp,'g.-');set(gca,'fontsize',font);
    plot(years,trpm,'b.-');set(gca,'fontsize',font);
  else
    Y=X(:,2+(i1-1)+length(zone)*(i2+1));
    Y=Y*sens(i1) ;
    plot(years,Y,'r.-');set(gca,'fontsize',font);grid;
  end

  axis tight
if i2==1 ; ylabel(zone(i1)    ,'fontsize',font) ; end
if i1==1 ; title (variable(i2),'fontsize',font) ; end
if i2==2 ; if i1==splt1 ; xlabel (name,'fontsize',14) ; end ; end
	end
	end
	end
	
                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_transports2.ps') ;
%               orient tall;eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/',name,'_transports2.jpg') ;
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
