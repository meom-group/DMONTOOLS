function [fig1]=plot_transports(fig,filename,name,print_fig);
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
	'Mass transport ' ;...
	'Heat transport ' ;...
	'Salt transport ' ;...
	} ; 

	zone2 = { ...
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

	sens1 = [ ...
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
X=load(filename); 
yr1 = X(nhead+1, 1 ) ;
yr2 = X(end, 1 ) ;
[nyrs,ncol]=size(X) ;
nsection=(ncol-4)/3 

fid=fopen(filename) ; fgetl(fid) ; % skip one line
comment=fgetl(fid);                % read second line which is $ nn   sections  nam_1 nam_2 .... nam_nn
fclose(fid) ;
tmp=rstrrep(comment,'  ',' ');      % eliminate double ' ' 
tmp2=strsplit(' ',tmp)            % convert  comment into string array; Section names are from 4 to nsection
zone1=tmp2(4:nsection+3) ;
sens(1:nsection)=1 ;

for ii=1:nsection

  zozo=zone1{ii} ; k=strfind(zozo,'_') ; ll=length(zozo) ; zaza=zozo(k+1:ll) ;
%   zone{ii}=zaza
  zone{ii}=strrep(zaza,'_','-');
end


%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;



%-----------------------------------------------
splt1 = nsection ;
splt2 = 3 ;
figure(fig);clf;

ii=0;
	for i1 = 1:splt1
	for i2 = 1:splt2
	if i1*i2 <= splt
	ii=ii+1;
subplot(splt1,splt2,ii);hold on
Y=X(:,2+(i1-1)+length(zone)*(i2-1));
if i2 == 1 
moyenne=mean(Y) ;
sens(i1)=sign(sens(i1)*moyenne) ;
end
Y=Y*sens(i1) ;

%plot(X(:,1),X(:,2+(i1-1)+length(zone)*(i2-1)),'r.-');set(gca,'fontsize',font);grid;
plot(X(:,1),Y,'r.-');set(gca,'fontsize',font);grid;
axis tight
if i2==1 ; ylabel(zone(i1)    ,'fontsize',font) ; end
if i1==1 ; title (variable(i2),'fontsize',font) ; end
if i2==2 ; if i1==splt1 ; xlabel (name,'fontsize',14) ; end ; end
	end
	end
	end
	
                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_transports1.ps') ;
%               orient tall;eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/', name,'_transports1.jpg') ;
                orient tall;eval(cmd)
                end


		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
