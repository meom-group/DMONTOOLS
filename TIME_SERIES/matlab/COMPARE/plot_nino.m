function [fig1]=plot_nino(fig,filename,name,figname,print_fig,style,maxyear);
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
	'SST ' ;...
	} ; 
	
	zone = { ...
	'NINO1+2 '          ;...
	'NINO3 '            ;...
	'NINO4 '          ;...
	'NINO3.4 '         ;...
	} ; 


nhead = 0 ; % 0 header lines
%%%%% READ
X=load(filename) ;
yr1 = X(nhead+1, 1 ) ;
[tmp,dum] = size ( X(nhead+1:end, 1 ) ) ;
yr2=tmp/12 ;
if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; tmp=12 * yr2 ; end ; end


%

datMOD=X(1:tmp,1)+(X(1:tmp,2)-1)/12;
if yr1 < 1900 ; datMOD=datMOD + ydeb - yr1 ; end
   MOD=X(1:tmp,3:2:end);
X1=min(datMOD);X2=max(datMOD)+1;

filedata = strcat(dataobsdir,'/','nino_obs.txt') ; Y=load(filedata) ; [I J]=size(Y);
datOBS=Y(:,1)+(Y(:,2)-1)/12;
   OBS=Y(:,3:2:end);

filedata = strcat(dataobsdir,'/','SOIindex.txt') ; Z=load(filedata) ; [I J]=size(Z);
datSOI=reshape((Z(:,1)*ones(1,12)+([0:11]'/12*ones(1,I))')',I*(J-1),1);
   SOI=reshape(Z(:,2:end)',I*(J-1),1);

%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;

%-----------------------------------------------
splt1 = 5 ;
splt2 = 1 ;

ii=0;
	for i1 = 1:splt1
	for i2 = 1:splt2
	if i1*i2 <= splt
	ii=ii+1;
subplot(splt1,splt2,ii);hold on
plot(datOBS,OBS(:,ii),'b-','linewidth',1);set(gca,'fontsize',font);axis tight;AA=axis;axis([X1 X2 AA(3) AA(4)]);
%plot(datMOD,MOD(:,ii),style,'linewidth',1);set(gca,'fontsize',font);axis tight;AA=axis;axis([X1 X2 AA(3) AA(4)]);
plot(datMOD,MOD(:,ii),style);set(gca,'fontsize',font);axis tight;AA=axis;axis([X1 X2 AA(3) AA(4)]);
ylabel(zone(ii),'fontsize',font) ; grid on;
if i1==1 ; title (strcat(name,'. Observation (blue)'),'fontsize',14) ; end
	end
	end
	end

subplot(splt1,splt2,splt1);plot(datSOI,SOI);axis tight;AA=axis;axis([X1 X2 AA(3) AA(4)]);grid;set(gca,'fontsize',font)
ylabel('SOI index','fontsize',font)
	
                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s%s ','print','-dpsc','nino.',figname,'.ps');
%               orient tall;eval(cmd)
    if maxyear ~= 0 
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100',plotdir,'/','beg_nino.',figname,'.jpg');
    else
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100',plotdir,'/','nino.',figname,'.jpg');
    end
                orient tall;eval(cmd)
                end

		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
