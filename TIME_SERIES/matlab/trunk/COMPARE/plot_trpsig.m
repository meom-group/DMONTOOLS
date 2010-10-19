function [fig1]=plot_trpsig(fig,filename,name,figname,print_fig,style,maxyear);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR DENSITY_CLASS_TRANSPORT (DCT)
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global plotdir ydeb

cc=colormap(jet);
%%%%% Declarations
font=8;
	variable = { ...
	'Transport (Sv) by sigma0 classes' }

	zone = { ...
	'Denmark Strait '          ;...
	'Faroe Bank Channel '            ;...
	} ; 

%CA=[-3 : .25 : 2.2];
%CA2=[-2 : .25 : 2 ];
CA=[-1 : .1 : 0.2];
%CA=[-1 : .1 : 0];


nhead = 1 ; % 1 header line
%%%%% READ
X=load(filename) ;
yr1 = X(nhead+1, 1 ) ;
[yr2,dum] = size(X(nhead+1:end, 1 )) ;
yr2=yr2/2 ;

if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ;   end ; end

lastline=nhead+2*yr2

latDCT=-1*X(1,2:end);
datDCT=X(2:2:lastline,1);ldat=length(datDCT);
ldat
X(X==0)=NaN;
if yr1 < 1900 ; datDCT = datDCT + ydeb -yr1 ; end

for y=1:ldat
DCT(:,:,y)=X(2+(y-1)*2:3+(y-1)*2,2:end);
end

DENMARK(:,:)=squeeze(DCT([1:2:2],:,:));
FAROES(:,:)=squeeze(DCT([2:2:2],:,:));

%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;

idens=find(latDCT<= -27.8 ) ;

subplot(2,1,1) ; hold on
plot(datDCT,nansum(DENMARK(idens,:)),style);axis tight;grid;ylabel('DS > 27.8 (Sv)','fontsize',font)
grid on
set(gca,'fontsize',font); axis tight

subplot(2,1,2) ; hold on
plot(datDCT,nansum(FAROES(idens,:)),style);axis tight;grid;ylabel('FBC > 27.8 (Sv)','fontsize',font)
grid on
set(gca,'fontsize',font); axis tight ; xlabel (name,'fontsize',14)



		if print_fig == 1
%                cmd=sprintf('%s  %s  %s%s%s ','print','-dpsc','trpsig.',figname,'.ps') 
% 		eval(cmd)
      if maxyear ~= 0 
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100',plotdir,'/','beg_trpsig.',figname,'.jpg') ;
      else
                cmd=sprintf('%s  %s  %s%s%s%s%s ','print','-djpeg100',plotdir,'/','trpsig.',figname,'.jpg') ;
      end
		eval(cmd)
		end
		

		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
