function [fig1]=plot_trpsig(fig,filename,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR DENSITY_CLASS_TRANSPORT (DCT)
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global plotdir

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
yr2 = X(end, 1 ) ;

latDCT=-1*X(1,2:end);
datDCT=X(2:2:end,1);ldat=length(datDCT);
X(X==0)=NaN;

for y=1:ldat
DCT(:,:,y)=X(2+(y-1)*2:3+(y-1)*2,2:end);
end

DENMARK(:,:)=squeeze(DCT([1:2:2],:,:));
FAROES(:,:)=squeeze(DCT([2:2:2],:,:));

%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;

%-----------------------------------------------
figure(fig);clf;

axes('Position',[0.1300 0.2838 0.2869 0.6412]);
pcolor(datDCT,latDCT,DENMARK);caxis([min(CA) max(CA)]);colorbar('North');set(gca,'fontsize',font);grid
ylabel(zone(1),'fontsize',font);
title(variable(1),'fontsize',font) ; shading interp
AA=axis ; axis ( [ AA(1) AA(2) -28.2 -25.3] )
hold on;
[c,h]=contour(datDCT,latDCT,DENMARK,[0 0],'w') ;
plot([AA(1) AA(2)],[-27.8 -27.8],'w-.')
xlabel(name,'fontsize',14) ;

axes('Position',[0.5703 0.2838 0.2869 0.6412]);
pcolor(datDCT,latDCT,FAROES);caxis([min(CA) max(CA)]);colorbar('North');set(gca,'fontsize',font);grid;
ylabel(zone(2),'fontsize',font);
title(variable(1),'fontsize',font) ; shading interp
AA=axis ; axis ( [ AA(1) AA(2) -28.2 -25.3] )
hold on;
[c,h]=contour(datDCT,latDCT,FAROES,[0 0],'w');
xlabel(name,'fontsize',14) ;
plot([AA(1) AA(2)],[-27.8 -27.8], 'w-.')


colormap(cc(length(cc):-1:1,:))

idens=find(latDCT<= -27.8 )
axes('Position',[0.1300 0.05 0.2869 0.14])
%plot(datDCT,nansum(DENMARK));axis tight;grid;ylabel('DS Total (Sv)','fontsize',font)
plot(datDCT,nansum(DENMARK(idens,:)));axis tight;grid;ylabel('DS > 27.8 (Sv)','fontsize',font)
set(gca,'fontsize',font);

axes('Position',[0.5703 0.05 0.2869 0.14]);
plot(datDCT,nansum(FAROES(idens,:)));axis tight;grid;ylabel('FBC > 27.8 (Sv)','fontsize',font)
set(gca,'fontsize',font);


%orient landscape


%subplot(2,1,1);contourf(datDCT,latDCT,squeeze(DENMARK(1,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datDCT,latDCT,squeeze(DENMARK(1,:,:)),[0 0],'w') ;colorbar;set(gca,'fontsize',font);grid;ylabel(zone(1),'fontsize',font);title(variable(1),'fontsize',font)  ;
%subplot(2,1,2);contourf(datDCT,latDCT,squeeze(FAROES(1,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datDCT,latDCT,squeeze(FAROES(1,:,:)),[0 0],'w'); colorbar;set(gca,'fontsize',font);grid;ylabel(zone(2),'fontsize',font);title(variable(1),'fontsize',font)  ;


%clabel(c,h), colorbar


		if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_trpsig.ps');
%		eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/',name,'_trpsig.jpg');
		eval(cmd)
		end
		

		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
