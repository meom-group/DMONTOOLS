function [fig1]=plot_mhts(fig,filename,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR TRANSPORTS
%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Declarations
font=8;
	variable = { ...
	'Southward Accumulated Q_{net}' ;...
	'Advective MHT                ' ;...
	} ; 

	zone = { ...
	'Global '          ;...
	'Atlantic '            ;...
	'Indo-Pacific '          ;...
	} ; 

CA=[-3 : .25 : 2.2];
CA2=[-2 : .25 : 2 ];

nhead = 1 ;  % 1 header line
%%%%% READ
X=load(filename) ; 
yr1 = X(nhead+1, 1 ) ;
yr2 = X(end, 1 ) ;

latMHT=X(1,2:end);
datMHT=X(2:6:end,1);ldat=length(datMHT);
X(X==0)=NaN;
for y=1:ldat
MHT(:,:,y)=X(2+(y-1)*6:7+(y-1)*6,2:end);
end
MHTa(:,:,:)=MHT([1:2:5],:,:);
MHTd(:,:,:)=MHT([2:2:6],:,:);

%%%%%%%% PLOTS
splt=length(zone)*length(variable) ;

%-----------------------------------------------
figure(fig);clf;
subplot(3,3,1);contourf(datMHT,latMHT,squeeze(MHTd(1,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datMHT,latMHT,squeeze(MHTd(1,:,:)),[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;ylabel('GLOBAL','fontsize',font);title(variable(1),'fontsize',font)  ;
subplot(3,3,2);contourf(datMHT,latMHT,squeeze(MHTa(1,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datMHT,latMHT,squeeze(MHTa(1,:,:)),[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;  				title(variable(2),'fontsize',font)  ;
tmp=squeeze(MHTa(1,:,:)-MHTd(1,:,:));%tmp(abs(tmp)<.05)=NaN;
subplot(3,3,3);contourf(datMHT,latMHT,tmp,CA2);caxis([min(CA2) max(CA2)]);hold on;[c,h]=contour(datMHT,latMHT,tmp,[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;  			title('Second minus First','fontsize',font)  ;

subplot(3,3,4);contourf(datMHT,latMHT,squeeze(MHTd(2,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datMHT,latMHT,squeeze(MHTd(2,:,:)),[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;ylabel('ATL','fontsize',font)	;
subplot(3,3,5);contourf(datMHT,latMHT,squeeze(MHTa(2,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datMHT,latMHT,squeeze(MHTa(2,:,:)),[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;
tmp=squeeze(MHTa(2,:,:)-MHTd(2,:,:));%tmp(abs(tmp)<.05)=NaN;
subplot(3,3,6);contourf(datMHT,latMHT,tmp,CA2);caxis([min(CA2) max(CA2)]);hold on;[c,h]=contour(datMHT,latMHT,tmp,[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;

subplot(3,3,7);contourf(datMHT,latMHT,squeeze(MHTd(3,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datMHT,latMHT,squeeze(MHTd(3,:,:)),[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;ylabel('INDO-PAC','fontsize',font);
subplot(3,3,8);contourf(datMHT,latMHT,squeeze(MHTa(3,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datMHT,latMHT,squeeze(MHTa(3,:,:)),[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;xlabel (name,'fontsize',14) 
tmp=squeeze(MHTa(3,:,:)-MHTd(3,:,:));%tmp(abs(tmp)<.05)=NaN;
subplot(3,3,9);contourf(datMHT,latMHT,tmp,CA2);caxis([min(CA2) max(CA2)]);hold on;[c,h]=contour(datMHT,latMHT,tmp,[0 0],'w');clabel(c,h), colorbar;set(gca,'fontsize',font);grid;

%subplot(3,3,1);pcolor(datMHT,latMHT,squeeze(MHTd(1,:,:)));caxis(CA);shading flat;colorbar;set(gca,'fontsize',font);grid;ylabel('GLOBAL','fontsize',font);title(variable(1),'fontsize',font)  ;
%subplot(3,3,2);pcolor(datMHT,latMHT,squeeze(MHTa(1,:,:)));caxis(CA);shading flat;colorbar;set(gca,'fontsize',font);grid;  				title(variable(2),'fontsize',font)  ;
%tmp=squeeze(MHTd(1,:,:)-MHTa(1,:,:));tmp(abs(tmp)<.05)=NaN;
%subplot(3,3,3);pcolor(datMHT,latMHT,tmp);caxis(CA2);shading flat;colorbar;set(gca,'fontsize',font);grid;  			title('First minus Second','fontsize',font)  ;

%subplot(3,3,4);pcolor(datMHT,latMHT,squeeze(MHTd(2,:,:)));caxis(CA);shading flat;colorbar;set(gca,'fontsize',font);grid;ylabel('ATL','fontsize',font)	;
%subplot(3,3,5);pcolor(datMHT,latMHT,squeeze(MHTa(2,:,:)));caxis(CA);shading flat;colorbar;set(gca,'fontsize',font);grid;
%tmp=squeeze(MHTd(2,:,:)-MHTa(2,:,:));tmp(abs(tmp)<.05)=NaN;
%subplot(3,3,6);pcolor(datMHT,latMHT,tmp);caxis(CA2);shading flat;colorbar;set(gca,'fontsize',font);grid;

%subplot(3,3,7);pcolor(datMHT,latMHT,squeeze(MHTd(3,:,:)));caxis(CA);shading flat;colorbar;set(gca,'fontsize',font);grid;ylabel('INDO-PAC','fontsize',font);
%subplot(3,3,8);pcolor(datMHT,latMHT,squeeze(MHTa(3,:,:)));caxis(CA);shading flat;colorbar;set(gca,'fontsize',font);grid;xlabel (name,'fontsize',14) 
%tmp=squeeze(MHTd(3,:,:)-MHTa(3,:,:));tmp(abs(tmp)<.05)=NaN;
%subplot(3,3,9);pcolor(datMHT,latMHT,tmp);caxis(CA2);shading flat;colorbar;set(gca,'fontsize',font);grid;

%%%%%% contourf(datMHT,latMHT,squeeze(MHTd(1,:,:)),CA);caxis([min(CA) max(CA)]);hold on;[c,h]=contour(datMHT,latMHT,squeeze(MHTd(1,:,:)),[0 0]);clabel(c,h), colorbar;
	
		if print_fig == 1
                cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_MHT1.ps');
 		orient tall;eval(cmd)
                cmd=sprintf('%s  %s  %s%s ','print','-djpeg100', name,'_MHT1.jpg');
		orient tall;eval(cmd)
		end
		

		
%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;
