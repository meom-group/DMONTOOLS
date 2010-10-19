function [fig1]=plot_icetrd_mini(fig,config,filename,name,figname,print_fig,style,maxyear);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR SEA-ICE
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global dataobsdir plotdir ydeb

  system(['mkmes.ksh ' filename] ) ;
  

  % for September --> Arctic
  tab=load(strcat('../tmp/',config,'_m09_ice.mtl'));
  [yr2,dum]=size(tab) ;
if maxyear ~= 0 ; if yr2 > maxyear ; yr2 = maxyear ; end ; end
  year=tab(1:yr2,1) ;
  yr1=year(1) ; if yr1 < 1900 ; year=year + ydeb -yr1 ; end

  vn=tab(1:yr2,2) ; % Volume
  an=tab(1:yr2,4) ; % Area
  en=tab(1:yr2,6) ; % Extent

  % NOAA
  tab=load('../tmp/N_noaa_m09_ice.mtl');
  datnoaa=tab(:,1) ; ssmr=find(datnoaa==1987) ;
  NorthAreaNoaa=tab(:,4);
     NorthAreaNoaa(1:ssmr)=NorthAreaNoaa(1:ssmr)+1.19 ;
     NorthAreaNoaa(ssmr+1:end)=NorthAreaNoaa(ssmr+1:end)+0.31 ;
  NorthExtentNoaa=tab(:,3);

  % for March --> Antarctic
  tab=load(strcat('../tmp/',config,'_m03_ice.mtl'));
  vs=tab(1:yr2,3) ;  % Volume 
  as=tab(1:yr2,5) ;  % Area
  es=tab(1:yr2,7) ;  % Extent

  % NOAA
  tab=load('../tmp/S_noaa_m03_ice.mtl');
  SouthAreaNoaa=tab(:,4) ;
  SouthExtentNoaa=tab(:,3) ;

  % ARCTIC
  % -------
  % VOLUMES
  subplot(2,3,1) ; hold on
  plot(year,vn,style) ; grid on ; axis tight ; title('Volume Arctic September')

  % AREA
  subplot(2,3,2) ; hold on
  plot(year,an,style) ; grid on ;  title('Area Arctic September')
  if year(1) > 1900 ; plot(datnoaa,NorthAreaNoaa,'b.-') ; axis tight ; end

  % EXTENT
  subplot(2,3,3) ; hold on
  plot(year,en,style) ; grid on ;  title('Extent Arctic September')
  if year(1) > 1900 ; plot(datnoaa,NorthExtentNoaa,'b.-') ; axis tight ; end

  % ANTARCTIC
  % -------
  % VOLUMES
  subplot(2,3,4) ; hold on
  plot(year,vs,style) ; grid on ; axis tight ; title('Volume Antarctic March')

  % AREA
  subplot(2,3,5) ; hold on
  plot(year,as,style) ; grid on ;  title('Area Antarctic March')
  xlabel(name,'fontsize',16)
  if year(1) > 1900 ; plot(datnoaa,SouthAreaNoaa,'b.-') ; axis tight ; end

  % EXTENT
  subplot(2,3,6) ; hold on
  plot(year,es,style) ; grid on ;  title('Extent Antarctic March')
  if year(1) > 1900 ; plot(datnoaa,SouthExtentNoaa,'b.-') ; axis tight ; end

                if print_fig == 1
%                cmd=sprintf('%s  %s%s%s ','print','-dpsc  icetrd.',figname,'.ps') ;
                %orient landscape;eval(cmd)
%                eval(cmd)
             if maxyear ~= 0
                cmd=sprintf('%s  %s %s%s%s%s ','print','-djpeg100 ',plotdir,'/beg_icetrd_mini.',figname,'.jpg') ;
             else
                cmd=sprintf('%s  %s %s%s%s%s ','print','-djpeg100 ',plotdir,'/icetrd_mini.',figname,'.jpg') ;
             end
                %orient landscape;eval(cmd)
                eval(cmd)
                end



%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;

