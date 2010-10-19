function [fig1]=plot_icetrd(fig,filename,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR SEA-ICE
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global dataobsdir plotdir


  config=name;
  system(['mkmes.ksh ' filename] ) ;

  % for March --> Arctic
  tab=load(strcat('../tmp/',config,'_m03_ice.mtl'));
  year=tab(:,1) ;
  vn=tab(:,2) ; % Volume
  an=tab(:,4) ; % Area
  en=tab(:,6) ; % Extent

  % NOAA
  tab=load(strcat('../tmp/','/N_noaa_m03_ice.mtl'));
  datnoaa=tab(:,1) ; ssmr=find(datnoaa==1987) ;
  NorthAreaNoaa=tab(:,4);
     NorthAreaNoaa(1:ssmr)=NorthAreaNoaa(1:ssmr)+1.19 ;
     NorthAreaNoaa(ssmr+1:end)=NorthAreaNoaa(ssmr+1:end)+0.31 ;
  NorthExtentNoaa=tab(:,3);

  % for September --> Antarctic
  tab=load(strcat('../tmp/',config,'_m09_ice.mtl'));
  vs=tab(:,3) ;  % Volume 
  as=tab(:,5) ;  % Area
  es=tab(:,7) ;  % Extent

  % NOAA
  tab=load(strcat('../tmp/','/S_noaa_m09_ice.mtl'));
  SouthAreaNoaa=tab(:,4) ;
  SouthExtentNoaa=tab(:,3) ;

  clf
  % ARCTIC
  % -------
  % VOLUMES
  subplot(2,3,1) ; hold on
  plot(year,vn,'k.-') ; grid on ; axis tight ; title('Volume Arctic March')

  % AREA
  subplot(2,3,2) ; hold on
  plot(year,an,'k.-') ; grid on ;  title('Area Arctic March')
 if year(1) > 1900 ;   plot(datnoaa,NorthAreaNoaa,'b.-') ; axis tight ; end

  % EXTENT
  subplot(2,3,3) ; hold on
  plot(year,en,'k.-') ; grid on ;  title('Extent Arctic March')
  if year(1) > 1900 ; plot(datnoaa,NorthExtentNoaa,'b.-') ; axis tight ; end

  % ANTARCTIC
  % -------
  % VOLUMES
  subplot(2,3,4) ; hold on
  plot(year,vs,'k.-') ; grid on ; axis tight ; title('Volume Antarctic September')

  % AREA
  subplot(2,3,5) ; hold on
  plot(year,as,'k.-') ; grid on ;  title('Area Antarctic September')
  xlabel(config,'fontsize',16)
  if year(1) > 1900 ; plot(datnoaa,SouthAreaNoaa,'b.-') ; axis tight ; end

  % EXTENT
  subplot(2,3,6) ; hold on
  plot(year,es,'k.-') ; grid on ;  title('Extent Antarctic September')
  if year(1) > 1900 ;   plot(datnoaa,SouthExtentNoaa,'b.-') ; axis tight ; end

                if print_fig == 1
%                cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_sea_icetrd.ps') ;
%                eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100', plotdir,'/',name,'_sea_icetrd.jpg') ;
                eval(cmd)
                end



%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;

