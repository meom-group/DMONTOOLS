function [fig1]=plot_trc_cfc(fig,filename,name,print_fig);
%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% PLOTTING ROUTINE FOR CFC
%%%%%%%%%%%%%%%%%%%%%%%

%  $Rev$
%  $Date$
%  $Id$
%--------------------------------------------------------------
global plotdir

font=12;
style='r.-' ;
tab=load(filename) ;

year=tab(2:3:end,1) ;
lat=tab(1,5:end-1);
% total content of tracers in the ocean
cfcmean=tab(2:3:end,2) ;
cfcinv=tab(2:3:end,5:end-1) ;
cfcint=tab(3:3:end,5:end-1) ;
cfcsurf=tab(4:3:end,5:end-1) ;

clf
subplot(2,2,1) ; hold on
   plot(year,cfcmean,style) ; grid  ; ylabel('Total CFC11 (mol)','fontsize',font) ;
                           title ('CFC11 monitoring','fontsize',18)

subplot(2,2,2) ; hold on
  contourf(year,lat,cfcinv') ; title('CFC zonal mean inv. (mol/m2)','fontsize',font) ;
                               colorbar ; axis tight ; grid on

subplot(2,2,3) ; hold on
  contourf(year,lat,cfcint') ; xlabel(name,'fontsize',18) ; colorbar ; axis tight ; grid on
                             title('CFC zonal int inv. (mol/m)','fontsize',font) ;
subplot(2,2,4) ; hold on
  contourf(year,lat,cfcsurf') ; title('CFC zonal mean surf.conc. (mol/m3)','fontsize',font) ;
                             colorbar;  axis tight ; grid on

                if print_fig == 1
%               cmd=sprintf('%s  %s  %s%s ','print','-dpsc', name,'_cfc.ps') ;
%               eval(cmd)
                cmd=sprintf('%s  %s  %s%s%s%s ','print','-djpeg100',plotdir,'/', name,'_cfc.jpg') ;
                eval(cmd)
                end



%-----------------------------------------------
fig1=fig+1;
if nargout==0,
 clear cs
end;


