#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""tsmeanlat : domain and lattude-bands average temperature and salinity budgets for drakkar runs
"""
#-----------------------------------------------------------------------
#                       Additional Documentation
#
# Modification History:
#  - May 2010: Original module by R. Dussin and J. Le Sommer
#
# Copyright (c) 2010, R. Dussin and J. Le Sommer. For licensing, distribution
# conditions, contact information, and additional documentation see the wiki :
# https://servforge.legi.grenoble-inp.fr/projects/DMONTOOLS/wiki/TIME_SERIES/python.
#=======================================================================

#-----------------------------------------------------------------------

#- Module General Import and Declarations

import sys,os
import numpy as npy
import pydmontools 
from pydmontools import __readtools__ as rs
from pydmontools import __plottools__ as ps
plt = ps.plt 

myargs = pydmontools.default_argdict
osp = os.sep

#- parameters

plot_name = 'tsmeanlat'
fig_size =  [15.,18.]
plt.rcParams.update({'figure.figsize': fig_size})

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfiles=[]):
    if fromfiles!=[]:                      # diagnostic mode... 
       if fromfiles[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfiles)==1):
             print 'please provide one netcdf filename'
             sys.exit() 
          return _readnc(fromfiles[0])
       elif fromfiles[0].endswith('.mtl'): # if mtlfile name is provided
          print 'mtl files are no longer supported'
	  sys.exit()
       else:                               
          pass
    elif fromfiles==[]:                    # production mode... 
       filenc, filelatn, filelats, filelateq   = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(filenc) and os.path.isfile(filelatn) and os.path.isfile(filelats) and os.path.isfile(filelateq):
          return _readnc(filenc, filelatn, filelats, filelateq) 
       else:
          print '>>> tsmeanlat.py : No files found for frequency : ' + argdict['monitor_frequency'] ; exit()
          
def _get_ncname(argdict=myargs):
    #
    if rs.check_freq_arg(argdict['monitor_frequency']):

        filename = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_TSMEAN.nc'

        filenamelatn = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_TSLATN.nc'

        filenamelats = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_TSLATS.nc'

        filenamelateq = argdict['datadir'] + osp + argdict['config'] + '-' \
                 + argdict['case'] + '_' + argdict['monitor_frequency'] + '_TSLATEQ.nc'

    return filename, filenamelatn, filenamelats, filenamelateq

#======================================================================= 

def _readnc(filenc=None,filelatn=None,filelats=None,filelateq=None):
    outdict = {} # creates the dictionnary which will contain the arrays 
    outdict['tmean']    = rs.readfilenc(filenc,'mean_3Dvotemper') 
    outdict['smean']    = rs.readfilenc(filenc,'mean_3Dvosaline') 
    outdict['sshmean']  = rs.readfilenc(filenc,'mean_3Dsossheig') 
    outdict['levels']   = rs.readfilenc(filenc,'gdept')           
    outdict['date']     = rs.get_datetime(filenc) 
    outdict['tmodel']   = rs.readfilenc(filenc,'mean_votemper')
    outdict['smodel']   = rs.readfilenc(filenc,'mean_vosaline')  
    outdict['tmodels']   = rs.readfilenc(filelats,'mean_votemper')
    outdict['smodels']   = rs.readfilenc(filelats,'mean_vosaline')  
    outdict['tmodeln']   = rs.readfilenc(filelatn,'mean_votemper')
    outdict['smodeln']   = rs.readfilenc(filelatn,'mean_vosaline')  
    outdict['tmodeleq']   = rs.readfilenc(filelateq,'mean_votemper')
    outdict['smodeleq']   = rs.readfilenc(filelateq,'mean_vosaline')  
    outdict['tlatn']    = rs.readfilenc(filelatn,'mean_3Dvotemper')
    outdict['slatn']    = rs.readfilenc(filelatn,'mean_3Dvosaline')
    outdict['sshlatn']  = rs.readfilenc(filelatn,'mean_3Dsossheig')
    outdict['datelatn'] = rs.get_datetime(filelatn) 
    outdict['tlats']    = rs.readfilenc(filelats,'mean_3Dvotemper')
    outdict['slats']    = rs.readfilenc(filelats,'mean_3Dvosaline')
    outdict['sshlats']  = rs.readfilenc(filelats,'mean_3Dsossheig')
    outdict['datelats'] = rs.get_datetime(filelats) 
    outdict['tlateq']   = rs.readfilenc(filelateq,'mean_3Dvotemper')
    outdict['slateq']   = rs.readfilenc(filelateq,'mean_3Dvosaline')
    outdict['sshlateq'] = rs.readfilenc(filelateq,'mean_3Dsossheig')
    outdict['datelateq']= rs.get_datetime(filelateq) 
    return outdict # return the dictionnary of values 

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs,figure=None,color='r',tmean=None,smean=None,sshmean=None,\
         tlatn=None,slatn=None,sshlatn=None,\
         tlats=None,slats=None,sshlats=None,\
         tlateq=None,slateq=None,sshlateq=None,\
         levels=None,date=None,datelatn=None,datelats=None,datelateq=None,\
         tmodel=None,smodel=None,\
         tmodeln=None, smodeln=None, \
         tmodels=None, smodels=None, \
         tmodeleq=None, smodeleq=None, \
         compare=False):
   
    _date = ps.mdates.date2num(date) # now a numerical value
    _datelatn = ps.mdates.date2num(datelatn) # now a numerical value
    _datelats = ps.mdates.date2num(datelats) # now a numerical value
    _datelateq = ps.mdates.date2num(datelateq) # now a numerical value

    if figure is None: # by default create a new figure
          figure = plt.figure()
    
    list3Dmean = ['tmean', 'smean', 'sshmean']
    titles3Dmean = ['3D T trend\n (mean removed)', '3D S trend\n(mean removed)', '3D SSH trend\n(mean removed)']
    nb3Dmean=len(list3Dmean)
    list3Dlatn = ['tlatn', 'slatn', 'sshlatn']
    list3Dlats = ['tlats', 'slats', 'sshlats']
    list3Dlateq = ['tlateq', 'slateq', 'sshlateq']
    # define marker style for global, north, south and equat curvs
    cgl=color + '.-'
    cno=color + '^-'
    cso=color + 'v-'
    ceq=color + 'x-'

    for k in range(nb3Dmean) :
            # use zvarxx and zmeanxx temporary arrays and scalar
            zvargl=vars()[list3Dmean[k]] ; zmeangl = zvargl.mean() ; zvargl = zvargl - zmeangl
            zvarno=vars()[list3Dlatn[k]] ; zmeanno = zvarno.mean() ; zvarno = zvarno - zmeanno
            zvarso=vars()[list3Dlats[k]] ; zmeanso = zvarso.mean() ; zvarso = zvarso - zmeanso
            zvareq=vars()[list3Dlateq[k]]; zmeaneq = zvareq.mean() ; zvareq = zvareq - zmeaneq

            minlist = [ min(zvargl) , min(zvarno) , min(zvarso) , min(zvareq) ]
            maxlist = [ max(zvargl) , max(zvarno) , max(zvarso) , max(zvareq) ]

            ax1 = figure.add_subplot(3,nb3Dmean,k+1)
            ax1.plot(date,      zvargl,  cgl,\
                     datelatn,  zvarno,  cno,\
                     datelateq, zvareq,  ceq,\
                     datelats,  zvarso,  cso )
            ax1.grid(True)
            ax1.axis([min(_date), max(_date),min(minlist),max(maxlist)])
            ps.set_dateticks(ax1)
            if not(compare) and k == 1 :
                plt.title(argdict['config'] + '-' + argdict['case']+'\n'+'Global (.), North of 60N (^), 60N-60S (x), South of 60S (V)'+'\n\n'+titles3Dmean[k])
            else :
                plt.title(titles3Dmean[k])

    ### 2) 2D PLOTS
    ncontour=75

    tmin=min(tmodel[-1,:]-tmodel[0,:])
    tminn=min(tmodeln[-1,:]-tmodeln[0,:])
    tmins=min(tmodels[-1,:]-tmodels[0,:])
    tmineq=min(tmodeleq[-1,:]-tmodeleq[0,:])

    tminpl= min(tmin, tminn, tmins, tmineq)

    tmax=max(tmodel[-1,:]-tmodel[0,:])
    tmaxn=max(tmodeln[-1,:]-tmodeln[0,:])
    tmaxs=max(tmodels[-1,:]-tmodels[0,:])
    tmaxeq=max(tmodeleq[-1,:]-tmodeleq[0,:])

    tmaxpl= max(tmax, tmaxn, tmaxs, tmaxeq)

    plt.subplot(3,nb3Dmean,nb3Dmean+1)
    plt.semilogy((tmodel[-1,:]  -tmodel[0,:]  ) , levels,cgl ,\
                 (tmodeln[-1,:] -tmodeln[0,:] ) , levels,cno ,\
                 (tmodels[-1,:] -tmodels[0,:] ) , levels,cso ,\
                 (tmodeleq[-1,:]-tmodeleq[0,:]) , levels,ceq  )
    plt.grid(True)
    plt.axis([tminpl, tmaxpl ,max(levels),min(levels)])
#   plt.axis([-1., 1.,max(levels),min(levels)])
    plt.ylabel('Log Depth')
    plt.title('Global change in T')

    smin=min(smodel[-1,:]-smodel[0,:])
    sminn=min(smodeln[-1,:]-smodeln[0,:])
    smins=min(smodels[-1,:]-smodels[0,:])
    smineq=min(smodeleq[-1,:]-smodeleq[0,:])

    sminpl= min(smin, sminn, smins, smineq)

    smax=max(smodel[-1,:]-smodel[0,:])
    smaxn=max(smodeln[-1,:]-smodeln[0,:])
    smaxs=max(smodels[-1,:]-smodels[0,:])
    smaxeq=max(smodeleq[-1,:]-smodeleq[0,:])

    smaxpl= max(smax, smaxn, smaxs, smaxeq)


    plt.subplot(3,nb3Dmean,2*nb3Dmean+1)
#    plt.semilogy((smodel[-1,:]-smodel[0,:]),levels,color)
    plt.semilogy((smodel[-1,:] -smodel[0,:]) , levels,cgl, \
                 (smodeln[-1,:]  -smodeln[0,:])  , levels,cno, \
                 (smodels[-1,:]  -smodels[0,:])  , levels,cso, \
                 (smodeleq[-1,:] -smodeleq[0,:]) , levels,ceq  )
    plt.grid(True)
    plt.axis([sminpl, smaxpl ,max(levels),min(levels)])
#   plt.axis([-0.1, 0.1 ,max(levels),min(levels)])
    plt.ylabel('Log Depth')
    plt.title('Global change in S')

    if not(compare):
       limits_temper=[-0.5, 0.5] ; step_temper = 0.005 ; step_tticks = step_temper * 20
       contours_temper=npy.arange(limits_temper[0],limits_temper[1]+step_temper,step_temper)
       ticks_temper=npy.arange(limits_temper[0],limits_temper[1]+step_tticks,step_tticks)
       contourl_temper=npy.arange(-1.,1.,step_tticks)

       limits_salin=[-0.2, 0.2] ; step_salin = 0.002 ; step_sticks = step_salin * 20
       contours_salin=npy.arange(limits_salin[0],limits_salin[1]+step_salin,step_salin)
       ticks_salin=npy.arange(limits_salin[0],limits_salin[1]+step_sticks,step_sticks)
       contourl_salin=npy.arange(-1.,1.,step_sticks)

       ax2 = figure.add_subplot(3,nb3Dmean,nb3Dmean+2)
       zval=npy.transpose(tmodels[:,:]-tmodels[0,:])
       plt.contour(date, levels, zval, contourl_temper)  #,color='w', )
       plt.contourf(date, levels, zval,contours_temper, extend='both')
       plt.colorbar(ticks=ticks_temper)
       ax2.grid(True)
       plt.yscale('log')
       ax2.axis([ min(_date), max(_date), max(levels),min(levels) ])
       plt.title('SOUTH\n Temperature Anomaly')
       ps.set_dateticks(ax2)

       ax3 = figure.add_subplot(3,nb3Dmean,2*nb3Dmean+2)
       zval=npy.transpose(smodels[:,:]-smodels[0,:])
       plt.contour(date, levels, zval, contourl_salin)  #,color='w', )
       plt.contourf(date, levels, zval, contours_salin,extend='both')
       plt.colorbar(ticks=ticks_salin)
       ax3.grid(True)
       plt.yscale('log')
       ax3.axis([ min(_date), max(_date), max(levels),min(levels) ])
       plt.title('SOUTH\n Salinity Anomaly')
       ps.set_dateticks(ax3)
    #
       ax4 = figure.add_subplot(3,nb3Dmean,nb3Dmean+3)
       zval=npy.transpose(tmodeln[:,:]-tmodeln[0,:])
       plt.contour(date, levels, zval, contourl_temper)  #,color='w', )
       plt.contourf(date, levels, zval,contours_temper,extend='both')
       plt.colorbar(ticks=ticks_temper)
       ax4.grid(True)
       plt.yscale('log')
       ax4.axis([ min(_date), max(_date), max(levels),min(levels) ])
       plt.title('NORTH\n Temperature Anomaly')
       ps.set_dateticks(ax4)

       ax5 = figure.add_subplot(3,nb3Dmean,2*nb3Dmean+3)
       zval=npy.transpose(smodeln[:,:]-smodeln[0,:])
       plt.contour(date, levels, zval, contourl_salin)  #,color='w', )
       plt.contourf(date, levels, zval, contours_salin,extend='both')
       plt.colorbar(ticks=ticks_salin)
       ax5.grid(True)
       plt.yscale('log')
       ax5.axis([ min(_date), max(_date), max(levels),min(levels) ])
       plt.title('NORTH\n Salinity Anomaly')
       ps.set_dateticks(ax5)
    #
    return figure

#=======================================================================
#--- Save the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    monit_freq = argdict['monitor_frequency']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_' + monit_freq + '_tsmeanlat.png')

#=======================================================================
#--- main 
#=======================================================================

def main(): 
   # get arguments
   parser = pydmontools.standard_pydmt_script_parser()
   (options, args) = parser.parse_args()
   # default argument dictionnary
   argdict = myargs 
   # updated with the command line options
   argdict.update(vars(options))
   #
   infiles = args     # default value is an empty list
   # read, plot and save   
   values = read(argdict=argdict,fromfiles=infiles)
   fig = plot(**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig)
   else:
      fig.savefig('./tsmeanlat.png')

if __name__ == '__main__':
    sys.exit(main() or 0)

