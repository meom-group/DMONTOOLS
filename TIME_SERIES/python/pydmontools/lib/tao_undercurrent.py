#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""tao_undercurrent: monitor the maximum of undercurrent for drakkar runs
"""
#-----------------------------------------------------------------------
#                       Additional Documentation
#
# Modification History:
#  - May 2010: Original module by R. Dussin and J. Le Sommer
#
# Copyright (c) 2010, R. Dussin, J. Le Sommer, D. Munier. For licensing, distribution
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

plot_name = 'tao_undercurrent'
fig_size =  [12.,9.] # a editer surement
yearmin=1980 # for plot : to adjust with data
yearmax=2010

#=======================================================================
#--- Reading the data 
#=======================================================================

def read(argdict=myargs,fromfile=[]):
    #
    if fromfile!=[]:                      # diagnostic mode...
       if fromfile[0].endswith('.nc'):    # if ncfile name is provided
          if not(len(fromfile)==1):
             print 'please provide one netcdf filename'
             sys.exit() 
          dummy, file_obs1, file_obs2, file_obs3, file_obs4, file_obs5 = _get_ncname(argdict=argdict)
          return _readnc(fromfile[0], file_obs1, file_obs2, file_obs3, file_obs4, file_obs5, argdict=argdict)
       elif fromfile[0].endswith('.mtl'): # if mtlfile name is provided
             print 'mlt not supported'
             sys.exit()
       else:                    
          pass
    elif fromfile==[]:                    # production mode
       file_nc, file_obs1, file_obs2, file_obs3, file_obs4, file_obs5 = _get_ncname(argdict=argdict)
       # first try to open a netcdf file
       if os.path.isfile(file_nc) and os.path.isfile(file_obs1) and os.path.isfile(file_obs2) \
       and os.path.isfile(file_obs3) and os.path.isfile(file_obs4) and os.path.isfile(file_obs5):
          return _readnc(file_nc, file_obs1, file_obs2, file_obs3, file_obs4, file_obs5, argdict=argdict) 
          
def _get_ncname(argdict=myargs):
    filename = argdict['datadir'] + osp + argdict['config'] + '-' \
             + argdict['case'] + '_TAO.nc' 
    fileobs5 = argdict['dataobsdir'] + osp + 'cur0n110w_mon.cdf'
    fileobs4 = argdict['dataobsdir'] + osp + 'cur0n140w_mon.cdf'
    fileobs3 = argdict['dataobsdir'] + osp + 'cur0n170w_mon.cdf'
    fileobs2 = argdict['dataobsdir'] + osp + 'cur0n165e_mon.cdf'
    fileobs1 = argdict['dataobsdir'] + osp + 'cur0n156e_mon.cdf'
    return filename, fileobs1, fileobs2, fileobs3, fileobs4, fileobs5

#=======================================================================

def _readnc(filenc=None,fileobs1=None,fileobs2=None,fileobs3=None,fileobs4=None,fileobs5=None,argdict=myargs):

    import matplotlib.pylab as plt
    from numpy import nan
    outdict = {} # creates the dictionnary which will contain the arrays
    coord=['0n156e','0n165e','0n170w','0n140w','0n110w']

    ##### Donnees d'observation ######

    for j in range(1,6):
        #Profondeur
        outdict['o_Profondeur'+str(j)] = rs.readfilenc(vars()['fileobs'+str(j)],'depth')
        depth_obs = rs.readfilenc(vars()['fileobs'+str(j)],'depth')
        #Vitesse
        vars()['Uzonale_obs'+str(j)]=rs.readfilenc(vars()['fileobs'+str(j)],'U_320')
        vars()['tabmoy'+str(j)]=[]

        for i in range(0,len(vars()['Uzonale_obs'+str(j)][0]),1):
            profil=vars()['Uzonale_obs'+str(j)][:,i]
            vars()['val_nul'+str(j)+'_'+str(i)]=[]

            for k in range(0,len(profil),1):
                if profil[k]>1e35:
                   vars()['val_nul'+str(j)+'_'+str(i)].append(k)
                   profil[k]=0

            vars()['tabmoy'+str(j)].append(rs.mean_0(profil))

        outdict['o_Vitesse'+str(j)] = vars()['tabmoy'+str(j)]

        ##Serie
        vars()['Indice_profondeur_serie'+str(j)]=rs.getIndex(vars()['tabmoy'+str(j)],max(vars()['tabmoy'+str(j)]))
        outdict['o_serie_Profondeur'+str(j)] = depth_obs[vars()['Indice_profondeur_serie'+str(j)]]
        vars()['Variation_serie_obs'+str(j)]=vars()['Uzonale_obs'+str(j)][:,vars()['Indice_profondeur_serie'+str(j)]]
        #Calendrier
        time=rs.readfilenc(vars()['fileobs'+str(j)],'time')
        vars()['temps_annee_obs'+str(j)]=time/365
        Origine_temps=[1991,1986,2002,1983,1980]
        vars()['Calendrier_obs'+str(j)]=Origine_temps[j-1]+vars()['temps_annee_obs'+str(j)]
        #Annee premiere observation
        vars()['Origine_o'+str(j)]=vars()['Calendrier_obs'+str(j)][0]
        #Annee derniere observation
        vars()[('shape_o'+str(j))]=plt.shape(vars()['Calendrier_obs'+str(j)])
        vars()['Fin_o'+str(j)]=int(vars()['Calendrier_obs'+str(j)][vars()['shape_o'+str(j)][0]-1])
        vars()['Duree_o'+str(j)]=vars()['Fin_o'+str(j)]-int(vars()['Origine_o'+str(j)])+1
        a=vars()['Origine_o'+str(j)]
        #Moyennes annuelles des observations
        vars()['Variation_moyenne_annuelle'+str(j)]=[]
        vars()['Annees'+str(j)]=[]

        for i in range(0,vars()['Duree_o'+str(j)]):
            vars()['Variation_sur_une_annee'+str(j)+'_'+str(i)]=[]
            vars()['Annees'+str(j)].append(a+i)
            for k in range(0,plt.shape(vars()['Calendrier_obs'+str(j)])[0]):
                if a+i<=vars()['Calendrier_obs'+str(j)][k]<=a+i+1:
                    vars()['Variation_sur_une_annee'+str(j)+'_'+str(i)].append(vars()['Variation_serie_obs'+str(j)][k])

            vars()['Moyenne_annuelle_obs'+str(j)+'_'+str(i)]=rs.mean_0(vars()['Variation_sur_une_annee'+str(j)+'_'+str(i)])
            vars()['Variation_moyenne_annuelle'+str(j)].append(vars()['Moyenne_annuelle_obs'+str(j)+'_'+str(i)])

        outdict['o_serie_Annees'+str(j)] = vars()['Annees'+str(j)]

        for i in range(0,plt.shape(vars()['Variation_moyenne_annuelle'+str(j)])[0]):
            if vars()['Variation_moyenne_annuelle'+str(j)][i]==0:
                vars()['Variation_moyenne_annuelle'+str(j)][i]=nan

        outdict['o_serie_Variation_moyenne_annuelle'+str(j)] = vars()['Variation_moyenne_annuelle'+str(j)]

    ###### Donnees Modele ######
    coordonnees = ['156e','165e','170w','140w','110w']
    outdict['m_Profondeur'] = rs.readfilenc(filenc,'depth')
    outdict['m_Temps_serie'] = rs.readfilenc(filenc,'time_counter')

    for j in range(1,6):
        temp = rs.readfilenc(filenc,'u_'+coordonnees[j-1])
        if len(temp.shape) == 1:
            outdict['m_Vitesse'+str(j)] = temp
        else:
            outdict['m_Vitesse'+str(j)] = temp[-1,:]
        outdict['m_Valeur_sous_courant'+str(j)] = rs.readfilenc(filenc,'u_'+coordonnees[j-1]+'_UC')
        #longitude
        #lon=rs.readfilenc(filenc,'nav_lon')
        #vars()['long_model'+str(j)]=''
        #if lon<0:
        #    vars()['long_model'+str(j)] = `int(-lon)`+'W'
        #else :
        #    vars()['long_model'+str(j)] = `int(lon)`+'E'
        #outdict['m_Longitude'+str(j)] = vars()['long_model'+str(j)]
        outdict['m_Longitude'+str(j)] = coordonnees[j-1].upper()
        #latitude
        #lat=rs.readfilenc(filenc,'nav_lat')
        lat=0.
        vars()['lati_model'+str(j)] = `int(lat)`+'N'
        if lat<0:
            vars()['lati_model'+str(j)]=`int(lat)`+'S'
        outdict['m_Latitude'+str(j)] = vars()['lati_model'+str(j)]
    #
    return outdict   #return the dictionnary of values

#=======================================================================
#--- Plotting the data 
#=======================================================================

def plot(argdict=myargs, figure=None, color='r', compare=False, **kwargs): 
    plt.rcParams['legend.numpoints']=4
    plt.rcParams['legend.axespad']=0.02
    #plt.rcParams['legend.borderaxespad']=0.02
    plt.rcParams['figure.subplot.hspace']=0.7
    plt.rcParams['figure.subplot.right']=0.90
    plt.rcParams['figure.subplot.left']=0.15
    plt.rcParams['figure.subplot.bottom']=0.05
    plt.rcParams['figure.subplot.top']=0.85
    #
    if figure is None: # by default create a new figure
        figure = plt.figure(figsize=fig_size)
    #
    for key in kwargs:
        exec(key+'=kwargs[key]')
    #
    for j in range(1,6):
        plt.subplot(5,1,j)
        plt.plot(vars()['o_serie_Annees'+str(j)],vars()['o_serie_Variation_moyenne_annuelle'+str(j)],'bx',
                 linewidth=0.5,label='Obs')
        #
        timeplt = npy.array((m_Temps_serie),'f')
        uplt    = npy.array((100*vars()['m_Valeur_sous_courant'+str(j)]),'f')
        plt.plot(timeplt,uplt,color+'-',linewidth=1,label='Model')
        if j==1:
            if compare :
	            plt.title('\nEUC - Annual Mean\n\n' 
                      + vars()['m_Longitude'+str(j)] + vars()['m_Latitude'+str(j)]+' '
                      + str(int(vars()['o_serie_Profondeur'+str(j)]))+'m', fontsize=10)
            else :
	            plt.title(argdict['config'] + '-' + argdict['case'] +  '\nEUC - Annual Mean\n\n' 
                      + vars()['m_Longitude'+str(j)] + vars()['m_Latitude'+str(j)]+' '
                      + str(int(vars()['o_serie_Profondeur'+str(j)]))+'m', fontsize=10)
#            plt.legend()
        if j!=1:
            plt.title(vars()['m_Longitude'+str(j)]+vars()['m_Latitude'+str(j)]+' '+
                      str(int(vars()['o_serie_Profondeur'+str(j)]))+'m',fontsize=10)
        if j==3:
            plt.ylabel('Velocity (cm/s)',fontsize=10)
        #
        plt.setp(plt.gca().get_yticklabels(),fontsize=8)
        plt.setp(plt.gca().get_xticklabels(),fontsize=8)
        plt.grid(linestyle='-',linewidth=0.05)
        plt.axis([yearmin,yearmax,0,150])
    #
    return figure

#=======================================================================
#--- saving the plot 
#=======================================================================

def save(argdict=myargs,figure=None):
    if figure is None:
       figure = plt.gcf()
    plotdir, config, case = argdict['plotdir'], argdict['config'], argdict['case']
    plotdir_confcase = plotdir + '/' + config + '/PLOTS/' + config + '-' + case + '/TIME_SERIES/'
    figure.savefig(plotdir_confcase + '/' + config + '-' + case + '_tao_undercurrent.png')

#=======================================================================

#=======================================================================
#--- main 
#=======================================================================

def main(): 
   # get arguments
   parser = pydmontools.standard_pydmt_script_parser()
   (options, args) = parser.parse_args()
   argdict = myargs
   argdict.update(vars(options))
   infiles = args # default value is an empty list
   # read, plot and save   
   values = read(argdict=argdict,fromfile=infiles)
   fig = plot(argdict=argdict,**values)
   if len(args)==0:
      save(argdict=argdict,figure=fig)
   else:
      fig.savefig('./tao_undercurrent.png')
if __name__ == '__main__':
    sys.exit(main() or 0)

