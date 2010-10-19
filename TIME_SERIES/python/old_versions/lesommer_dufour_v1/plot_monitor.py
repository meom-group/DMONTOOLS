#!/usr/bin/env python
##!/usr/local/stow/python-2.4.4/bin/python
#
# Monitoring module for SouthernCross project
# Plot the timeseries for web site. 
#
# Modification History:
# ---------------------
# - Dec 2008: Initial plots with matplotlib  (R. Dussin)
# - Mar 2009: Modular implementation (J. Le Sommer, C. Dufour)
# - May 2009: EKE plots (C. Dufour)
# - July 2009: cosmetic changes (J. Le Sommer)
#
#
# Suggestions and Future improvments
# ----------------------------------
# - split the module into submodules
# - adapt to produce non-timeseries plots
#
#-------------------------------------------------------------------
#- import modules 
import matplotlib.pylab as plt
import numpy as npy 
import os
import sys
import getopt
from PyDom import gridutils,ncio,timeutils

#------------------------- parameters ---------------------------------

#  Setting general plot parameters
params = {'axes.labelsize': 12,
          'text.fontsize': 12,
          'legend.fontsize': 12,
          'xtick.labelsize': 10,
          'ytick.labelsize': 10}
plt.rcParams.update(params)


listcolors = ['r','k','b','g','c','m','y']
#listcolors = ['r','c','m']

#------------------------ Configuration ----------------------------------
#- verbosity
def comment(mycomment):
    if rc.verbose:
       print mycomment
 
#- I/O directories
def datadir(config=None,case=None):
    """Return the data directory for (config, case)
    """
    return rc.root_datadir + config + '/' + config + '-' + case + '-' +  'MONITOR/'

def datameandir(config=None,case=None):
    """Return the data MEAN directory for (config, case)
    """
    return rc.root_datadir + config + '/' + config + '-' + case + '-' +  'MEAN/'

def dataobsdir():
    """Return the data observation directory
    """
    return rc.root_dataobsdir

def plotdir_single(config=None,case=None):
    """Return the plot directory for (config, case)
    """
    return rc.root_plotdir + config + '/' + 'PLOTS' + '/' + config + '-' + case + '/' + 'TIME_SERIES/'

def plotdir_multi(configs=None,cases=None):
    """Return the plot directory for (config, case)
    """
    directory = rc.root_plotdir  + 'COMPARE/'
    for config,case in zip(configs,cases):
        directory+= config + '-' + case + '_'
    directory = directory[:-1] + '/TIME_SERIES/ '
    return directory

def local_checkdir(locdir):
    """Check whether local directory locdir exists. create it if not.
    """
    locdir_bits = locdir.split('/')[1:]
    ldir = '/'
    for bit in locdir_bits:
       if not(os.path.isdir(ldir)):
          os.mkdir(ldir)
       ldir+= bit + '/'

#- Loading utilities
def _get_dic_from_module(module=None,excepting=[]):
    """Return a dictionnary which keys match `module` components
    """
    exec ('import ' + module + ' as local_module')
    out = {}
    for strkey in dir(local_module):
        if strkey not in excepting and not(strkey.startswith('__')):
           exec("out['" + strkey + "']= local_module." + strkey )
    return out

def _get_dic_from_file(file=None,excepting=[]):
    """Return a dictionnary which keys match `file` components
    """
    import sys,os
    if file.startswith('~'):
       file = file.replace('~',os.path.expanduser("~"))
    # test whether the path is in sys.path
    path2module = file[:-file.split('/')[-1].__len__()-1]
    if (path2module not in sys.path) and path2module not in ['','.']:
       sys.path.append(path2module)
       sys_path_is_modified = True
    else:
       sys_path_is_modified = False
    # local import of the datasets_module
    str_local_module = file.split('/')[-1][:-3]
    out  =  _get_dic_from_module(module=str_local_module,excepting=excepting)
    # clean sys.path if needed
    if sys_path_is_modified:
       sys.path.remove(path2module)
    return out

#- Storing users-configuration
class _store_rc:
    """This class stores user-defined parameters.
    """
    def __init__(self,file='./rc_plot_monitor.py'):
        dic = _get_dic_from_file(file)
        self._update_from_dic(dic)
 
    def _update_from_dic(self,dic):
        for key in dic.keys():
            exec('self.' + key + "= dic['"+key+"']")

    def __call__(self,**kwargs):
        self._update_from_dic(kwargs)

    def __repr__(self):
        repr = '\n'
        for key in dir(self):
            if not(key.startswith('_')):
               strkey = eval('self.' + key).__repr__()
               repr+= key + '\n' + '               ' + strkey + '\n'
        return repr 
        
rc = _store_rc()


#-------------------------- Remote machine --------------------------------------
#
# we could also use pyssh or paramiko instead of os.system calls.
#
def _check_remote_directory(machine=None,directory=None,username=None):
    """Check whether directory exists on username@machine, creates it if not.
    """
    cmd = "ssh " + machine + " -l " + username + " ' if [ ! -d "+ directory+ " ] ; then mkdir "+ directory+" ; fi '"
    comment(' execute os command :\n' + cmd)
    os.system(cmd)

def check_remote_directory_single(config=None,case=None):
    """Check whether the output directory for config case exists on the remote machine, creates it if not.
    """
    machine = rc.remote_machine
    username = rc.remote_username
    basedir = rc.remote_basedir
    directory = basedir + config + '/'
    _check_remote_directory(machine=machine,directory=directory,username=username)
    directory+=  config + '-' + case + '/' 
    _check_remote_directory(machine=machine,directory=directory,username=username)
    directory+=  'TIME_SERIES/'
    _check_remote_directory(machine=machine,directory=directory,username=username)
    return directory

def check_remote_directory_multi(configs=None,cases=None):
    """Check whether the output directory for configs/cases comparison exists on the remote machine, creates it if not.
    """
    machine = rc.remote_machine
    username = rc.remote_username
    basedir = rc.remote_basedir  + 'COMPARE/'
    directory = basedir
    for config,case in zip(configs,cases):
        directory+= config + '-' + case + '_'
    directory = directory[:-1] + '/'
    _check_remote_directory(machine=machine,directory=directory,username=username)
    directory+=  'TIME_SERIES/'
    _check_remote_directory(machine=machine,directory=directory,username=username)
    return directory

def send_file(file=None,machine=None,directory=None,username=None):
    locfile = file.split('/')[-1]
    cmd = "scp " + file + " " + username + "@" + machine + ":" + directory + "/" + locfile
    os.system(cmd)

#---------------------------  Main class -------------------------------------------------

class plot_timeseries:
    def __init__(self,configs=None,cases=None,plot_names=None,remote_send=None,local_send=None):
        """
        Parameters 
        ----------
        configs : list of strings
          configurations to be monitored
        cases : list of strings
          cases to be monitored
        plot_names : list of strings
          required plots
        """
        self.configs = configs
        self.cases = cases
        #
        if remote_send is None:
           self.remote_send = rc.remote_send
        else:
           self.remote_send = remote_send
        #
        if local_send is None:
           self.local_send = rc.local_send
        else:
           self.local_send = local_send
        #
        if configs.__len__()==1 and cases.__len__()==1:
           self.plotclassdic = available_plotting_single
           self.single = True
        else:
           self.plotclassdic = available_plotting_single_for_multi
           self.single = False 
        # 
        if plot_names is None:
#           self.plot_names = self.plotclassdic.keys()
            self.plot_names = rc.plot_names
        else:
           self.plot_names = plot_names

    def proceed(self):
        if self.remote_send:
           comment(' Checking the remote directory...')
           self.check_remote()
        if self.local_send:
           comment(' Checking the local directory...')
           self.check_local()
        self.loop()

    def check_remote(self):
        """check the remote directory.
        """
        self.remote_machine = rc.remote_machine
        self.remote_username = rc.remote_username
        if self.single:
           self.remote_directory = check_remote_directory_single(config=self.configs[0],case=self.cases[0])
        else:
           self.remote_directory = check_remote_directory_multi(configs=self.configs,cases=self.cases)
           
    def check_local(self):
        """check the local plotting directory.
        """      
        if self.single:
           self.local_directory = plotdir_single(config=self.configs[0],case=self.cases[0])
        else:
           self.local_directory = plotdir_multi(configs=self.configs,cases=self.cases)
        local_checkdir(self.local_directory)


    def loop(self):
        """loop on plot types.
        """
        for plot_name in self.plot_names: 
            comment(' Preparing plot  '+ plot_name)
            mkplt_single = self.plotclassdic[plot_name]
            mkplt = mkplt_multi(configs=self.configs,cases=self.cases,mkplt_single=mkplt_single) # works also for single  
            comment(' processing...')       
            filename = mkplt.proceed()
            comment(' ...done')
            if self.remote_send: 
               self.send2remote(filename) 
            if self.local_send:
               self.send2local(filename)      
    
    def send2remote(self,file):
        """Send file 
        """
        lfile = file.split('/')[-1]
        destination = self.remote_username + '@' + self.remote_machine + ':' + self.remote_directory
        comment(' sending '+ lfile + ' to ' + destination + '...')
        send_file(file=file,machine=self.remote_machine,directory=self.remote_directory,username=self.remote_username) 
        comment(' ...done')
   
    def send2local(self,file):
        lfile = file.split('/')[-1]
        comment(' archive file ' + lfile)
        os.system('mv ' + file + ' ' + self.local_directory)
        comment(' ...done')
#
#
#--------------------------- Plotting Classes ---------------------------------
#- dealing with the y-limits
def max_ylims(ylim1,ylim2):
    """The largest y axis limits.
    """
    if not(ylim1 is None or ylim2 is None):
       maxlims = (min(ylim1[0],ylim2[0]),max(ylim1[1],ylim2[1]))
    elif ylim1 is None:
       maxlims = ylim2
    elif ylim2 is None:
       maxlims = ylim1
    else: 
       maxlims = None
    return maxlims

def get_ylims():
    lims = plt.ylim() 
    if lims==(0.0,1.0):
       lims = None
    return lims 

#- template
class _template_mkplt_single: # use this template to create new plots
   plot_name = ''
   mtl_sfx = ''
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the monitoring plot class.
       """
       self.config = config
       self.case = case
       self.mtlfile = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx +'.mtl'
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       self.save()
       plt.close(self.figure)

   def _set_general_parameters(self):
       """Set general parameters.
       """
       pass

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       pass 

   def read(self):
       """Read and process the entry mtl file.
       """
       pass

   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       pass

   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case)
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)

#- Monitoring annual mean EKE on the global domain and in basins
class mkplt_eke_single: 
   plot_name = 'eke'
#   mtl_sfx = ''
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the monitoring plot class.
       """
       self.config = config
       self.case = case
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename

   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.yrdeb = 1980
       self.yrend = 2004
       self.fig_size =  [15.,20.]
       self.def_config = 'PERIANT05'
       self.def_ijdom = {'i_min':1,'i_max':723,'j_min':1,'j_max':203}
#       self.yrdeb = 1969
#       self.yrend = 2001
#       self.fig_size =  [15.,20.]
#       self.def_config = 'PERIANT8'
#       self.def_ijdom = {'i_min':1,'i_max':2883,'j_min':1,'j_max':799}
       self.def_zdom = {'k_min':0,'k_max':46}       
       self.datalist=['Global','Drake','Kerguelen','Campbell','Indian','Pacific','Atlantic']
       plt.rc('figure',figsize=self.fig_size)

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       if 'PERIANT05' in self.config:
         self.def_config = 'PERIANT05'
         self.def_ijdom = {'i_min':1,'i_max':723,'j_min':1,'j_max':203}
         self.def_zdom = {'k_min':0,'k_max':46}
         self.dom = gridutils.OPA_C_Grid(xydom=self.def_ijdom,config=self.def_config,z_grid_method='nemo3_partial_cells')
#       if 'PERIANT8' in self.config:
#         self.def_config = 'PERIANT8'
#         self.def_ijdom = {'i_min':1,'i_max':2883,'j_min':1,'j_max':799}
#         self.def_zdom = {'k_min':0,'k_max':46}
#         self.dom = gridutils.OPA_C_Grid(xydom=self.def_ijdom,config=self.def_config,z_grid_method='nemo3_partial_cells')
         self.datalist=['Global','Drake','Kerguelen','Campbell','Indian','Pacific','Atlantic']
         self.coord = npy.zeros((4,len(self.datalist)-1))
         # Drake coordinates
         self.coord[0,0],self.coord[2,0] = self.dom.find_point(lat0=-70,lon0=-100)
         self.coord[1,0],self.coord[3,0] = self.dom.find_point(lat0=-30,lon0=-20)
         self.cond1 = (self.dom.gphit>-70)*(self.dom.gphit<-30)*(self.dom.glamt>-100)*(self.dom.glamt<-20)
         # Kerguelen coordinates
         self.coord[0,1],self.coord[2,1] = self.dom.find_point(lat0=-70,lon0=30)
         self.coord[1,1],self.coord[3,1] = self.dom.find_point(lat0=-30,lon0=110)
         self.cond2 = (self.dom.gphit>-70)*(self.dom.gphit<-30)*(self.dom.glamt>30)*(self.dom.glamt<72.75) \
                     +(self.dom.gphit>-70)*(self.dom.gphit<-30)*(self.dom.glamt>=72.75)*(self.dom.glamt<110)
         # Campbell coordinates
         self.coord[0,2],self.coord[2,2] = self.dom.find_point(lat0=-70,lon0=100)
         self.coord[1,2],self.coord[3,2] = self.dom.find_point(lat0=-30,lon0=180) 
         self.cond3 = (self.dom.gphit>-70)*(self.dom.gphit<-30)*(self.dom.glamt>100)*(self.dom.glamt<180) 
         # Indian coordinates (like in Meredith and Hogg, 2006)
         self.coord[0,3],self.coord[2,3] = self.dom.find_point(lat0=-57,lon0=37)
         self.coord[1,3],self.coord[3,3] = self.dom.find_point(lat0=-42,lon0=150)
         self.cond4 = (self.dom.gphit>-57)*(self.dom.gphit<-42)*(self.dom.glamt>37)*(self.dom.glamt<72.75)\
                    + (self.dom.gphit>-57)*(self.dom.gphit<-42)*(self.dom.glamt>=72.75)*(self.dom.glamt<150)
         # Pacific coordiantes (like in Meredith and Hogg, 2006)
         self.coord[0,4],self.coord[2,4] = self.dom.find_point(lat0=-65,lon0=150)
         self.coord[1,4],self.coord[3,4] = self.dom.find_point(lat0=-46,lon0=-68)
         self.cond5 = (self.dom.gphit>-65)*(self.dom.gphit<-46)*(self.dom.glamt>150)*(self.dom.glamt<180) \
                     +(self.dom.gphit>-65)*(self.dom.gphit<-46)*(self.dom.glamt>=-180)*(self.dom.glamt<-68)
         # Atlantic coordinates (like in Meredith and Hogg, 2006)
         self.coord[0,5],self.coord[2,5] = self.dom.find_point(lat0=-55,lon0=-35)
         self.coord[1,5],self.coord[3,5] = self.dom.find_point(lat0=-45,lon0=12)
         self.cond6 = (self.dom.gphit>-55)*(self.dom.gphit<-45)*(self.dom.glamt>-35)*(self.dom.glamt<12)

   def read(self):
       """Read and process the entry mtl file.
       """
       datalist = self.datalist
       dom = self.dom
       self.eke_tot = npy.zeros((self.yrend-self.yrdeb+1,len(datalist)))
       dom.get_surf()
       i = 0
       for y in range(self.yrdeb,self.yrend+1):
          filename = datameandir(self.config,self.case)+str(y)+'/'+self.config+'-'+self.case+'_y'+str(y)+'_EKE.nc'
          eke = ncio.load(filenam=filename,var='voeke')
          #
          self.eke_tot[i,0] = dom.integrate2D_dxdy(eke[0,1,...],where=None,grid='t')/dom.t_surf.sum()
          #
          self.eke_tot[i,1] = dom.integrate2D_dxdy(eke[0,1,...],where=self.cond1,grid='t') \
                  /dom.t_surf[self.coord[0,0]:self.coord[1,0],self.coord[2,0]:self.coord[3,0]].sum()
          #
          self.eke_tot[i,2] = dom.integrate2D_dxdy(eke[0,1,...],where=self.cond2,grid='t') \
                  / (dom.t_surf[self.coord[0,1]:self.coord[1,1],self.coord[2,1]:-1].sum() \
                   + dom.t_surf[self.coord[0,1]:self.coord[1,1],0:self.coord[3,1]].sum())
          #
          self.eke_tot[i,3] = dom.integrate2D_dxdy(eke[0,1,...],where=self.cond3,grid='t') \
                  /dom.t_surf[self.coord[0,2]:self.coord[1,2],self.coord[2,2]:self.coord[3,2]].sum()
          #
          self.eke_tot[i,4] = dom.integrate2D_dxdy(eke[0,1,...],where=self.cond4,grid='t') \
                  / (dom.t_surf[self.coord[0,3]:self.coord[1,3],self.coord[2,3]:-1].sum() \
                   + dom.t_surf[self.coord[0,3]:self.coord[1,3],0:self.coord[3,3]].sum())
          #
          self.eke_tot[i,5] = dom.integrate2D_dxdy(eke[0,1,...],where=self.cond5,grid='t') \
                  /dom.t_surf[self.coord[0,4]:self.coord[1,4],self.coord[2,4]:self.coord[3,4]].sum()
          #
          self.eke_tot[i,6] = dom.integrate2D_dxdy(eke[0,1,...],where=self.cond6,grid='t') \
                  /dom.t_surf[self.coord[0,5]:self.coord[1,5],self.coord[2,5]:self.coord[3,5]].sum()
          i = i+1

       self.eke_tot = timeutils.timearray(self.eke_tot,freq='yr',unit='years',t0=self.yrdeb)


   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       plt.rc('text', usetex=False)
       for k in range(1,len(self.datalist)+1) :
          plt.subplot(4,2,k)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(self.eke_tot.time_axis,self.eke_tot[:,k-1], color+'.-')
          plt.axis([self.yrdeb,self.yrend,self.eke_tot[:,k-1].min(),self.eke_tot[:,k-1].max()])
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.ylabel('EKE (m2/s2)',fontsize='x-large')
          plt.xlabel('time (years)',fontsize='small')
          plt.title('%s annual EKE mean at depth=10m' %(self.datalist[k-1]),fontsize='large')


   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case)
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename


#- Monitoring run trends for single plots
class mkplt_tsmean_single:
   """Produce a monitoring plot for diagnosing run trends (config, case).
   """
   plot_name = 'tsmean'
   mtl_sfx1 = 'TMEAN'
   mtl_sfx2 = 'SMEAN'
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the monitoring plot class.
       """
       self.config = config
       self.case = case
       self.mtlfile1 = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx1 +'.mtl'
       self.mtlfile2 = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx2 +'.mtl'
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename

   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.datalist1 = ['unused1','unused2','unused3','levels'] # line 2 in TMEAN.mtl and SMEAN.mtl
       self.datalist2 = ['year', 'sshmean', 'tmean', 'tmodel'] # following lines in TMEAN.mtl
       self.datalist3 = ['year2', 'sshmean2','smean', 'smodel'] # following lines in SMEAN.mtl
       self.fig_size =  [15.,18.]
       plt.rc('figure',figsize=self.fig_size)

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       pass

   def read(self):
       """Read and process the entry mtl file.
       """
       #- read the file
       file1 = open(self.mtlfile1,'r')
       lignes1 = [lignes1 for lignes1 in file1.readlines() if lignes1.strip() ] # remove empty lines
       file1.close()
       file2 = open(self.mtlfile2,'r')
       lignes2 = [lignes2 for lignes2 in file2.readlines() if lignes2.strip() ] # remove empty lines
       file2.close()
       #- initialize arrays
       for dat in self.datalist1 :
         exec(dat + '= []')
       for dat in self.datalist2 :
         exec(dat + '= []')
       for dat in self.datalist3 :
         exec(dat + '= []')
       #- data manipulation
       for chaine in lignes1[2:3] :
          element=chaine.split()
          for k in range(self.datalist1.index('levels'), len(element)) : # get levels depth
             levels.append(+1*(float(element[k])))
       for chaine in lignes1[3:] :
          element=chaine.split()
          for k in range(0,self.datalist2.index('tmodel')) : # get single values before tmodel
             vars()[self.datalist2[k]].append(float(element[k]))
          for k in range(self.datalist2.index('tmodel'), len(element)) : # get tmodel in each level
             tmodel.append(float(element[k]))
       for chaine in lignes2[3:] :
          element=chaine.split()
          for k in range(0,self.datalist3.index('smodel')) : # get single values before smodel
             vars()[self.datalist3[k]].append(float(element[k]))
          for k in range(self.datalist3.index('smodel'), len(element)) : # get smodel in each level
             smodel.append(float(element[k]))
       tmodel = npy.reshape(tmodel,(npy.size(year),-1))
       smodel = npy.reshape(smodel,(npy.size(year),-1))
       self.tmodel = tmodel
       self.smodel = smodel
       self.year = year
       self.year2 = year2
       self.levels = levels
       self.tmean = tmean
       self.smean = smean
       self.sshmean = sshmean

   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       plt.rc('text', usetex=False)
       tmodel = self.tmodel
       smodel = self.smodel
       tmean = self.tmean
       smean = self.smean
       sshmean = self.sshmean
       levels = self.levels
       year = self.year
       year2 = self.year2
       ncontour=25
       # 1) PLOTS OF 3D MEANS
       list3Dmean = ['tmean', 'smean', 'sshmean']
       titles3Dmean = ['3D Global T Mean', '3D Global S Mean', '3D Global SSH Mean']
       nb3Dmean=len(list3Dmean)
       for k in range(nb3Dmean) :
          plt.subplot(3,nb3Dmean,k+1)
          plt.plot(year, vars()[list3Dmean[k]], color+'.-')
          plt.grid(True)
          plt.axis([min(year), max(year),min(vars()[list3Dmean[k]]),max(vars()[list3Dmean[k]])])
          plt.title(titles3Dmean[k])
       # 2) 2D PLOTS
       plt.subplot(3,nb3Dmean,nb3Dmean+1)
       plt.semilogy((tmodel[-1,:]-tmodel[0,:]),levels,color+'.-')
       plt.grid(True)
       plt.axis([min(tmodel[-1,:]-tmodel[0,:]), max(tmodel[-1,:]-tmodel[0,:]),min(levels),max(levels)])
       plt.ylabel('Log Depth')
       plt.title('Global change in T')
       ###########
       plt.subplot(3,nb3Dmean,nb3Dmean+2)
       plt.contourf(year, npy.log10(levels), npy.transpose(tmodel[:,:]-tmodel[0,:]),ncontour)
       plt.colorbar()
       plt.grid(True)
       plt.ylabel('Temperature anomaly')
       ###########
       plt.subplot(3,nb3Dmean,2*nb3Dmean+1)
       plt.semilogy((smodel[-1,:]-smodel[0,:]),levels,color+'.-')
       plt.grid(True)
       plt.axis([min(smodel[-1,:]-smodel[0,:]), max(smodel[-1,:]-smodel[0,:]),min(levels),max(levels)])
       plt.ylabel('Log Depth')
       plt.title('Global change in S')
       ###########
       plt.subplot(3,nb3Dmean,2*nb3Dmean+2)
       plt.contourf(year, npy.log10(levels), npy.transpose(smodel[:,:]-smodel[0,:]),ncontour)
       plt.colorbar()
       plt.grid(True)
       plt.ylabel('Salinity anomaly')
       self.figure = figure

   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case)
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename

#-  Monitoring run trends for multi plots
class mkplt_tsmeans_multi:
   """Produce a monitoring plot for diagnosing run trends (config, case).
   """
   plot_name = 'tsmeans'
   mtl_sfx1 = 'TMEAN'
   mtl_sfx2 = 'SMEAN'
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the monitoring plot class.
       """
       self.config = config
       self.case = case
       self.mtlfile1 = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx1 +'.mtl'
       self.mtlfile2 = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx2 +'.mtl'
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename

   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.datalist1 = ['unused1','unused2','unused3','levels'] # line 2 in TMEAN.mtl and SMEAN.mtl
       self.datalist2 = ['year', 'sshmean', 'tmean', 'tmodel'] # following lines in TMEAN.mtl
       self.datalist3 = ['year2', 'sshmean2','smean', 'smodel'] # following lines in SMEAN.mtl
       self.fig_size =  [15.,18.]
       plt.rc('figure',figsize=self.fig_size)

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       pass

   def read(self):
       """Read and process the entry mtl file.
       """
       #- read the file
       file1 = open(self.mtlfile1,'r')
       lignes1 = [lignes1 for lignes1 in file1.readlines() if lignes1.strip() ] # remove empty lines
       file1.close()
       file2 = open(self.mtlfile2,'r')
       lignes2 = [lignes2 for lignes2 in file2.readlines() if lignes2.strip() ] # remove empty lines
       file2.close()
       #- initialize arrays
       for dat in self.datalist1 :
         exec(dat + '= []')
       for dat in self.datalist2 :
         exec(dat + '= []')
       for dat in self.datalist3 :
         exec(dat + '= []')
       #- data manipulation
       for chaine in lignes1[2:3] :
          element=chaine.split()
          for k in range(self.datalist1.index('levels'), len(element)) : # get levels depth
             levels.append(+1*(float(element[k])))
       for chaine in lignes1[3:] :
          element=chaine.split()
          for k in range(0,self.datalist2.index('tmodel')) : # get single values before tmodel
             vars()[self.datalist2[k]].append(float(element[k]))
          for k in range(self.datalist2.index('tmodel'), len(element)) : # get tmodel in each level
             tmodel.append(float(element[k]))
       for chaine in lignes2[3:] :
          element=chaine.split()
          for k in range(0,self.datalist3.index('smodel')) : # get single values before smodel
             vars()[self.datalist3[k]].append(float(element[k]))
          for k in range(self.datalist3.index('smodel'), len(element)) : # get smodel in each level
             smodel.append(float(element[k]))
       tmodel = npy.reshape(tmodel,(npy.size(year),-1))
       smodel = npy.reshape(smodel,(npy.size(year),-1))
       self.tmodel = tmodel
       self.smodel = smodel
       self.year = year
       self.year2 = year2
       self.levels = levels
       self.tmean = tmean
       self.smean = smean
       self.sshmean = sshmean

   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       tmodel = self.tmodel
       smodel = self.smodel
       tmean = self.tmean
       smean = self.smean
       sshmean = self.sshmean
       levels = self.levels
       year = self.year
       year2 = self.year2
       ncontour=25
       # 1) PLOTS OF 3D MEANS
       list3Dmean = ['tmean', 'smean', 'sshmean']
       titles3Dmean = ['3D Global T Mean', '3D Global S Mean', '3D Global SSH Mean']
       nb3Dmean=len(list3Dmean)
       for k in range(nb3Dmean) :
          plt.subplot(3,nb3Dmean,k+1)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(year, vars()[list3Dmean[k]], color+'.-')
          plt.axis([min(year),max(year),min(vars()[list3Dmean[k]]),max(vars()[list3Dmean[k]])])
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.title(titles3Dmean[k])
       # 2) 2D PLOTS
       plt.subplot(3,nb3Dmean,nb3Dmean+1)
       ylim_before = get_ylims()                  # dealing with ylims
       plt.semilogy((tmodel[-1,:]-tmodel[0,:]),levels,color+'.-')
       plt.axis([min(tmodel[-1,:]-tmodel[0,:]), max(tmodel[-1,:]-tmodel[0,:]),min(levels),max(levels)])
       ylim_after = get_ylims()                   #
       plt.ylim(max_ylims(ylim_before,ylim_after))#
       plt.grid(True)
       plt.ylabel('Log Depth')
       plt.title('Global change in T')
       ###########
       plt.subplot(3,nb3Dmean,2*nb3Dmean+1)
       ylim_before = get_ylims()                  # dealing with ylims
       plt.semilogy((smodel[-1,:]-smodel[0,:]),levels,color+'.-')
       plt.axis([min(smodel[-1,:]-smodel[0,:]), max(smodel[-1,:]-smodel[0,:]),min(levels),max(levels)])
       ylim_after = get_ylims()                   #
       plt.ylim(max_ylims(ylim_before,ylim_after))#
       plt.grid(True)
       plt.ylabel('Log Depth')
       plt.title('Global change in S')
       self.figure = figure

   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case)
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename

#-  Monitoring ice maximum volume, area and extent compared to observations 
class mkplt_icetrd_single:
   """Produce the maximum ice volume, extent and area versus observations monitoring plot for a particular (config, case).
   """
   plot_name = 'icetrds'
   mtl_sfx = 'icemonth'
   mtl_sfx2 = 'North'
   mtl_sfx3 = 'South'
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the monitoring plot class.
       """
       self.config = config
       self.case = case
       self.mtlfile = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx +'.mtl'
       self.mtlfile2 = dataobsdir() + '/' + self.mtl_sfx2 +'.mtl'
       self.mtlfile3 = dataobsdir() + '/' + self.mtl_sfx3 +'.mtl'
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self): 
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename

   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.month1 = 3
       self.month2 = 9
       self.nmonth = 12
       self.convfactor = 1000

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       if 'PERIANT' in self.config :
          self.test = 1
          self.datalist1 = ['year','vant','aant','eant']
          self.datalist3 = ['yearobs2sept','sedatasept','sadatasept']
          self.fig_size = [15.,6.]
          plt.rc('figure',figsize=self.fig_size)
       else :
          self.test = 2
          self.datalist1 = ['year','varc','vant','aarc','aant','earc','eant']
          self.datalist2 = ['yearobsmarch','nedatamarch','nadatamarch']
          self.datalist3 = ['yearobs2sept','sedatasept','sadatasept']
          self.fig_size = [15.,15.]
          plt.rc('figure',figsize=self.fig_size)
 

   def read(self):
       """Read and process the entry mtl file.
       """
       month1 = self.month1
       month2 = self.month2
       nmonth = self.nmonth
       convfactor = self.convfactor
       #- read the file
       file = open(self.mtlfile,'r')
       lignes = [lignes for lignes in file.readlines() if lignes.strip() ] # remove empty lines
       file.close()

       if 'ORCA' in self.config :
          file2 = open(self.mtlfile2,'r')
          lignes2 = [lignes2 for lignes2 in file2.readlines() if lignes2.strip() ] # remove empty lines
          file2.close()

       file3 = open(self.mtlfile3,'r')
       lignes3=[lignes3 for lignes3 in file3.readlines() if lignes3.strip() ] # remove empty lines
       file3.close()
       #- initialize arrays
       for dat in self.datalist1:
           exec(dat + '= []')
       if 'ORCA' in self.config :
          for dat in self.datalist2:
             exec(dat + '= []')
       for dat in self.datalist3:
           exec(dat + '= []')
       #- data manipulation
       for chaine in lignes[3:] :
        element=chaine.split()
        if 'ORCA' in self.config :
          for k in range(1,1+nmonth) :
                vars()[self.datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
                vars()[self.datalist1[1]].append(float(element[k])/convfactor)
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[self.datalist1[2]].append(float(element[k])/convfactor)
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[self.datalist1[3]].append(float(element[k])/convfactor)
          for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
                vars()[self.datalist1[4]].append(float(element[k])/convfactor)
          for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
                vars()[self.datalist1[5]].append(float(element[k])/convfactor)
          for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
                vars()[self.datalist1[6]].append(float(element[k])/convfactor)

        elif 'PERIANT' in self.config :
          for k in range(1,1+nmonth) :
                vars()[self.datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
                vars()[self.datalist1[1]].append(float(element[k])/convfactor)
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[self.datalist1[2]].append(float(element[k])/convfactor)
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[self.datalist1[3]].append(float(element[k])/convfactor)

        if 'ORCA' in self.config :
           for chaine in lignes2[0:] :
              element=chaine.split()
              if float(element[1]) == month1 :
                 vars()[self.datalist2[0]].append(float(element[0]))
                 vars()[self.datalist2[1]].append(float(element[2]))
                 vars()[self.datalist2[2]].append(float(element[3]))

       for chaine in lignes3[0:] :
          element=chaine.split()
          if float(element[1]) == month2 :
             vars()[self.datalist3[0]].append(float(element[0]))
             vars()[self.datalist3[1]].append(float(element[2]))
             vars()[self.datalist3[2]].append(float(element[3]))

       # Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
       if 'ORCA' in self.config :
          indN=vars()[self.datalist2[0]].index(1987)
          for k in range(indN+1) :
             vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+1.19
          for k in range(indN+1,len(datalist2[0])) :
             vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+0.31
       
       self.year = year
       if 'ORCA' in self.config :
          self.yearobsmarch = yearobsmarch
          self.varc = varc
          self.aarc = aarc
          self.earc = earc
          self.nedatamarch= nedatamarch
          self.nadatamarch = nadatamarch
       elif ('ORCA' in self.config) + ('PERIANT' in self.config) :
          self.yearobs2sept = yearobs2sept
          self.vant = vant
          self.aant = aant
          self.eant = eant
          self.sedatasept = sedatasept
          self.sadatasept = sadatasept
 
         
   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       year = self.year
       test = self.test
       if 'ORCA' in self.config :
            plt.subplot(2,3,1)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.varc[2::12], color+'.-')
            plt.axis([min(year[::12]),max(year[::12]),min(self.varc[2::12]),max(self.varc[2::12])])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Volume Arctic March',fontsize='large')
            ############
            plt.subplot(2,3,2)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.aarc[2::12], color+'.-',self.yearobsmarch, self.nadatamarch, 'm--')
            mini = min(min(self.aarc[2::12]),min(self.nadatamarch))
            maxi = max(max(self.aarc[2::12]),max(self.nadatamarch))
            plt.axis([min(year[::12]),max(year[::12]),mini,maxi])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Area Arctic March',fontsize='large')
            ###########
            plt.subplot(2,3,3)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.earc[2::12], color+'.-',self.yearobsmarch, self.nedatamarch, 'm--')
            mini = min(min(self.earc[2::12]),min(self.nedatamarch))
            maxi = max(max(self.earc[2::12]),max(self.nedatamarch))
            plt.axis([min(year[::12]),max(year[::12]),mini, maxi])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Extent Arctic March',fontsize='large')
 
       if ('ORCA' in self.config) + ('PERIANT' in self.config) :
            plt.subplot(test,3,test*test)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.vant[8::12], color+'.-')
            plt.axis([min(year[::12]),max(year[::12]),min(self.vant[8::12]),max(self.vant[8::12])])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Volume Antarctic September',fontsize='large')
            ############
            plt.subplot(test,3,test*test+1)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.aant[8::12], color+'.-',self.yearobs2sept, self.sadatasept, 'm--')
            mini = min(min(self.aant[8::12]),min(self.sadatasept))
            maxi = max(max(self.aant[8::12]),max(self.sadatasept))
            plt.axis([min(year[::12]),max(year[::12]),mini,maxi])         
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Area Antarctic September',fontsize='large')
            ############
            plt.subplot(test,3,test*3)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.eant[8::12], color+'.-', self.yearobs2sept, self.sedatasept, 'm--')
            mini = min(min(self.eant[8::12]),min(self.sedatasept))
            maxi = max(max(self.eant[8::12]),max(self.sedatasept))
            plt.axis([min(year[::12]),max(year[::12]),mini,maxi])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Extent Antarctic September',fontsize='large')
       self.figure = figure  

   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case):
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename

#-  Monitoring ice minimum volume, area and extent compared to observations
class mkplt_icetrd_mini_single: # use this template to create new plots
   """Produce the ice maximum volume, extent and area versus observations monitoring plot for a particular (config, case).
   """
   plot_name = 'icetrds_mini'
   mtl_sfx = 'icemonth'
   mtl_sfx2 = 'North'
   mtl_sfx3 = 'South'
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the monitoring plot class.
       """
       self.config = config
       self.case = case
       self.mtlfile = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx +'.mtl'
       self.mtlfile2 = dataobsdir() + '/' + self.mtl_sfx2 +'.mtl'
       self.mtlfile3 = dataobsdir() + '/' + self.mtl_sfx3 +'.mtl'
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename

   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.month1 = 3
       self.month2 = 9
       self.nmonth = 12
       self.convfactor = 1000

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       if 'PERIANT' in self.config :
          self.test = 1
          self.datalist1 = ['year','vant','aant','eant']
          self.datalist3 = ['yearobs2march','sedatamarch', 'sadatamarch']
          self.fig_size = [15.,6.]
          plt.rc('figure',figsize=self.fig_size)
       else :
          self.test = 2
          self.datalist1 = ['year','varc','vant','aarc','aant','earc','eant']
          self.datalist2 = ['yearobssept','nedatasept','nadatasept']
          self.datalist3 = ['yearobs2march','sedatamarch','sadatamarch'] 
          self.fig_size = [15.,15.]
          plt.rc('figure',figsize=self.fig_size)


   def read(self):
       """Read and process the entry mtl file.
       """
       month1 = self.month1
       month2 = self.month2
       nmonth = self.nmonth
       convfactor = self.convfactor
       #- read the file
       file = open(self.mtlfile,'r')
       lignes = [lignes for lignes in file.readlines() if lignes.strip() ] # remove empty lines
       file.close()

       if 'ORCA' in self.config :
          file2 = open(self.mtlfile2,'r')
          lignes2 = [lignes2 for lignes2 in file2.readlines() if lignes2.strip() ] # remove empty lines
          file2.close()

       file3 = open(self.mtlfile3,'r')
       lignes3=[lignes3 for lignes3 in file3.readlines() if lignes3.strip() ] # remove empty lines
       file3.close()
       #- initialize arrays
       for dat in self.datalist1:
           exec(dat + '= []')
       if 'ORCA' in self.config :
          for dat in self.datalist2:
             exec(dat + '= []')
       for dat in self.datalist3:
           exec(dat + '= []')
       #- data manipulation
       for chaine in lignes[3:] :
        element=chaine.split()
        if 'ORCA' in self.config :
          for k in range(1,1+nmonth) :
                vars()[self.datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
                vars()[self.datalist1[1]].append(float(element[k])/convfactor)
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[self.datalist1[2]].append(float(element[k])/convfactor)
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[self.datalist1[3]].append(float(element[k])/convfactor)
          for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
                vars()[self.datalist1[4]].append(float(element[k])/convfactor)
          for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
                vars()[self.datalist1[5]].append(float(element[k])/convfactor)
          for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
                vars()[self.datalist1[6]].append(float(element[k])/convfactor)

        elif 'PERIANT' in self.config :
          for k in range(1,1+nmonth) :
                vars()[self.datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
                vars()[self.datalist1[1]].append(float(element[k])/convfactor)
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[self.datalist1[2]].append(float(element[k])/convfactor)
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[self.datalist1[3]].append(float(element[k])/convfactor)

        if 'ORCA' in self.config :
           for chaine in lignes2[0:] :
              element=chaine.split()
              if float(element[1]) == month2 :
                 vars()[self.datalist2[0]].append(float(element[0]))
                 vars()[self.datalist2[1]].append(float(element[2]))
                 vars()[self.datalist2[2]].append(float(element[3]))

       for chaine in lignes3[0:] :
          element=chaine.split()
          if float(element[1]) == month1 :
             vars()[self.datalist3[0]].append(float(element[0]))
             vars()[self.datalist3[1]].append(float(element[2]))
             vars()[self.datalist3[2]].append(float(element[3]))

       # Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
       if 'ORCA' in self.config :
          indN=vars()[self.datalist2[0]].index(1987)
          for k in range(indN+1) :
             vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+1.19
          for k in range(indN+1,len(datalist2[0])) :
             vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+0.31
          for k in range(indN) :
            vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+1.19
          for k in range(indN,len(datalist2[0])) :
             vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+0.31

       self.year = year
       if 'ORCA' in self.config :
          self.yearobssept = yearobssept
          self.varc = varc
          self.aarc = aarc
          self.earc = earc
          self.nedatasept= nedatasept
          self.nadatasept = nadatasept
       elif ('ORCA' in self.config) + ('PERIANT' in self.config) :
          self.yearobs2march = yearobs2march
          self.vant = vant
          self.aant = aant
          self.eant = eant
          self.sedatamarch = sedatamarch
          self.sadatamarch = sadatamarch


   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       year = self.year
       test = self.test
       if 'ORCA' in self.config :
            plt.subplot(2,3,1)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.varc[8::12], color+'.-')
            plt.axis([min(year[::12]),max(year[::12]),min(self.varc[8::12]), max(self.varc[8::12])])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Volume Arctic September',fontsize='large')
            #############
            plt.subplot(2,3,2)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.aarc[8::12], color+'.-',self.yearobssept, self.nadatasept, 'm--')
            mini = min(min(self.aarc[8::12]),min(self.nadatasept))
            maxi = max(max(self.aant[8::12]),max(self.nadatasept))
            plt.axis([min(year[::12]),max(year[::12]),mini,maxi])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Area Arctic September',fontsize='large')
            #############
            plt.subplot(2,3,3)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.earc[8::12], color+'.-',self.yearobssept, self.nedatasept, 'm--')
            mini = min(min(self.earc[8::12]),min(self.nedatasept))
            maxi = max(max(self.eant[8::12]),max(self.nedatasept))
            plt.axis([min(year[::12]),max(year[::12]),mini,maxi])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.ylabel('Extent Arctic September',fontsize='large')

       if ('ORCA' in self.config) + ('PERIANT' in self.config) :
            plt.subplot(test,3,test*test)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.vant[2::12], color+'.-')
            plt.axis([min(year[::12]),max(year[::12]),min(self.vant[2::12]), max(self.vant[2::12])])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Volume Antarctic March',fontsize='large')
            ############
            plt.subplot(test,3,test*test+1)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.aant[2::12], color+'.-',self.yearobs2march, self.sadatamarch, 'm--')
            mini = min(min(self.aant[2::12]),min(self.sadatamarch))
            maxi = max(max(self.aant[2::12]),max(self.sadatamarch))
            plt.axis([min(year[::12]),max(year[::12]),mini, maxi])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Area Antarctic March',fontsize='large')
            ############
            plt.subplot(test,3,test*3)
            ylim_before = get_ylims()                  # dealing with ylims
            plt.plot(year[::12], self.eant[2::12], color+'.-', self.yearobs2march, self.sedatamarch, 'm--')
            mini = min(min(self.eant[2::12]),min(self.sedatamarch))
            maxi = max(max(self.eant[2::12]),max(self.sedatamarch))
            plt.axis([min(year[::12]),max(year[::12]),mini, maxi])
            ylim_after = get_ylims()                   #
            plt.ylim(max_ylims(ylim_before,ylim_after))#
            plt.grid(True)
            plt.title('Extent Antarctic March',fontsize='large')
       self.figure = figure
  
   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case):
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename 


#- Monitoring ice area and extent compared to NOAA data
class mkplt_icenoaa_single:
   """Produce an ice extent and area versus NOAA data  monitoring plot for a particular (config, case).
   """
   plot_name = 'icenoaas'
   mtl_sfx = 'icemonth'
   mtl_sfx2 = 'North'
   mtl_sfx3 = 'South'
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the monitoring plot class.
       """
       self.config = config
       self.case = case
       self.mtlfile = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx +'.mtl'
       self.mtlfile2 = dataobsdir() + '/' + self.mtl_sfx2 +'.mtl'
       self.mtlfile3 = dataobsdir() + '/' + self.mtl_sfx3 +'.mtl'
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename

   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.nmonth = 12
       self.convfactor = 1000

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       if 'PERIANT' in self.config :
          self.datalist1 = ['year','vant','aant','eant']
          self.datalist3 = ['yearobs2','sedata', 'sadata']
          self.listplot1 = ['eant','aant']
          self.listplot2 = ['sedata','sadata']
          self.listylabel = ['Antarctic Extent', 'Antarctic Area']
          self.nbfig = max(len(self.listplot1),len(self.listplot2))
          self.fig_size = [15.,6.]
          plt.rc('figure',figsize=self.fig_size)
       else :
          self.datalist1 = ['year','varc','vant','aarc','aant','earc','eant']
          self.datalist2 = ['yearobs','nedata','nadata']
          self.datalist3 = ['yearobs2','sedata', 'sadata']
          self.listplot1 = ['earc','aarc','eant','aant']
          self.listplot2 = ['nedata','nadata','sedata','sadata']
          self.listylabel = ['Arctic Extent', 'Arctic Area', 'Antarctic Extent', 'Antarctic Area']
          self.nbfig = max(len(self.listplot1),len(self.listplot2))
          self.fig_size = [15.,15.]
          plt.rc('figure',figsize=self.fig_size)

   def read(self):
       """Read and process the entry mtl file.
       """
       nmonth = self.nmonth
       convfactor = self.convfactor
       #- read the file
       file = open(self.mtlfile,'r')
       lignes = [lignes for lignes in file.readlines() if lignes.strip() ] # remove empty lines
       file.close()

       if 'ORCA' in self.config :
          file2 = open(self.mtlfile2,'r')
          lignes2 = [lignes2 for lignes2 in file2.readlines() if lignes2.strip() ] # remove empty lines
          file2.close()

       file3 = open(self.mtlfile3,'r')
       lignes3=[lignes3 for lignes3 in file3.readlines() if lignes3.strip() ] # remove empty lines
       file3.close()
       #- initialize arrays
       for dat in self.datalist1:
           exec(dat + '= []')
       if 'ORCA' in self.config :
          for dat in self.datalist2:
             exec(dat + '= []')
       for dat in self.datalist3:
           exec(dat + '= []')
       #- data manipulation
       for chaine in lignes[3:] :
          element=chaine.split()
          if 'ORCA' in self.config :
             for k in range(1,1+nmonth) :
                vars()[self.datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
                vars()[self.datalist1[1]].append(float(element[k]))
             for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[self.datalist1[2]].append(float(element[k]))
             for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[self.datalist1[3]].append(float(element[k])/convfactor)
             for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
                vars()[self.datalist1[4]].append(float(element[k])/convfactor)
             for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
                vars()[self.datalist1[5]].append(float(element[k])/convfactor)
             for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
                vars()[self.datalist1[6]].append(float(element[k])/convfactor)

          elif 'PERIANT' in self.config :
             for k in range(1,1+nmonth) :
                vars()[self.datalist1[0]].append(float(element[0]) + ((float(k)-1)/nmonth) )
                vars()[self.datalist1[1]].append(float(element[k]))
             for k in range( 1+nmonth,1+(2*nmonth) ) :
                vars()[self.datalist1[2]].append(float(element[k])/convfactor)
             for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                vars()[self.datalist1[3]].append(float(element[k])/convfactor)

       if 'ORCA' in self.config :
          for chaine in lignes2[0:] :
             element=chaine.split()
             vars()[self.datalist2[0]].append(float(element[0]) + ((float(element[1])-1)/nmonth) )
             vars()[self.datalist2[1]].append(float(element[2]))
             vars()[self.datalist2[2]].append(float(element[3]))

       for chaine in lignes3[0:] :
          element=chaine.split()
          vars()[self.datalist3[0]].append(float(element[0]) + ((float(element[1])-1)/nmonth ) )
          vars()[self.datalist3[1]].append(float(element[2]))
          vars()[self.datalist3[2]].append(float(element[3]))

       # Correction for pole area not seen by sensor: 1.19  before june 1987, 0.31 after ( Change of satellite SSM/R SSM/I )
       if 'ORCA' in self.config :
          indN=vars()[self.datalist2[0]].index(1987.5)
          for k in range(indN) :
             vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+1.19
          for k in range(indN,len(datalist2[0])) :
             vars()[self.datalist2[2]][k]=vars()[self.datalist2[2]][k]+0.31
       self.year = year
       if 'ORCA' in self.config :
          self.yearobs = yearobs
          self.varc = varc
          self.aarc = aarc
          self.earc = earc
          self.nedata = nedata
          self.nadata = nadata
       elif ('ORCA' in self.config) + ('PERIANT' in self.config) :
          self.yearobs2 = yearobs2      
          self.vant = vant
          self.aant = aant
          self.eant = eant
          self.sedata = sedata
          self.sadata = sadata
    
   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       nbfig = self.nbfig
       listylabel = self.listylabel
       year = self.year
       if 'ORCA' in self.config :
          varc = self.varc
          vant = self.vant
          aarc = self.aarc
          aant = self.aant
          earc = self.earc
          eant = self.eant
          yearobs = self.yearobs
          yearobs2 = self.yearobs2
          nedata = self.nedata
          sedata = self.sedata
          nadata = self.nadata
          sadata = self.sadata
          for k in range(nbfig) :
             plt.subplot(nbfig,1,k+1)
             ylim_before = get_ylims()                  # dealing with ylims
             plt.plot(year, vars()[self.listplot1[k]], color+'.-', self.yearobs, vars()[self.listplot2[k]], 'm')
             plt.axis([min(year), max(year), min(vars()[self.listplot1[k]]), max(vars()[self.listplot1[k]])])
             ylim_after = get_ylims()                   #
             plt.ylim(max_ylims(ylim_before,ylim_after))#
             plt.grid(True)
             plt.ylabel(listylabel[k],fontsize='large')
       elif 'PERIANT' in self.config :
          vant = self.vant
          aant = self.aant
          eant = self.eant
          yearobs2 = self.yearobs2
          sedata = self.sedata
          sadata = self.sadata
          for k in range(nbfig) :
             plt.subplot(nbfig,1,k+1)
             ylim_before = get_ylims()                  # dealing with ylims 
             plt.plot(year, vars()[self.listplot1[k]], color+'.-', self.yearobs2, vars()[self.listplot2[k]], 'm')
             plt.axis([min(year),max(year), min(vars()[self.listplot1[k]]), max(vars()[self.listplot1[k]])]) 
             ylim_after = get_ylims()                   #
             plt.ylim(max_ylims(ylim_before,ylim_after))#
             plt.grid(True)
             plt.ylabel(listylabel[k],fontsize='large') 
       self.figure = figure

   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case)
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename


#- Monitoring ice area and extent by month
class mkplt_icemonth_single:
   """Produce an icemonth monitoring plot for a particular (config, case).
   """
   plot_name = 'icemonths'
   mtl_sfx = 'icemonth'
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the icemonth monitoring plot class.
       """
       self.config = config
       self.case = case
       self.mtlfile = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx +'.mtl'
       self._set_general_parameters()
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename

   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.nmonth = 12

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       if 'PERIANT' in self.config:
          self.test = 1
          self.datalist = ['year','vant','aant','eant']
          self.fig_size =  [15.,6.]
          plt.rc('figure',figsize=self.fig_size)
       else:
          self.test = 2
          self.datalist = ['year','varc','vant','aarc','aant','earc','eant']
          self.fig_size =  [15.,15.]
          plt.rc('figure',figsize=self.fig_size)

   def read(self):
       """Read and process the entry mtl file.
       """
       nmonth = self.nmonth
       #- read the file
       file=open(self.mtlfile,'r')
       lignes=[lignes for lignes in file.readlines() if lignes.strip() ] # remove empty lines
       file.close()
       #- initialize arrays
       for dat in self.datalist:
           exec(dat + '= []')
       #- data manipulation
       for chaine in lignes[3:] :
        element=chaine.split()
        if 'ORCA' in self.config :
          for k in range(1,1+nmonth) :
                year.append(float(element[0]) + ((float(k)-1)/nmonth) )
                varc.append(float(element[k]))
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                vant.append(float(element[k]))
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                aarc.append(float(element[k]))
          for k in range( 1+(3*nmonth),1+(4*nmonth) ) :
                aant.append(float(element[k]))
          for k in range( 1+(4*nmonth),1+(5*nmonth) ) :
                earc.append(float(element[k]))
          for k in range( 1+(5*nmonth),1+(6*nmonth) ) :
                eant.append(float(element[k]))
          self.varc = varc
          self.vant = vant
          self.aarc = aarc
          self.aant = aant
          self.earc = earc
          self.eant = eant

        elif 'PERIANT' in self.config :
          for k in range(1,1+nmonth) :
                year.append(float(element[0]) + ((float(k)-1)/nmonth) )
                vant.append(float(element[k]))
          for k in range( 1+nmonth,1+(2*nmonth) ) :
                aant.append(float(element[k]))
          for k in range( 1+(2*nmonth),1+(3*nmonth) ) :
                eant.append(float(element[k]))
          self.vant = vant
          self.aant = aant
          self.eant = eant
                  
        self.year = year
        

   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       if 'ORCA' in self.config :
          varc = self.varc
          vant = self.vant
          aarc = self.aarc
          aant = self.aant
          earc = self.earc
          eant = self.eant
       if 'PERIANT' in self.config :
          vant = self.vant
          aant = self.aant
          eant = self.eant
       test = self.test
       year = self.year
       if 'ORCA' in self.config :
          plt.subplot(2,3,1)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(year, varc, color+'-')
          plt.axis([min(year), max(year), min(varc), max(varc)])
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.ylabel('Northern',fontsize='large')
          plt.title('Ice Volume (10**9 m**3)',fontsize='large')

          plt.subplot(2,3,2)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(year, aarc, color+'-')
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.axis([min(year), max(year), min(aarc), max(aarc)])
          plt.title('Ice Area (10**9 m**2)',fontsize='large')

          plt.subplot(2,3,3)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(year, earc, color+'-')
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.axis([min(year), max(year), min(earc), max(earc)])
          plt.title('Ice extent (10**9 m**2)',fontsize='large')

       if ('ORCA' in self.config) + ('PERIANT' in self.config) :
          plt.subplot(test,3,test*test)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(year, vant, color+'-')
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.axis([min(year), max(year), min(vant), max(vant)])
          plt.ylabel('Southern',fontsize='large')
          if test == 1 :
             plt.title('Ice Volume (10**9 m**3)',fontsize='large')

          plt.subplot(test,3,test*test+1)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(year, aant, color+'-')
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.axis([min(year), max(year), min(aant), max(aant)])
          if test == 1 :
             plt.title('Ice Area (10**9 m**2)',fontsize='large')

          plt.subplot(test,3,test*3)
          ylim_before = get_ylims()                  # dealing with ylims
          plt.plot(year, eant, color+'-')
          ylim_after = get_ylims()                   #
          plt.ylim(max_ylims(ylim_before,ylim_after))#
          plt.grid(True)
          plt.axis([min(year), max(year), min(eant), max(eant)])
          if test == 1 :
             plt.title('Ice extent (10**9 m**2)',fontsize='large')
       self.figure = figure


   def save(self,filename=None):
       """Save the file.
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case)
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename


#- Monitoring mass, heat and salt transport
class mkplt_transport_single:
   """Produce a transport monitoring plot for a particular (config, case).
   """
   plot_name = 'transports'
   mtl_sfx = 'matrix'
   def __init__(self,config=None,case=None,figure=None):
       """Initialize the transport monitoring plot class. 
       """
       self.config = config
       self.case = case
       self.mtlfile = datadir(config,case) + '/' + config + '-' + case + '_'+ self.mtl_sfx +'.mtl'
       self._set_general_parameters()  
       self._set_config_dependant_parameters()
       self.read()

   def proceed(self):
       self.plot()
       filename = self.save()
       plt.close(self.figure)
       return filename
 
   def _set_general_parameters(self):
       """Set general parameters.
       """
       self.datalist = ['year','name','mass','heat','salt','sshmean','tmean','smean']
       self.fig_size =  [15.,20.]
       plt.rc('figure',figsize=self.fig_size)

   def _set_config_dependant_parameters(self):
       """Set config dependant parameters.
       """
       if 'NATL' in self.config:
          self.nsection = 6
          self.sens = npy.array([1, 1, 1, 1, 1, -1 ])
       elif 'PERIANT' in self.config:
          self.nsection = 8
          self.sens = npy.array([1, 1, 1, 1, 1, -1, -1, -1])
       else:
          self.nsection = 11
          self.sens = npy.array([-1., 1., 1., 1., 1., 1., -1., 1., 1., -1. , 1.])

   def read(self):
       """Read and process the entry mtl file.
       """
       nsection = self.nsection
       #- read the file
       file = open(self.mtlfile,'r')
       lignes = [lignes for lignes in file.readlines() if lignes.strip() ] # remove empty lines
       file.close()
       #- initialize arrays
       for dat in self.datalist:
           exec(dat + '= []')    
       #- data manipulation
       for chaine in lignes[1:2] :
           element=chaine.split()
           for k in range(3,3+nsection) :
               tempname = element[k]
               tempname = tempname.replace('_','-')
               name.append(tempname[3:])
       for chaine in lignes[3:] :
           element = chaine.split()
           year.append(float(element[0]))
           for k in range(1,1+nsection) :
               mass.append(float(element[k]))
           for k in range( 1+nsection,1+(2*nsection) ) :
               heat.append(float(element[k]))
           for k in range( 1+(2*nsection),1+(3*nsection) ) :
               salt.append(float(element[k]))
               sshmean.append(float(element[1+(3*nsection)+0]))
               tmean.append(float(element[1+(3*nsection)+1]))
               smean.append(float(element[1+(3*nsection)+2]))
       mass = npy.reshape(mass, (-1,nsection))
       heat = npy.reshape(heat, (-1,nsection))
       salt = npy.reshape(salt, (-1,nsection))
       bigsens=npy.ones((npy.size(year),1,)) * self.sens
       self.year = year
       self.name = name 
       self.massplt = mass * bigsens
       self.heatplt = heat * bigsens
       self.saltplt = salt * bigsens

   def plot(self,figure=None,color='r'):
       """Produce the plot on figure.
       """
       plt.rc('text', usetex=False)
       nsection = self.nsection
       year = self.year
       name = self.name 
       massplt = self.massplt
       heatplt = self.heatplt 
       saltplt = self.saltplt
       if figure is None:
          figure = plt.figure()
       for k in range(1,nsection+1) :
           plt.subplot(nsection,3,3*(k-1)+1)
           ylim_before = get_ylims()                  # dealing with ylims
           plt.plot(year, massplt[:,k-1], color+'.-')
           plt.axis([min(year),max(year),min(massplt[:,k-1]),max(massplt[:,k-1])])
           ylim_after = get_ylims()                   #
           plt.ylim(max_ylims(ylim_before,ylim_after))#
           plt.grid(True)
           plt.ylabel(name[k-1],fontsize='x-large')
           if k==1 :
              plt.title('Mass Transport',fontsize='large')
           plt.subplot(nsection,3,3*(k-1)+2)
           ylim_before = get_ylims()                  # dealing with ylims
           plt.plot(year, heatplt[:,k-1], color+'.-')
           plt.axis([min(year),max(year),min(heatplt[:,k-1]),max(heatplt[:,k-1])])
           ylim_after = get_ylims()                   #
           plt.ylim(max_ylims(ylim_before,ylim_after))#
           plt.grid(True)
           if k==1 :
              plt.title('Heat Transport',fontsize='large')
           plt.subplot(nsection,3,3*(k-1)+3)
           ylim_before = get_ylims()                  # dealing with ylims
           plt.plot(year, saltplt[:,k-1], color+'.-')
           plt.axis([min(year),max(year),min(saltplt[:,k-1]),max(saltplt[:,k-1])])
           ylim_after = get_ylims()                   #
           plt.ylim(max_ylims(ylim_before,ylim_after))#
           plt.grid(True)
           if k==1 :
              plt.title('Salt Transport',fontsize='large')
       self.figure = figure
       
   def save(self,filename=None):
       """Save the file. 
       """
       if filename is None:
          filename = './'#plotdir(config=self.config,case=self.case) 
          filename+= self.plot_name + '.' + self.config + '-' + self.case + '.png'
       self.figure.savefig(filename)
       return filename       

#-------------------------- Available plots ---------------------------------------------
#

available_plotting_single = {\
 'transports': mkplt_transport_single, \
 'icemonths': mkplt_icemonth_single, \
 'icenoaas': mkplt_icenoaa_single, \
 'icetrds': mkplt_icetrd_single, \
 'icetrds_mini': mkplt_icetrd_mini_single, \
 'tsmean': mkplt_tsmean_single, \
 'eke':mkplt_eke_single\
} 
     
available_plotting_single_for_multi = {\
 'transports': mkplt_transport_single, \
 'icemonths': mkplt_icemonth_single, \
 'icenoaas': mkplt_icenoaa_single, \
 'icetrds': mkplt_icetrd_single, \
 'icetrds_mini': mkplt_icetrd_mini_single, \
 'tsmeans': mkplt_tsmeans_multi, \
 'eke': mkplt_eke_single\
}

 
#------------------------------ General multiplot class --------------------------------
#       

class mkplt_multi:
      """Produce a monitoring plot for multiple (config, case) with mkplt_single.
      """
      def __init__(self,configs=None,cases=None,mkplt_single=None):
          """Initialize.
          """
          self.mkplt_single = mkplt_single
          self.configs = configs
          self.cases = cases
          self.len = configs.__len__()
          self.configs_cases = [configs[k] + '-' + cases[k] for k in range(self.len) ]
          self._get_single_mkplt_classes()

      def _get_single_mkplt_classes(self):
          """Get all the plotting classes.
          """
          self.single_mkplt_classes = []
          for config,case in zip(self.configs,self.cases):
              self.single_mkplt_classes.append(self.mkplt_single(config=config,case=case))

      def plot(self):
          """Overlay all the plots.
          """
          self.figure = plt.figure()
          self.figure.text(0.5,0.01,self.title(),horizontalalignment='center')
          for icl,config,case in zip(range(self.len),self.configs,self.cases):
              self.single_mkplt_classes[icl].plot(figure=self.figure,color=listcolors[icl])

      def title(self):
          """Return the plot legend
          """
          basename = ''
          for il,config_case in enumerate(self.configs_cases):
              basename+= config_case + ' (' + listcolors[il] + '); '
          basename = basename[:-2]
          return basename

      def _plotname(self):
          """Return the plot name
          """
          basename = self.mkplt_single.plot_name +'.'
          for config_case in self.configs_cases:
              basename+= config_case + '_'
          basename = basename[:-1] + '.png'
          return basename

      def save(self,filename=None):
          """Save the plot.
          """
          if filename is None:
              filename = './' + self._plotname()
          self.figure.savefig(filename)
          return filename

      def proceed(self):
          """Produce and the plot.
          """
          self.plot()
          filename = self.save()
          plt.close(self.figure)
          return filename


#--------------------- testing -----------------------------
def test_plotting_class(plotting_class_single):
    mkplt = mkplt_multi(configs=['PERIANT05','PERIANT05'],\
            cases=['GCdmpH1','GCdmpH3'],mkplt_single=plotting_class_single)
    filename = mkplt.proceed()
    return filename
    

#--------------------- main --------------------------
def optlist2optdic(optlist):
    optdic = {}
    for tuple in optlist:
        optdic[tuple[0][2:]] = tuple[1]
    if optdic.has_key('plot_names'):
       optdic['plot_names'] = optdic['plot_names'].split(',')
    for key in ['remote_send','verbose','local_send']:
        if optdic.has_key(key):
           optdic[key] = eval(optdic[key])
    return optdic

def print_help_message(argv0):
    mymsg = """[--plot_names] [plot_1,...,plot_n]
                      [--root_datadir] [datadir]
                      [--local_send] True/False
                      [--root_plotdir] [root_plotdir]
                      [--remote_send] True/False
                      [--remote_machine] [remote_machine]
                      [--remote_username] [remote_username]
                      [--remote_basedir] [remote_basedir]
                      [--verbose] True/False
                      [config-case_1] ... [config-case_n]
             """
    print "usage:", argv0, mymsg

    

def main():
    # read sys.argv
    long_optlist = ['help','plot_names=','root_datadir=','local_send=','root_plotdir=',\
                    'remote_send=','remote_machine=','remote_username=','remote_basedir=',\
                    'verbose=']
    try:
        optlist, args = getopt.getopt(sys.argv[1:],'',long_optlist)
    except getopt.error, msg:
        print msg
        print_help_message(sys.argv[0])
        return 2
    if optlist[0][0][2:]=='help':
       print_help_message(sys.argv[0])
       return 2
    # store configs/cases
    configs=[]
    cases = []
    for arg in args: 
        configs.append(arg.split('-')[0])
        cases.append(arg.split('-')[-1])
    # update rc with sys.argv
    optdic = optlist2optdic(optlist)
    rc._update_from_dic(optdic) # actual update
    comment('\nResource configuration\n----------------------')
    comment(rc.__repr__())
    # produce timeseries plots
    comment('\nProceed\n-------')
    monitor = plot_timeseries(configs=configs,cases=cases)
    monitor.proceed()
    return 

if __name__ == '__main__':
    sys.exit(main() or 0)

