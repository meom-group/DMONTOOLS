#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""monitor_compare.py : timeseries comparison plots for drakkar monitoring
"""
#-----------------------------------------------------------------------
#                       Additional Documentation
#
# Modification History:
#  - June 2010: Original module by R. Dussin and J. Le Sommer
#
# Copyright (c) 2010, R. Dussin and J. Le Sommer. For licensing, distribution
# conditions, contact information, and additional documentation see the wiki :
# https://servforge.legi.grenoble-inp.fr/projects/DMONTOOLS/wiki/TIME_SERIES/python.
#=======================================================================

#-----------------------------------------------------------------------

#- Module General Import and Declarations
import os, user, sys
import pydmontools as pydmt
import matplotlib.pylab as plt


#------------------------- Parameters ---------------------------------
#  Setting general plot parameters
listcolors = ['r','k','b','g','c','m','y']


#------------------------ Configuration ----------------------------------
#- verbosity
def comment(mycomment):
    if rc.verbose:
       print mycomment
 
#- I/O directories
def datadir(config=None,case=None):
    """Return the data directory for (config, case)
    """
    return rc.datadir + config + _osp + config + '-' + case + '-' +  'MONITOR' + _osp

#def datameandir(config=None,case=None):
#    """Return the data MEAN directory for (config, case)
#    """
#    return rc.datadir + config + '/' + config + '-' + case + '-' +  'MEAN/'

#def dataobsdir():
#    """Return the data observation directory
#    """
#    return rc.dataobsdir

def plotdir_single(config=None,case=None):
    """Return the plot directory for (config, case)
    """
    return rc.plotdir + config + _osp + 'PLOTS' + _osp + config + '-' + case + _osp + 'TIME_SERIES' + _osp

def plotdir_multi(configs=None,cases=None):
    """Return the plot directory for (config, case)
    """
    directory = rc.plotdir  + 'COMPARE' + _osp
    for config,case in zip(configs,cases):
        directory+= config + '-' + case + '_'
    directory = directory[:-1] + _osp+'TIME_SERIES' + _osp + ' '
    return directory

def local_checkdir(locdir):
    """Check whether local directory locdir exists. create it if not.
    """
    locdir=os.path.abspath(locdir)
    locdir_bits = locdir.split(_osp)[1:]
    ldir = _osp
    for bit in locdir_bits:
       ldir+= bit + _osp	    
       if not(os.path.isdir(ldir)):
          os.mkdir(ldir)

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
_osp = os.sep
class _store_rc(pydmt._core_arguments):
    """This class stores machine- or user- dependent parameters.
    """
    def __init__(self,file= user.home + _osp + '.pydmt' + _osp + 'rc_monitor_compare.py'):
        pydmt._core_arguments.__init__(self) # set 'datadir','plotdir','dataobsdir'
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



#---------------------------  Core classes -----------------------------------------------

class plot_and_send_all_pages:
    def __init__(self,configs=None,cases=None,plotnames=None,remote_send=None,local_send=None):
        """
        Parameters 
        ----------
        configs : list of strings
          configurations to be monitored
        cases : list of strings
          cases to be monitored
        plotnames : list of strings
          required plots
        """
        self.configs = configs
        self.cases = cases
	self.len = configs.__len__()
	self.compare = (self.len>1)
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
        if plotnames is None:
            self.plotnames = rc.plotnames
        else:
           self.plotnames = plotnames

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
        if not(self.compare):
           self.remote_directory = check_remote_directory_single(config=self.configs[0],case=self.cases[0])
        else:
           self.remote_directory = check_remote_directory_multi(configs=self.configs,cases=self.cases)
           
    def check_local(self):
        """check the local plotting directory.
        """      
        if not(self.compare):
           self.local_directory = plotdir_single(config=self.configs[0],case=self.cases[0])
        else:
           self.local_directory = plotdir_multi(configs=self.configs,cases=self.cases)
        local_checkdir(self.local_directory)


    def loop(self):
        """loop on plot types.
        """
        for plotname in self.plotnames: 
            comment(' Preparing plot  '+ plotname)
            mkplt = plot_one_page(configs=self.configs,cases=self.cases,plotname=plotname) 
            comment(' processing...')
            try:
               filename = mkplt.proceed()
               comment(' ...done')
               if self.remote_send: 
                  self.send2remote(filename) 
               if self.local_send:
                  self.send2local(filename)
		  #os.remove(filename)
               comment(' \n')
            except:
               comment('...problem with plot ' + plotname)
	       if rc.debug:
		   traceback.print_exc(file=sys.stdout)


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



class plot_one_page:
      """Produce a monitoring plot for multiple (config, case) from a pydmt plotting script.
      """
      def __init__(self,configs=None,cases=None,plotname=None,base_argdict=None):
          """Initialize.
          """
          self.plotname = plotname
          exec('from pydmontools import ' + plotname + ' as plotmodule')
          self.plotmodule = plotmodule
	  try: self.fig_size = plotmodule.fig_size
	  except: self.fig_size = None
          self.configs = configs
          self.cases = cases
          self.len = configs.__len__()
          self.configs_cases = [configs[k] + '-' + cases[k] for k in range(self.len) ]
          self.argdict = {'dataobsdir':rc.dataobsdir} # will be appended later
          self.compare = (self.len>1) # test whether there will be pultiple layers in the plot

      def _update_argdict(self,config=None,case=None):
          self.argdict['config'] = config
          self.argdict['case'] = case
          self.argdict['datadir'] =  datadir(config=config,case=case)
          
      def plot_one_run(self,figure=None,config=None,case=None,color=None):
          """Add a plot layer corresponding to config-case to the specified figure.
          """
          self._update_argdict(config=config,case=case)
          values = self.plotmodule.read(argdict=self.argdict) # read the data 
          self.plotmodule.plot(argdict=self.argdict,figure=figure,compare=self.compare,color=color,**values) # plot the data
          
      def plot_all_runs(self):
          """Overlay all the plots.
          """
          self.figure = plt.figure(figsize=self.fig_size)
          self.figure.text(0.5,0.01,self.title(),horizontalalignment='center')
          for icl,config,case in zip(range(self.len),self.configs,self.cases):
              self.plot_one_run(figure=self.figure,config=config,case=case,color=listcolors[icl])

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
          basename = self.plotname +'.'
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
          self.plot_all_runs()
          filename = self.save()
          plt.close(self.figure)
          return filename



#--------------------- main --------------------------

def ParserOptions2rcdic(options):
    """Converts the parser options in a resource configuration (rc) dictionnary.
    """
    optdic = vars(options)
    for key in optdic.keys():
	if optdic[key] in [None,'']:
           del(optdic[key])
    for key in optdic.keys():
	if key in ['remote_send','verbose','local_send']:
	   optdic[key] = eval(optdic[key])
    if optdic.has_key('plotnames'):
           optdic['plotnames'] = optdic['plotnames'].split(',')
    return optdic


def myparser():
    myargs = pydmt.default_argdict
    from optparse import OptionParser
    usage = "usage: %prog [options] config-case_1 ... config-case_n"
    parser = OptionParser(usage=usage)
    parser.add_option("-g", "--debug", dest="debug",\
		    help="run in debug mode with pdb on: True/False", default=False)
    parser.add_option("-p", "--plotnames", dest="plotnames",\
		    help="plot1,...,plotn", default=None)
    parser.add_option("-d", "--datadir", dest="datadir",\
            help="root monitoring data directory", default=None)
    parser.add_option("-c", "--dataobsdir", dest="dataobsdir",\
            help="root monitoring observational data directory", default=myargs['dataobsdir'])
    parser.add_option("-l", "--local_send", dest="local_send",\
            help="send the plots to the local plotdir : False/True", default=None)    
    parser.add_option("-o", "--plotdir", dest="plotdir",\
            help="root monitoring plot directory", default= myargs['plotdir'])
    parser.add_option("-r", "--remote_send", dest="remote_send",\
            help="send the plots to the remote website : False/True", default=None)
    parser.add_option("-m", "--remote_machine", dest="remote_machine",\
            help="name of the remote machine to send the plots to", default=None)
    parser.add_option("-u", "--remote_username", dest="remote_username",\
            help="username on the remote machine", default=None)
    parser.add_option("-b", "--remote_basedir", dest="remote_basedir",\
            help="root monitoring plot directory on the remote machine", default=None)    
    parser.add_option("-v", "--verbose", dest="verbose",\
            help="print the comments : False/True ", default=None)
    return parser


def main():
    parser = myparser()
    (options, args) = parser.parse_args()
    if len(args)==0: # if no args are provided print the help message
        parser.print_help()
        sys.exit()
    # store the config-cases    
    configs=[]
    cases = []
    for arg in args: 
        configs.append(arg.split('-')[0])
        cases.append(arg.split('-')[-1])
    # update rc with sys.argv
    rc._update_from_dic(ParserOptions2rcdic(options))  
    comment('\nResource configuration\n----------------------')
    comment(rc.__repr__())
    # debugging mode 
    if rc.debug:
	import traceback
        #import pdb; pdb.set_trace()
    # produce timeseries plots
    comment('\nProceed\n-------')
    monitor = plot_and_send_all_pages(configs=configs,cases=cases)
    monitor.proceed()
    return 


if __name__ == '__main__':
    sys.exit(main() or 0)
