#!/usr/bin/env python
#=======================================================================
#                        General Documentation

"""__plottools__.py : A set of utilities for producing  plots  
                      for the standard drakkar monitoring 
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

import os
import pydmontools as pydmt
import matplotlib.pylab as plt
import numpy as npy

import datetime
import numpy as np
import matplotlib
import matplotlib.pyplot as pyp
import matplotlib.dates as mdates
import matplotlib.mlab as mlab


params = {'axes.labelsize': 10,
          'text.fontsize': 10,
          'legend.fontsize': 10,
          'xtick.labelsize': 8,
          'ytick.labelsize': 8,
          }
plt.rcParams.update(params)

osp = os.sep

# adapted from http://matplotlib.sourceforge.net/mpl_examples/api/date_demo.py
years    = mdates.YearLocator()   # every year
months   = mdates.MonthLocator()  # every month
yearsFmt = mdates.DateFormatter('%Y')

def set_dateticks(ax):
    """Set a pretty xtick for ax subplot.
    nota bene : ax should have been created as follows : 
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(date,mydata)
    set_dateticks(ax)
    
    in the plotting script, add also this command 
    fig.autofmt_xdate()
    """
    # format the ticks
    ax.xaxis.set_major_locator(years)
    ax.xaxis.set_major_formatter(yearsFmt)
    ax.xaxis.set_minor_locator(months)


def plotdir_confcase_single(argdict): # could be used in individual plotting scripts...
    dirname = argdict['plotdir'] + osp + argdict['config'] + '-' + argdict['case'] 
    dirname+=  osp + 'TIME_SERIES' + osp
    return dirname

def std_save_single(argdict=None,figure=None,plotname=None):
    dirname = plotdir_confcase_single(argdict)
    fname = argdict['config'] + '-' + argdict['case'] + '_' + plotname + '.png'
    return dirname + fname

def sature_plot(tableau,vmin,vmax):
    tableau[npy.where(tableau>=vmax)] = vmax
    tableau[npy.where(tableau<=vmin)] = vmin
    return tableau

def sature_plot_masked(tableau,vmin,vmax):
    import numpy as npy
    masque = tableau.mask
    tableau=npy.array((tableau),'f')
    tableau[npy.where(tableau>=vmax)] = vmax
    tableau[npy.where(tableau<=vmin)] = vmin
    out = npy.ma.array(tableau, mask=masque)
    return out

