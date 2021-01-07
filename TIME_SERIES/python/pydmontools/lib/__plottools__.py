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
          'font.size': 10,
          'image.cmap': 'jet',
          'legend.fontsize': 10,
          'xtick.labelsize': 8,
          'ytick.labelsize': 8,
          }
plt.rcParams.update(params)

osp = os.sep

# adapted from http://matplotlib.sourceforge.net/mpl_examples/api/date_demo.py
# http://www.packtpub.com/article/advanced-matplotlib-part2
# mdates.AutoDateLocator : http://matplotlib.sourceforge.net/api/dates_api.html#matplotlib.dates.AutoDateLocator
# http://matplotlib.sourceforge.net/api/dates_api.html
# http://matplotlib.sourceforge.net/examples/pylab_examples/date_demo_rrule.html

#mLoc = mdates.AutoDateLocator
#mFmt = mdates.AutoDateFormatter


def set_dateticks(ax,aspect_ratio=1.):
    """Set a pretty xtick for ax subplot.
    aspect_ratio : aspect ratio of the plot (used for formatting dates...)
    nota bene : ax should have been created as follows : 
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(date,mydata)
    set_dateticks(ax)
    
    in the plotting script, add also this command 
    fig.autofmt_xdate()
    """
    # get the time interval
    xlims = np.array(ax.get_xlim())/aspect_ratio
    datelims = mdates.num2date(xlims)
    dt = (datelims[-1] - datelims[0]) 
    dtyr = datetime.timedelta(weeks=52) # delta = 1 yr 
    # set the ticks intervals
    if dt<=2*dtyr:
       yr_int = 1
       mth_int = 1
    elif dt<=5*dtyr:
       yr_int = 1
       mth_int = 3
    elif dt <= 10*dtyr:
       yr_int = 1
       mth_int= 6 * yr_int
    elif dt <= 20*dtyr:
       yr_int = 2
       mth_int= None
    elif dt <= 40*dtyr:
       yr_int = 5
       mth_int= None
    else:
       yr_int = 10
       mth_int= None 
    # set the ticks locators and formatters
    # the if loop is a fix because YearLocator can fail in case yrdeb is smaller than yr_int
    if datelims[0].year>yr_int: # change the Locator only if the condition is met
       majorLoc   = mdates.YearLocator(base=yr_int)
       majorFmt = mdates.DateFormatter('%Y')
    else:                       # reapply the same Locator and Formatter
       majorLoc =  ax.xaxis.get_major_locator()
       majorFmt =  ax.xaxis.get_major_formatter()
    # format the ticks
    ax.xaxis.set_major_locator(majorLoc)
    ax.xaxis.set_major_formatter(majorFmt)
    if mth_int is not None:
       minorLoc   = mdates.MonthLocator(interval=mth_int)
       ax.xaxis.set_minor_locator(minorLoc)

def get_vminmax(data1,data2,ex=0.1):
    """Return min and max values for comparison plots.
    """
    vmin = min(data1.min(),data2.min())
    vmax = max(data1.max(),data2.max())
    d = abs(vmax-vmin)
    return (vmin - ex * d, vmax + ex * d)

def get_tminmax(date):
    """Return tmin and tmax from a datetime object 
    """
    _date = mdates.date2num(date) # now a numerical value (in days)
    tmin = min(_date)
    tmax = max(_date) 
    return tmin,tmax

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

