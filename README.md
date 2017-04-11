# DMONTOOLS
 DMONTOOLS is a software package that performs the monitoring of the DRAKKAR runs, achieved with NEMO ocean model. DMONTOOLS stands for DRAKKAR MONitoring TOOLS. It is developped in the frame of the DRAKKAR project (<https://www.drakkar-ocean.eu/>). It is now available on GitHub under the CeCILL license (<http://www.cecill.info/licences/Licence_CeCILL_V2-en.html>).

The monitoring of a run consists in the evaluation of global and compact indicators in order to assess eventual problems during the model integration. In order have a compact information, most of the diagnostics are performed on annual mean fields, unless the seasonality is an important factor (in case of sea-ice, for example). The package produces diagnostics netcdf files, and 3 types of plots: (i) time-series plots for integrated values (*eg* mean 3D temperature, transport across sections, ice volume and extent ...), (ii) maps of some fields or derived fields at specific deptht (*eg* EKE, salinity, temperature ...) and (iii) zonal or meridional vertical sections for specific fields (*eg* temperature, salinity, potential vorticity etc.).

Resulting plots are intended to be automatically copied on the DRAKKAR monitoring web-site.

## Using DMONTOOLS

 DMONTOOLS is a collection of bash/ksh and python scripts which make use of CDFTOOLS (for computing the diagnostics), CHARTOOLS (for plotting maps and sections) and matplotlib python modules (for plotting the time-series).

 There are 3 steps in the monitoring process:
 1. Compute the annual (and monthly) mean fields.
 1. Compute the diagnostics from annual/monthly means.
 1. Plot the results
   * Maps and sections
   * Time-series

 Each step is associated to particular scripts, provided in the DMONTOOLS package.

#### Cloning the git repository
To retrieve a copy of the DMONTOOLS on a working directory, execute the following command line:

```> git clone https://github.com/meom-group/DMONTOOLS ```

#### Installing the DMONTOOLS

#### Running the DMONTOOLS
