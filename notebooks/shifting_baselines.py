####  Shifting Baseline Script
# Generate the difference in climatology periods as netcdf


# Libraries
import os
import xarray as xr
import datetime
import regionmask
import numpy as np
import pandas as pd
import notebooks.oisstools as ot




#### 1. Set workspace
workspace = "local"

# Root paths
root_locations = {"local"  : "/Users/akemberling/Box/",
                  "docker" : "/home/jovyan/"}

# Set root with workspace
box_root = root_locations[workspace]
clim_root = f"{box_root}RES_Data/OISST/oisst_mainstays/daily_climatologies/"

# load data
old_clim = xr.open_dataset(f"{clim_root}daily_clims_1982to2011.nc")
new_clim = xr.open_dataset(f"{clim_root}daily_clims_1991to2020.nc")


# Get Difference in Baselines
clim_shift = old_clim - new_clim

# Save Climate Shift
clim_shift.attrs = {
  "title" : "Difference in 30-year climatologies. 1991-2020 and 1982-2011 climatologies used.",
  "institution" : "Gulf of Maine Research Institute",
  "source" : "source': 'NOAA/NCDC  ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/",
  'references': 'https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html', 
  'dataset_title': 'GMRI 30-Year Climatology Shifting Baseline - OISSTv2'
}

# Save it
clim_shift.to_netcdf(path = f"{clim_root}clim_shift_82to91baselines.nc")
