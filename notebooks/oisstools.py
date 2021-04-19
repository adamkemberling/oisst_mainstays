# OISST Mainstays Support Functions
# 2/28/2021

import requests
from bs4 import BeautifulSoup
import os
import xarray as xr
import datetime
import regionmask
import numpy as np
import pandas as pd





#-----------------------------------------------------
#
# Set Workspace Path to Box
#
#-----------------------------------------------------
def set_workspace(workspace: str) -> None:
  """
  Switch from local path to docker volume path with a workspace parameter
  
  Args:
    workspace (str): Indication of whether code should be run "local" or whether via "docker"
  
  """
  # Root paths
  root_locations = {"local"  : "/Users/akemberling/Box/",
                    "docker" : "/home/jovyan/"}
  
  # Set root with workspace
  box_root = root_locations[workspace]
  
  return box_root


#-----------------------------------------------------
#
# Set Cache Root to OISST Mainstays
#
#-----------------------------------------------------
def set_cache_root(box_root: str) -> None:
  """
  Set route to OISST Daily Cache Locations. Toggles based on the box_root returned by 
  ot.set_workspace()
  
  Args:
    box_root (str): text string indicating proper path to box
  
  """
  # Global cache root
  _cache_root = f"{box_root}RES_Data/OISST/oisst_mainstays/"
  return _cache_root




#-----------------------------------------------------
#
# Get Update Month Based on current Date
#
#-----------------------------------------------------
def get_update_month(return_this_month: bool) -> True:
  """
  Check date and return what the current or most recent month was for updating.
  
  Args:
    return_this_month (bool): True returns current month, false returns last month. 
    Last month returned as 12 if current month is January.
  """
  now = datetime.datetime.now()
  this_month = str(now.month).rjust(2, "0")
  last_month = str(now.month - 1 if now.month > 1 else 12).rjust(2, "0")
  if return_this_month == True:
    return this_month
  elif return_this_month == False:
    return last_month
  
#-----------------------------------------------------
#
# Get Update Year to Match Update Month
#
#-----------------------------------------------------
def check_update_yr(for_this_month: bool) -> True:
  """
  Check date and return correct update year for either the current month or
  for last month, correcting for year transitions.
  
  Args:
    for_this_month (bool): True returns current year, 
    False returns whatever year it was last month.
  
  """
  now = datetime.datetime.now()
  update_yr  = now.year
  last_month = str(now.month - 1 if now.month > 1 else 12)
  if for_this_month == True:
    return update_yr
  elif last_month != "12":
    return update_yr
  else:
    update_yr = update_yr - 1
    return update_yr
    
    

  
  
#----------------------------------------------------
#
# Check for repeated list elements
#
#----------------------------------------------------
def Repeat(x): 
  """
  Check for repeated list contents:
  Code contributed by Sandeep_anand, somewhere on stack overflow
  
  Args:
    x: list
  
  """
  _size = len(x) 
  repeated = [] 
  for i in range(_size): 
      k = i + 1
      for j in range(k, _size): 
          if x[i] == x[j] and x[i] not in repeated: 
              repeated.append(x[i]) 
  return repeated 



#-----------------------------------------------------
#
#  Cache Daily Files for Updating Month of OISST
#
#-----------------------------------------------------
def cache_oisst(cache_month, update_yr, workspace = "local", verbose = True):
    """
    Download OISSTv2 Daily Updates using Beautiful Soup

    Args:
        cache_month (str): Month to update
        update_yr (str): Year directory for month
        workspace (str): String indicating whether to build local paths or docker paths
        verbose : True or False to print progress
        

    """
  
    ####  Setup
    
    # Format month string
    this_month = str(cache_month).rjust(2, "0")
    
    # Root paths
    root_locations = {"local"  : "/Users/akemberling/Box/",
                      "docker" : "/home/jovyan/"}
  
    # Set root with workspace
    box_root = root_locations[workspace]
    
    # Global cache root
    _cache_root = f"{box_root}RES_Data/OISST/oisst_mainstays/"
    
    # Cache Subdirectory Locations
    cache_locs = {
      "annual_obs"        : f"{_cache_root}annual_observations/",
      "month_cache"       : f"{_cache_root}update_caches/{this_month}/"}
    
    
    # Set the output location for where things should save to:
    month_cache = cache_locs["month_cache"]   
    
    
    # This URL will grab the desired update month 
    # Thank you to Eric Bridger for initial code and for locating this link
    fetch_url = f"https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/{update_yr}{this_month}/"
  
  
    ####  Open the http directory listing
    req = requests.get(fetch_url)
  
    # Print error message if link does not work
    if req.status_code != requests.codes.ok:
        print(f"Request Error, Reason: {req.reason}")
    
    ####   Parse with BS4
    
    # Parse the url with BS and its html parser.
    soup = BeautifulSoup(req.text, 'html.parser')

    # Find all href anchors in the html text
    anchors = soup.find_all("a")
    
    
    ####  Downloading The Current Month
  
    # list to store download paths
    new_downloads = []
  
    # Find all the links in fetch_url which end with ".nc"
    for link in anchors:
        
        # Find links that match update year
        if link.get('href').endswith(f'.nc'):
            
            # Get the link(s) that match
            href = link.get('href')
            
            # Use requests to build download paths
            req_link = fetch_url + href
            req = requests.get(fetch_url + href)
            if req.raise_for_status():
                exit()
            
            # Open link
            dl_path = f"{month_cache}{href}"
            file = open(dl_path, 'wb')
            chunk_size = 17000000
            
            # Add to log
            new_downloads.append(dl_path)
            
            # Process in chunks to save daily files
            for chunk in req.iter_content(chunk_size):
                file.write(chunk)
            file.close()
            
            # Download Progress Text
            if verbose == True:
                print(f"Caching Daily NETCDF File: {href}")
    
    
    ####  Review Cache Files
    
    # list for files that have preliminary data suffix
    prelim_dates = []
    
    # Check cache for preliminary data
    for cache_file in os.listdir(month_cache):
        if cache_file.endswith("_preliminary.nc"): 
          
            # locate date text by its position relative to update yr/month in file
            start_idx = cache_file.find(f"{update_yr}{this_month}")
            end_idx = start_idx + 8
            step = int(1)
            date_id = cache_file[start_idx: end_idx: step]
            prelim_dates.append(date_id)
            
            # Report what files are preliminary data
            if verbose == True:
                print(f"Current month preliminary data found for: {date_id}")
            
    
    
    # # Code from stackoverflow to check for repeated values:
    # # Program to check for repeated list contents
    # # This code was contributed by Sandeep_anand , origins unknown
    # def Repeat(x): 
    #     _size = len(x) 
    #     repeated = [] 
    #     for i in range(_size): 
    #         k = i + 1
    #         for j in range(k, _size): 
    #             if x[i] == x[j] and x[i] not in repeated: 
    #                 repeated.append(x[i]) 
    #     return repeated 
    
    
    ####  Check among the cache for files that were preliminary but have been finalized
    
    # list of finalized dates, dates where we can ignore the preliminary data
    finalized_dates = []
    
    # check all download links for this month
    for link in os.listdir(month_cache):
        
        # check each preliminary data date
        for prelim_date in prelim_dates:
            
            # If the date for the link matches dates with preliminary data for a link, flag the date
            if prelim_date in link:
                start_idx = link.find(f"{update_yr}{this_month}")
                end_idx = start_idx + 8
                step = int(1)
                date_id = link[start_idx: end_idx: step]
                
                # and add those to the list of dates where there is prelim and final data
                finalized_dates.append(date_id)
                
        
    # Report the dates with both preliminary and finalized data  
    if verbose == True:
        print("Prelim and Finalized Data Found for:")
        print (Repeat(finalized_dates))    
      
      
    ####  Remove Preliminary data that is no longer needed
    # Pull out the dates for situations where there is both a preliminary and final file.
    remove_prelim = Repeat(finalized_dates)
    
    
    # Build out the preliminary names that these would be, drop them from cache.
    for repeated_date in remove_prelim:
        
        # Build full file name
        file_name = f"{month_cache}oisst-avhrr-v02r01.{repeated_date}_preliminary.nc"
        
        # Print the ones we removed    
        if verbose == True:
            print(f"File Removed for Finalized Data: {file_name}")
        
        # Remove them from the folder. Don't need them anymore.
        if os.path.exists(file_name):
            os.remove(file_name)
            
    
    
    # End Function
    print(f"OISSTv2 Cache for {update_yr}/{this_month} Updated Succesfully.")
          
          
          
          
#-----------------------------------------------------
#
#  Build Annual File from Month Caches
#
#-----------------------------------------------------
def build_annual_from_cache(last_month, this_month, workspace = "local", verbose = True):
    """
    Assemble OISSTv2 Annual File Using Monthly Caches:
      
    Should be run after the current and last month have had their caches updated.

    Args:
        last_month (str): Previous Month's data to assemble from cache
        this_month (str): Previous Month's data to assemble from cache
        workspace (str): String indicating whether to build local paths or docker paths
        verbose : True or False to print progress
        

    """
          
    ####  Cache Locations  ####
    
    # Format month string
    last_month = str(last_month).rjust(2, "0")
    this_month = str(this_month).rjust(2, "0")
    
    # Root paths
    root_locations = {"local"  : "/Users/akemberling/Box/",
                      "docker" : "/home/jovyan/"}
  
    # Set root with workspace
    box_root = root_locations[workspace]
    
    # Global cache root
    _cache_root = f"{box_root}RES_Data/OISST/oisst_mainstays/"
    
    # Cache Subdirectory Locations
    cache_locs = {
      "annual_files"     : f"{_cache_root}annual_observations/",
      "this_month"       : f"{_cache_root}update_caches/{this_month}/",
      "last_month"       : f"{_cache_root}update_caches/{last_month}/"}
    
    # Individual months
    annual_loc       = cache_locs["annual_files"]
    last_month_cache = cache_locs["last_month"]
    this_month_cache = cache_locs["this_month"]
    
    
    
    #####  Assemble list of file names for the two months just updates  ####
    
    # List of update files
    daily_files = []
    
    # Option 1 :  Using any month prior to build dataset from caches only
    month_folders = ["%.2d" % i for i in range(1, int(this_month) + 1)]
    for folder in month_folders:
      for file in os.listdir(f"{_cache_root}update_caches/{folder}"):
        if file.endswith(".nc"):
          daily_files.append(f"{_cache_root}update_caches/{folder}/{file}")
    
    # Use open_mfdataset to access all the new downloads as one file
    oisst_update = xr.open_mfdataset(daily_files, combine = "by_coords")     
            
    
    
    ####  Clean up Dataset structure  ####
    
    # Get all dates where the time indexes are not (~) duplicated
    oisst_noreps = oisst_update.sel(time = ~oisst_update.get_index("time").duplicated())
    
    # Select just sst and drop Zlev coordinate from the Array
    norep_sst = oisst_noreps["sst"][:, 0, :, :].drop("zlev")
    
    # Change to xr.Dataset
    update_prepped = xr.Dataset({"sst" : norep_sst})
    
    # remove attributes, going to add back later
    update_prepped.attrs = {}
    
    # End Testing, use if not loading annual file
    oisst_combined = update_prepped
      
    #### Last check, again for duplicates to remove
    oisst_combined = oisst_combined.sel(time = ~oisst_combined.get_index("time").duplicated())
    
    
    ####  Add Attributes Back  ####
    # Hard code the standard attributes, could also take from previous file, 
    # but this will ensure all ours are consistent
    oisst_attributes = {
      'Conventions'  : 'CF-1.5',
      'title'        : 'NOAA/NCEI 1/4 Degree Daily Optimum Interpolation Sea Surface Temperature (OISST) Analysis, Version 2.1',
      'institution'  : 'NOAA/National Centers for Environmental Information',
      'source'       : 'NOAA/NCEI https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/',
      'References'   : 'https://www.psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html',
      'dataset_title': 'NOAA Daily Optimum Interpolation Sea Surface Temperature',
      'version'      : 'Version 2.1',
      'comment'      : 'Reynolds, et al.(2007) Daily High-Resolution-Blended Analyses for Sea Surface Temperature (available at https://doi.org/10.1175/2007JCLI1824.1). Banzon, et al.(2016) A long-term record of blended satellite and in situ sea-surface temperature for climate monitoring, modeling and environmental studies (available at https://doi.org/10.5194/essd-8-165-2016). Huang et al. (2020) Improvements of the Daily Optimum Interpolation Sea Surface Temperature (DOISST) Version v02r01, submitted.Climatology is based on 1971-2000 OI.v2 SST. Satellite data: Pathfinder AVHRR SST and Navy AVHRR SST. Ice data: NCEP Ice and GSFC Ice. Data less than 15 days old may be subject to revision.'}
    
    # Add the attributes to the combined dataset
    oisst_combined.attrs = oisst_attributes
    
    # Load the full year into memory
    oisst_combined = oisst_combined.load()
    
    # Return the combined data
    return oisst_combined
  
  
#-----------------------------------------------------
#
# Save Updated OISST Annual File
#
#-----------------------------------------------------
def export_annual_update(cache_root, update_yr, oisst_update):
  """
  Save OISST Annual File to Box using cache root path
  
  Args:
    cache_root (str): Path to Box/RES_Data/OISST/oisst_mainstays
  """
  # Build out destination folder:
  out_folder       = f"{cache_root}annual_observations/"
  naming_structure = f"sst.day.mean.{update_yr}.v2.nc"
  out_path         = f"{out_folder}{naming_structure}"
  
  
  # Save File to Output Path
  oisst_update.to_netcdf(path = out_path)
  print(f"File Saved to {out_path}")
  
  
  
########################################################
#########  Begin Anomaly Processing Section  ###########
########################################################
  
  
  
  
#-----------------------------------------------------
#
# Load OISST from Box
#
#-----------------------------------------------------
def load_box_oisst(box_root, start_yr, end_yr, anomalies = False, do_parallel = False):
  """
  Load OISST Resources from box using xr.open_mfdataset()
  
  Shorthand to reduce copying this code everywhere.
  """
  
  
  # Set location to OISST data & base file name
  if anomalies == False:
    oisst_location  = f"{box_root}RES_Data/OISST/oisst_mainstays/annual_observations/"
    base_fname      = "sst.day.mean."
    file_ending     = ".v2.nc"
  
  elif anomalies == True:
    oisst_location = f"{box_root}RES_Data/OISST/oisst_mainstays/annual_anomalies/1982to2011_climatology/"
    base_fname     = "daily_anoms_"
    file_ending    = ".nc"
  
  
  # Set start and end years for the update
  start_yr = int(start_yr)
  end_yr   = int(end_yr)
  
  # Load the annual files for oisst
  fpaths = []
  for yr in range(start_yr, end_yr + 1):
      fpaths.append(f'{oisst_location}{base_fname}{yr}{file_ending}')
      
  # Lazy-load using xr.open_mfdataset
  grid_obj = xr.open_mfdataset(fpaths, combine = "by_coords", parallel = do_parallel)
  
  return grid_obj




#-----------------------------------------------------
#
# Load OISST Climatologies
#
#-----------------------------------------------------
def load_oisst_climatology(box_root, reference_period = "1982-2011"):
  """
  Load climatology NetCDF from box 
  
  Args:
    box_root (str): Base location to box from either local path or docker volume
    reference_period (str): start and end year of climatology linked by "-", e.g. "1982-2011"
  
  """
  
  # Set climatology source choices
  climatologies = {"1982-2011" : "daily_clims_1982to2011.nc",
                   "1985-2014" : "daily_clims_1985to2014.nc",
                   "1991-2020" : "daily_clims_1991to2020.nc"}

  # Build file name
  clim_root = f"{box_root}RES_Data/OISST/oisst_mainstays/daily_climatologies/"
  climate_period = climatologies[reference_period]
  clim_file = f"{clim_root}{climate_period}"
  
  # Open and return climatology
  oisst_clim = xr.open_dataset(clim_file)
  return oisst_clim

  


#-----------------------------------------------------
#
#  Add Modified Ordinal Day
#
#-----------------------------------------------------
def add_mod(grid_obj, time_dim, out_coord = "modified_ordinal_day"):
    """
    Add modified ordinal day to xarray dataset as new coordinate dimension

    Args:
        grid_obj : xarray object to add the new coordinate dimension
        time_dim (str) : Time coordinate dimension to pull day of year from
        out_coord (str) : 

    """
    # Flag days that are not in leap years
    not_leap_year        = ~grid_obj.indexes[time_dim].is_leap_year
    
    # Flag days in the year that are march or later
    march_or_later       = grid_obj[time_dim].dt.month >= 3
    
    # Pull day of year
    ordinal_day          = grid_obj[time_dim].dt.dayofyear
    
    # Bump day of year if after march and a leap year
    modified_ordinal_day = ordinal_day + (not_leap_year & march_or_later)
    
    # Add MOD as a variable
    grid_obj = grid_obj.assign(modified_ordinal_day = modified_ordinal_day)
    
    # Add as coordinate as well
    grid_obj = grid_obj.assign_coords(MOD = modified_ordinal_day)
    
    # return the grid with new var/coordinates
    return grid_obj
    
#-----------------------------------------------------
#   
# Calculate Anomalies from Observed SST and Climatology  
# 
#-----------------------------------------------------
def calc_anom(daily_sst, daily_clims):
    """
    Return Anomaly for Matching Modified Ordinal Day (day of year 1-366 adjusted for leap-years)
    
    daily_sst : xarray data array of sea surface temperatures containing "MOD" coordinate to pair with daily_clims
    daily_clim : xarray data array of sea surface temperature climatologic means.
    
    """
  
    return daily_sst - daily_clims.sel(modified_ordinal_day = daily_sst["MOD"])
  
  
  
  
#------------------------------------------------------
#
# Apply OISST Attributes
#
#------------------------------------------------------
def apply_oisst_attributes(oisst_grid, anomalies = False, reference_period = "1982-2011"):
  """
  Attach appropriate NetCDF attributes to OISST observations or Anomalies
  
  Args:
    oisst_grid : xr.Dataset to apply attributes to
    anomalies (bool): False to apply OISSTv2 attributes, True for anomaly specs
    reference_period (str): Optional string to apply for anomaly reference period, default 1982-2011
  """
  # Attributes for Anomalies
  anom_attrs = {
    'title'         : f'Sea surface temperature anomalies from NOAA OISSTv2 SST Data using {reference_period} Climatology',
    'institution'   : 'Gulf of Maine Research Institute',
    'source'        : 'NOAA/NCDC  ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/',
    'comment'       : 'Climatology used represents mean SST for the years 1982-2011',
    'history'       : 'Anomalies calculated 3/9/2021',
    'references'    : 'https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html',
    'dataset_title' : 'Sea Surface Temperature Anomalies - OISSTv2',


  }
  
  # Attributes for Observed SST
  obs_attrs = {
    "Conventions"   : "CF-1.5",
    "title"         : "NOAA/NCEI 1/4 Degree Daily Optimum Interpolation Sea Surface Temperature (OISST) Analysis, Version 2.1",
    "institution"   : "NOAA/National Centers for Environmental Information",
    "source"        : "NOAA/NCEI https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/",
    "References"    : "https://www.psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html",
    "dataset_title" : "NOAA Daily Optimum Interpolation Sea Surface Temperature",
    "version"       : "Version 2.1",
    "comment"       : "Reynolds, et al.(2007) Daily High-Resolution-Blended Analyses for Sea Surface Temperature (available at https://doi.org/10.1175/2007JCLI1824.1). Banzon, et al.(2016) A long-term record of blended satellite and in situ sea-surface temperature for climate monitoring, modeling and environmental studies (available at https://doi.org/10.5194/essd-8-165-2016). Huang et al. (2020) Improvements of the Daily Optimum Interpolation Sea Surface Temperature (DOISST) Version v02r01, submitted.Climatology is based on 1971-2000 OI.v2 SST. Satellite data: Pathfinder AVHRR SST and Navy AVHRR SST. Ice data: NCEP Ice and GSFC Ice. Data less than 15 days old may be subject to revision."
    
  }
  
  
  # Toggle which to use
  if anomalies == True:
    attr_dict = anom_attrs
  elif anomalies == False:
    attr_dict = obs_attrs
  
  # Apply them
  oisst_grid.attrs = attr_dict
  
  # Set the Time encodings
  oisst_grid.time.encoding = {"units" : "days since '1800-01-01'"}
  
  # Return the grid
  return oisst_grid
  

#-----------------------------------------------------
#
#  Get Log-Likelihood of Anomaly
#
#-----------------------------------------------------
def calc_ll(row, var_name, clim_mu, clim_sd):
  
    """
    Get Log-Likelihood of Event from Normal Distribution (mu, sigma) for use with assign()

    Args:
        row : Row of pandas dataframe
        var name (str): String for column name indicating the values to be assessed for their likelihood
        clim_mu (str): String for column name indicating mean of the distribution
        clim_sd (str): String for column name indicating the standard deviation of the distribution
        

    """
    # log likelihood of event from normal distribution.
    n = 1
    anom  = row[f"{var_name}"]
    mu    = row[f"{clim_mu}"]
    sigma = row[f"{clim_sd}"]
    log_lik = n * math.log(2 * math.pi * (sigma ** 2)) / 2 + np.sum(((anom - mu) ** 2) / (2 * (sigma ** 2)))
    return log_lik














#---------------------------------------------------
#
# Masked Timseries from xr.Dataset
#
#----------------------------------------------------
def calc_ts_mask(grid_obj, shp_obj, shp_name, var_name = "sst"):
  """
  Return a timeseries using data that falls within shapefile. 
  
  Standard deviation
  not included so that this function can be used for any period of time.
  
  Args:
    grid_obj       : xr.Dataset of the desired input data to mask
    shp_obj        : shapefile polygon to use as a mask
    shp_name (str) : String to use as name when making mask
    var_name (str) : Optional string identifying the variable to use
  """

  #### 1. Make the mask
  area_mask = regionmask.Regions(shp_obj.geometry,
                                 name = shp_name)

  #### 2. Mask the array with gom_mask to get nan test
  mask = area_mask.mask(grid_obj, lon_name = "lon", lat_name = "lat")

  
  #### 3. Extract data that falls within the mask
  masked_ds = grid_obj.where(~np.isnan(mask))

  
  #### 4. Calculate timeseries mean

  # Get the timeseries mean of the desired variable
  masked_ts = getattr(masked_ds, var_name).mean(dim = ("lat", "lon"))

  
  #### 5. Change time index rownames to a column 

  # Convert to Timeseries Dataframe
  masked_ts_df = masked_ts.to_dataframe()

  # Reset the index, rename variables
  masked_ts_df = masked_ts_df.reset_index()[["time", var_name]]

  
  # Return the table as output
  return masked_ts_df
  


#-----------------------------------------------------
#
# Append Timeseries Updates to Existing Masked Timeseries w/ Climatology
#
#-----------------------------------------------------
def append_sst_ts(old_ts, update_ts):
   """
   Append update period to OISST masked timeseries to create complete timeline
   
   Args:
     old_ts    : Timeseries of existing data, needs time and sst column, rest are dropped
     update_ts : Timeseries of update period
   
   """
   # Remove dates from old timeseries overlap from the update timeseries
   old_sst = old_ts[["time", "sst"]]
   not_overlapped = ~old_sst.time.isin(update_ts.time)
   old_sst  = old_sst[not_overlapped]


   # Concatenate onto the original
   appended_ts = pd.concat([ old_sst, update_ts ])
   
   # Format time as datetime
   appended_ts["time"] = appended_ts["time"].astype("datetime64")
    
   # Add modified ordinal day
   
   # Pull climatology info
   
   # Get anomalies
    
   return appended_ts


#-----------------------------------------------------
#
# Add Modified Ordinal Day to pd.DataFrame that has time column
#
#-----------------------------------------------------
def add_mod_to_ts(new_ts):
    """
    Add modified ordinaly day column to pd.DataFrame from time column
    
    Args:
        new_ts : timeseries dataframe that contains "time" datetime64 column.
    
    """
    # Add modified ordinal day, for day-to-day calculation and leapyear adjustment
    not_leap_year  = ~new_ts.time.dt.is_leap_year
    march_or_later =  new_ts.time.dt.month >= 3
    ordinal_day    =  new_ts.time.dt.dayofyear
    mod            =  ordinal_day + (not_leap_year & march_or_later)
    new_ts["modified_ordinal_day"] = mod
    return new_ts

#-----------------------------------------------------
#
# Rejoin Regional Climatology, re-calculate regional anomalies
#
#-----------------------------------------------------
def rejoin_climatology(old_ts, new_ts):
    """
    Merge Climatology and climate standard deviation from one dataframe 
    into a second by merging on modified ordinal days, the units of the climatology.
    
    Args:
        old_ts : Dataframe with "modified_ordinal_day", "sst_clim", "clim_sd" used to build climatology key for merge
        new_ts : Second dataframe that only has time and sst, but matches the region mask used to prepare old_ts
    
    
    """
    # pull unique climatology values from existing timeline
    clim = old_ts[["modified_ordinal_day", "sst_clim", "clim_sd"]]
    clim = clim.drop_duplicates()
    
    # Add MOD to new timeseries
    new_ts = add_mod_to_ts(new_ts)
   
    # Merge to new timeline using Modified day of year
    anom_timeline = new_ts.merge(clim, how = "left", on = "modified_ordinal_day")


    # Subtract climate mean to get anomalies
    anom_timeline["sst_anom"] = anom_timeline["sst"] - anom_timeline["sst_clim"]

    return anom_timeline




#-----------------------------------------------------
#
# Timeseries Region Catalog
#
#-----------------------------------------------------
def get_region_names(region_group):
  """
  Return a consistent list of region names to use when iterating through
  region groups for masked timeseries evaluation and updates.
  
  names are presented in the save-name form, without spaces or underscores.
  
  This function will be the access points to file path conventions, with changes here impacting 
  loading and saving text.
  
  Args:
    region_group (str): Choice of "gmri_sst_focal_areas", "lme", "nmfs_trawl_regions", "nelme_regions"
  
  """
  
  # GMRI sst focal areas
  gmri_focal_areas = ["apershing_gulf_of_maine", "cpr_gulf_of_maine", "aak_northwest_atlantic"]
  
  # NMFS Regions
  nmfs_regions = ["georges_bank",         "gulf_of_maine", 
                  "southern_new_england", "mid_atlantic_bight"]
                  
                  
  # NELME Regions
  nelme_regions = ["GoM", "NELME", "SNEandMAB"]
                  
  # Large Marine Ecosystems
  lme_regions =  ["agulhas_current",                        "aleutian_islands",                      
                  "antarctica",                             "arabian_sea",                           
                  "baltic_sea",                             "barents_sea",                           
                  "bay_of_bengal",                          "beaufort_sea",                          
                  "benguela_current",                       "black_sea",                             
                  "california_current",                     "canadian_eastern_arctic_west_greenland",
                  "canadian_high_arctic_north_greenland",   "canary_current",                        
                  "caribbean_sea",                          "celtic_biscay_shelf",                   
                  "central_arctic",                         "east_bering_sea",                       
                  "east_brazil_shelf",                      "east_central_australian_shelf",         
                  "east_china_sea",                         "east_siberian_sea",                     
                  "faroe_plateau",                          "greenland_sea",                         
                  "guinea_current",                         "gulf_of_alaska",                        
                  "gulf_of_california",                     "gulf_of_mexico",                        
                  "gulf_of_thailand",                       "hudson_bay_complex",                    
                  "humboldt_current",                       "iberian_coastal",                       
                  "iceland_shelf_and_sea",                  "indonesian_sea",                        
                  "insular_pacific_hawaiian",               "kara_sea",                              
                  "kuroshio_current",                       "labrador_newfoundland",                 
                  "laptev_sea",                             "mediterranean_sea",                     
                  "new_zealand_shelf",                      "north_australian_shelf",                
                  "north_brazil_shelf",                     "north_sea",                             
                  "northeast_australian_shelf",             "northeast_us_continental_shelf",        
                  "northern_bering_chukchi_seas",           "northwest_australian_shelf",            
                  "norwegian_sea",                          "oyashio_current",                       
                  "pacific_central_american_coastal",       "patagonian_shelf",                      
                  "red_sea",                                "scotian_shelf",                         
                  "sea_of_japan",                           "sea_of_okhotsk",                        
                  "somali_coastal_current",                 "south_brazil_shelf",                    
                  "south_china_sea",                        "south_west_australian_shelf",           
                  "southeast_australian_shelf",             "southeast_us_continental_shelf",        
                  "sulu_celebes_sea",                       "west_bering_sea",                       
                  "west_central_australian_shelf",          "yellow_sea"]     


  # Dictionary Lookup
  region_catalog = {"gmri_sst_focal_areas" : gmri_focal_areas, 
                    "lme"                  : lme_regions, 
                    "nmfs_trawl_regions"   : nmfs_regions,
                    "nelme_regions"        : nelme_regions}
                    
  # Return Selected List
  region_selections = region_catalog[region_group]
  return region_selections



#-----------------------------------------------------
#
# Timeseries Path Directory
#
#-----------------------------------------------------
def get_timeseries_paths(box_root, region_list, region_group, polygons = False):
  """
  Get the Full Path for either masked timeseries or the polygons used to 
  create them by the group name and the base path to box.
  
  Also Helpful for maintaining consistency on save paths for updating
  
  Args:
    box_root (str)     : String path to box for whichever user is accessing things
    region_list        : List of region names returned from ot.get_region_names
    region_group (str) : String indicating group of regions to look up. 
                         Options are gmri_sst_focal_areas, lme, nmfs_trawl_regions, nelme_regions
    polygons (bool)    : Indication of whether to look up paths
  
  """
  
  # 1. Set file structure for shapefile lookup
  poly_root = "RES_Data/Shapefiles/"
  
  # How the start of the file names go 
  if region_group == "nmfs_trawl_regions":
    poly_start = "nmfs_trawl_"
  else:
    poly_start = ""
  
  # How the file endings go
  poly_end = {"gmri_sst_focal_areas" : ".geojson",
              "lme"                  : "_exterior.geojson",
              "nmfs_trawl_regions"   : ".geojson",
              "nelme_regions"        : "_sf.shp"}
  
  # Path to different groups
  poly_extensions = {"gmri_sst_focal_areas" : f"{box_root}{poly_root}gmri_sst_focal_areas/{poly_start}",
                     "lme"                  : f"{box_root}{poly_root}large_marine_ecosystems/{poly_start}",
                     "nmfs_trawl_regions"   : f"{box_root}{poly_root}nmfs_trawl_regions/{poly_start}",
                     "nelme_regions"        : f"{box_root}{poly_root}NELME_regions/{poly_start}"}
                     
  
  
  
  
  # 2. Set file structure for timeseries lookup                   
  ts_root = "RES_Data/OISST/oisst_mainstays/regional_timeseries/"
  
  # How all masked timeseries file names start
  ts_start = "OISSTv2_anom_"
  
  # How they all end
  ts_ending = ".csv"
  
  # Path to different groups
  timeseries_extensions = {"gmri_sst_focal_areas" : f"{box_root}{ts_root}gmri_sst_focal_areas/{ts_start}",
                           "lme"                  : f"{box_root}{ts_root}large_marine_ecosystems/{ts_start}",
                           "nmfs_trawl_regions"   : f"{box_root}{ts_root}nmfs_trawl_regions/{ts_start}",
                           "nelme_regions"        : f"{box_root}{ts_root}NELME_regions/{ts_start}"}



  # 3. Toggle File Start and Endings for Shapefiles or for timeseries
  if polygons == True:
    leading_path = poly_extensions[region_group]
    file_ending  = poly_end[region_group]
  elif polygons == False:
    leading_path = timeseries_extensions[region_group]
    file_ending  = ts_ending
    

  # Build full paths using region_list
  region_paths = []
  for region_name in region_list:
    full_region_path = f"{leading_path}{region_name}{file_ending}"
    region_paths.append(full_region_path)
    
    
  # Return all the file paths
  return region_paths
    
    
