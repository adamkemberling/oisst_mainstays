# OISST Mainstays Support Functions
# 2/28/2021

import requests
from bs4 import BeautifulSoup
import os
import xarray as xr
import datetime


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
            
    
    
    # Code from stackoverflow to check for repeated values:
    # Program to check for repeated list contents
    # This code was contributed by Sandeep_anand , origins unknown
    def Repeat(x): 
        _size = len(x) 
        repeated = [] 
        for i in range(_size): 
            k = i + 1
            for j in range(k, _size): 
                if x[i] == x[j] and x[i] not in repeated: 
                    repeated.append(x[i]) 
        return repeated 
    
    
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
#  Build and Update Annual File from Month Caches
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
    
    # Last month
    for file in os.listdir(f"{last_month_cache}"):
      if file.endswith(".nc"):
        daily_files.append(f"{last_month_cache}{file}")
    
    # This month
    for file in os.listdir(f"{this_month_cache}"):
      if file.endswith(".nc"):
        daily_files.append(f"{this_month_cache}{file}")
            
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
    
    
    
    
    ####  Load and Append to Year File  ####
    
    # Pull the current year since I don't trust myself to not overwrite old files
    update_yr = datetime.datetime.now().year
    
    # Load the yearly file we're appending to
    try:
        oisst = xr.open_dataset(f"{annual_loc}sst.day.mean.{update_yr}.v2.nc")
    except:
        last_file = int(update_yr)-1
        oisst = xr.open_dataset(f"{annual_loc}sst.day.mean.{last_file}.v2.nc")
        oisst = oisst.drop("sst")
    
    
  
    # Remove dates from annual file that overlap with updates.
    # This will make it so the current month will overwrite as it gets finalized.
    
    # Boolean flag for whether time is before the update_months set in beginning
    def before_update(month):
        return (month < int(last_month) )
      
      # Boolean flag for whether time is after the update_months set in beginning
    def beyond_update(month):
        return (month > int(this_month) )
    
    
    # Subset dates out of annual file so there isn't overlap on update month
    # Don't bother if its January or February
    if int(last_month) > 1:
  
      # First - Check for any dates before
      if any(oisst.time.dt.month < int(last_month)):

          # Returns the dates
          b4_update = before_update(oisst['time.month'])
          oisst_subset = oisst.sel(time = b4_update)
      
      # If no dates before, use all    
      else:
          oisst_subset = oisst
          
      
      # Next - Check for any dates beyond update
      if any(oisst_subset.time.dt.month > int(this_month)):
          
          # Returns the dates
          after_update = beyond_update(oisst_subset['time.month'])
          oisst_subset = oisst_subset.sel(time = after_update)
          
      #  Once subset, append/combine updates to previous months to form combined annual file
      oisst_combined = xr.combine_by_coords(datasets = [oisst_subset, update_prepped]).load
      oisst_subset.close()
      
    # If last month is not > 1 then we only have the update date and no combining is necessary  
    else:
      oisst_combined = update_prepped
      
      
      
    #### check again for duplicates, and remove
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
    
    # Close the annual file we will be overwriting
    oisst.close()
    
    # Return the combined data
    return oisst_combined
  
  
  
  
