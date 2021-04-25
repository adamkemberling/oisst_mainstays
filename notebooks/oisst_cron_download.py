
# Scheduling OISSTv2 Downloads:
# Direct port of: Update_00_Update_Year.ipynb
# Replaced with prefect approach. Also this didn't work

# # Libraries
# from bs4 import BeautifulSoup
# import requests
# import os
# import xarray as xr
# import datetime
# import notebooks.oisstools as ot
# 
# 
# #### 1. Set workspace
# workspace = "local"
# 
# # Root paths
# root_locations = {"local"  : "/Users/akemberling/Box/",
#                   "docker" : "/home/jovyan/"}
# 
# # Set root with workspace
# box_root = root_locations[workspace]
# 
# # Global cache root
# _cache_root = f"{box_root}RES_Data/OISST/oisst_mainstays/"
# 
# 
# #### 2. Set Months to Update
# now = datetime.datetime.now()
# update_yr  = now.year
# this_month = str(now.month).rjust(2, "0")
# last_month = str(now.month - 1 if now.month > 1 else 12).rjust(2, "0")
# 
# 
# 
# #### 3. Update the Previous Month
# # Use cache_oisst function to update cache for last month
# ot.cache_oisst(cache_month = last_month, 
#                update_yr = update_yr, 
#                workspace = workspace, 
#                verbose = True)
#                
#                
# #### 4. Update Current Month
# ot.cache_oisst(cache_month = this_month, 
#                update_yr = update_yr, 
#                workspace = workspace, 
#                verbose = True)
# 
# 
# 
# #### 5. Assemble Annual File
# oisst_update = ot.build_annual_from_cache(last_month = last_month, 
#                                           this_month = this_month, 
#                                           workspace = workspace, 
#                                           verbose = True)
# 
# 
# 
# #### 6. Export the Update
# 
# # Build out destination folder:
# out_folder       = f"{_cache_root}annual_observations/"
# naming_structure = f"sst.day.mean.{update_yr}.v2.nc"
# out_path         = f"{out_folder}{naming_structure}"
# 
# 
# # Save File to Output Path
# oisst_update.to_netcdf(path = out_path)




