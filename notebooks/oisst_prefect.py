## Prefect Scheduling for OISST Downloads

# Build dependency structure for downloading OISSTv2 Data
# Processing and maintaining anomalies and climatologies
# regional timelines etc.

####  Import Libraries  ####
from prefect import task, Flow, Parameter
from prefect.schedules import IntervalSchedule
from bs4 import BeautifulSoup
import requests
import os
import xarray as xr
from datetime import timedelta, datetime
import notebooks.oisstools as ot
import graphviz

####  Task Definitions  ####

####  Flow 1 - Downloading Daily Caches - updating annual file

# 1. Set workspace
set_workspace = task(ot.set_workspace)

# 2. Set Cache Locations
set_cache_root = task(ot.set_cache_root)

# 3. Get Year for Current and Last Month
get_update_month = task(ot.get_update_month, name = "get_update_month")
get_prev_month   = task(ot.get_update_month, name = "get_prev_month")

# 4. Get Current & Previous Months
get_update_yr     = task(ot.check_update_yr, name = "get_update_yr")
get_prev_month_yr = task(ot.check_update_yr, name = "get_prev_month_yr")

# 5. Cache Current & Previous Months
cache_current_month = task(ot.cache_oisst, name = "cache_current_month")
cache_prev_month    = task(ot.cache_oisst, name = "cache_prev_month")

# 6. Assemble Annual File(s)
oisst_update = task(ot.build_annual_from_cache)

# 8. Save Update
oisst_save_update = task(ot.export_annual_update)


####  Flow 2 - Updating Global Anomalies

# update_global_anoms = ot.


####  Flow 3 - Regional Timeseries

# These should be easier









####  Local Workflow  ####

with Flow("OISST weekly download") as oisst_flow:
  # Downloading Daily Files
  workspace = Parameter("workspace", default = "local")        
  box_root = set_workspace(workspace)
  cache_root = set_cache_root(box_root)
  this_month = get_update_month(return_this_month = True)
  this_yr = get_update_yr(for_this_month = True)
  this_month_cache = cache_current_month(cache_month = this_month, 
                                         update_yr = this_yr, 
                                         workspace = workspace, 
                                         verbose = True)
  last_month = get_prev_month(return_this_month = False)
  last_month_yr = get_prev_month_yr(for_this_month = False)
  last_month_cache = cache_prev_month(cache_month = last_month, 
                                      update_yr = last_month_yr, 
                                      workspace = workspace, 
                                      verbose = True)
  oisst_annual_update = oisst_update(last_month = last_month, 
                                     this_month = this_month, 
                                     workspace = workspace, 
                                     verbose = True)
  save_updated_yr = oisst_save_update(cache_root = cache_root,
                                      update_yr = this_yr,
                                      oisst_update = oisst_annual_update)
  
  # Upcoming: Processing Anomalies




####  Running the workflow with parameters  ####

# Visualize the workflow
# oisst_flow.visualize()

# oisst workflow
oisst_flow.run(workspace = "local")












####  Scheduled Workflow  ####


# Scheduling
schedule = IntervalSchedule(
  start_date = datetime.utcnow() + timedelta(seconds = 1),
  interval = timedelta(days = 5)
)

# Build Flow
with Flow("OISST weekly download", schedule = schedule) as oisst_flow:  
  
  # Downloading Daily Files
  workspace = Parameter("workspace", default = "local")        
  box_root = set_workspace(workspace)
  cache_root = set_cache_root(box_root)
  this_month = get_update_month(return_this_month = True)
  this_yr = get_update_yr(for_this_month = True)
  this_month_cache = cache_current_month(cache_month = this_month, 
                                         update_yr = this_yr, 
                                         workspace = workspace, 
                                         verbose = True)
  last_month = get_prev_month(return_this_month = False)
  last_month_yr = get_prev_month_yr(for_this_month = False)
  last_month_cache = cache_prev_month(cache_month = last_month, 
                                      update_yr = last_month_yr, 
                                      workspace = workspace, 
                                      verbose = True)
  oisst_annual_update = oisst_update(last_month = last_month, 
                                     this_month = this_month, 
                                     workspace = workspace, 
                                     verbose = True)
  save_updated_yr = oisst_save_update(cache_root = cache_root,
                                      update_yr = this_yr,
                                      oisst_update = oisst_annual_update)
  
  # Upcoming: Processing Anomalies
  
  
  
  # Upcoming: Regional Timelines




####  Registering the Flow with perfect cloud  ####
oisst_flow.register(project_name = "oisst_mainstays")

# Setting up universal deploy using local agent and 
# oisst_flow.run_agent()



# # Cloud local agent
# oisst_flow.run_agent(token = "YCbKcDHpFaFZyal6OFAKkw") # to force the assignment
