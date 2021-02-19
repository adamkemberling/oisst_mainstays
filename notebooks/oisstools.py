# OISST Mainstays Support Functions
# 1/19/2021


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
