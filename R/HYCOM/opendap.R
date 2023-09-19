# Install and load the ncdf4 package if not already installed
# install.packages("ncdf4")
library(ncdf4)
library(lubridate)



####  USE This URL to Generate an OPENDap URL  ####
# http://tds.hycom.org/thredds/dodsC/GLBu0.08/expt_19.1.html

# Then just work in xarray
"
import xarray as xr
# import regionmask
import geopandas

# Does HYCOM URL open correctly? No for first one, yes for second, needd to fix dates
#url = 'http://tds.hycom.org/thredds/dodsC/GLBu0.08/expt_19.1?depth[0:1:39],lat[0:1:2000],lon[0:1:4499],time[0:1:6324],water_temp[0:1:0][0:1:0][0:1:0][0:1:0],salinity[0:1:0][0:1:0][0:1:0][0:1:0]'
url = 'http://tds.hycom.org/thredds/dodsC/GLBu0.08/expt_19.1'
ds = xr.open_dataset(url, decode_times = False)
ds


"



# Define the URL of the OPeNDAP server and the dataset you want to access
opendap_url <- "http://tds.hycom.org/thredds/dodsC/GLBu0.08/expt_19.1"
dataset_path <- "path_to_your_dataset"


# Open the connection to the OPeNDAP server
nc <- nc_open(opendap_url)

# List the available variables in the dataset
variables <- names(nc$var)
variables

# Choose a variable you want to retrieve
selected_variable <- "salinity"


## Get dataset's dimensions 
# Longitude
lon <- ncvar_get(nc, "lon")
nlon <- dim(lon)

# Latitude
lat <- ncvar_get(nc, "lat")
nlat <- dim(lat)

# Check dimensions
print(c(nlon,nlat))

# Time
time<-ncvar_get(nc,"time")
nt <- dim(time)
t_units <- ncatt_get(nc, "time", "units")
t_units

# convert time -- split the time units string into fields
t_ustr <- strsplit(t_units$value, " ")
t_dstr <- strsplit(unlist(t_ustr)[3], "-")
date <- ymd(t_dstr) + dhours(time)      
date

# Depth
depth<-ncvar_get(nc,"depth")
dim(depth)


####  Set Bounding Box information  ####

x <- c(72, 77)                # longitude
y <- c(9, 18)                # latitude
t <- c("2001-01-01", "2001-01-02")  # time
z <- c(0,50)                       # depth

# Function to get the indices from the ranges
btw <- function(data, num){
  c(min(which(min(num)<=data)), max(which(max(num)>=data)))
}

# Starting indices
lon_indices   <- btw(data = lon, num = x)
lat_indices   <- btw(data = lat, num = y)
time_indices  <- btw(data = as.Date(date), num = as.Date(t))
depth_indices <- btw(data = depth, num = z)

# Count number of indices to extract along each dimension
lon_range <- lon_indices[-1] - lon_indices[1]+1
lat_range <- lat_indices[-1] - lat_indices[1]+1
time_range <- time_indices[-1]+1 - time_indices[1]+1 #Add +1 to time_indices[-1]
depth_range <- depth_indices[-1] - depth_indices[1]+1

# Start and Count vectors
offset <- c(lon_indices[1], lat_indices[1], depth_indices[1], time_indices[1])    #lon,lat,depth,time
count <- c(lon_range, lat_range, depth_range, time_range)

# Get subsetted variable   
dat <- ncvar_get(nc, selected_variable, start = offset, count = count)


# List the values of the dimensions of the dataset ds
lat_list   <- list(nc[["dim"]][["lat"]][["vals"]])
lon_list   <- list(nc[["dim"]][["lon"]][["vals"]])
time_list  <- list(nc[["dim"]][["time"]][["vals"]])
depth_list <- list(nc[["dim"]][["depth"]][["vals"]])


# Function to select a range from a list
select_range <- function(lst, start, end) {
  sub_lst <- lst[[1]]  # Get the list
  
  if (start < 1 || end > length(sub_lst)) {
    stop("Invalid range")
  }
  return(sub_lst[start:end])
}

# Extract the dimension range using the indices
range_lat   <- select_range(lat_list, lat_indices[1], lat_indices[2])    #lat_indices
range_lon   <- select_range(lon_list, lon_indices[1], lon_indices[2])   #lon_indices
range_time  <- select_range(time_list, time_indices[1], time_indices[2])  #time_indices +1
range_depth <- select_range(depth_list, depth_indices[1], depth_indices[2])    #depth_indices

# Define the dimensions
dim_lon   <- ncdim_def("lon", "degrees_east", range_lon)
dim_lat   <- ncdim_def("lat", "degrees_north", range_lat)
dim_time  <- ncdim_def("time", t_units$value, range_time)
dim_depth <- ncdim_def("depth", "m", range_depth)

# Define the dimensions of the variable
dimVAR <- ncdim_def( selected_variable, "unk", count)

# Define the variable by integrating the dimensions
var_nc <- ncvar_def(selected_variable, "unk", list(dim_lon, dim_lat, dim_time, dim_depth), 99999, longname = selected_variable, prec = "float")

# Create a new NetCDF file object
ncnew <- nc_create("test.nc", var_nc)

# Write the nc vile
ncvar_put(ncnew, var_nc, dat, start = NA, count = NA)

# Close the netCDF to save it
nc_close(ncnew)

# Close the connection
nc_close(nc)

# Now you can work with the 'data' variable, which contains the data you retrieved

test_nc <- raster::stack("test.nc")
plot(test_nc)


