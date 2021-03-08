####  Comparing Warming Rates Through Resampling
####  3/2/2021
####  Objectives:
####  Build an array that allows you to resample a given pixel sized area with overflow around edges
####  Randomly select pixels and get area around that pixel to get warming rates



####  Building array

# basic idea is to mirror the warming rates raster so that:
# East and West continues to flow, so repeat original raster R:R:R
# For North South do the same, but invert
# If original is O and upside down is U, end with a 3x3 array (unit is original raster)
#
# U:U:U
# O:0:O
# U:U:U

# For resampling procedure randomly select pixel within 0. Then expend necessary directions around it
# If a certain amount of NA values exist then redraw
# If past a certain latitude throw a warning, possibly redraw
# should allow boxes to creep around map
