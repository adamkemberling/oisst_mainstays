OISST Mainstays
================

### A Repository for OISSTv2 Sea Surface Temperature Reporting

#### About:

This repository maintains the core workflows for accessing, processing,
and mapping patterns in OISSTv2 sea surface temperature data. There are
two main SST processing routines housed here:

1.  [The acquisition and data transformation of global OISSTv2 data,
    done in
    python](https://github.com/adamkemberling/oisst_mainstays/tree/master/notebooks)

2.  [The programmatic synthesis reports done R using
    Rmarkdown/Quarto](https://github.com/adamkemberling/oisst_mainstays/tree/master/R)

All report drafts are published using github pages and can be found
here: [OISST Mainstays
Reports](https://github.com/adamkemberling/oisst_mainstays/blob/master/index.md)

## Repository Organization

There are two main processes at work within this repository. The first
is a **ETL Workflow** that downloads and processes sea surface
temperature anomalies and regional timelines.

This workflow is done primarily in python and lives within the
`notebooks/` folder. Much of the heavy lifting is done using [xarray]()
and [regionmask]() libraries. There is also an `oisstools.py` module
within the `notebooks/` folder that supplies many of the discrete
processing steps, which are flexible for adding new study regions.

The second side of this repo is for **Analysis and Reporting** where I
handle the processing and manipulation of these data products for
climate reports.

This section is done using R and Rmarkdown for consistent reporting (and
because I am much better with these tools). The processed data is
accessed from our cloud storage using our in-house r-package
[{gmRi}](www.github.com/gulfofmaine/gmri).

### 1. Global/Regional Gridded Data Processing - Python

The notebooks in this repo form the core workflow for processing
climatology products from the global OISSTv2 products and can be found
in the `R/notebooks/` directory. Notebooks are numbered to suggest a
step-wise workflow that will take the original files containing sea
surface temperature observations and step the user through calculating a
climatology, and for getting timeseries for particular regions of
interest.

If starting from scratch begin with the notebooks with names beginning
with `BASE_`. These will construct building blocks used to generate
things like climatologies etc.

Ongoing development is being done now to create an automatic updating
procedure. At present the core workflow is contained within three
notebooks with names beginning with `UPDATE_`.

Testing and prototyping is done in notebooks beginning with the name
`TESTING_`.

### 2. Web-Based Reports - R

The R-workflows in this repository are primarily html report driven and
can be found in the `R/markdown_reports/` directory. Here you will find
code for our seasonal SST update, the Gulf of Maine SST Report, and the
impacts of shifting climatology periods from 1982-2011 to 1991-2020.

## Reference Data Access

[OISSTv2](https://www.ncdc.noaa.gov/oisst/optimum-interpolation-sea-surface-temperature-oisst-v21)
data products used for this project have been accessed directly from the
[NOAA Physical Sciences
Laboratory](https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html).
For accessing daily sea surface temperature data closer to real-time we
access daily files directly from NCEI.
