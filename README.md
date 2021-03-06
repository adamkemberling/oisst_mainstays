OISST Mainstays
================

# About

This repository maintains core workflows for accessing, processing, and
mapping OISST data. For many of the global extent netcdf files
processing will be done using python and xarray.

The processing and manipulation of tabular data and other files will be
done using R and Rmarkdown for consistent reporting (and becauseI am
much better with these tools)

## Working with Docker and Local Paths

### `make up` & `make down`

These commands provide a quick way to spin up notebooks and rstudio
server.

Enter `make up` into the terminal for a quick start. Watch for the
notebook service to give you a URL with a login token. Toss that link
into the browser to start the notebook.

Once the environment is running Rstudio server can be found at
`localhost:8787` which can be entered into any browser.

### docker-compose.override.yaml

For colleagues wishing to access shared resources a
`docker-compose.override.yaml` file will need to be placed in the main
directory of this repository to link shared paths to shared resources.

### Operating locally for `BASE` steps

The workflow steps that make up the core foundation of the workflow are
often too large to run succesfully in docker. To get around this a
suitable conda environment may be needed to run the notebooks. In my
case I need to navigate to this repo in the terminal and type `conda
activate py36`.

Most of the notebooks will have a “workspace” parameter to be set which
determines the path structures for the rest of the notebooks
accordingly.

## Data Access

[OISSTv2](https://www.ncdc.noaa.gov/oisst/optimum-interpolation-sea-surface-temperature-oisst-v21)
data products used for this project have been accessed directly from the
[NOAA Physical Sciences
Laboratory](https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html).
The focus of theese workflows is on sea surface temperature products,
with gridded datasets containing daily mean sea surface temperature
downloaded individually prior to any re-working or processing steps.

## Notebooks

The notebooks in this repo form the core workflow for processing
climatology products from the global OISSTv2 products. The notebooks are
numbered to suggest a step-wise workflow that will take the original
files containing sea surface temperature observations and step the user
through calculating a climatology, and for getting timeseries for
particular regions of interest. Ongoing development is being done now to
create an automatic updating procedure.

## Rstudio Products

.Rmd files are then used to produce consistent reports for the processed
sea surface temperature products.

As reports are completed the will be made available [here using github
pages](https://github.com/adamkemberling/oisst_mainstays/blob/master/index.md)
