OISST Mainstays
================

# About:

This repository maintains the core workflows for accessing, processing,
and mapping patterns in OISSTv2 sea surface temperature data.

## Repository Organization

There are two main processes at work within this repository. The first
is a **Continuous-integration workflow** that downloads and processes
sea surface temperature anomalies and regional timelines. This workflow
is done primarily in python and lives within the `notebooks/` folder.
Much of the heavy lifting is done using [xarray]() and [regionmask]()
libraries. There is also an oisstools module within the `notebooks/`
folder that supplies many of the discrete processing steps, which are
flexible for adding new study regions.

The second side of this repo is for **Analysis and Reporting** where I
handle the processing and manipulation of these data products for
climate reports. This section is done using R and Rmarkdown for
consistent reporting (and because I am much better with these tools).
The processed data is accessed from our cloud storage using our in-house
r-package [{gmRi}](www.github.com/gulfofmaine/gmri). Reports are
published using github pages and can be found here: [OISST Mainstays
Reports](https://github.com/adamkemberling/oisst_mainstays/blob/master/index.md)

### Jupyter Notebook Workflows

The notebooks in this repo form the core workflow for processing
climatology products from the global OISSTv2 products. The notebooks are
numbered to suggest a step-wise workflow that will take the original
files containing sea surface temperature observations and step the user
through calculating a climatology, and for getting timeseries for
particular regions of interest. Ongoing development is being done now to
create an automatic updating procedure.

### R Workflows

The R-workflows in this repository are primarily report driven. Here you
will find code for our seasonal SST update, the Gulf of Maine SST
Report, and the impacts of shifting climatology periods from 1982-2011
to 1991-2020.

## Working with Docker and Local Paths

The code for this repo was set up originally to be run using docker, and
a suitable docker image configuration is contained here for users
wishing to take advantage of that. When operating within the docker
image, be sure to set the workspace variable in any of the notebooks to
“docker” to direct paths to the mounted volumes and not local paths.

### Docker `make up` & `make down`

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

### Operating locally for `BASE` & `UPDATE` steps

The workflow steps that make up the core foundation of the workflow are
often too large to run successfully in docker. To get around this a
suitable conda environment may be needed to run the notebooks. In my
case I need to navigate to this repo in the terminal and type
`conda activate py36`.

Most of the notebooks will have a “workspace” parameter to be set which
determines the path structures for the rest of the notebooks
accordingly.

## Reference Data Access

[OISSTv2](https://www.ncdc.noaa.gov/oisst/optimum-interpolation-sea-surface-temperature-oisst-v21)
data products used for this project have been accessed directly from the
[NOAA Physical Sciences
Laboratory](https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html).
For accessing daily sea surface temperature data closer to real-time we
access daily files directly from NCEI.
