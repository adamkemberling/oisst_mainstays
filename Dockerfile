### Dockerfile

# ####  Miniconda Image - Testing  ####
# # Testing connection to conda/miniconda for cron
# # builds off environment.yml file in  main directory
# FROM continuumio/miniconda3
# 
# # Create the oisst_mainstays environment:
# COPY environment.yml .
# RUN conda env create -f environment.yml
# 
# # Make RUN commands use the new environment:
# SHELL ["conda", "run", "-n", "oisst_mainstays_env", "/bin/bash", "-c"]
# 
# # Make sure the environment is activated:
# RUN echo "Make sure datetime and other dependencies are installed:"
# RUN python -c "import datetime"
# 
# # The code to run when container is started:
# COPY conda_check.py .
# ENTRYPOINT ["conda", "run", "-n", "oisst_mainstays_env", "python", "conda_check.py"]



####  Docker Base Image for R-Shiny Environment  ####
# # Disabling 12/1/2020 because it just adds unnecessary complexity
# # Build from roxygen/geospatial image
# FROM rocker/geospatial:3.6.1

# # Installing R Package Dependencies
# RUN install2.r --error \
#     ggpmisc \
#     janitor \
#     here \
#     lubridate \
#     patchwork \
#     plotly \
#     noaaoceans \
#     heatwaveR \
#     devtools \
#     kableExtra \
#     knitr
    
# # Installation of non-CRAN packages
# RUN R -e "devtools::install_github('gulfofmaine/gmri', upgrade = 'never')"
# RUN R -e "devtools::install_github('ropensci/rnaturalearth', upgrade = 'never')"
# RUN R -e "devtools::install_github('ropensci/rnaturalearthhires', upgrade = 'never')"


