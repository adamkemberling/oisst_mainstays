####  Ranking Warming Among LME's and Gulf of Maine


####  Packages  ####
library(gmRi)
library(here)
library(lubridate)
library(patchwork)
library(heatwaveR)
library(tidyverse)
library(ggforce)

#box paths
box_paths <- research_access_paths()
res_path <-  box_paths$res

# Support Functions
source(here("R/oisst_support_funs.R"))
source(here("R/temp_report_support.R"))

# set ggplot theme for figures
theme_set(theme_bw())


# OISST Data

# Gulf of Maine
gom_oisst <- oisst_access_timeseries(oisst_path = box_paths$oisst_mainstays, 
                                     region_family = "gmri focus areas", 
                                     poly_name = "apershing gulf of maine")
gom_oisst <- mutate(gom_oisst, time = as.Date(time))

# Large Marine Ecosystems
lme_names <- get_region_names("lme") # names
lme_paths <- get_timeseries_paths("lme") # paths
lme_oisst <- map(lme_names, ~ read_csv(lme_paths[[.x]][["timeseries_path"]])) # data
lme_oisst <- map(lme_oisst, ~ mutate(.x, time = as.Date(time)))
lme_oisst <- setNames(lme_oisst, lme_names)


# Add Gulf of Maine to LME list
lme_oisst[["gulf_of_maine"]] <- gom_oisst
lme_oisst <- map(lme_oisst, pull_heatwave_events, threshold = 90, clim_ref_period = c("1982-01-01", "2011-12-31"))




####  Warming Rates  ####
lme_8220 <- map(lme_oisst, ~ mutate(.x, year = year(time)) %>% filter(year %in% c(1982:2020)))

# Get warming rates
lme_rates <- map_dfr(lme_8220, function(lme_data){
  lme_yearly <- group_by(lme_data, year) %>% summarise(sst = mean(sst, na.rm = T))
  yrly_warming <- lm(sst ~ year, data = lme_yearly)
  mod_details <- broom::tidy(yrly_warming)
  yrly_rate <- mod_details[2,"estimate"]
  names(yrly_rate) <- "annual_warming_rate_C"
  yrly_rate
}, .id = "Region") %>% 
  arrange(desc(annual_warming_rate_C))


lme_rates %>% slice(1:10)


