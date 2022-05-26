####  Mapping the trawl areas, exists elsewhere but needed for something  ####

library(gmRi)
library(here)
library(tidyverse)
library(sf)


# Research access paths
box_paths <- research_access_paths(mac_os = "mojave")
res_path <- box_paths$res

# Load polygons for mapping
new_england  <- ne_states("united states of america") %>% st_as_sf(crs = 4326) 
canada       <- ne_states("canada") %>% st_as_sf(crs = 4326)
trawl_strata <- read_sf(str_c(res_path, "Shapefiles/BottomTrawlStrata/BTS_Strata.shp"))
trawl_strata <- trawl_strata %>%  janitor::clean_names()

# Stratum Key for filtering specific areas
strata_key <- list(
  "Georges Bank"          = as.character(13:23),
  "Gulf of Maine"         = as.character(24:40),
  "Southern New England"  = stringr::str_pad(as.character(1:12),
                                             width = 2, pad = "0", side = "left"),
  "Mid-Atlantic Bight"    = as.character(61:76))

# Add labels to the data
trawl_strata <- trawl_strata %>%
  mutate(
    strat_num = str_sub(strata, 2, 3),
    survey_area =  case_when(
      strat_num %in% strata_key$`Georges Bank`         ~ "Georges Bank",
      strat_num %in% strata_key$`Gulf of Maine`        ~ "Gulf of Maine",
      strat_num %in% strata_key$`Southern New England` ~ "Southern New England",
      strat_num %in% strata_key$`Mid-Atlantic Bight`   ~ "Mid-Atlantic Bight",
      TRUE                                             ~ "not found"))


# Optional, Use strata_select to pull the strata we want individually
strata_select <- c(
  strata_key$`Georges Bank`,
  strata_key$`Gulf of Maine`,
  strata_key$`Southern New England`,
  strata_key$`Mid-Atlantic Bight`)


# Filtering with strata_select
trawl_strata <- trawl_strata %>% 
  filter(
    strata >= 01010,
    strata <= 01760,
    strata != 1310,
    strata != 1320,
    strata != 1330,
    strata != 1350,
    strata != 1410,
    strata != 1420,
    strata != 1490,
    strat_num %in% strata_select)

# mapping them
ggplot() +
  geom_sf(data = trawl_strata, aes(fill = survey_area)) +
  geom_sf(data = new_england) +
  geom_sf(data = canada) +
  coord_sf(xlim = c(-76, -66), ylim = c(35,46)) +
  theme(legend.title = element_blank())

