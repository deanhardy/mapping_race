rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(sf)

# census_api_key("") ## install personal census API key

##############################################################
## data import and prepping
##############################################################

## define year for census data import
yr <- '2016'

## define region for census import
cnty <- c("Whatcom")

## import race variables of interest
race_vars <- c(white = "B03002_003E", black = "B03002_004E", 
               native_american = "B03002_005E", asian = "B03002_006E", 
               hawaiian = "B03002_007E", other = "B03002_008E", 
               multiracial = "B03002_009E", latinx = "B03002_012E")

## import area of interest data
aoi <- get_acs(geography = "block group", 
               variables = race_vars,
               state = "WA",
               year = yr) %>%
  dplyr::select(-moe, -NAME) %>%
  spread(key = "variable", value = "estimate")

## import spatial data for "cnty" region
shp <- get_acs(geography = "block group", 
               variables = "B03002_001E",
               state = "WA", 
               year = yr, geometry = TRUE)
shp <- st_zm(shp) ## drop "Z" data

## append census race data to spatial data
aoi_shp <- left_join(shp, aoi, by = "GEOID", copy = TRUE) %>%
  dplyr::select(-moe, -variable, -NAME) %>%
  rename(B03002_001 = estimate) %>%
  mutate(perc_POC = 1-(B03002_003/B03002_001), count_POC = B03002_001 - B03002_003) %>%
  st_as_sf() %>%
  st_transform(4269)

tm_shape(aoi_shp) + 
  tm_fill('perc_POC', palette = "Purples")
