rm(list=ls())

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)

# census_api_key("") ## Already installed

yr <- '2016'

## define variables of interest
vars <- c(white = "B03002_003E", black = "B03002_004E", 
          native_american = "B03002_005E", asian = "B03002_006E", 
          hawaiian = "B03002_007E", other = "B03002_008E", 
          multiracial = "B03002_009E", latinx = "B03002_012E")

## import ACS data 
df <- get_acs(geography = "county", 
              variables = vars,
              year = yr) %>%
  dplyr::select(-moe, -NAME) %>%
  spread(key = "variable", value = "estimate")

## import spatial data
shp <- get_acs(geography = "county", 
               variables = "B03002_001E",
               year = yr, geometry = TRUE)
shp <- st_zm(shp) ## drop "Z" data

## append census race data to spatial data
df.shp <- left_join(shp, df, by = "GEOID", copy = TRUE) %>%
  dplyr::select(-moe, -variable, -NAME) %>%
  rename(B03002_001 = estimate) %>%
  mutate(perc_POC = 1-(B03002_003/B03002_001)) %>%
  st_as_sf() 

## plot
tm_shape(df.shp) +
  tm_polygons(fill = "perc_POC")

