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
  dplyr::select(-moe, -variable) %>%
  rename(B03002_001 = estimate) %>%
  mutate(perc_POC = 1-(B03002_003/B03002_001)) %>%
  filter(grepl(c('^02'), GEOID) == FALSE,
         grepl(c('^15'), GEOID) == FALSE,
         grepl(c('^72'), GEOID) == FALSE) %>% # filter AK, HI, PR
  st_as_sf() %>%
  st_transform(4326)
  # st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 
  #                         +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") # Albers Equal Area

## plot
tm_shape(df.shp) +
  tm_polygons("perc_POC",
              palette = "Purples")


#####################
## build leaflet map
####################
library(leaflet)

pal <- colorNumeric("Purples", df.shp$perc_POC)
# use brewer.pal.info to get list of palettes
# also see colorFactor(), colorBin(), colorQuantile()

leaflet() %>%
  addTiles() %>%
  setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%
  addPolygons(data = df.shp,
              layer = ~NAME,
              group = "hover",
              fillColor = ~pal(perc_POC),
              fillOpacity = 0.5,
              weight = 1) %>%
  addPolygons(data = df.shp,
              layer = ~NAME,
              group = "click",
              fillColor = ~pal(perc_POC),
              fillOpacity = 0.5,
              weight = 1) %>%
  addLayersControl(baseGroups = c("hover", "click")) %>%
  addScaleBar() %>%
  addLegend(pal = pal,
            values = df.shp$perc_POC,
            title = "People of Color (%)")



