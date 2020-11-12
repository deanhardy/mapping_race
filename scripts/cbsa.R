rm(list=ls())

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(readxl)

# census_api_key("") ## Already installed

yr <- 2018

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/mapping-race'

## define variables of interest
## https://data.census.gov/cedsci/table?q=race&d=ACS%201-Year%20Estimates%20Detailed%20Tables&tid=ACSDT1Y2019.B03002
vars <- c(total_pop = "B03002_001",
          white = "B03002_003", black = "B03002_004", 
          native_american = "B03002_005", asian = "B03002_006", 
          hawaiian = "B03002_007", other = "B03002_008", 
          multiracial = "B03002_009", latinx = "B03002_012", latinx_black = "B03002_014", medincome = "B19013_001")

v18 <- load_variables(2018, "acs5", cache = TRUE)

## import ACS data 
df <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = vars,
  year = yr) %>%
  dplyr::select(-moe) %>%
  spread(key = "variable", value = "estimate") %>%
  filter(GEOID %in% c(35620, 31080))

# df <- get_estimates(
#   geography = "metropolitan statistical area/micropolitan statistical area",
#   product = 'characteristics',
#   breakdown = 'RACE',
#   year = yr)

## import spatial data
shp <- get_acs(geography = "county", 
               variables = "B03002_001E",
               year = yr, geometry = TRUE,
               shift_geo = T)
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

## descriptive stats
##
sum(df.shp$B03002_004 + df.shp$B03002_014)/sum(df.shp$B03002_001)
## 2018 5-yr ACS Black cont USA = 0.1236775
## 2009 5-yr ACS Black cont USA = 0.121677


cc.shp <- df.shp %>%
  filter(GEOID %in% cc$GEOID)

cc.ga <- cc.shp %>%
  filter(str_detect(NAME, ".*Georgia"))

## BIPOC coastal GA
1 - sum(cc.ga$B03002_003)/sum(cc.ga$B03002_001)
## 2018 5-yr ACS POC fof GA = 0.4581245
## 2009 5-yr ACS POC fof GA = 0.4149089

## Black coastal GA
sum(cc.ga$B03002_004 + cc.ga$B03002_014)/sum(cc.ga$B03002_001)
## 2018 5-yr ACS Black fof GA = 0.3388784
## 2009 5-yr ACS Black fof GA = 0.3427395

cc.not <- cc %>%
  filter(!(GEOID %in% df.shp$GEOID))

## descriptive stats
## POC in contiguous US
1- sum(cc.shp$B03002_003)/sum(cc.shp$B03002_001)

## 2009 5-yr ACS POC 0.4635508
## 2018 5-yr ACS POC 0.5141572

## plot coastal counties
# tm_shape(df.shp) +
#   tm_polygons() + 
tm_shape(cc.shp) +
  tm_polygons("perc_POC",
              palette = "Purples")

## plot contiguous US
tm_shape(df.shp) +
  tm_polygons("perc_POC",
              palette = "Purples")


#####################
## Chatham County GA
####################
chatham <- get_acs(geography = "block group",
                   state = 'Georgia',
                   county = 'Chatham',
                   variables = vars,
                   year = yr) %>%
  dplyr::select(-moe, -NAME) %>%
  spread(key = "variable", value = "estimate")

## import spatial data
ch.shp <- get_acs(geography = "block group",
               state = 'Georgia',
               county = 'Chatham',
               variables = "B03002_001E",
               year = yr, geometry = TRUE)
ch.shp <- st_zm(ch.shp) ## drop "Z" data

## append census race data to spatial data
ch.shp2 <- left_join(ch.shp, chatham, by = "GEOID", copy = TRUE) %>%
  dplyr::select(-moe, -variable) %>%
  rename(B03002_001 = estimate) %>%
  mutate(perc_POC = 1-(B03002_003/B03002_001)) %>%
  filter(grepl(c('^02'), GEOID) == FALSE,
         grepl(c('^15'), GEOID) == FALSE,
         grepl(c('^72'), GEOID) == FALSE) %>% # filter AK, HI, PR
  st_as_sf() %>%
  st_transform(4326)

#####################
## build leaflet map
####################
library(leaflet)

pal <- colorNumeric("Purples", ch.shp2$perc_POC)
# use brewer.pal.info to get list of palettes
# also see colorFactor(), colorBin(), colorQuantile()

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>%
  addPolygons(data = ch.shp2,
              label = ~NAME,
              group = "hover",
              fillColor = ~pal(perc_POC),
              fillOpacity = 0.5,
              weight = 1) %>%
  addPolygons(data = ch.shp2,
              popup = ~NAME,
              group = "click",
              fillColor = ~pal(perc_POC),
              fillOpacity = 0.5,
              weight = 1) %>%
  addLayersControl(baseGroups = c("hover", "click")) %>%
  addScaleBar() %>%
  addLegend(pal = pal,
            values = ch.shp2$perc_POC,
            title = "People of Color (%)")
m

### exploring exporting as html file for offline exploration
library(htmlwidgets)
saveWidget(m, file="m.html")


