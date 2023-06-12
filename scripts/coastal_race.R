rm(list=ls())

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(readxl)
library(tigris)

# census_api_key("") ## Already installed

yr <- 2021

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/mapping-race'

## define variables of interest
## https://data.census.gov/cedsci/table?q=race&d=ACS%201-Year%20Estimates%20Detailed%20Tables&tid=ACSDT1Y2019.B03002
vars <- c(white = "B03002_003E", black = "B03002_004E", 
          native_american = "B03002_005E", asian = "B03002_006E", 
          hawaiian = "B03002_007E", other = "B03002_008E", 
          multiracial = "B03002_009E", latinx = "B03002_012E", latinx_black = "B03002_014E")

## import list of coastal counties
# cc <- st_read(file.path(datadir, "coastal-counties")) %>%
#   rename(GEOID = CNTYIDFP)
# 
# cc.list <- read.csv(file.path(datadir, 'cc-list.csv'), stringsAsFactors = F) %>%
#   mutate(GEOID = as.character(GEOID))
cc <- read_excel(file.path(datadir, 'coastline-counties-list.xlsx'), skip = 5)

## import ACS data 
df <- get_acs(geography = "county", 
              variables = vars,
              year = yr) %>%
  dplyr::select(-moe, -NAME) %>%
  spread(key = "variable", value = "estimate")

## import county spatial data
shp <- get_acs(geography = "county", 
               variables = "B03002_001E",
               year = yr, geometry = TRUE)
shp <- st_zm(shp) ## drop "Z" data

## append census race data to county spatial data
df.shp <- left_join(shp, df, by = "GEOID", copy = TRUE) %>%
  dplyr::select(-moe, -variable) %>%
  rename(B03002_001 = estimate) %>%
  mutate(perc_POC = 1-(B03002_003/B03002_001)) %>%
  filter(grepl(c('^02'), GEOID) == FALSE,
         grepl(c('^15'), GEOID) == FALSE,
         grepl(c('^72'), GEOID) == FALSE) %>% # filter AK, HI, PR
  st_as_sf() %>%
  st_transform(4326) %>%
  shift_geometry()
  # st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 
  #                         +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") # Albers Equal Area

## filter to coastal counties
cc.shp <- df.shp %>%
  filter(GEOID %in% cc$GEOID)

## import states spatial data
st <- states(cb = TRUE, resolution = "500k", year = NULL) %>%
  # filter(REGION != 9) %>%
  filter(STATEFP < 60 & !NAME %in% c("Alaska", "Hawaii")) %>%
  shift_geometry()

## world countries
## https://biostats-r.github.io/biostats/workingInR/140_maps.html
# data("World")
library(rnaturalearth)
world <- ne_countries(scale = 50, returnclass = "sf") %>%
  filter(sov_a3 %in% c('USA', 'CAN', 'MEX')) %>%
  st_transform(5070)
fnt = 1.4

tmap_options(unit = 'mi')
## plot coastal counties
cc.map <- tm_shape(st) +
  tm_polygons() +
tm_shape(world) +
  tm_polygons() +
tm_shape(st) +
  tm_polygons(col = 'white') +
tm_shape(cc.shp) +
  tm_polygons("perc_POC",
              palette = "Purples",
              border.col = 'grey50',
              lwd = 0.5,
              title = 'BIPOC (%)') + 
tm_layout(bg.color = "skyblue", 
          legend.text.size = fnt,
          legend.title.size = fnt,
          frame.lwd = 0, 
          outer.margins = c(0,0,0,0)) +
tm_compass(position = c(0.89, 0.1), size = 3) + 
tm_scale_bar(breaks = c(0,100,200), text.size = fnt)

cc.map

## export map
png(file.path(datadir, 'figures/coastal-race-map.png'), res = 150, unit = 'in',
    width = 13.33, height = 7.5)
cc.map
dev.off()

# ggplot() + 
#   geom_sf(data = st) + 
#   geom_sf(data = world) +
#   geom_sf(data = st) + 
#   geom_sf(data = cc.shp, aes(fill = perc_POC)) + 
#   # coord_sf(xlim = c(120, 66), ylim = c(24, 50))
#   coord_sf(crs = st_crs(5070), xlim = c(-2500000, 2500000), 
#            ylim = c(-1300000, 100000))

## plot contiguous US
# tm_shape(df.shp) +
#   tm_polygons("perc_POC",
#               palette = "Purples")


#####################
## descriptive stats
#####################
## National Black population
sum(df.shp$B03002_004 + df.shp$B03002_014)/sum(df.shp$B03002_001)
## 2018 5-yr ACS Black cont USA = 0.1236775
## 2009 5-yr ACS Black cont USA = 0.121677

## national BIPOC
1 - sum(df.shp$B03002_003)/sum(df.shp$B03002_001) ## national BIPOC

cc.ga <- cc.shp %>%
  filter(str_detect(NAME, "Georgia"))
cc.gulf <- cc.shp %>%
  filter(str_detect(NAME, "Texas|Louisiana|Mississippi|Alabama|Florida"))

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

## Gulf states
cc.gulf <- cc.shp %>%
  filter(str_detect(NAME, "Texas|Louisiana|Mississippi|Alabama|Florida")) %>%
  filter(!str_detect(NAME, "Nassau|Duval|Johns|Flagler|Volusia|Brevard|Indian|Lucie|Martin|Palm|Broward|Miami"))

## BIPOC Gulf coastal counties
1 - sum(cc.gulf$B03002_003)/sum(cc.gulf$B03002_001)

########################
## County specific ESDA
## Chatham County GA
########################
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


