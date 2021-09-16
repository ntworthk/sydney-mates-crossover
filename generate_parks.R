#--- Script details ------------------------------------------------------------
# Creation date: 16 September 2021
# Description:   Sydney and Melbourne parks
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(sf)

#--- Import data ---------------------------------------------------------------

# OSM data
shapefiles <- list.files(glue::glue("{Sys.getenv('SERVER_ADDRESS')}australia-latest-free.shp/"), pattern = "shp", full.names = T)

parks <- read_sf(shapefiles[2])
beaches <- read_sf(shapefiles[3])

# ABS data
gccsas <- read_sf(glue::glue("{Sys.getenv('SERVER_ADDRESS')}ASGS 2016 Volume 1.gpkg"), layer = "GCCSA_2016_AUST")

greater_sydney <- gccsas %>%
  st_transform(4326) %>% 
  filter(GCCSA_CODE_2016 == "1GSYD")

greater_melbourne <- gccsas %>%
  st_transform(4326) %>% 
  filter(GCCSA_CODE_2016 == "2GMEL")

greater_cities <- list(greater_sydney, greater_melbourne)

greater_pois <- lapply(greater_cities, function(city) {
  
  city_parks <- parks %>% 
    filter(fclass %in% c("park", "allotments", "heath", "meadow", "nature_reserve", "recreation_ground")) %>%
    filter(st_intersects(geometry, city, sparse = FALSE))

  city_beaches <- beaches %>% 
    filter(fclass %in% c("beach")) %>%
    filter(st_intersects(geometry, city, sparse = FALSE))
  
  bind_rows(city_parks, city_beaches) %>% 
    filter(is.na(name) | name != "Sydney Harbour National Park")
  
  
})

write_rds(greater_pois[[1]], "sydney_parks.rds")
write_rds(greater_pois[[2]], "melbourne_parks.rds")
