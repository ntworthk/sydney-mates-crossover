library(sf)
library(dplyr)

lgas <- readRDS("lgas_small.rds")

lgas <- lgas %>%
  mutate(of_concern = FALSE
  #          ABB_NAME %in% c(
  #   "Bayside",
  #   "Blacktown",
  #   "Burwood",
  #   "Campbelltown",
  #   "Canterbury-Bankstown",
  #   "Cumberland",
  #   "Fairfield",
  #   "Georges River",
  #   "Liverpool",
  #   "Parramatta",
  #   "Strathfield",
  #   "Penrith"
  # )
  )

saveRDS(lgas, "lgas_small.rds")

parks <- readRDS("sydney_parks.rds")
botanic_gardens <- readRDS("botanic_gardens.rds")

botanic_gardens <- botanic_gardens %>% 
  st_as_sf %>%
  rename(geometry = x) %>%
  mutate(osm_id = "3744999", code = 7202L, fclass = "park", name = "Royal Botanic Garden", col = "green")

parks <- parks %>% bind_rows(botanic_gardens)

saveRDS(parks, "sydney_parks.rds")
