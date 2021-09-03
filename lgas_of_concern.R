library(sf)
library(dplyr)

lgas <- readRDS("lgas_small.rds")

lgas <- lgas %>%
  mutate(of_concern = ABB_NAME %in% c(
    "Bayside",
    "Blacktown",
    "Burwood",
    "Campbelltown",
    "Canterbury-Bankstown",
    "Cumberland",
    "Fairfield",
    "Georges River",
    "Liverpool",
    "Parramatta",
    "Strathfield",
    "Penrith"
  ))

saveRDS(lgas, "lgas_small.rds")
