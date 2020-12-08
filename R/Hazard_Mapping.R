## Cloropleth polygon requirements
# Hazard maps are just a conditional reclass for now as the color scheme is diverging.
# This initial function will be for medians specifically
library(raster)

bp_stats <- read.csv("E:/Quarantine/BYK_February_2020/Outputs/500k_Spring/500k_Stats.csv")

median_fire_intensity <- stack(
                          list.files("E:/Quarantine/BYK_February_2020/Outputs/500k_Spring/",
                                     pattern = "FIMed.tif",
                                     full.names = T,
                                     recursive = T)
                               )

burn_count <- stack(
                list.files("E:/Quarantine/BYK_February_2020/Outputs/500k_Spring/",
                           pattern = "Probability.tif$",
                           full.names = T,
                           recursive = T)
                )
## There is a summed raster (for burn probability) that needs to be excluded from the weighting process
bp <- (burn_count[[12]]/ nrow(bp_stats))*100
burn_count <- burn_count[[-12]]

## We are looking to obtain the weighted mean of the medians to obtain the "best, least, unbiased estimator of the whole population median"
wm_out <- raster::weighted.mean(median_fire_intensity,
                                burn_count)

