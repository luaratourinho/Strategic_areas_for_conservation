library(sp)
library(rgdal)
library(raster)
# 
# target_species <-
#   read.csv(
#     "./04_n_thinned_records_byme.csv",
#     stringsAsFactors = FALSE,
#     sep = ";"
#   ) %>%
#   pull(species)
# 
# 
# sp.n <- target_species[-c(8,15,19,20)]

# Load polygon
nivel6 <- shapefile("./nivel6/PAT_ottonivel6.shp")

# Load all rasters
caminhos <- list.files("./outputs/soil/", ".tif", full.names = TRUE)
suitability <- stack(caminhos)

# Reproject polygon
projection(suitability) <- projection(raster())

nivel6 <- spTransform(nivel6, projection(suitability))

# Crop to area
suitability <- crop(suitability, nivel6)

# Extract data (if you want to run for one: raster(suitability, i))
ext_suit <- extract(suitability, nivel6)

resultado <- lapply(ext_suit, function(x) {
  apply(x,
        2,
        mean,
        na.rm = TRUE)
}) 

resultado <- do.call(rbind, resultado)

# Add the mean to the shapefile
nivel6@data <- cbind(nivel6@data, resultado) 



# Plot --------------------------------------------------------------------


library(tidyverse)
library(sf)

nivel6_sf <- st_as_sf(nivel6)

loop_list <- colnames(nivel6_sf)[-(1:4)]

g <- list()

for (i in 1:length(loop_list)) {
  g[[i]] <- ggplot(nivel6_sf) +
    geom_sf(aes_string(fill = loop_list[i])) +
    scale_fill_viridis_b()
}
g[[1]]
