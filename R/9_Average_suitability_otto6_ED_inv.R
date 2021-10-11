library(sp)
library(rgdal)
library(raster)
library(dplyr)

target_species <-
  read.csv(
    "./occs/n_records_to_report.csv",
    stringsAsFactors = FALSE) %>%
  pull(species)

target_species <- target_species[-c(8,15,19,20)]


# Load polygon
nivel6 <- shapefile("./PAT_territorio/PAT_ottonivel6.shp")

# Load all rasters
outputs <- list.files("./outputs/crop_PAT/ED", ".tif", full.names = TRUE)
suitability <- stack(outputs)

# Invert scale

library("spatialEco")
suitability_inv <- (((suitability - max(suitability)) * -1) + min(suitability))
names(suitability_inv) <- names(suitability)
suitability <- suitability_inv

# Reproject polygon
projection(suitability) <- projection(raster())

nivel6 <- spTransform(nivel6, projection(suitability))

# Crop to area
suitability <- crop(suitability, nivel6)

# Extract data (if you want to run for one: extract(raster(suitability, i), nivel6))
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

writeOGR(nivel6, dsn = "./outputs/crop_PAT/nivel6",
         layer = "nivel6_ED_inv", driver="ESRI Shapefile", overwrite=T)


# Plot --------------------------------------------------------------------

library(tidyverse)
library(sf)
library("gridExtra")

nivel6 <- shapefile("./outputs/crop_PAT/nivel6/nivel6_ED_inv.shp")

number_ticks <- function(n) {
  function(limits)
    pretty(limits, n)
}

nivel6_sf <- st_as_sf(nivel6)

loop_list <- colnames(nivel6_sf)[-(1:4)]
loop_list <- loop_list[-24]

g <- list()

# for (i in 1:length(loop_list)) {
#   g[[i]] <- ggplot(nivel6_sf) +
#     geom_sf(aes_string(fill = loop_list[i])) +
#     scale_fill_viridis_b()
# }
# g[[1]]

for (i in 1:length(loop_list)) {
  g[[i]] <- ggplot(nivel6_sf) +
    geom_sf(aes_string(fill = loop_list[i])) + 
    scico::scale_fill_scico(palette = "lajolla") +
    theme_bw() +
    coord_sf() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, angle = 90),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_blank(),
      legend.key = element_blank()
    ) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(0.9, 0.05),
          legend.text = element_text(size=15)) +
    # labs(title = "Acritopappus harleyi\n") +
    labs(title = paste0(target_species[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      face = "italic",
      size = 20
    )) +
    coord_sf(expand = T) +
    scale_x_continuous(breaks = -42:-39) +
    scale_y_continuous(breaks = -12:-14)
  
}

g[[1]]


# Figures arrangement -----------------------------------------------------

p_arrange <-
  grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]],
               g[[8]], nrow = 4)
ggsave(
  p_arrange,
  file = "./Figures/ED/nivel6_ED_1_8_2.tiff",
  height = 50,
  width = 30,
  units = "cm"
)

p_arrange2 <-
  grid.arrange(g[[9]], g[[10]], g[[11]], g[[12]], g[[13]], g[[14]], g[[15]], 
               g[[16]], nrow = 4)
ggsave(
  p_arrange2,
  file = "./Figures/ED/nivel6_ED_9_16_2.tiff",
  height = 50,
  width = 30,
  units = "cm"
)

p_arrange3 <-
  grid.arrange(g[[17]],g[[18]], g[[19]], g[[20]], g[[21]], g[[22]], g[[23]], nrow = 4)

ggsave(
  p_arrange3,
  file = "./Figures/ED/nivel6_ED_17_23_2.tiff",
  height = 50,
  width = 30,
  units = "cm"
)

