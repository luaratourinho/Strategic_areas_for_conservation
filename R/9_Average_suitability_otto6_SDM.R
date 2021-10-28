library(sp)
library(rgdal)
library(raster)
library(dplyr)
library("gridExtra")

target_species <-
  read.csv(
    "./occs/n_records_to_report.csv",
    stringsAsFactors = FALSE) %>%
  pull(species)

target_species <- target_species[c(8,15,19,20)]


# Load polygon
nivel6 <- shapefile("./PAT_territorio/PAT_ottonivel6.shp")

# Load all rasters
outputs <- list.files("./outputs/crop_PAT/SDM", ".tif", full.names = TRUE)
suitability <- stack(outputs)

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
         layer = "nivel6_SDM", driver="ESRI Shapefile", overwrite=T)


# Renaming -------------------------------------------------------------

nivel6_SDM <- shapefile("./outputs/crop_PAT/nivel6/nivel6_SDM.shp")

names_cols <- c("NUNIV", "VERSA", "Shp_L",
                "Shp_A", "Hyba_albu", "Ooce_nubi",
                "Phil_bahi",	"Piri_flam")

names(nivel6_SDM)[] <- names_cols

writeOGR(nivel6_SDM, dsn = "./outputs/crop_PAT/nivel6",
         layer = "nivel6_SDM_names", driver="ESRI Shapefile", overwrite=T)


# Plot --------------------------------------------------------------------

library(tidyverse)
library(sf)

number_ticks <- function(n) {
  function(limits)
    pretty(limits, n)
}

nivel6_sf <- st_as_sf(nivel6)

loop_list <- colnames(nivel6_sf)[-(1:4)]
loop_list <- loop_list[-5]

p <- list()

# for (i in 1:length(loop_list)) {
#   g[[i]] <- ggplot(nivel6_sf) +
#     geom_sf(aes_string(fill = loop_list[i])) +
#     scale_fill_viridis_b()
# }
# g[[1]]

for (i in 1:length(loop_list)) {
  p[[i]] <- ggplot(nivel6_sf) +
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
    ))+
      coord_sf(expand = T) +
      scale_x_continuous(breaks = -42:-39) +
      scale_y_continuous(breaks = -12:-14)
      # scale_y_continuous(labels = scales::number_format(accuracy = 1),
      #                    breaks = number_ticks(3))
}

p[[1]]


# Figures arrangement -----------------------------------------------------

p_arrange <-
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2)
ggsave(
  p_arrange,
  file = "./Figures/SDM/nivel6_SDM_1_4.tiff",
  height = 20,
  width = 26,
  units = "cm"
)
