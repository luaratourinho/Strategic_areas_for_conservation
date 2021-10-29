library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)
library(ggmap)
library(ggplot2)
library(scico)
library("gridExtra")
library(sf)

# Cave species

# Coarazuphium cessaima Gnaspini, Vanin & Godoy 1998
# Metagonia diamantina Machado, Ferreira & Brescovit, 2011


# Reading data ------------------------------------------------------------

results_per_sp <- shapefile("./Final_results/Results_rest.shp")
results_per_sp_r <- shapefile("./Final_results/Results_conser.shp")


# Macthing to cave extension ----------------------------------------------

cave <- shapefile("./PAT_territorio/cavernas_nivel6.shp")
cave_PAT <- match(cave$NUNIVOTTO6, results_per_sp_r$NUNIVOTTO6)

cave_PAT_c <-results_per_sp[cave_PAT, ]
cave_PAT_r <-results_per_sp_r[cave_PAT, ]

# Saving results in the shapefile
writeOGR(cave_PAT_c, dsn = "./Final_results",
         layer = "cave_PAT_c", driver="ESRI Shapefile", overwrite=T)

writeOGR(cave_PAT_r, dsn = "./Final_results",
         layer = "cave_PAT_r", driver="ESRI Shapefile", overwrite=T)



# Plot --------------------------------------------------------------------

target_species <-
  read.csv(
    "./occs/n_records_to_report.csv",
    stringsAsFactors = FALSE) %>%
  pull(species)

target_species <- target_species[c(5, 12)]


# Conservation

n <- length(cave_PAT_c@data[,2:29])
p <- list()
PAT_extent <- st_as_sf(results_per_sp)
results_per_sp_sf <- st_as_sf(cave_PAT_c)


for (i in 1:n) {
  p[[i]] <-
    ggplot() +
    geom_sf(data = PAT_extent, fill = "white") +
    geom_sf(data = results_per_sp_sf, 
            aes_string(fill = colnames(cave_PAT_c@data[,2:29])[i])) + 
    theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(0.9, 0.05),
          legend.text = element_text(size=15)) +
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


p[[2]]


# Restoration

n <- length(cave_PAT_r@data[,2:29])
g <- list()
results_per_sp_r_sf <- st_as_sf(cave_PAT_r)

for (i in 1:n) {
  g[[i]] <-
    ggplot() +
    geom_sf(data = PAT_extent, fill = "white") +
    geom_sf(data = results_per_sp_r_sf, 
            aes_string(fill = colnames(cave_PAT_r@data[,2:29])[i])) + 
    theme_bw() +
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
    scico::scale_fill_scico(palette = "lajolla") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(0.9, 0.05),
          legend.text = element_text(size=15)) +
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


g[[2]]


# Figures arrangement -----------------------------------------------------

p_arrange <-
  grid.arrange(p[[1]], p[[2]], g[[1]], g[[2]], nrow = 2)
ggsave(
  p_arrange,
  file = "./Figures/Final_results/Cave_c_r.tiff",
  height = 30,
  width = 40,
  units = "cm"
)
