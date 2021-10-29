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


# Reading data ------------------------------------------------------------

results_per_sp_r <- shapefile("./Final_results/Results_conser.shp")

# Plot --------------------------------------------------------------------

target_species <-
  read.csv(
    "./occs/n_records_to_report.csv",
    stringsAsFactors = FALSE) %>%
  pull(species)

target_species_ed <- target_species[-c(8,15,19,20)]
target_species <- c(target_species_ed,target_species[c(8,15,19,20)], "Todas as espécies")

# Restoration

n <- length(results_per_sp_r@data[,2:29])
g <- list()
results_per_sp_r_sf <- st_as_sf(results_per_sp_r)

for (i in 1:n) {
  g[[i]] <-
    ggplot(results_per_sp_r_sf) +
    geom_sf(aes_string(fill = colnames(results_per_sp_r@data[,2:29])[i])) + 
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


g[[2]]


# Figures arrangement -----------------------------------------------------

p_arrange <-
  grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]],
               g[[8]], nrow = 4)
ggsave(
  p_arrange,
  file = "./Figures/Final_results/Rest_1_8.tiff",
  height = 50,
  width = 30,
  units = "cm"
)

p_arrange2 <-
  grid.arrange(g[[9]], g[[10]], g[[11]], g[[12]], g[[13]], g[[14]], g[[15]], 
               g[[16]], nrow = 4)
ggsave(
  p_arrange2,
  file = "./Figures/Final_results/Rest_9_16.tiff",
  height = 50,
  width = 30,
  units = "cm"
)

p_arrange3 <-
  grid.arrange(g[[17]],g[[18]], g[[19]], g[[20]], g[[21]], g[[22]], g[[23]], nrow = 4)

ggsave(
  p_arrange3,
  file = "./Figures/Final_results/Rest_17_23.tiff",
  height = 50,
  width = 30,
  units = "cm"
)
