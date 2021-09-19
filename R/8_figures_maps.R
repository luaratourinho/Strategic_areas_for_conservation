
library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)
library(ggmap)
library(ggplot2)
library(scico)
library("gridExtra")


# Reading files -----------------------------------------------------------

# For species that I ran Euclidean Distance

# Reading rasters

outputs_mask <-
  list.files("./INEMA/ENM/outputs/soil/crop_mask_PAT/wgs84",
             full.names = T,
             'tif$')
head(outputs_mask)
outputs_mask <- stack(outputs_mask)
outputs_mask <- outputs_mask[[-c(8,15,19,20)]]


# Reading species names

target_species <-
  read.csv(
    "./INEMA/Tabelas/occs/04_n_thinned_records_byme.csv",
    stringsAsFactors = FALSE,
    sep = ";"
  ) %>%
  pull(species)

target_species <- target_species[-c(8,15,19,20)]


# Figures -----------------------------------------------------

number_ticks <- function(n) {
  function(limits)
    pretty(limits, n)
}

#n <- outputs_mask@data@nlayers
n <- length(outputs_mask@layers)
p <- list()

for (i in 1:n) {
  map.p <- rasterToPoints(outputs_mask[[i]])
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Distância Euclidiana")
  
  p[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = MAP)) + theme_bw() +
    coord_equal() +
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
    theme(legend.justification = c(0.5, 0),legend.position = c(0.9, 0.05)) +
    labs(title = "Acritopappus harleyi\n") +
    theme(plot.title = element_text(
      lineheight = .8,
      face = "italic",
      size = 20
    )) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1),
                       breaks = number_ticks(3))
  
}

#(title = paste0(target_species[[i]] ,"\n")

# Figures arrangement -----------------------------------------------------


p_arrange <-
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow = 2)
ggsave(
  p_arrange,
  file = "./INEMA/ENM/outputs/soil/crop_mask_PAT/wgs84/fig_ED.tiff",
  height = 30,
  width = 42,
  units = "cm"
)

p_arrange <-
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]],
               p[[8]], p[[9]], p[[10]], p[[11]], p[[12]], nrow = 4)
ggsave(
  p_arrange,
  file = "./fig_ED.tiff",
  height = 70,
  width = 94,
  units = "cm"
)


p_arrange <- grid.arrange(p[[13]], p[[14]], p[[15]], p[[16]], p[[17]],
                          p[[18]], p[[19]], p[[20]], p[[21]], p[[22]], p[[23]], p[[24]], p[[25]],
                          p[[26]], p[[27]], nrow = 20)




# References --------------------------------------------------------------

#https://www.nrel.colostate.edu/this-is-how-i-did-it-mapping-in-r-with-ggplot2/
#https://ggplot2-book.org/scale-colour.html

#limites 0 a 1
#https://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2