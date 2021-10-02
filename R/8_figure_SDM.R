
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
  list.files("./outputs/crop_PAT/SDM",
             full.names = T,
             'tif$')
head(outputs_mask)
outputs_mask <- stack(outputs_mask)

# Reading species names

target_species <-
  read.csv(
    "./occs/n_records_to_report.csv",
    stringsAsFactors = FALSE) %>%
  pull(species)

target_species <- target_species[c(8,15,19,20)]


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
  colnames(df) <- c("Longitude", "Latitude", "Adequabilidade ambiental")
  
  p[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Adequabilidade ambiental`)) + theme_bw() +
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
    scale_y_continuous(labels = scales::number_format(accuracy = 1),
                       breaks = number_ticks(3))
  
}


p[[1]]


# Figures arrangement -----------------------------------------------------

p_arrange <-
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2)
ggsave(
  p_arrange,
  file = "./Figures/SDM/SDM_1_4.tiff",
  height = 20,
  width = 26,
  units = "cm"
)

p_arrange <-
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]],
               p[[8]], nrow = 4)
ggsave(
  p_arrange,
  file = "./Figures/SDM/ED_1_8.tiff",
  height = 50,
  width = 30,
  units = "cm"
)


# References --------------------------------------------------------------

#https://www.nrel.colostate.edu/this-is-how-i-did-it-mapping-in-r-with-ggplot2/
#https://ggplot2-book.org/scale-colour.html

#limites 0 a 1
#https://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2