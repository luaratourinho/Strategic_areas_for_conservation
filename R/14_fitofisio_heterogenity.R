library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(memisc)
library(vegan)

# Load polygon
nivel6 <- shapefile("./PAT_territorio/PAT_ottonivel6_wgs84.shp")

# Load rasters
fito <- raster("./fitofisio/cobertura_vegetal_nivel4_NA_wgs.tif")


# Percentage of habitat per feature ---------------------------------------

fito_per_pol <- extract(fito, nivel6)

#species <- unique(fito[])

richness_I <- function(x){specnumber(x)}
richness <- sapply(fito_per_pol, richness_I)
richness

shannon_I <- function(x){
  abund_total <- sum(x)
  pi <- x/abund_total
  ln_pi <- log(pi)
  pi_mult <- pi* ln_pi
  result <- -sum(pi_mult)
}
shannon <- sapply(fito_per_pol, shannon_I)
shannon

simpson_I <- function(x){
  abund_total <- sum(x)
  pi <- x/abund_total
  result2 <- 1- sum(pi^2)
  return(result2)
}
simpson <- sapply(fito_per_pol, simpson_I)
simpson

# Add the percentage to the shapefile
nivel6@data <- cbind(nivel6@data, richness)
nivel6@data <- cbind(nivel6@data, shannon) 
nivel6@data <- cbind(nivel6@data, simpson)

# Saving results in the shapefile
writeOGR(nivel6, dsn = "./fitofisio",
         layer = "fitofisio_nivel6", driver="ESRI Shapefile", overwrite=T)



# Different weight for different domains ----------------------------------

# Load polygon
caatinga <- shapefile("./PAT_territorio/nivel6_CAA.shp")
mataatl <- shapefile("./PAT_territorio/nivel6_MA.shp")

nivel6_CAA <- crop(nivel6, caatinga)
nivel6_MA <- crop(nivel6, mataatl)

# Normalizing
range01  <- function(x){(x-min(x))/(max(x)-min(x))}
shan_norm_CAA <- range01(nivel6_CAA@data$shannon)
shan_norm_CAA
nivel6@data <- cbind(nivel6@data, shan_norm_CAA) 

range01  <- function(x){(x-min(x))/(max(x)-min(x))}
shan_norm_MA <- range01(nivel6_MA@data$shannon)
shan_norm_MA
nivel6@data <- cbind(nivel6@data, shan_norm_MA) 

shan_norm_w  <- rbind(shan_norm_CAA, shan_norm_MA)
nivel6@data <- cbind(nivel6@data, shan_norm_w ) 

# Saving results in the shapefile
writeOGR(nivel6, dsn = "./fitofisio",
         layer = "fitofisio_nivel6_w", driver="ESRI Shapefile", overwrite=T)


# Join data ---------------------------------------------------------------

#Aqui juntar todas colunas no nivel6 que acaba com fogo que foi o ultimo


# Plot --------------------------------------------------------------------


library(tidyverse)
library(sf)
library(ggplot2)
library("gridExtra")

# Habitat amount for conservation areas

nivel6_sf <- st_as_sf(nivel6)

p <- ggplot(nivel6_sf) +
  geom_sf(aes_string(fill = "shannon")) + 
  #scale_fill_gradient(low = "white", high = "darkgreen")+
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
  #labs(title = "Quantidade de habitat (%)\n") +
  theme(plot.title = element_text(
    lineheight = .8,
    face = "italic",
    size = 20
  ))+
  coord_sf(expand = T) +
  scale_x_continuous(breaks = -42:-39) +
  scale_y_continuous(breaks = -12:-14)

p 

ggsave(
  p,
  file = "./INEMA/APPs/Union_APPs/APPs_fig.tiff",
  height = 20,
  width = 26,
  units = "cm"
)