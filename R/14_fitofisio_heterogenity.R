library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(memisc)
library(vegan)
library(tibble)

# Load polygon
nivel6 <- shapefile("./PAT_territorio/PAT_ottonivel6_wgs84.shp")

# Load rasters
fito <- raster("./fitofisio/cobertura_vegetal_nivel4_NA_wgs.tif")


# Percentage of habitat per feature ---------------------------------------

#species <- unique(fito[])
fito_per_pol <- extract(fito, nivel6)

df <- enframe(fito_per_pol)


# Creating table of habitat abundance per ottobacia 

# Creating empty matrix
sps = 15 # species number
n <- nrow(df) # ottobacia number
pam <- matrix(nrow = n, ncol = sps) # pam = presece absence matrix
colnames(pam) <- paste0("fito", 1:sps)

# Filling abundance table
for (i in 1:n) {
  pam[i, ] <- table(factor(df[i, 2][[1]][[1]], levels =  1:sps))
}
View(pam)

# Calculating diversity indexes 

richness <- specnumber(pam)
shannon <- diversity(pam) # Shannon I is the default
simpson <- diversity(pam, "simpson")

# richness equal to 0 or 1 had shannon equal to 0:
plot(x = specnumber(pam), y = shannon)

# Add results to shapefile
nivel6@data <- cbind(nivel6@data, richness)
nivel6@data <- cbind(nivel6@data, simpson)
nivel6@data <- cbind(nivel6@data, shannon) 

# Saving results in the shapefile
writeOGR(nivel6, dsn = "./fitofisio",
         layer = "fito_n6_divers", driver="ESRI Shapefile", overwrite=T)


# Different weight for different domains ----------------------------------

# Load polygon
caatinga <- shapefile("./PAT_territorio/nivel6_CAA.shp")
mataatl <- shapefile("./PAT_territorio/nivel6_MA.shp")

nivel6_CAA <- crop(nivel6, caatinga)
nivel6_MA <- crop(nivel6, mataatl)

pos_CAA <- match(nivel6_CAA$NUNIVOTTO6, nivel6$NUNIVOTTO6)
pos_MA <- match(nivel6_MA$NUNIVOTTO6, nivel6$NUNIVOTTO6)

# Normalizing
range01  <- function(x){(x-min(x))/(max(x)-min(x))}

shan_norm_CAA <- range01(nivel6_CAA$shannon)
shan_norm_CAA
shan_norm_MA <- range01(nivel6_MA$shannon)
shan_norm_MA

nivel6@data$shan_w_int <- NA
nivel6@data$shan_w_int[pos_CAA] <-  shan_norm_CAA
nivel6@data$shan_w_int[pos_MA] <-  shan_norm_MA

# Saving results in the shapefile
writeOGR(nivel6, dsn = "./fitofisio",
         layer = "fito_n6_divers_w_int", driver="ESRI Shapefile", overwrite=T)



# Plot --------------------------------------------------------------------


library(tidyverse)
library(sf)
library(ggplot2)
library("gridExtra")
library(colourpicker)

# Habitat amount for conservation areas

nivel6_sf <- st_as_sf(nivel6)

p <- ggplot(nivel6_sf) +
  geom_sf(aes_string(fill = "shan_w_int")) + 
  scale_fill_gradient(low = "white", high = "#CD6889")+
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
  file = "./fitofisio/shannon_fig.tiff",
  height = 20,
  width = 26,
  units = "cm"
)
