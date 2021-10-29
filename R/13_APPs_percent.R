library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(memisc)
library(maptools)


# Polygon to raster -------------------------------------------------------

# Fiz no ArcGis com 0.003, pois por aqui deu erro
# poly <- shapefile("./INEMA/APPs/Union_APPs/APP_all_corrgeo.shp")
# raster_model <- raster("./INEMA/Spatial_files_PAT/Mapbiomas/PAT_extensao_original.tif")
# app <- rasterize(poly, raster_model)
# writeRaster(app, "./PAT_territorio/PAT_APPs_all.tif")

# Load polygon
nivel6_2 <- shapefile("./PAT_territorio/PAT_ottonivel6_wgs84.shp")
PAT <- shapefile("./PAT_territorio/PAT_wgs84.shp")

# Load rasters
app <- raster("./APP/Recl_app_all_diss.tif")

# Reproject polygon
projection(app) <- projection(raster())
nivel6_2 <- spTransform(nivel6_2, projection(app))
PAT <- spTransform(PAT, projection(app))

app[is.na(app)] <-0
app <- mask(app, PAT)
app[app != 0 & !is.na(app)] <- 1
plot(app)
plot(nivel6_2, border = "white")
plot(nivel6_2[130, ], col = "red", add = T) #polygon to small

app_per_pol <- extract(app, nivel6_2)
#app_per_pol <- extract(app_1km_2, nivel6_2) #to test only

func_perc <- function(x) {
  (sum(x == 1, na.rm=T) / length(x)) * 100
}

# func_perc(app_per_pol[[3]]) #to see only one
app_perc <- sapply(app_per_pol, func_perc)
app_perc

# Add the percentage to the shapefile
nivel6_2@data <- cbind(nivel6_2@data, app_perc) 

# Normalizing
range01  <- function(x){(x-min(x))/(max(x)-min(x))}
app_norm <- range01(app_perc)
app_norm
nivel6_2@data <- cbind(nivel6_2@data, app_norm)
# nivel6_2@data <- cbind(nivel6_2@data, app_perc_norm, app_percent) 
# nivel6_2@data$app_percent <- app_percent

# Save result
writeOGR(nivel6_2, dsn = "./APP/",
         layer = "app_perc_norm", driver="ESRI Shapefile", overwrite=T)



# Plot -----------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggplot2)
library("gridExtra")

# Habitat amount for conservation areas

nivel6_2_sf <- st_as_sf(nivel6_2)

p <- ggplot(nivel6_2_sf) +
  geom_sf(aes_string(fill = "app_norm")) + 
  scale_fill_gradient(low = "white", high = "darkblue")+
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
  file = "./APP/APPs_fig.tiff",
  height = 20,
  width = 26,
  units = "cm"
)
