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
nivel6 <- shapefile("./PAT_territorio/PAT_ottonivel6_wgs84.shp")
PAT <- shapefile("./PAT_territorio/PAT_wgs84.shp")

# Load rasters
app <- raster("./APP/Recl_app_all_diss.tif")

# Reproject polygon
projection(app) <- projection(raster())
nivel6 <- spTransform(nivel6, projection(app))
PAT <- spTransform(PAT, projection(app))

app[is.na(app)] <-0
app <- mask(app, PAT)
app[app != 0 & !is.na(app)] <- 1
#plot(app)


app_per_pol <- extract(app, nivel6)
#app_per_pol <- extract(app_1km_2, nivel6) #to test only

func_perc <- function(x) {
  (sum(x == 1) / length(x)) * 100
}

# func_perc(app_per_pol[[3]]) #to see only one
#resultado <- lapply(app_per_pol, func_perc) #could use lapply, but then you need organize the list later
app_percent <- sapply(app_per_pol, func_perc)
app_percent

# Add the percentage to the shapefile
nivel6@data <- cbind(nivel6@data, app_percent) 

# Normalizing
range01  <- function(x){(x-min(x))/(max(x)-min(x))}
app_perc_norm <- range01(app_percent)
app_perc_norm
nivel6@data <- cbind(nivel6@data, app_perc_norm) 

# Save result
writeOGR(nivel6, dsn = "./INEMA/Spatial_files_PAT/Join_results/",
         layer = "PAT_n6_edsdm_mapb_peso_neig_fg_app", driver="ESRI Shapefile", overwrite=T)



# Plot -----------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggplot2)
library("gridExtra")

# Habitat amount for conservation areas

nivel6_sf <- st_as_sf(nivel6)

p <- ggplot(nivel6_sf) +
  geom_sf(aes_string(fill = "app_percent")) + 
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
  file = "./INEMA/APPs/Union_APPs/APPs_fig.tiff",
  height = 20,
  width = 26,
  units = "cm"
)