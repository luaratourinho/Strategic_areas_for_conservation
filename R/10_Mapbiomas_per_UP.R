library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(memisc)

# Load polygon
nivel6 <- shapefile("./INEMA/Spatial_files_PAT/Join_results/PAT_n6_wgs_edsdm_idnv_diffuniao.shp")

# Load rasters
veg_cover <- raster("./INEMA/Spatial_files_PAT/Mapbiomas/PAT_nivel6_buf10km.tif")

# Add NA with is needed
# veg_cover_NA <- veg_cover
# veg_cover_NA[veg_cover_NA == 0] <- NA
# veg_cover_NA[!is.na(veg_cover_NA)] <- 1

#writeRaster(veg_cover_NA, "./PAT_territorio/veg_cover_NA.tif")

# Test with coarse resolution raster
# 1 km, i.e., res = 0.008333333, before was 0.0002694946
# res <- raster("./outputs/soil_ED/Acritopappus harleyi_ED.tif")
# res_crop <- crop(res,veg_cover)
# veg_cover_1km <- resample(veg_cover_NA, res_crop, method="bilinear")
# veg_cover_1km_2 <- veg_cover_1km
# veg_cover_1km_2[!is.na(veg_cover_1km_2)] <- 1
# veg_cover_1km_2[is.na(veg_cover_1km_2)] <- 0


# Percentage of habitat per feature ---------------------------------------

# Extract data (if you want to run for one: extract(raster(suitability, i), nivel6))

#veg_cover_crop <- crop(veg_cover,nivel6)

veg_cover_per_pol <- extract(veg_cover, nivel6)
#veg_cover_per_pol <- extract(veg_cover_1km_2, nivel6) #to test only

func_perc <- function(x) {
  (sum(x == 1) / length(x)) * 100
}

# func_perc(veg_cover_per_pol[[3]]) #to see only one
#resultado <- lapply(veg_cover_per_pol, func_perc) #could use lapply, but then you need organize the list later
percent_mapbiomas <- sapply(veg_cover_per_pol, func_perc)
percent_mapbiomas

# Add the percentage to the shapefile
nivel6@data <- cbind(nivel6@data, percent_mapbiomas) 

# Normalizing
range01  <- function(x){(x-min(x))/(max(x)-min(x))}
perc_mapbi_norm <- range01(percent_mapbiomas)
perc_mapbi_norm
nivel6@data <- cbind(nivel6@data, perc_mapbi_norm) 

# Save result
writeOGR(nivel6, dsn = "./INEMA/Spatial_files_PAT/Mapbiomas",
         layer = "PAT_n6_edsdm_diffuniao_mapbi", driver="ESRI Shapefile", overwrite=T)


# Ps.: the weight for restoration of this criterion I added by excel manually

# 19.4% to 0% less relevant --> 0.8 at max
values_1_122 <- seq(from=0, to=0.9, length = 119)
# 19.5 to 19.9 <- 0.9
# 40.4% a 20% very relevant --> 1
# 40.5 to 40.9 <- 0.9
# 60.4% a 41% relevant --> 0.8
# > 60.5% pouco relevante --> 0.7 at max
values_360_565 <- seq(from=0, to=0.8, length = 205)
NA_gap_123_259 <- rep("NA", times = 136)

values_add_to_table <- as.data.frame(cbind(values_1_122,NA_gap_123_259,values_360_565))
write.csv(values_add_to_table, "./INEMA/Spatial_files_PAT/Join_results/values_add_to_table.csv")

# Cut for PAT extension to plot ----------------------------------------

nivel6_PAT <- shapefile("./INEMA/Spatial_files_PAT/PAT_territorio/nivel6/PAT_ottonivel6_wgs84.shp")
nivel6_mapbi <- shapefile("./INEMA/Spatial_files_PAT/Join_results/PAT_n6_edsdm_diffuniao_mapbi_peso.shp")

#PAT_nivel6_Mapbiomas <- crop(nivel6, nivel6_PAT)
PAT_nivel6_Mapbiomas_peso <- crop(nivel6_mapbi, nivel6_PAT)


# Plot -----------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggplot2)
library("gridExtra")

# Habitat amount for conservation areas

nivel6_sf <- st_as_sf(PAT_nivel6_Mapbiomas_peso)

p <- ggplot(nivel6_sf) +
    geom_sf(aes_string(fill = "prcnt_m")) + 
  scale_fill_gradient(low = "white", high = "darkgreen")+
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
    labs(title = "Quantidade de habitat (%)\n") +
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


p 

# ggsave(
#   p,
#   file = "./INEMA/Spatial_files_PAT/Mapbiomas/Mapbiomas_0_100.tiff",
#   height = 20,
#   width = 26,
#   units = "cm"
# )


# Habitat amount weighted for restoration areas 

#nivel6_w_sf <- st_as_sf(PAT_nivel6_Mapbiomas_peso)

p2 <- ggplot(nivel6_w_sf) +
  geom_sf(aes_string(fill = "peso")) + 
  scale_fill_gradient(low = "white", high = "darkgreen")+
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
  labs(title = "Quantidade de habitat sob peso\n") +
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


p2 


p_arrange <-
  grid.arrange(p, p2, nrow = 1)
ggsave(
  #p,
  p_arrange,
  file = "./INEMA/Spatial_files_PAT/Mapbiomas/Figures/Mapbiomas_conserv_resto.tiff",
  height = 20,
  width = 26,
  units = "cm"
)

#other options
#scale_fill_stepsn(colours = terrain.colors(30), trans = "reverse")+
#scico::scale_fill_scico(palette = "tokyo") +

#Reference
#https://heima.hafro.is/~einarhj/education/ggplot2/scales.html