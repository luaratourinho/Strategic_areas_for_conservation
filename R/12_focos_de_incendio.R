library(tidyverse)
library(sf)
library(ggplot2)
library("gridExtra")


# Focos de incêndio por UP ------------------------------------------------

# Read polygon
focos <- shapefile("./Fogo/Focos_por_UP_wgs84.shp")

# Normalizing -------------------------------------------------------------

range01  <- function(x){(x-min(x))/(max(x)-min(x))}
focos_norm <- range01(focos@data$NUMPOINTS)
focos_norm
focos@data <- cbind(focos@data, focos_norm) 

# Invert scale

library("spatialEco")
focos_norm_inv <- (((focos_norm - max(focos_norm)) * -1) + min(focos_norm))
names(focos_norm_inv) <- names(focos_norm)
foc_inv <- focos_norm_inv
focos@data <- cbind(focos@data, foc_inv)

names_cols <- c("NUNIV", "VERSA", "Shp_L",
                "Shp_A", "NUMPOIN", "focos_norm",
                "foc_inv")

names(focos)[] <- names_cols

# Save result
writeOGR(focos
, dsn = "./Fogo",
         layer = "Focos_por_UP_inv", driver="ESRI Shapefile", overwrite=T)


# save in genereal result shapefile (I did manually)
# nivel6 <- shapefile("./INEMA/Spatial_files_PAT/Join_results/PAT_n6_edsdm_difuniao_mapb_peso_neig.shp")
# 
# n_focos <- focos@data$NUMPOINTS
# nivel6@data <- cbind(nivel6@data, n_focos)
# nivel6@data <- cbind(nivel6@data, focos_norm)
# 
# # Save result
# writeOGR(nivel6, dsn = "./INEMA/Spatial_files_PAT/Join_results",
#          layer = "PAT_n6_edsdm_difuniao_mapb_peso_neig_fg", driver="ESRI Shapefile", overwrite=T)



# Plots -------------------------------------------------------------------

#focos <- shapefile("./INEMA/Spatial_files_PAT/Focos_incendio/focos_norm_wgs.shp")

focos_sf <- st_as_sf(focos)

p <- ggplot(focos_sf) +
  geom_sf(aes_string(fill = "NUMPOINTS")) + 
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
  #labs(title = "Focos de incêndio\n") +
  theme(plot.title = element_text(
    lineheight = .8,
    #face = "italic",
    size = 20
  ))+
  coord_sf(expand = T) +
  scale_x_continuous(breaks = -42:-39) +
  scale_y_continuous(breaks = -12:-14)


p 

ggsave(
  p,
  file = "./Fogo/Focos_por_UP_figure.tiff",
  height = 20,
  width = 26,
  units = "cm"
)


# Normalized

p2 <- ggplot(focos_sf) +
  geom_sf(aes_string(fill = "foc_inv")) + 
  scico::scale_fill_scico(palette = "lajolla", direction = -1) +
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
  #labs(title = "Focos de incêndio 0-1\n") +
  theme(plot.title = element_text(
    lineheight = .8,
    #face = "italic",
    size = 20
  ))+
  coord_sf(expand = T) +
  scale_x_continuous(breaks = -42:-39) +
  scale_y_continuous(breaks = -12:-14)


p2 

ggsave(
  p2,
  file = "./Fogo/Focos_por_UP_figure_norm_inv.tiff",
  height = 20,
  width = 26,
  units = "cm"
)

p_arrange <-
  grid.arrange(p, p2, nrow = 1)
ggsave(
  #p,
  p_arrange,
  file = "./Fogo/Focos_por_UP_figure_both.tiff",
  height = 20,
  width = 26,
  units = "cm"
)
