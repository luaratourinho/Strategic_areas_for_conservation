library(tidyverse)
library(sf)
library(ggplot2)
library("gridExtra")




# Cut for PAT extension to plot ----------------------------------------

nivel6_PAT <- shapefile("./INEMA/Spatial_files_PAT/PAT_territorio/nivel6/PAT_ottonivel6_wgs84.shp")

nivel6_PAT_results <- crop(nivel6, nivel6_PAT)

nivel6_sf <- st_as_sf(nivel6_PAT_results)


# Plot --------------------------------------------------------------------

# Habitat amount conservation areas

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
  #labs(title = "Quantidade de habitat (%)\n") +
  theme(plot.title = element_text(
    lineheight = .8,
    #face = "italic",
    size = 20
  ))+
  coord_sf(expand = T) +
  scale_x_continuous(breaks = -42:-39) +
  scale_y_continuous(breaks = -12:-14)
# scale_y_continuous(labels = scales::number_format(accuracy = 1),
#                    breaks = number_ticks(3))


p 


# Habitat amount weighted for restoration areas 

p2 <- ggplot(nivel6_sf) +
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
  #labs(title = "Quantidade de habitat sob peso\n") +
  theme(plot.title = element_text(
    lineheight = .8,
    #face = "italic",
    size = 20
  ))+
  coord_sf(expand = T) +
  scale_x_continuous(breaks = -42:-39) +
  scale_y_continuous(breaks = -12:-14)


p2 


# Habitat amount conservation areas multipled by neighbours

p3 <- ggplot(nivel6_sf) +
  geom_sf(aes_string(fill = "hab_conser")) + 
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
  #labs(title = "\nQuantidade de habitat das UP \ne suas vizinhas\n") +
  theme(plot.title = element_text(
    lineheight = .8,
    #face = "italic",
    size = 20
  ))+
  coord_sf(expand = T) +
  scale_x_continuous(breaks = -42:-39) +
  scale_y_continuous(breaks = -12:-14)

p3 


# Habitat amount weighted for restoration areas  multipled by neighbours


p4 <- ggplot(nivel6_sf) +
  geom_sf(aes_string(fill = "hab_restor")) + 
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
  #labs(title = "\nQuantidade de habitat das UP \nsob peso e suas vizinhas \n") +
  theme(plot.title = element_text(
    lineheight = .8,
    #face = "italic",
    size = 20
  ))+
  coord_sf(expand = T) +
  scale_x_continuous(breaks = -42:-39) +
  scale_y_continuous(breaks = -12:-14)

p4 


p_arrange <-
  grid.arrange(p, p2, p3, p4, nrow = 2)
ggsave(
  #p,
  p_arrange,
  file = "./INEMA/Spatial_files_PAT/Mapbiomas/Figures/Mapbiomas_conserv_resto_neig.tiff",
  height = 20,
  width = 26,
  units = "cm"
)
