
# Credits ---------------------------

# Created by
# Sara Mortara (https://github.com/saramortara/data_cleaning)
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 04 May 2021


# Cleaning records --------------------------------------------------------



# Required packages

library(tidyverse)
library(CoordinateCleaner)
library(countrycode)



# reading data

splist <- c("Ybyrapora gamba","Coarazuphium cessaima","Metagonia diamantina",
            "Helictere srufipila","Hybanthus albus","Ilex auricula",
            "Mandevilla hatschbachii","Melochia illicioides","Micranthocereus streckeri",
            "Microlicia subalata","Oocephalus nubicola", "Passiflora timboensis",
            "Pavonia palmeirenss", "Philcoxia bahiensis","Piriqueta flammea",
            "Rayleya bahiensis","Sinningia macrophylla", "Stylotrichium glomeratum",
            "Trichogoniopsis morii","Acritopappus harleyi","Bauhinia glaziovii",
            "Ormosia timboenses" ,"Senegalia ricoae","Fulcaldea stuessyi")

gbif_specieslink  <- read_csv("./occs/01_search_refined_results.csv")
mylist  <- read_csv("./occs/inema_list.csv")
searches_df  <- rbind(mylist,gbif_specieslink)

# removing records with NA coordinates, keeping only species from our list
searches_occs <- searches_df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  filter(species %in% splist)


# Viewing unclean records
ggplot()+ coord_fixed()+
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = searches_occs, aes(x = lon, y = lat),
             colour = "yellow", size = 0.5)+
  theme_bw()

# standardizing country names
searches_occs$countrycode <- countrycode(searches_occs$country, origin = 'iso2c', destination = 'iso3c')
searches_occs$countrycode <- ifelse(is.na(searches_occs$countrycode), 
                                    "BRA", searches_occs$countrycode)

# NA <- 0
searches_occs$lon <- ifelse(is.na(searches_occs$lon), 
                            0, searches_occs$lon)

flags_occs <- clean_coordinates(
  x = searches_occs,
  lon = "lon",
  lat = "lat",
  countries = "countrycode",
  centroids_rad = 2000,
  # had to increase this limit because was not flagging the centroid of Brazil
  species = "species",
  tests = c(
    "capitals",
    # flags records at adm-0 capitals
    "centroids",
    # flags records at country centroids
    "equal",
    # flags records with equal lon and lat
    "gbif",
    # flags records at gbif headquarters
    "institutions",
    # flags records at biodiversity institutions
    "seas",
    # flags records at sea
    "zeros"
  )
) # flags records with zero lon or lat

# Viewing flagged records
plot(flags_occs, lon = "lon", lat = "lat")

# Removing flagged records and duplicates
searches_occs_clean1 <- searches_occs[flags_occs$.summary, ] %>%
  distinct()

mylist2  <- read.csv("./occs/inema_list.csv")
searches_occs_clean2 <- as.data.frame(searches_occs_clean1)
gbifsplinkmydate  <- as.tibble(rbind(mylist2,searches_occs_clean2[,-9]))


# Cleaning by worldclim files ------------------------------------------------

# library(raster)
# 
# searches_occs_clean2 = searches_occs_clean1
# 
# variable_world <- raster("D:/Luara Tourinho/OneDrive/Documentos/Felidae/data/env_cropped/present_SA_NA/_wc2.1_2.5m_bio_1.tif")
# # All Americas
# # variable <- crop(variable_world, c(-160, -28, -60, 90))
# # Central and South Americas
# variable <- crop(variable_world, c(-160, -28, -60, 35))
# coordinates(searches_occs_clean2) <- ~lon+lat
# proj4string(searches_occs_clean2)=CRS("+proj=longlat +datum=WGS84")
# searches_prj<-spTransform(searches_occs_clean2,
#                           CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(variable, axes=T)
# plot(searches_prj, add=T, col= "red")
# 
# varcrop = crop(variable, searches_prj)
# 
# searches_extract <- raster::extract(varcrop, searches_prj, method = "bilinear")
# searches_prjext<- data.frame(searches_prj,searches_extract)
# 
# which(searches_prjext$searches_extract ==0)
# which(is.na(searches_prjext$searches_extract))
# search_occ_extracted <- searches_prjext[!is.na(searches_prjext$searches_extract),]
# head(search_occ_extracted)
# 
# search_occ_extracted <- search_occ_extracted[,c("family", "species", "lon", 
#                                                 "lat", "year", "country", "scientificName")]
# head(search_occ_extracted)
# dim(search_occ_extracted)
# 
# 
# 
# # Cleaning non-endemic species (example) ----------------------------------
# 
# 
# # which species have records in other continent outside Americas? (lon < -20)
# 
# clean_df = search_occ_extracted
# 
# western_species <- clean_df %>%
#   mutate(western = lon <= -20) %>%
#   filter(western == TRUE) %>%
#   #group_by(species, western) %>%
#   #summarize(n_records = n()) %>%
#   pull(species) %>%
#   unique() %>%
#   tibble() %>%
#   rename("westernmost_species" = ".") %>%
#   arrange(westernmost_species)
# 
# 
# # removing these species from clean_df
# `%notin%` <- Negate(`%in%`)
# 
# clean_df <- clean_df %>%
#   filter(species %notin% as.character(western_species$westernmost_species))
# 
# 
# # plotting clean records # All Americas
# # ggplot() +
# #   borders("world", colour="gray80", fill="gray80") +
# #   geom_point(data = clean_df, aes(x = lon, y = lat),
# #              colour = '#d95f02', size = 1.5) +
# #   coord_sf(xlim = c(-100, -30), ylim = c(-60,20)) +
# #   theme_bw()
# 
# g <- ggplot() +
#   borders("world", colour="gray80", fill="gray80") +
#   geom_point(data = clean_df, aes(x = lon, y = lat, col = species),
#              colour = '#7570b3', size = 1.5) +
#   coord_sf(xlim = c(-85, -30), ylim = c(-35,-5)) +
#   theme_classic() +
#   xlab("") +
#   ylab("") +
#   theme(
#     strip.text = element_text(face = "italic"),
#     legend.text = element_text(face = "italic"),
#     legend.position = "none")
# theme_bw()
# g1 <- g + facet_wrap(species ~ ., ncol = 5)
# g1
# 
# ggsave(g1, file = "./Figure_occurrences2.tiff",height = 15, 
#        width = 22, units = "cm")

# Cleaning old date -------------------------------------------------------

search_occ_by_date <- searches_occs_clean1 %>%
  filter(year >= 1950)

# Number of records -------------------------------------------------------

n_records <- count(search_occ_by_date, species)
n_records_before <- count(searches_occs_clean1, species)
n_gbifsplinkmydate <- count(gbifsplinkmydate, species)
n_mylist2 <- count(mylist2, species)

# Writing outputs ---------------------------------------------------------

write_csv(n_records, path = "./occs/02_n_records_bydate.csv")
write_csv(search_occ_by_date, path = "./occs/02_clean_occ_bydate.csv")

write_csv(n_records_before, path = "./occs/02_n_records.csv")
write_csv(searches_occs_clean1, path = "./occs/02_clean_occ.csv")

write_csv(n_gbifsplinkmydate, path = "./occs/02_n_records_gspmy.csv")
write_csv(gbifsplinkmydate, path = "./occs/02_clean_occ_gspmy.csv")

