
# Credits ---------------------------

# Script created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 28 Aug 2021


# Comparing tables occs -------------------------------------------------



# Required packages
library(dplyr)
library(tidyverse)

unclean_records_gbif <- read_csv("INEMA/Tabelas/occs/01_unclean_records_gbif.csv")
unclean_records_specieslink <- read_csv("INEMA/Tabelas/occs/01_unclean_records_specieslink.csv")
inema_list <- read_csv("INEMA/Tabelas/occs/inema_list.csv")
clean_occs <- read_csv("INEMA/Tabelas/occs/03_clean_df_thin_1.csv")
# Tables from Marcio Verdi
table_MV_19 <- read_csv("INEMA/Tabelas/Avaliacoes_2018_jun2019_MV.csv")
table_MV_14 <- read_csv("INEMA/Tabelas/pontos_ameacadas_atualizado_portaria_443_2014_MV.csv")


# Creating a common column ------------------------------------------------


#cleaned <- clean_occs %>% unite("lonlat", lon:lat, remove = FALSE)
#unite("xy", x:y)
#separate(xy, c("x", "y"))

#using mutate

gbif <- unclean_records_gbif %>% 
  mutate("splonlat" = paste(species, decimalLongitude, decimalLatitude, sep = "_"))
gbif$splonlat
gbif$source <- rep("gbif",nrow(gbif))

splink <- unclean_records_specieslink %>% 
  mutate("splonlat" = paste(scientificName, decimalLongitude, decimalLatitude, sep = "_"))
splink$source <- rep("splink",nrow(splink))

inemaoccs <- inema_list %>% 
  mutate("splonlat" = paste(species, lon, lat, sep = "_"))
inemaoccs$source <- rep("inema",nrow(inemaoccs))

cleaned <- clean_occs %>% 
  mutate("splonlat" = paste(species, lon, lat, sep = "_"))
cleaned$source <- rep("cleaned",nrow(cleaned))

MV_19 <- table_MV_19 %>% 
  mutate("splonlat" = paste(specie, longitude, latitude, sep = "_"))
MV_19$source <- rep("Avaliacoes_2018_jun2019",nrow(MV_19))

MV_14 <- table_MV_14 %>% 
  mutate("splonlat" = paste(nome_cient, POINT_X, POINT_Y, sep = "_"))
MV_14$source <- rep("portaria_443_2014",nrow(MV_14))

# Joying tables ------------------------------------------------------------


cleaned_inema <- left_join(cleaned, inemaoccs, by="splonlat")
sum(duplicated(cleaned_inema$splonlat))
#which(duplicated(cleaned_inema$splonlat))
cleaned_inema_nodupl <- cleaned_inema[!duplicated(cleaned_inema$splonlat),]
write.csv(cleaned_inema, "./INEMA/Tabelas/conferindo_nomes/cleaned_inema.csv")

cleaned_inema_nodupl2 <- cleaned_inema_nodupl %>% 
  mutate("sources" = paste(source.x, source.y, sep = "_"))

#saving table with all duplicates
cleaned_inema_gbif0 <- left_join(cleaned_inema, gbif, splink, by="splonlat")
write.csv(cleaned_inema_gbif0, "./INEMA/Tabelas/conferindo_nomes/cleaned_inema_gbif_splink.csv")


cleaned_inema_gbif <- left_join(cleaned_inema_nodupl2, gbif, by="splonlat")
cleaned_inema_gbif1 <- cleaned_inema_gbif %>% 
  mutate("sources2" = paste(sources, source, sep = "_"))

sum(duplicated(cleaned_inema_gbif1$splonlat))
which(duplicated(cleaned_inema_gbif1$splonlat))
cleaned_inema_gbif_nodupl <- 
  cleaned_inema_gbif1[!((duplicated(cleaned_inema_gbif1$splonlat) | 
                          duplicated(cleaned_inema_gbif1$splonlat, fromLast = TRUE)) &
                          cleaned_inema_gbif1$source =="gbif"), ]


cleaned_inema_gbif_splink <- left_join(cleaned_inema_gbif_nodupl, splink, by="splonlat")
cleaned_inema_gbif_splink1 <- cleaned_inema_gbif_splink %>% 
  mutate("sources3" = paste(sources2, source.y.y, sep = "_"))

sum(duplicated(cleaned_inema_gbif_splink1$splonlat))
which(duplicated(cleaned_inema_gbif_splink1$splonlat))
cleaned_inema_gbif_splink_nodupl <- 
  cleaned_inema_gbif_splink1[!duplicated(cleaned_inema_gbif_splink1$splonlat),]
write.csv(cleaned_inema_gbif_splink_nodupl, "./INEMA/Tabelas/conferindo_nomes/cleaned_inema_gbif_splink_nodupl.csv")

cleaned_inema_gbif_splink_nodupl_MV_19 <- left_join(cleaned_inema_gbif_splink_nodupl,MV_19, by="splonlat")
cleaned_tdsMV_19 <- cleaned_inema_gbif_splink_nodupl_MV_19 %>% 
  mutate("sources4" = paste(sources3, source, sep = "_"))

sum(duplicated(cleaned_tdsMV_19$splonlat))
#which(duplicated(cleaned_tdsMV_19$splonlat))
#cleaned_tdsMV_19_nodupl <- cleaned_tdsMV_19[!duplicated(cleaned_tdsMV_19$splonlat),]

cleaned_tdsMV_19_14 <- left_join(cleaned_tdsMV_19,MV_14, by="splonlat")
cleaned_tds_MV_19_14 <- cleaned_tdsMV_19_14 %>% 
  mutate("sources5" = paste(sources4, source.y.y.y, sep = "_"))

sum(duplicated(cleaned_tds_MV_19_14$splonlat))
which(duplicated(cleaned_tds_MV_19_14$splonlat))
cleaned_tds_MV_19_14_nodupl <- cleaned_tds_MV_19_14[!duplicated(cleaned_tds_MV_19_14$splonlat),]
write_csv(x = cleaned_tds_MV_19_14_nodupl, file = "./INEMA/Tabelas/conferindo_nomes/cleaned_tds_MV_19_14_nodupl.csv")

colnames(cleaned_tds_MV_19_14_nodupl)
#cleaned_final <- cleaned_tds_MV_19_14_nodupl[,!c("source.x")]
#cleaned_final <- subset(cleaned_tds_MV_19_14_nodupl, select = -get("source.x"))
cleaned_final <- cleaned_tds_MV_19_14_nodupl %>% 
  select(-c(splonlat, source.x,species.y,lon.y,lat.y,family.x,source.y,sources,
            year.x,country.x,scientificName.x,infraspecificEpithet,decimalLatitude.x,
            decimalLongitude.x,coordinateUncertaintyInMeters,coordinatePrecision.x,
            depth,depthAccuracy,establishmentMeans,genus.x,species,source.x.x,
            sources2,kingdom.y,class.y,order.y,family.x.x,genus.y,specificEpithet,
            decimalLongitude.y,decimalLatitude.y,phylum.y,fieldNumber,
            previousCatalogNumber,sex,source.y.y,sources3,preccncflo,
            family.y.y,specie,longitude,latitude,source.x.x.x,sources4,familia,
            POINT_Y,POINT_X,nome_cient,autor, categoria.y, source.y.y.y))

write_csv(x = cleaned_final, file = "./INEMA/Tabelas/conferindo_nomes/cleaned_final.csv")





# MV_19_test <-MV_19
# colnames(MV_19_test) <- c("family","species","id","col_code","catalog_n","recordedby",
#                     "record_n","year_","month_","day_","state","city","locality",
#                     "longitude","latitude","precision_","protocol","obs__de_SI",
#                     "categoria","projeto","splonlat","source")
