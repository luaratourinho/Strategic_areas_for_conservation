
# Required packages
library(dplyr)
library(tidyverse)

gbif_raw <- read_csv("./INEMA/Tabelas/occs/01_unclean_records_gbif.csv")
splink_raw <- read_csv("./INEMA/Tabelas/occs/01_unclean_records_specieslink.csv")
inema_list <- read_csv("INEMA/Tabelas/inema_list.csv")
table_MV_19 <- read_csv("INEMA/Tabelas/Avaliacoes_2018_2019.csv")
table_MV_14 <- read_csv("INEMA/Tabelas/pontos_ameacadas_portaria_443_2014.csv")
campo_sara <- read_csv("INEMA/Tabelas/sara_campo.csv")

inema_list <- subset(inema_list[,-c(2,3,6,7,8)])

gbif_raw$source <- rep("gbif",nrow(gbif_raw))
splink_raw$source <- rep("splink",nrow(splink_raw))
inema_list$source <- rep("inema",nrow(inema_list))
table_MV_19$source <- rep("Avaliacoes_2018_jun2019",nrow(table_MV_19))
table_MV_14$source <- rep("portaria_443_2014",nrow(table_MV_14))
campo_sara$source <- rep("campo_sara",nrow(campo_sara))

sara_inema <- full_join(campo_sara, inema_list)
sara_inema_19 <- full_join(sara_inema, table_MV_19)
sara_inema_19_14 <- full_join(sara_inema_19, table_MV_14)
sara_inema_19_14_gbif <- full_join(sara_inema_19_14, gbif_raw)

colnames(splink_raw) <- c("record_id","modified","institutionCode","collectionCode",
                "catalogNumber","basisOfRecord","kingdom","class","order",
                "family","genus","specificEpithet","scientificName",
                "scientificNameAuthorship","recordedBy","year","month_2","day_2",
                "continentOcean","country","locality","decimalLongitude",
                "decimalLatitude","verbatimLongitude","verbatimLatitude",
                "occurrenceRemarks","barcode","imagecode","typeStatus",
                "stateProvince","relatedCatalogItem","identifiedBy","phylum",
                "recordNumber","county","minimumElevationInMeters",
                "maximumElevationInMeters","yearIdentified","monthIdentified",
                "coordinatePrecision","geoFlag","dayIdentified","preparationType",
                "individualCount","fieldNumber","previousCatalogNumber","sex","source")

sara_inema_19_14_gbif_splink <- full_join(sara_inema_19_14_gbif, splink_raw)

write_csv(sara_inema_19_14_gbif_splink, file = "./INEMA/Tabelas/sara_inema_19_14_gbif_splink_fulljoin.csv")