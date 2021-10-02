
# Required packages
library(dplyr)
library(tidyverse)

# Reading all sources
gbif_raw <- read_csv("./INEMA/Tabelas/occs/01_unclean_records_gbif.csv")
splink_raw <- read_csv("./INEMA/Tabelas/occs/01_unclean_records_specieslink.csv")
inema_list <- read_csv("INEMA/Tabelas/inema_list.csv")
table_MV_19 <- read_csv("INEMA/Tabelas/Avaliacoes_2018_2019.csv")
table_MV_14 <- read_csv("INEMA/Tabelas/pontos_ameacadas_portaria_443_2014.csv")
campo_sara <- read_csv("INEMA/Tabelas/sara_campo.csv")

# choosing only species, lon, lat
inema_list <- subset(inema_list[,-c(2,3,6,7,8)])

# creating source column
gbif_raw$source <- rep("gbif",nrow(gbif_raw))
splink_raw$source <- rep("splink",nrow(splink_raw))
inema_list$source <- rep("inema",nrow(inema_list))
table_MV_19$source <- rep("Avaliacoes_2018_jun2019",nrow(table_MV_19))
table_MV_14$source <- rep("portaria_443_2014",nrow(table_MV_14))
campo_sara$source <- rep("campo_sara",nrow(campo_sara))

# joining all table with all columns
sara_inema <- full_join(campo_sara, inema_list)
sara_inema_19 <- full_join(sara_inema, table_MV_19)
sara_inema_19_14 <- full_join(sara_inema_19, table_MV_14)
sara_inema_19_14_gbif <- full_join(sara_inema_19_14, gbif_raw)

# Changing column names
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

# joining the left table
sara_inema_19_14_gbif_splink <- full_join(sara_inema_19_14_gbif, splink_raw)

# reading species names
target_species <-
  read.csv(
    "./INEMA/Tabelas/occs/04_n_thinned_records_byme.csv",
    stringsAsFactors = FALSE,
    sep = ";"
  ) %>%
  pull(species)

# choosing only target species (because some table have more species)
sara_inema_19_14_gbif_splink_sp <- sara_inema_19_14_gbif_splink %>% 
  subset(species %in% target_species)

# saving
write_csv(sara_inema_19_14_gbif_splink_sp, file = "./INEMA/Tabelas/sara_inema_19_14_gbif_splink_fulljoin.csv")



# CNCFlora only -----------------------------------------------------------

# tables provided from CNCFlora
table_MV_19 <- read_csv("INEMA/Tabelas/Avaliacoes_2018_2019.csv")
table_MV_14 <- read_csv("INEMA/Tabelas/pontos_ameacadas_portaria_443_2014.csv")

# reading target species' names 
target_species <-
  read.csv(
    "./INEMA/Tabelas/occs/04_n_thinned_records_byme.csv",
    stringsAsFactors = FALSE,
    sep = ";"
  ) %>%
  pull(species)

# selecting only target species
table_MV_19_2 <- table_MV_19 %>% 
  subset(species %in% target_species)

# selecting only target species
table_MV_14_2 <- table_MV_14 %>% 
  subset(species %in% target_species)

# saving
write_csv(table_MV_19_2, file = "./INEMA/Tabelas/table_MV_19_2_PAT.csv")
write_csv(table_MV_14_2, file = "./INEMA/Tabelas/table_MV_14_2_PAT.csv")

# creating a table for one species (Hybanthus albus, to check later)
hy_al <- sara_inema_19_14_gbif_splink_sp %>% 
  subset(species %in% target_species[8])

# saving
write_csv(hy_al, file = "./INEMA/Tabelas/Hybanthus_albus.csv")

# joining CNCFlota tables
cncflora_valided <- full_join(table_MV_19_2,table_MV_14_2)

# removing Hybanthus albus from target species list (as Juliana Paula Souza will check it)
target_species_MV <- cncflora_valided %>% pull(species)
target_species_MV <- unique(target_species_MV)
target_species_MV <- target_species_MV[-10]

# selecting only target species without Hybanthus albus
cncflora_valided_no_hyal <- cncflora_valided %>% 
  subset(species %in% target_species_MV)

# checking the species list
unique(cncflora_valided_no_hyal$species)

# choosing only 3 columns
cncflora_valided_no_hyal_end <- cncflora_valided_no_hyal %>% 
  select(species, decimalLongitude, decimalLatitude)

# rename columns
colnames(cncflora_valided_no_hyal_end) <- c("species", "lon", "lat")

# saving CNCFlora list for target species without Hybanthus albus
write_csv(cncflora_valided_no_hyal_end, file = "./INEMA/Tabelas/cncflora_valided_no_hyal.csv")



# Checking lefting species ------------------------------------------------

all_sp <- read_csv("./INEMA/Tabelas/sara_inema_19_14_gbif_splink_fulljoin.csv")
cncflora_valided_fauna <- read_csv("./INEMA/Tabelas/Coordenadas_utilizadas_ENM/cncflora_valided_no_hyal_11sp_Ful_stu_fauna.csv")
hy_al <- read_csv("./INEMA/Tabelas/Coordenadas_utilizadas_ENM/Hybanthus_albus_Juliana_Paula_Souza.csv")

valided_sp <- full_join(cncflora_valided_fauna,hy_al)

write_csv(valided_sp, file = "./INEMA/Tabelas/valided_sp.csv")


# choosing only 3 columns
all_sp <- all_sp %>% 
  select(species, decimalLongitude, decimalLatitude)

# rename columns
colnames(all_sp) <- c("species", "lon", "lat")


valided_sp_unique <- as.vector(unique(valided_sp$species))
all_sp_unique <- unique(all_sp$species)

unique(valided_sp$species)

no_valid_sp <- subset(all_sp, !species %in% valided_sp_unique)

unique(no_valid_sp$species)

# run 3_geographical_cleaning_novalid
write_csv(no_valid_sp, file = "./INEMA/Tabelas/no_valid_sp.csv")

#creating final table to repot
novalid_sp <- read_csv("./INEMA/Tabelas/Coordenadas_utilizadas_ENM/novalid_thin.csv")
cncflora_valided_fauna <- read_csv("./INEMA/Tabelas/Coordenadas_utilizadas_ENM/cncflora_valided_no_hyal_11sp_Ful_stu_fauna_thin.csv")
hy_al <- read_csv("./INEMA/Tabelas/Coordenadas_utilizadas_ENM/Hybanthus_albus_Juliana_Paula_Souza.csv")

valid_only <- cncflora_valided_fauna %>%
  full_join(hy_al, by = c("species", "lon", "lat"))

valid_and_no_report <- valid_only %>%
  full_join(novalid_sp, by = c("species", "lon", "lat"))

write_csv(valid_and_no_report, path = "./INEMA/Tabelas/Coordenadas_utilizadas_ENM/all_sp_thin_to_report.csv")
