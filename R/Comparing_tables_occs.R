
# Credits ---------------------------

# Script created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 28 Aug 2021


# Comparing tables occs -------------------------------------------------



# Required packages
library(dplyr)
library(tidydiverse)

unclean_records_gbif <- read_csv("INEMA/Tabelas/old_cleaning/01_unclean_records_gbif.csv")
unclean_records_specieslink <- read_csv("INEMA/Tabelas/old_cleaning/01_unclean_records_specieslink.csv")

dataonline <- left_join(unclean_records_gbif, unclean_records_specieslink, by=c("binomial"="Scientific_name"))

clean_occs <- read_csv("INEMA/Tabelas/03_clean_occs.csv")

clean_occs_with_infos <- left_join(dataonline, clean_occs, by="blabla")

table_MV_19 <- read_csv("INEMA/Tabelas/Avaliacoes_2018_jun2019_MV.csv")
table_MV_14 <- read_csv("INEMA/Tabelas/pontos_ameacadas_atualizado_portaria_443_2014_MV.csv")



