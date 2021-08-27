# Credits ---------------------------

# Script created by
# Aiello-Lammens et al. (https://cran.r-project.org/web/packages/spThin/spThin.pdf)
# Aiello-Lammens et al. 2015 (doi: 10.1111/ecog.01132)

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)
# and 
# Bruno Carvalho (https://github.com/brunomc-eco)

# Date: 01 abr 2021



# Geographical cleaning using spThin package ------------------------------



# Required packages

library(spThin) # Aiello-Lammens et al. 2015
library(tidyverse)
library(data.table)


# loading clean occs
clean_df <- read_csv("./occs/02_clean_occ_gspmy.csv")


# getting clean species list
spp <- sort(unique(clean_df$species))

# Example of two distances in km
# Run to two distances you are interested in, then choose one option to proceed

# thinning records by 5 km
thin_5 <- list()
for(i in 1:length(spp)){
  df <- clean_df %>%
    filter(species %in% spp[i])
  thinned <- thin(df,
                  lat.col = "lat",
                  long.col = "lon",
                  spec.col = "species",
                  thin.par = 5, # distance in km
                  reps = 1,
                  locs.thinned.list.return = TRUE,
                  write.files = FALSE,
                  write.log.file = FALSE)
  thin_5[[i]] <- data.frame(species = rep(spp[i], nrow(thinned[[1]])),
                            lon = thinned[[1]]$Longitude,
                            lat = thinned[[1]]$Latitude)
}
clean_df_thin_5 <- rbindlist(thin_5)


# thinning records by 1 km
thin_1 <- list()
for(i in 1:length(spp)){
  df <- clean_df %>%
    filter(species %in% spp[i])
  thinned <- thin(df,
                  lat.col = "lat",
                  long.col = "lon",
                  spec.col = "species",
                  thin.par = 1, # distance in km
                  reps = 1,
                  locs.thinned.list.return = TRUE,
                  write.files = FALSE,
                  write.log.file = FALSE)
  thin_1[[i]] <- data.frame(species = rep(spp[i], nrow(thinned[[1]])),
                             lon = thinned[[1]]$Longitude,
                             lat = thinned[[1]]$Latitude)
}
clean_df_thin_1 <- rbindlist(thin_1)


# Check thinned records
ggplot() +
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = clean_df_thin_5, aes(x = lon, y = lat),
             colour = "blue", size = 1.5) +
  geom_point(data = clean_df_thin_1, aes(x = lon, y = lat),
             colour = "red", size = 1.0) +
  coord_sf(xlim = c(-100, -28), ylim = c(-50,0)) +
  theme_bw()


# counting records by species
n_5 <- clean_df_thin_5 %>%
  group_by(species) %>%
  summarize(n_thin_5 = n())

n_1 <- clean_df_thin_1 %>%
  group_by(species) %>%
  summarize(n_thin_1 = n())


# adding counts to the n_records table
n_records <- read_csv("./occs/02_n_records_gspmy.csv")
mylist2  <- read_csv("./occs/inema_list.csv")
#n_inema <- count(mylist2, species)
n_inema <- mylist2 %>%
  group_by(species) %>%
  summarize(n_inema = n())

n_records <- n_records %>%
  left_join(n_5, by = "species") %>%
  left_join(n_1, by = "species") %>%
  replace_na(list(n_thin_5 = 0, n_thin_1 = 0))

n_records2 <- n_records %>%
  left_join(n_inema, by = "species")

n_records3 <- n_records2[,c(1,5,2,4,3)]

# writing outputs
write_csv(n_records3, path = "./occs/03_n_thinned_records.csv")
write_csv(clean_df_thin_5, path = "./occs/03_clean_df_thin_5.csv")
write_csv(clean_df_thin_1, path = "./occs/03_clean_df_thin_1.csv")
