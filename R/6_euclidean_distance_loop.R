
# Function from Vilela et al. 2018 ----------------------------------------

# Authors: Bruno Vilela, Filipe Augusto Nascimento & Marcos Vinícius Carneiro Vital
# Tittle: Impacts of climate change on small-ranged amphibians of the northern Atlantic Forest
# Oecologia Australis 22(2) 2018
# doi: 10.4257/oeco.2018.2202.03

# Edited by Luara Tourinho (https://github.com/luaratourinho)

# Date: 10 Sep 2021


# Euclidean distance is a good option for rare species modelling ----------



# Required packages

library(dismo)
library(raster)
library(vegan)
source("./R/dist_euc.R")

# Before running the script below, run the function "Euclidean_distance_function.R"

# We can apply the function using the example data from the package dismo.
# So,first we load the data.

# Occurrence points
occs <- read.table('./04_clean_byme.csv', header=TRUE, sep=';')
sp.names <- as.character(unique(occs$species))
n <-length(sp.names)


# Predictors
fnames  <- list.files("./extent/pca_clim/pres", full.names = T, 'tif$')
predictors <- stack(fnames)


for(i in 1:n){
  
occ2 <- occs[occs$species == sp.names[i],]
occ <- occ2[,-1]

result <- dist_euc(occ, predictors, method = "mean", suitability = FALSE,
                   decostand.method = "standardize")


writeRaster(result, filename = paste0("./outputs/clim/", sp.names[i],
                                         "_ED_clim.tif"),
            format="GTiff", overwrite=T)

}
