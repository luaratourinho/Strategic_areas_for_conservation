# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 16 sep 2021


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)

# Cropping outputs to interested area ---------------------------------------

mypolygon <- readOGR(dsn="./INEMA/arquivos felipe/Territorio_PAT", layer="territorio") 

crs.wgs84 <-
  CRS("+proj=longlat +datum=WGS84 +no_defs")
mypolygon_wgs84 <- spTransform(mypolygon, crs.wgs84)


# Reading shape to crop ---------------------------------------------------

# Euclidean Distance results

outputs <- list.files("./INEMA/ENM/outputs/soil", 
                     full.names = T, 'tif$')
head(outputs)
outputs_st <- stack(outputs)
outputs_e <- crop(outputs_st, mypolygon_wgs84)
outputs_mask <- mask(outputs_e, mypolygon_wgs84)

# SDM results

sp1 <- raster("./INEMA/ENM/outputs/models/Ensemble_species/Hybanthus_albus_TSSmax_ensemble_weighted_average.tif")
sp1 <- crop(sp1, mypolygon_wgs84)
sp1 <- mask(sp1, mypolygon_wgs84)

sp2 <- raster("./INEMA/ENM/outputs/models/Ensemble_species/Oocephalus_nubicola_TSSmax_ensemble_weighted_average.tif")
sp2 <- crop(sp2, mypolygon_wgs84)
sp2 <- mask(sp2, mypolygon_wgs84)

sp3 <- raster("./INEMA/ENM/outputs/models/Ensemble_species/Philcoxia_bahiensis_TSSmax_ensemble_weighted_average.tif")
sp3 <- crop(sp3, mypolygon_wgs84)
sp3 <- mask(sp3, mypolygon_wgs84)

sp4 <- raster("./INEMA/ENM/outputs/models/Ensemble_species/Piriqueta_flammea_TSSmax_ensemble_weighted_average.tif")
sp4 <- crop(sp4, mypolygon_wgs84)
sp4 <- mask(sp4, mypolygon_wgs84)

outputs_mask_2 <- stack(sp1,sp2,sp3,sp4)

# Normalizing -------------------------------------------------------------

n <- outputs_mask@data@nlayers

for(z in 1:n){
  adeq = outputs_mask[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  outputs_mask[[z]] <- calc(adeq, adeq_norm)
}

# Saving ------------------------------------------------------------------

# wgs84

proj4string(outputs_mask) <- crs.wgs84

writeRaster(outputs_mask, filename='./INEMA/ENM/outputs/soil/crop_mask_PAT/wgs84/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)

writeRaster(outputs_mask_2, filename='./INEMA/ENM/outputs/models/recorte_PAT/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)


# dando erro
crs.sirgas <-
  CRS(" +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
outputs_mask_sirgas <- projectRaster(outputs_mask, crs.sirgas)

writeRaster(outputs_mask_sirgas, filename='./INEMA/ENM/outputs/soil/crop_mask_PAT/sirgas2000/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)

