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

mypolygon <- readOGR(dsn="./PAT_territorio/", layer="PAT_wgs84") 

# crs.wgs84 <-
#   CRS("+proj=longlat +datum=WGS84 +no_defs")
# mypolygon_wgs84 <- spTransform(mypolygon, crs.wgs84)


# Reading shape to crop ---------------------------------------------------

# SDM results

outputs2 <- list.files("./outputs/Ensemble_SDM", 
                       full.names = T, 'tif$')
head(outputs2)
outputs_st2 <- raster(outputs2[[1]])
outputs_e2 <- crop(outputs_st2, mypolygon)
outputs_mask2 <- mask(outputs_e2, mypolygon)

outputs_st3 <- raster(outputs2[[2]])
outputs_e3 <- crop(outputs_st3, mypolygon)
outputs_mask3 <- mask(outputs_e3, mypolygon)

outputs_st4 <- raster(outputs2[[3]])
outputs_e4 <- crop(outputs_st4, mypolygon)
outputs_mask4 <- mask(outputs_e4, mypolygon)

outputs_st5 <- raster(outputs2[[4]])
outputs_e5 <- crop(outputs_st5, mypolygon)
outputs_mask5 <- mask(outputs_e5, mypolygon)

outputs_modleR <- stack(outputs_mask2,outputs_mask3,outputs_mask4,outputs_mask5)


# Euclidean Distance results

outputs <- list.files("./outputs/soil_ED", 
                      full.names = T, 'tif$')
head(outputs)
outputs_st <- stack(outputs)
outputs_e <- crop(outputs_st, mypolygon)
outputs_mask <- mask(outputs_e, mypolygon)

# Normalizing -------------------------------------------------------------

# ED

outputs_mask_ED <- outputs_mask
n <- outputs_mask_ED@data@nlayers

for(z in 1:n){
  adeq = outputs_mask_ED[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  outputs_mask_ED[[z]] <- calc(adeq, adeq_norm)
}

names(outputs_mask_ED) <- names(outputs_mask)

# SDM

outputs_mask_SDM <- outputs_modleR
n <- outputs_mask_ED@data@nlayers

for(z in 1:n){
  adeq = outputs_mask_SDM[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  outputs_mask_SDM[[z]] <- calc(adeq, adeq_norm)
}

names(outputs_mask_SDM) <- names(outputs_modleR)

# Saving ------------------------------------------------------------------

writeRaster(outputs_mask_ED, filename='./outputs/crop_PAT/ED/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)

writeRaster(outputs_mask_SDM, filename='./outputs/crop_PAT/SDM/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)

