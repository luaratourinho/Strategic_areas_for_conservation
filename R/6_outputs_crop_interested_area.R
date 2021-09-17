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

outputs <- list.files("./INEMA/ENM/outputs/soil", 
                     full.names = T, 'tif$')
head(outputs)
outputs_st <- stack(outputs)
outputs_e <- crop(outputs_st, mypolygon_wgs84)
outputs_mask <- mask(outputs_e, mypolygon_wgs84)


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


# dando erro
crs.sirgas <-
  CRS(" +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
outputs_mask_sirgas <- projectRaster(outputs_mask, crs.sirgas)

writeRaster(outputs_mask_sirgas, filename='./INEMA/ENM/outputs/soil/crop_mask_PAT/sirgas2000/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)

