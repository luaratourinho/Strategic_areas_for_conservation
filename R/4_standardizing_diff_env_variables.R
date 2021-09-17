
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 01 abr 2021


# Cropping variables from Worldclim ---------------------------------------


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)
library(beepr)


# Tips --------------------------------------------------------------------

# Some command lines may take a while. the beep function serves to warn when 
# the command has finished running, so we don't need to wait and see the 
# "stop" to go out
# beepr::beep(2)

## Extent to crop ------------------------------------------------------

#e <- extent(-105,-30,-55,20)  # SouthAmerica
e <- extent(-85,-30,-40,12) # smaller extent

# Reading rasters ---------------------------------------------------------

# Chelsa variables (bioclimatic) ------------------------------------------

# use pattern = '.tif$' or something else if you have multiple files in this folder
chelsa <- list.files("D:/Luara Tourinho/OneDrive/Documentos/Environmental_data/Chelsa/bio", 
                     full.names = T, 'tif$|bil$')
head(chelsa)
chelsa_st <- stack(chelsa)
chelsa_st_sa <- crop(chelsa_st, e)


# Worldclim variable (elevation) ------------------------------------------

elevation <- list.files("D:/Luara Tourinho/OneDrive/Documentos/Environmental_data/Worldclim/wc2.1_30s_elev", 
                        full.names = T, 'tif$|bil$')
head(elevation)
elevation_st <- stack(elevation)
elevation_st_sa <- crop(elevation_st, e)

# standardizing rasters
chelsa_st_sa_resam <- resample(chelsa_st_sa, elevation_st_sa, method="bilinear")
chelsa_st_sa_mask <- mask(chelsa_st_sa_resam, elevation_st_sa)


# Soil FAO ----------------------------------------------------------------

soil <- list.files("D:/Luara Tourinho/OneDrive/Documentos/Environmental_data/Soil qualities for crop production - FAO", 
                        full.names = T, '.asc$')
head(soil)
soil_st <- stack(soil)
soil_st_sa <- crop(soil_st, e)

crs.wgs84 <-
  CRS(" +proj=longlat +datum=WGS84 +no_defs ")
proj4string(soil_st_sa) <- crs.wgs84

soil_st_sa_resam <- resample(soil_st_sa, elevation_st_sa, method="bilinear")
soil_st_sa_mask <- mask(soil_st_sa_resam, elevation_st_sa)



# Stack variables ---------------------------------------------------------

env <- stack(elevation_st_sa, chelsa_st_sa_mask, soil_st_sa_mask)

writeRaster(env, filename='./INEMA/data/extent/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)



