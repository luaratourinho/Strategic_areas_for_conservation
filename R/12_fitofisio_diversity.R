library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(memisc)

# Load polygon
nivel6 <- shapefile("./INEMA/Spatial_files_PAT/PAT_territorio/nivel6/PAT_ottonivel6_wgs84.shp")

# Load rasters
fito <- raster("./INEMA/Spatial_files_PAT/vegetacao/cobertura_vegetal_nivel4_NA_wgs.tif")

# Crop
fito <- crop(fito,nivel6)

# Percentage of habitat per feature ---------------------------------------

fito_per_pol <- extract(fito, nivel6)

#Functions



resultado <- sapply(fito_per_pol, shannon)
resultado

# Add the percentage to the shapefile
nivel6@data <- cbind(nivel6@data, resultado) 

writeOGR(nivel6, dsn = "./fitofisio",
         layer = "fitofisio_nivel6", driver="ESRI Shapefile", overwrite=T)







# testando ----------------------------------------------------------------

# 8. Calcule o índice de diversidade de Shannon de um local.
# H' = - somatorio(pi ln(pi))
# pi = ni/N abundancia relativa
# ni = Número de indivíduos da espécie i.
# N= número total de indivíduos amostrados.

shannon <- function(x){
  abund_total <- sum(x)
  pi <- x/abund_total
  ln_pi <- log(pi)
  pi_mult <- pi* ln_pi
  result <- -sum(pi_mult)
  return(result)
}

# 9. Calcule o índice de diversidade de Simpson de um local. 
# D = 1- somatorio (pi^2)
# quanto MAIOR o valor de “D”, MENOR a diversidade

simpson <- function(x){
  abund_total <- sum(x)
  pi <- x/abund_total
  result2 <- 1- sum(pi^2)
  return(result2)
}

