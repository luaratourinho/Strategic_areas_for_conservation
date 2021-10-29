library(raster)
library(sp)
library(rgdal)
library(dplyr)


# Opening data

# Species distribution
species_c <- shapefile("./Final_results/C_distr.shp")
species_r <- shapefile("./Final_results/R_distr.shp")


# other criterias
C_fg_ft_hb <- shapefile("./Final_results/C_fg_ft_hb.shp")
R_fg_ft_hb_app <- shapefile("./Final_results/R_fg_ft_hb_app.shp")


# Final results -----------------------------------------------

# Conservation strategy areas

#apply(results_per_sp@data, 2, function(x){which(is.nan(x))})

sum_ffh <- C_fg_ft_hb@data$foc_cw + C_fg_ft_hb@data$fito_crw +
  C_fg_ft_hb@data$hab_cw

C_fg_ft_hb_sum <- C_fg_ft_hb
C_fg_ft_hb_sum@data <- cbind(C_fg_ft_hb_sum@data, sum_ffh)

C_fg_ft_hb_sp <- species_c@data[,2:28] + C_fg_ft_hb_sum@data$sum_ffh

results_per_sp <- species_c[,1]

C_fg_ft_hb_sp$all_sp <- rowSums(C_fg_ft_hb_sp[1:27])

results_per_sp@data <- cbind(results_per_sp@data, C_fg_ft_hb_sp)

writeOGR(results_per_sp, dsn = "./Final_results",
         layer = "Results_conser", driver="ESRI Shapefile", overwrite=T)
  


# Restoration strategy areas

sum_ffhap <- R_fg_ft_hb_app@data$foc_rw + R_fg_ft_hb_app@data$fito_crw +
  R_fg_ft_hb_app@data$hab_rw + R_fg_ft_hb_app@data$app_rw

R_fg_ft_hb_app_sum <- R_fg_ft_hb_app
R_fg_ft_hb_app_sum@data <- cbind(R_fg_ft_hb_app_sum@data, sum_ffhap)

R_fg_ft_hb_app_sp_r <- species_r@data[,2:28] + R_fg_ft_hb_app_sum@data$sum_ffhap

results_per_sp_r <- species_r[,1]

R_fg_ft_hb_app_sp_r$all_sp <- rowSums(R_fg_ft_hb_app_sp_r[1:27])

results_per_sp_r@data <- cbind(results_per_sp_r@data, R_fg_ft_hb_app_sp_r)

writeOGR(results_per_sp_r, dsn = "./Final_results",
         layer = "Results_rest", driver="ESRI Shapefile", overwrite=T)

