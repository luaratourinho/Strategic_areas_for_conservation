library(raster)
library(sp)
library(rgdal)
library(dplyr)

# All results x their weights ---------------------------------------------

# Read polygon base

nivel6_original <- shapefile("./PAT_territorio/PAT_ottonivel6.shp")


# Critérios:
# Distribuição potencial das espécies	
#C 0.31	R 0.26
# Focos de incêndio	
#C 0.15	R 0.17
# Diversidade de fitofisionomia	
#C 0.04	R 0.04
# Quantidade de habitat da UP e de suas UPs vizinhas (%) 	
#C 0.50	R 0.46
# Áreas de Proteção Permanente (APPs) (%)
#R 0.07

#apply(C_distr@data, 2, function(x){which(is.nan(x))}) # Check nans


# Results from SDM and ED -------------------------------------------------
#C 0.31	R 0.26

distribution <- shapefile("./outputs/crop_PAT/nivel6/nivel6_ED_SDM.shp")
distrib <- distribution@data[, c(1,28:50,55:58)]
distrib_cw <- distrib

for(i in 2:dim(distrib)[2]){
  distrib_cw[,i]<-distrib_cw[,i]*0.31
}

distrib_rw <- distrib

for(i in 2:dim(distrib)[2]){
  distrib_rw[,i]<-distrib_rw[,i]*0.26
}


colnames(distrib_cw) <- c("NUNIV",
  "Acr_har_cw", "Acr_pin_cw","Avi_gam_cw", 
  "Bau_gla_cw", "Coa_ces_cw", "Ful_stu_cw",
  "Hel_ruf_cw", "Ile_aur_cw", "Man_hat_cw",
  "Mel_ill_cw", "Met_dia_cw", "Mic_str_cw",
  "Mic_sub_cw", "Orm_tim_cw", "Pas_tim_cw",
  "Pav_pal_cw", "Ray_bah_cw", "Sen_ric_cw",
  "Sin_mac_cw", "Sty_glo_cw", "Tri_mor_cw",
  "Vel_can_cw", "Xyr_fib_cw", "Hyb_alb_cw",
  "Ooc_nub_cw", "Phi_bah_cw",	"Pir_fla_cw")
  
colnames(distrib_rw) <- c("NUNIV",
  "Acr_har_rw", "Acr_pin_rw","Avi_gam_rw", 
  "Bau_gla_rw", "Coa_ces_rw", "Ful_stu_rw",
  "Hel_ruf_rw", "Ile_aur_rw", "Man_hat_rw",
  "Mel_ill_rw", "Met_dia_rw", "Mic_str_rw",
  "Mic_sub_rw", "Orm_tim_rw", "Pas_tim_rw",
  "Pav_pal_rw", "Ray_bah_rw", "Sen_ric_rw",
  "Sin_mac_rw", "Sty_glo_rw", "Tri_mor_rw",
  "Vel_can_rw", "Xyr_fib_rw", "Hyb_alb_rw",
  "Ooc_nub_rw", "Phi_bah_rw",	"Pir_fla_rw")


# Join information

C_distr <- nivel6_original
R_distr <- nivel6_original

C_distr@data <- cbind(C_distr@data, distrib_cw[,2:28])
C_distr <- C_distr[, -(2:4)]
R_distr@data <- cbind(R_distr@data, distrib_rw[,2:28])
R_distr <- R_distr[, -(2:4)]

writeOGR(C_distr, dsn = "./Final_results",
         layer = "C_distr", driver="ESRI Shapefile", overwrite=T)

writeOGR(R_distr, dsn = "./Final_results",
         layer = "R_distr", driver="ESRI Shapefile", overwrite=T)

# C_distr <- shapefile("./Final_results/C_distr.shp")
# R_distr <- shapefile("./Final_results/R_distr.shp")


# Results from fire -------------------------------------------------------
#C 0.15	R 0.17

focos <- shapefile("./Fogo/Focos_por_UP_inv.shp")
focos <- focos[, -(2:4)]

foc_cw <- focos@data$foc_inv * 0.15
focos@data <- cbind(focos@data, foc_cw)
foc_rw <- focos@data$foc_inv * 0.17
focos@data <- cbind(focos@data, foc_rw)

writeOGR(focos, dsn = "./Final_results/Patial",
         layer = "focos_C_R_w", driver="ESRI Shapefile", overwrite=T)

# Data to join with next criteria
C_fg <- focos[, -c(2:4,6)]
R_fg <- focos[, -c(2:4,5)]


# Results from phytophysionomy ----------------------------------------------
#C 0.04	R 0.04

phyto <- shapefile("./fitofisio/fito_n6_divers_w_int.shp")
phyto <- phyto[,c(1,5:8)]

fito_crw <- phyto@data$shan_w_int * 0.04
phyto@data <- cbind(phyto@data, fito_crw)

writeOGR(phyto, dsn = "./Final_results/Patial",
         layer = "fito_r_sim_sh_shwin_shw", driver="ESRI Shapefile", overwrite=T)

C_fg_ft <- C_fg
C_fg_ft@data <- cbind(C_fg_ft@data, fito_crw)
R_fg_ft <- R_fg
R_fg_ft@data <-cbind(R_fg_ft@data, fito_crw)

C_fg_ft <- shapefile("./Final_results/Patial/C_fg_ft.shp")
names_cols <- c("NUNIVOTTO6", "foc_cw", "fito_crw")
names(C_fg_ft)[] <- names_cols

R_fg_ft <- shapefile("./Final_results/Patial/R_fg_ft.shp")
names_cols <- c("NUNIVOTTO6", "foc_rw", "fito_crw")
names(R_fg_ft)[] <- names_cols


# Results from habitat amount ---------------------------------------------
#C 0.50	R 0.46

hab <- shapefile("./Mapbiomas/PAT_n6_edsdm_difuniao_mapb_peso_neig.shp")
hab <- hab[1:317,c(1,35:41)]

# Renaming
names_cols <- c("NUNIVOTTO6", "perc_hab", "perc_norm", "rw_int", "neig_avg",
                "neig_norm", "hab_conser", "hab_restor")
names(hab)[] <- names_cols

hab_cw <- hab@data$hab_conser * 0.5
hab@data <- cbind(hab@data, hab_cw)
hab_rw <- hab@data$hab_restor * 0.46
hab@data <- cbind(hab@data, hab_rw)

writeOGR(hab, dsn = "./Final_results/Patial",
         layer = "hab_perc_norm_wint_crw", driver="ESRI Shapefile", overwrite=T)


# Join results

C_fg_ft_hb <- C_fg_ft
R_fg_ft_hb <- R_fg_ft

C_fg_ft_hb@data <- cbind(C_fg_ft_hb@data, hab_cw)
R_fg_ft_hb@data <-cbind(R_fg_ft_hb@data, hab_rw)

writeOGR(C_fg_ft_hb, dsn = "./Final_results",
         layer = "C_fg_ft_hb", driver="ESRI Shapefile", overwrite=T)


# Results from APPs -------------------------------------------------------
#R 0.07

app <- shapefile("./APP/app_perc_norm.shp")
app <- app[,c(1,5,6)]

app_rw <- app@data$app_norm * 0.07
app@data <- cbind(app@data, app_rw)

writeOGR(app, dsn = "./Final_results/Patial",
         layer = "app_perc_norm_rw", driver="ESRI Shapefile", overwrite=T)


# Join results

R_fg_ft_hb_app <- R_fg_ft_hb
R_fg_ft_hb_app@data <-cbind(R_fg_ft_hb_app@data, app_rw)

writeOGR(R_fg_ft_hb_app, dsn = "./Final_results",
         layer = "R_fg_ft_hb_app", driver="ESRI Shapefile", overwrite=T)

