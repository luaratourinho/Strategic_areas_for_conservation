library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(rgdal)
library(maptools)


# SDM ---------------------------------------------------------------------

nivel6_SDM <- shapefile("./outputs/crop_PAT/nivel6/nivel6_SDM_names.shp")


# Normalizing -------------------------------------------------------------

# one by one
# minimo <- min(nivel6_SDM@data$Hyba_albu, na.rm=T)
# maximo <- max(nivel6_SDM@data$Hyba_albu, na.rm=T)
# normalizar <- function(x) {
#   (x-minimo)/(maximo-minimo)}
# norm <- normalizar(nivel6_SDM@data$Hyba_albu)
# Hyb_alb_01 <- as.vector(norm)
# nivel6_SDM@data <- cbind(nivel6_SDM@data, Hyb_alb_01) 


# Loop

for(i in 5:length(nivel6_SDM@data)){

minimo <- min(nivel6_SDM@data[,i], na.rm=T)
maximo <- max(nivel6_SDM@data[,i], na.rm=T)

normalizar <- function(x) {
  (x-minimo)/(maximo-minimo)}

norm <- normalizar(nivel6_SDM@data[,i])
norm_v <- as.vector(norm)
nivel6_SDM@data <- cbind(nivel6_SDM@data, norm_v)

}


# Renaming ----------------------------------------------------------------

names_cols <- c("NUNIV", "VERSA", "Shp_L",
                "Shp_A", "Hyba_albu", "Ooce_nubi",
                "Phil_bahi",	"Piri_flam", 
                "Hyb_alb_01", "Ooc_nub_01", "Phi_bah_01",	"Pir_fla_01")

names(nivel6_SDM)[] <- names_cols

writeOGR(nivel6_SDM, dsn = "./outputs/crop_PAT/nivel6",
         layer = "nivel6_SDM_norm", driver="ESRI Shapefile", overwrite=T)



# ED ----------------------------------------------------------------------

nivel6_ED <- shapefile("./outputs/crop_PAT/nivel6/nivel6_ED_inv.shp")

# Normalizing -------------------------------------------------------------

# Loop

for(i in 5:length(nivel6_ED@data)){
  
  minimo <- min(nivel6_ED@data[,i], na.rm=T)
  maximo <- max(nivel6_ED@data[,i], na.rm=T)
  
  normalizar <- function(x) {
    (x-minimo)/(maximo-minimo)}
  
  norm <- normalizar(nivel6_ED@data[,i])
  norm_v <- as.vector(norm)
  nivel6_ED@data <- cbind(nivel6_ED@data, norm_v)
  
}


# Renaming ----------------------------------------------------------------

names_cols <- c("NUNIV", "VERSA", "Shp_L",
                "Shp_A", "Acri_harl", "Acri_pint",
                "Avic_gamb", "Bauh_glaz", "Coar_cess",
                "Fulc_stue", "Heli_rufi", "Ilex_auri",
                "Mand_hats", "Melo_illi", "Meta_diam",
                "Micr_stre", "Micr_suba", "Ormo_timb",
                "Pass_timb", "Pavo_palm", "Rayl_bahi",
                "Sene_rico", "Sinn_macr", "Styl_glom",
                "Tric_mori", "Vello_cane", "Xyri_fibr",
                "Acr_har_01", "Acr_pin_01",
                "Avi_gam_01", "Bau_gla_01", "Coa_ces_01",
                "Ful_stu_01", "Hel_ruf_01", "Ile_aur_01",
                "Man_hat_01", "Mel_ill_01", "Met_dia_01",
                "Mic_str_01", "Mic_sub_01", "Orm_tim_01",
                "Pas_tim_01", "Pav_pal_01", "Ray_bah_01",
                "Sen_ric_01", "Sin_mac_01", "Sty_glo_01",
                "Tri_mor_01", "Vel_can_01", "Xyr_fib_01")

names(nivel6_ED)[] <- names_cols

writeOGR(nivel6_ED, dsn = "./outputs/crop_PAT/nivel6",
         layer = "nivel6_ED_norm", driver="ESRI Shapefile", overwrite=T)


# Join results ------------------------------------------------------------

nivel6_ED_SDM = nivel6_ED
nivel6_ED_SDM@data <- cbind(nivel6_ED_SDM@data, nivel6_SDM@data[,5:12])

writeOGR(nivel6_ED_SDM, dsn = "./outputs/crop_PAT/nivel6",
         layer = "nivel6_ED_SDM", driver="ESRI Shapefile", overwrite=T)



