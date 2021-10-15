library(spdep)
library(sp)
library(raster)
library(rgdal)
library(rgeos)


# Load polygon

nivel6_mapbi <- shapefile("./INEMA/Spatial_files_PAT/Join_results/PAT_n6_edsdm_diffuniao_mapbi_peso.shp")
nivel6_mapbi@data


# Neast neighbours --------------------------------------------------------


nivel6_test <- nivel6_mapbi
nb <- poly2nb(nivel6_test, row.names = nivel6_mapbi@data$id)
mat <- nb2mat(nb, style="B")
colnames(mat) <- rownames(mat)
mat

mat2 <- as_tibble(mat) 
#colnames(mat2) <- seq(from=1, to=564)

perc_1 <- as_tibble(cbind(nivel6_mapbi@data$id, nivel6_mapbi@data$prcnt_m))
colnames(perc_1) <- c("id","percent")
perc_1$percent <- as.numeric(perc_1$percent)


# Maneira 1
n <- nrow(mat2)
vizinho_media <- numeric(n)
for (i in 1:n) {
  vizinho_media[i] <- mean(perc_1[as.logical(mat2[i, ]), 2]$percent) # as.logical(mat2[i, ]) == (mat2[i, ] == 1)[1, ]
}

# Maneira 2
vizinho_media2 <- apply(mat2, 1, function(x){mean(perc_1[as.logical(x), 2]$percent)})


# Maneira 3
n <- nrow(mat)
mat3 <- matrix(ncol = ncol(mat), nrow = n)

for (i in 1:n) {
  pos <- mat[i, ] == 1
  mat3[i, pos] <- perc_1$percent[pos] # para ver os ids com percent perc_1[pos, ]
}

vizinho_media3 <- rowMeans(mat3, na.rm = TRUE)


# teste se tudo deu igual 
all.equal(vizinho_media, vizinho_media3); all.equal(vizinho_media, vizinho_media2)


# Adding results to the shapefile ---------------------------------------------

nivel6 <- shapefile("./INEMA/Spatial_files_PAT/Join_results/PAT_n6_edsdm_diffuniao_mapbi_peso.shp")

neig_avg <- vizinho_media
nivel6@data <- cbind(nivel6@data, neig_avg)

range01  <- function(x){(x-min(x))/(max(x)-min(x))}
neig_norm <- range01(vizinho_media)
neig_norm
nivel6@data <- cbind(nivel6@data, neig_norm) 

prc_mp <- nivel6@data$prc_mp_
peso <- nivel6@data$peso

hab_conser <- prc_mp * neig_norm
hab_restor <- peso * neig_norm

hab_conser <- range01(hab_conser)
nivel6@data <- cbind(nivel6@data, hab_conser)

hab_restor <- range01(hab_restor)
nivel6@data <- cbind(nivel6@data, hab_restor)


# Save result
writeOGR(nivel6, dsn = "./INEMA/Spatial_files_PAT/Join_results",
         layer = "PAT_n6_edsdm_difuniao_mapb_peso_neig", driver="ESRI Shapefile", overwrite=T)


# Trechos que nao funcionaram ---------------------------------------------


# neigh_perc <- mat2 %>% 
#   mutate(`1` = perc_1[1,2])
# 
# neigh_perc <- mat2 %>% 
#   mutate(`1` = ifelse(mat2$`1` == 1, perc_1[1,2], NA))
# 
# 
# neigh_perc_2 <- mat2 %>% 
#   for(i in 1:nrow(mat2)){
#   mutate(`i` = ifelse(mat2$`i` == 1, perc_1[i,2], NA))}
#   #mutate(mat2[[,i]] = ifelse(mat2[[,i]]) == 1, perc_1[[i,2]], NA)}
#   #mutate(cols[[i]]= ifelse(cols[[i]] == 1, perc_1[i,2], NA))}
# 
# neigh_perc_2 <- for(i in seq_along(mat2)) {
#   mat2[[i]] <- (perc_1[[i,2]])
# }
# 
# neigh_perc_2 <- mat2 %>% 
#   mutate(
#     name_match = is_match(mat2, perc_1)
#     ) %>% 
#   # Order like columns next to each other for easier comparison
#   if_else(is.na(perc_1$percent), FALSE, perc_1$percent)
# 
# 
# ##########
# 
# is_match <- function(value_1, value_2) {
#   result <- value_1 == value_2
#   result <- if_else(is.na(result), FALSE, result)
#   result
# }

###############

#mas nao pode ser a media direta, tem que ignorar os 0, considerar so a qntidade de celulas que tem valores (qnts diff de 0)
#neigh_perc_out <- neigh_perc %>%  summarise(mean(c_across(`1`:`564`)), na.rm = TRUE)
# mat3_out <-
#   mat3 %>%  
#   mutate(mean = rowMeans(across(where(is.numeric))), na.rm = TRUE)
# 
# neigh_perc_out
