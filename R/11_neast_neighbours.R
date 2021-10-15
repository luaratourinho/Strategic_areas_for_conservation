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

neigh_perc <- mat2 %>% 
  mutate(`1` = perc_1[1,2])

neigh_perc <- mat2 %>% 
  mutate(`1` = ifelse(mat2$`1` == 1, perc_1[1,2], NA))


neigh_perc_2 <- mat2 %>% 
  for(i in 1:nrow(mat2)){
  mutate(`i` = ifelse(mat2$`i` == 1, perc_1[i,2], NA))}
  #mutate(mat2[[,i]] = ifelse(mat2[[,i]]) == 1, perc_1[[i,2]], NA)}
  #mutate(cols[[i]]= ifelse(cols[[i]] == 1, perc_1[i,2], NA))}

neigh_perc_2 <- for(i in seq_along(mat2)) {
  mat2[[i]] <- (perc_1[[i,2]])
}

neigh_perc_2 <- mat2 %>% 
  mutate(
    name_match = is_match(mat2, perc_1)
    ) %>% 
  # Order like columns next to each other for easier comparison
  if_else(is.na(perc_1$percent), FALSE, perc_1$percent)


##########

is_match <- function(value_1, value_2) {
  result <- value_1 == value_2
  result <- if_else(is.na(result), FALSE, result)
  result
}

people %>% 
  mutate(
    name_first_match = is_match(name_first_1, name_first_2),
    name_last_match  = is_match(name_last_1, name_last_2),
    street_match     = is_match(street_1, street_2)
  ) %>% 
  # Order like columns next to each other for easier comparison
  select(id_1, starts_with("name_f"), starts_with("name_l"), starts_with("s"))


df <- merge(as1, as2, by.x=c("ID", "pID"), by.y=c("ID", "pid"), all=TRUE)
df <- cbind(df[c(1,2)], "Values"=with(df, ifelse(is.na(Values.y), Values.x, Values.y)))


###############

#mas nao pode ser a media direta, tem que ignorar os 0, considerar so a qntidade de celulas que tem valores (qnts diff de 0)
#neigh_perc_out <- neigh_perc %>%  summarise(mean(c_across(`1`:`564`)), na.rm = TRUE)
neigh_perc_out <-
  neigh_perc %>%  
  mutate(mean = rowMeans(across(where(is.numeric))), na.rm = TRUE)

neigh_perc_out
