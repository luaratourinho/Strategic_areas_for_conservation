library("FactoMineR")
library(RStoolbox)
library(raster)

vari <- list.files("./extent", 
                        full.names = T, 'tif$')
head(vari)
vari_st <- stack(vari)

envi <- rasterPCA(vari_st, nComp=13,scores = TRUE, 
                           cor=TRUE, spca = TRUE, bylayer=TRUE, 
                           filename="PCA.grd", overwrite=TRUE)

summ <- summary(envi$model) 

env.selected <-stack(envi$map)
env.selected
res(env.selected)
plot(env.selected)
names(env.selected) 

sink("summary_pca_RStoolbox.txt")
print(summ)
sink()


writeRaster(env.selected, filename='./extent/pca_soil_newscript/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)


# Reference
# https://github.com/pedroeisenlohr/niche_modelling/blob/master/Niche%20modelling_Current%20Climate.R
