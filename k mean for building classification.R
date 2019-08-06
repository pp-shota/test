#### Load Libraries ####
library(dplyr)
library(reshape2)
library(factoextra)
library(tidyverse)
library(readxl)



#### Load Dataset ####
msiinR <- read_excel("./data/msiinR.xlsx") ## @mainfile for MI modified from github
msi.res.only <- msiinR %>% filter(Occupation == "residential") ## @ filter only residential building
colnames(msi.res.only)[1] <- "ID"

#data_for_kmean <- msi.res.only %>% dplyr::select(steel,wood,concrete,cement,aggregates,brick,mortar_plaster, TotalMSI)
## select main materials categories ##
data_for_kmean <- msi.res.only %>% 
  dplyr::select(ID,metal_based,biomass_based,construction_mineral,other_minerals,
                concrete,brick,mortar_plaster,aggregates,TotalMSI)
## fill NA with 0, to avoid error in k mean clustering ##
data_for_kmean[is.na(data_for_kmean)] <- 0
#data_for_kmean <- na.omit(data_for_kmean)

#msi.res.only.naomit <- subset(msi.res.only, ID %in% data_for_kmean$ID)
data.for.kmean.cleaned <- data_for_kmean %>% dplyr::filter(metal_based > 8 | biomass_based>15 | construction_mineral >498)%>% 
  dplyr::filter(TotalMSI>100)

data_for_kmean <- data.for.kmean.cleaned

## fill NA with mean of column # @Default no use
#for (i in 1:length(data_for_kmean)) {
 # data_for_kmean[, i][is.na(data_for_kmean[, i])] <- colMeans(data_for_kmean[i], na.rm = T)
#}


#### Run K-Mean clustering ####
fviz_nbclust(data_for_kmean,kmeans, method = "wss") + ggtitle("Optimal number for k of the MI parameters")

## run for difference k value ##
set.seed(1)
k4 <- kmeans(data_for_kmean, 4, nstart = 30) ## k = 4
set.seed(1)
k5 <- kmeans(data_for_kmean, 5, nstart = 30) ## k = 5
set.seed(1)
k6 <- kmeans(data_for_kmean, 6, nstart = 30) ## k = 6
set.seed(1)
k7 <- kmeans(data_for_kmean, 8, nstart = 30) ## k = 7

## plot clustering ##
p1 <- fviz_cluster(k4, geom = "point" , data_for_kmean) + ggtitle("k=4")
p2 <- fviz_cluster(k5, geom = "point" , data_for_kmean) + ggtitle("k=5")
p3 <- fviz_cluster(k6, geom = "point" , data_for_kmean) + ggtitle("k=6")
p4 <- fviz_cluster(k7, geom = "point" , data_for_kmean) + ggtitle("k=7")

plot_clus_1st <- grid.arrange(p1,p2,p3,p4)

## summarise data and crete the report dataset ##
set.seed(1)
k_final <- kmeans(data_for_kmean, 7, nstart = 30) ## @choose k value based on the optimal result
k_final_data <- cbind(msi.res.only.naomit,k_final$cluster) ## @bind cluster class into msi.res.only dataset
colnames(k_final_data)[82] <- "cluster.id" ## @change variable name ##

k_final_selected_data <- k_final_data %>% dplyr::select(Country,building_description,82,metal_based,biomass_based,construction_mineral,other_minerals,brick,concrete,aggregates,TotalMSI)

#write.csv(k_final_data, file = "Kmean for MI BD Type without_outliners.csv") ## @write files for report file


#### ggplot MI ####
d.for.ggplot <- cbind(data_for_kmean,k_final$cluster)
colnames(d.for.ggplot)[7] <- "Clus"
d.for.ggplot <- d.for.ggplot %>% dplyr::select(metal_based,biomass_based,construction_mineral,other_minerals,Clus)

clus_mi_for_plot <- melt(d.for.ggplot, id ="Clus")

ggplot(clus_mi_for_plot, aes(x = value)) + geom_histogram(aes(y=..density..,fill = ..count..)) + 
  geom_density()+ facet_wrap(Clus ~ variable, scale = "free")+ scale_fill_gradient(low = "blue", high = "red")

clus_mi_for_plot %>% dplyr::filter(Clus == 3) %>% ggplot(aes(x = value,fill = variable)) + geom_histogram() + facet_wrap(.~ variable)


