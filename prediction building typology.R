library(class)


mi.data.for.model <- read.csv("C:/Users/X.Vilaysouk/material_stock_resid_bottom_up/data/mi.data.training.clean.csv",stringsAsFactors = TRUE)

mi.data.cleaned <- mi.data.for.model %>% dplyr::select(metal_based,biomass_based,concrete_based,brick_wall_based,
                                                       other_minerals,total_mi,urbanization_1,building_description)


## sameple the data ##
set.seed(69)
ind <- sample(2,nrow(mi.data.cleaned),replace = TRUE, prob = c(0.67,0.33))

mi.data.train.set <- mi.data.cleaned[ind == 1, 1:7]
mi.data.test.set <- mi.data.cleaned[ind == 2, 1:7]
mi.data.test.set.check <- mi.data.cleaned[ind == 2, 1:8]

## take training set and test set labels ##
mi.data.train.set.labels <- mi.data.cleaned[ind ==1,8]
mi.data.test.set.labels <- mi.data.cleaned[ind ==2,8]

pred.set <- knn(train = mi.data.train.set, test = mi.data.test.set, cl = mi.data.train.set.labels,k=6)

accuratecy <- (CrossTable(x = mi.data.test.set.labels, y = pred.set))

checkss <- cbind(mi.data.test.set.check,pred.set)

check.melt <- checkss %>% dplyr::select(1:6,9) %>% melt(id = "pred.set")

check.melt %>% ggplot(aes(x=value, fill = pred.set))+geom_histogram(aes(y = ..density..), bins = 20)+ 
  facet_wrap(pred.set ~ variable)

check.melt %>% ggplot(aes(x=value, fill = pred.set))+geom_density()+ 
  facet_wrap(pred.set ~ variable, scales = "free")

checkss$result <- checkss$building_description == checkss$pred.set
## filter
miss.predict <- checkss %>% dplyr::filter(result == FALSE)





### test bigger set ###
mi.data.for.model <- read.csv("C:/Users/X.Vilaysouk/material_stock_resid_bottom_up/data/mi.data.training.csv",stringsAsFactors = TRUE)

mi.data.cleaned <- mi.data.for.model %>% dplyr::select(metal_based,biomass_based,concrete_based,brick_wall_based,
                                                       other_minerals,total_mi,urbanization_1,building_description)



mi.data.test.set <- mi.data.cleaned[ , 1:7]
mi.data.test.set.check <- mi.data.cleaned[, 1:8]
mi.data.test.set.labels <- mi.data.cleaned[ ,8]

pred.set <- knn(train = mi.data.train.set, test = mi.data.test.set, cl = mi.data.train.set.labels,k=7)
accuratecy <- (CrossTable(x = mi.data.test.set.labels, y = pred.set,prop.chisq=FALSE))

checkss <- cbind(mi.data.test.set.check,pred.set)


check.melt <- checkss %>% dplyr::select(1:6,9) %>% melt(id = "pred.set")

check.melt %>% ggplot(aes(x=value, fill = pred.set))+geom_histogram()+ facet_wrap(pred.set ~ variable, scales = "free")
check.melt %>% ggplot(aes(x=value, fill = variable))+geom_density()+ 
  facet_wrap(pred.set ~ variable, scales = "free")

check.melt %>% dplyr::group_by(pred.set,variable) %>% dplyr::summarise(mi = median(value), uncer = quantile(value,.25))
