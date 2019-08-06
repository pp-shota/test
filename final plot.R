  dt <- k_final_data %>% dplyr::select(construction_period_end,other_minerals,metal_based,biomass_based,construction_mineral,
                                wood,brick,concrete,aggregates,TotalMSI,cluster.id,Country,Authors)%>%
    filter(construction_period_end >0)
  
  colnames(dt)[1] <- "year"
  
  
  
  dt.melt <- dt %>% melt(id = c("year","cluster.id","Authors","Country"))
  dt.melt[is.na(dt.melt)] <- 0
  dt.melt$value <- as.numeric(dt.melt$value)
  dt.melt$Authors <- substr(dt.melt$Authors,1,10)
  
  dt.sum <- dt.melt %>% group_by(year,Authors,variable,cluster.id,Country) %>% summarise(value = mean(value, na.rm = T))
  
  cluster.id <- c(1,2,3,4,5,6,7)
  
  plot_by_cluster1 <- dt.sum %>% ggplot(aes(x = year, y = value,shape = as.factor(cluster.id), color = factor(variable))) + geom_point()+ 
    geom_smooth(method = "gam")+ facet_wrap(.~ Country)+
    scale_shape_discrete(label = c("a","b","c","d","e","f","g"), name = c(1,2,3,4,5,6,7))
  plot_by_cluster1

  
plot_by_cluster2 <- dt.sum %>% ggplot(aes(x = year, y = value, color = factor(variable))) + geom_point()+ 
  geom_smooth(method = "gam")+ facet_wrap(.~ cluster.id)#,scales = "free")+
plot_by_cluster2


dt.sum <- dt.melt %>% group_by(year,cluster.id,variable,Country) %>% summarise(value = mean(value))
plot_by_country <- dt.sum %>% ggplot(aes(x = year, y = value, color = factor(variable))) + geom_point()+
  geom_smooth(method = "lm")+ facet_wrap(.~ Country[2:7],scales = "free")
plot_by_country



mi_all <- msiinR %>% dplyr::select(construction_period_end,other_minerals,metal_based,biomass_based,construction_mineral,
                        wood,brick,concrete,aggregates,TotalMSI,Country)%>%
  filter(construction_period_end >0)

ggplotly(mi_all %>% melt(id = "construction_period_end") %>% ggplot(aes(x=construction_period_end,y=value,color=variable))+
           geom_point()+ geom_smooth(method = "lm"))









dt <- k_final_data %>% dplyr::select(construction_period_end,metal_based,biomass_based,construction_mineral,
                                     other_minerals,cluster.id,global_region,Authors)%>%
  filter(construction_period_end >0)

colnames(dt)[1] <- "year"



dt.melt <- dt %>% melt(id = c("year","cluster.id","Authors","global_region"))
dt.melt[is.na(dt.melt)] <- 0
dt.melt$value <- as.numeric(dt.melt$value)
dt.melt$Authors <- substr(dt.melt$Authors,1,10)

dt.sum <- dt.melt %>% dplyr::filter(cluster.id>1) %>% group_by(year,Authors,variable,cluster.id,global_region) %>% summarise(value = mean(value, na.rm = T))


dt.sum %>% ggplot(aes(x = year, y = value,shape = as.factor(global_region), color = factor(variable))) + geom_point()+ facet_wrap(.~ cluster.id)


### plot histogram of cluster ####
dt <- k_final_data %>% dplyr::select(construction_period_end,metal_based,biomass_based,construction_mineral,
                                     other_minerals,cluster.id,global_region,Authors)%>%
  filter(construction_period_end >0)

colnames(dt)[1] <- "year"



dt.melt <- dt %>% melt(id = c("year","cluster.id","Authors","global_region"))
#dt.melt[is.na(dt.melt)] <- 0
dt.melt$value <- as.numeric(dt.melt$value)
dt.melt$Authors <- substr(dt.melt$Authors,1,10)

dt.sum <- dt.melt %>% dplyr::filter(cluster.id>1) %>% group_by(year,Authors,variable,cluster.id,global_region) %>% summarise(value = mean(value, na.rm = T))


dt.sum %>% dplyr::filter(cluster.id>1) %>% ggplot(aes(x=value))+geom_histogram(aes(y=..density..,fill=..count..), alpha=.75, bins =6) +
  geom_density()+
  facet_wrap(.~cluster.id, scales = "free", ncol = 4)+
  scale_fill_gradient("Count", low = "blue", high = "red")


