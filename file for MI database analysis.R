library(tidyverse)
library(dplyr)
library(plotly)
library(gridExtra)
library(readxl)
library(reshape2)


msiinR <- read_excel("./data/msiinR.xlsx")
msiinR[is.na(msiinR)] <- 0

msi.filtered <- dplyr::select(msiinR,3,6,7:38,TotalMSI, Agregate,global_region,-38, gdp_1,urbanization_1, 
                       Occupation, building_type)
msi.filtered <- filter(msi.filtered, construction_period_end >0, Occupation == "residential")

##### plot MS by wood ####
tt <- "Wood"
sds.wood <- msi.filtered %>% group_by(Country, Occupation, construction_period_end) %>%
  summarise(msi.wood = mean(wood,na.rm = TRUE), msi.wood.sd = sd(wood, na.rm = TRUE),
            uncer = msi.wood.sd/msi.wood*100)

plot.msi.wood <- sds.wood %>% ggplot(aes(x=construction_period_end, y = msi.wood, color = factor(Occupation)))+
  geom_point()+ facet_wrap(.~ Country)+ 
  geom_errorbar(aes(ymin = msi.wood-msi.wood.sd, ymax = msi.wood+msi.wood.sd), width = 0.1)+
  labs(x = "Construction Period", y = "MI kg per m^2", color = "Building Type", title = paste("Material Intensity of",tt))+
  scale_color_discrete(label = c(1,2,3,4,5,6,7,8,9))
plot.msi.wood 
stat.wood <- summary(sds.wood$uncer)

##### plot MS by concrete ####
tt <- "concrete"
sds.concrete <- msi.filtered %>% group_by(Country, Occupation, construction_period_end) %>%
  summarise(msi.concrete = mean(concrete,na.rm = TRUE), msi.concrete.sd = sd(concrete, na.rm = TRUE),
            uncer = msi.concrete.sd/msi.concrete*100)
plot.msi.concrete <- sds.concrete %>% ggplot(aes(x=construction_period_end, y = msi.concrete, color = factor(Occupation)))+
  geom_point()+ facet_wrap(.~ Country)+ 
  geom_errorbar(aes(ymin = msi.concrete-msi.concrete.sd, ymax = msi.concrete+msi.concrete.sd), width = 0.1)+
  labs(x = "Construction Period", y = "MI kg per m^2", color = "Building Type", title = paste("Material Intensity of",tt))+
  scale_color_discrete(label = c(1,2,3,4,5,6,7,8,9))
plot.msi.concrete 
stat.concrete <- summary(sds.concrete$uncer)

##### plot MS by agregate ####
sds.Agregate <- msi.filtered %>% group_by(Country, Occupation, construction_period_end) %>%
  summarise(msi.Agregate = mean(Agregate,na.rm = TRUE), msi.Agregate.sd = sd(Agregate, na.rm = TRUE),uncer = msi.Agregate.sd/msi.Agregate*100)
plot.msi.Agregate <- sds.Agregate %>% ggplot(aes(x=construction_period_end, y = msi.Agregate, color = factor(Occupation)))+
  geom_point()+ facet_wrap(.~ Country)+ 
  geom_errorbar(aes(ymin = msi.Agregate-msi.Agregate.sd, ymax = msi.Agregate+msi.Agregate.sd), width = 0.1)
plot.msi.Agregate 
stat.Agregate <- fivenum(sds.Agregate$uncer)

##### plot MS by TotalMSI ####
sds.TotalMSI <- msi.filtered %>% group_by(Country, Occupation, construction_period_end) %>%
  summarise(msi.TotalMSI = mean(TotalMSI,na.rm = TRUE), msi.TotalMSI.sd = sd(TotalMSI, na.rm = TRUE), uncer = msi.TotalMSI.sd/msi.TotalMSI*100)

plot.msi.TotalMSI <- sds.TotalMSI %>% ggplot(aes(x=construction_period_end, y = msi.TotalMSI, color = factor(Occupation)))+
  geom_point()+ facet_wrap(.~ Country)+ 
  geom_errorbar(aes(ymin = msi.TotalMSI-msi.TotalMSI.sd, ymax = msi.TotalMSI+msi.TotalMSI.sd), width = 0.1)
plot.msi.TotalMSI
stat.TotalMSI<- fivenum(sds.TotalMSI$uncer)


##### Plot TotalMSI hist for all countries ##### 
p1 <- msi.filtered %>% filter(Occupation == "residential") %>% ggplot(aes(x=TotalMSI,fill = Country),na.rm = T) + geom_histogram() + facet_wrap(.~ Country)

ggplotly(p1)

msi.filtered %>% group_by(Country) %>% summarise(msi.median = median(TotalMSI, na.rm = T),msi.sd = sd(TotalMSI, na.rm = T),msi.CV = sd(TotalMSI, na.rm = T)/mean(TotalMSI, na.rm = T)*100, data_point = n())

msi.res.only <- msiinR %>% filter(Occupation == "residential")

sd(msi.res.only$TotalMSI)/mean(msi.res.only$TotalMSI)

fivenum(msi.res.only$TotalMSI)
mean(msi.res.only$TotalMSI)



dplyr::select(msi.res.only,metal_based,biomass_based,construction_mineral,other_minerals) %>% melt()%>%
  ggplot(aes(x=value,fill = variable))+geom_histogram()+ facet_wrap(.~ variable)+
  ggplot2::labs(x="kg per m^2", y = "", title = "Material Intensity Distribution of Residential Building",fill = "Materials")+
  theme(plot.background = element_rect(fill = "grey"))


dplyr::select(msi.res.only,concrete,cement,aggregates,brick,mortar_plaster) %>% melt()%>%
  ggplot(aes(x=value,fill = variable, label = variable))+ facet_wrap(.~ variable)+ geom_histogram()+
  ggplot2::labs(x="kg per m^2", y = "", title = "Material Intensity Distribution of Residential Building",fill = "Materials")+
  theme(plot.background = element_rect(fill = "grey"))


dplyr::select(msi.res.only,metal_based,steel,copper,aluminum) %>% melt()%>%
  ggplot(aes(x=value,fill = variable, label = variable))+ facet_wrap(.~ variable)+ geom_histogram()+
  ggplot2::labs(x="kg per m^2", y = "", title = "Material Intensity Distribution of Residential Building",fill = "Materials")+
  theme(plot.background = element_rect(fill = "grey"))

dplyr::select(msi.res.only,straw,wood,paper_cardboard) %>% melt()%>%
  ggplot(aes(x=value,fill = variable, label = variable))+ facet_wrap(.~ variable)+ geom_histogram()+
  ggplot2::labs(x="kg per m^2", y = "", title = "Material Intensity Distribution of Residential Building",fill = "Materials")+
  theme(plot.background = element_rect(fill = "grey"))

dum <- msi.res.only %>%  dplyr::select(straw,wood,paper_cardboard) %>% melt()
dum %>% group_by(variable) %>% summarise(msi.median = median(value,na.rm = TRUE),msi.sd = sd(value,na.rm = TRUE),msi.CV = sd(value,na.rm = TRUE)/mean(value,na.rm = TRUE)*100)

