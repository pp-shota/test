library(tidyverse)
library(fitdistrplus)

MI.dataset <- read_excel("./data/msiinR.xlsx") ## @mainfile for MI modified from github
MI.Res.Only <- MI.dataset %>% dplyr::filter(Occupation == "residential")

#### fit the distribution of TotalMSI ####
TotalMSI <- MI.Res.Only$TotalMSI
fw <- fitdist(TotalMSI, "weibull") #fit the Weibull distribution
fln <- fitdist(TotalMSI, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(TotalMSI,"norm") #estimate the distribution parameters of Normal distribution
gofstat(list(fw,fln,fn)) #compare the distribution functions
hist(TotalMSI)


#### fit the distribution of construction_mineral ####
construction_mineral <- MI.Res.Only$construction_mineral
construction_mineral <- as.numeric(na.omit(construction_mineral))
fw <- fitdist(construction_mineral, "weibull") #fit the Weibull distribution
fln <- fitdist(construction_mineral, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(construction_mineral,"norm") #estimate the distribution parameters of Normal distribution
l <- gofstat(list(fw,fln,fn)) #compare the distribution functions
l <- gofstat(list(fw,fln,fn)) #compare the distribution functions
mm <- which.min(l$cvm) %>% print()
hist(construction_mineral)

#### fit the distribution of metal_based ####
metal_based <- MI.Res.Only$metal_based
metal_based <- as.numeric(na.omit(metal_based))
fw <- fitdist(metal_based, "weibull") #fit the Weibull distribution
fln <- fitdist(metal_based, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(metal_based,"norm") #estimate the distribution parameters of Normal distribution
l <- gofstat(list(fw,fln,fn)) #compare the distribution functions
mm <- which.min(l$cvm) %>% print()
hist(metal_based)

#### fit the distribution of copper ####
copper <- MI.Res.Only$copper
copper <- abs(as.numeric(na.omit(copper)))
fw <- fitdist(copper, "weibull") #fit the Weibull distribution
fln <- fitdist(copper, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(copper,"norm") #estimate the distribution parameters of Normal distribution
l <- gofstat(list(fw,fln,fn)) #compare the distribution functions
mm <- which.min(l$cvm) %>% print()
hist(copper)
summary(copper)

#### fit the distribution of biomass_based ####
biomass_based <- MI.Res.Only$biomass_based
biomass_based <- as.numeric(na.omit(biomass_based))
fw <- fitdist(biomass_based, "weibull") #fit the Weibull distribution
fln <- fitdist(biomass_based, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(biomass_based,"norm") #estimate the distribution parameters of Normal distribution
l <- gofstat(list(fw,fln,fn)) #compare the distribution functions
mm <- which.min(l$cvm) %>% print()
hist(biomass_based)
summary(biomass_based)

#### fit the distribution of wood ####
wood <- MI.Res.Only$wood
wood <- as.numeric(na.omit(wood))
fw <- fitdist(wood, "weibull") #fit the Weibull distribution
fln <- fitdist(wood, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(wood,"norm") #estimate the distribution parameters of Normal distribution
l <- gofstat(list(fw,fln,fn)) #compare the distribution functions
mm <- which.min(l$cvm) %>% print()
hist(wood)
summary(wood)

#### fit the distribution of other_minerals ####
other_minerals <- MI.Res.Only$other_minerals
other_minerals <- as.numeric(na.omit(other_minerals))
fw <- fitdist(other_minerals, "weibull") #fit the Weibull distribution
fln <- fitdist(other_minerals, "lnorm") #estimate the distribution parameters of Lognormal distribution
fn <-fitdist(other_minerals,"norm") #estimate the distribution parameters of Normal distribution
l <- gofstat(list(fw,fln,fn)) #compare the distribution functions
mm <- which.min(l$cvm) %>% print()
summary(other_minerals)
hist(other_minerals)

