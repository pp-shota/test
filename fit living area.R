library(readxl)
library(reshape2)
library(dplyr)
library(tidyverse)
library(plotly)
library(forecast) #load the forecast package

Book1 <- read_excel("./data/Book1.xlsx")
area <- Book1$area
year <- Book1$year
a0 = 3
a.st = 50

nls1 <- nls(area~a.st/(1+(a.st/a0-1)*exp(a*(1-exp(b*(year-1950))))),start=list(a=.19,b=0.027),trace=TRUE)
A<-summary(nls1)$parameter[1,1] #get the value for aA<-summary(nls1)$parameter[1,1] #get the value for a
B<-summary(nls1)$parameter[2,1] #get the value for b
year_sim<-1950:2050 #define the simulation duration
fitted_area<-a.st/(1+(a.st/a0-1)*exp(A*(1-exp(B*(year_sim-1950))))) #compute the fitted floor area per capita without stochastic errors

area.dat <- read.csv("./data/area.csv")
area.dat <- area.dat[,-3:-4]
area.dat <- cbind(area.dat,fitted_area)
area.sim <- melt(area.dat, id = "year")
living_area <- (ggplot(area.sim,aes(x= year,y=value,color = variable, shape = variable)) + geom_point())+
  scale_color_discrete(label = c("Statistics","Model"))+ guides(shape = F)+
  scale_x_continuous(breaks = seq(from = 1990, to = 2050, by = 10),limits = c(1990,2050))+
  labs(color = "", x = "", y = "Living Area per capita (m^2/person)",title = "Residential Living Area in Lao PDR")
living_area
ggplotly(living_area)
#write.csv(fitted_area, file = "./data/area.sim.csv")

rate <- ts(summary(nls1)$residuals, start = 1990, frequency = 1) #get the residuals of the Nonlinear Least Squares and set the residuals as a time series
arima1<-auto.arima(rate,trace=T) #run the auto.arima command to get the parameters of ARIMA
r.sims <- matrix(nrow=36, ncol=5000) #define a matrix to store the 5,000 simulations of time series from 2013-2100
M<-5000 #simulation times
for(i in 1:M)
{
  set.seed(i)
  ts <- simulate(arima1, nsim =36) #simulate the residuals of the floor area per capita
  r.sims[,i]<-as.numeric(ts+ fitted_area[66:101]) #get the floor area per capita
}

#get the median and 95% CIs of the floor area per capita
median<-apply(r.sims,1,quantile,probs=c(0.025,0.5,0.975))[2,]
quantile2.5<-apply(r.sims,1,quantile,probs=c(0.025,0.5,0.975))[1,]
quantile97.5<-apply(r.sims,1,quantile,probs=c(0.025,0.5,0.975))[3,]
