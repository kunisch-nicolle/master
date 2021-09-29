library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)
Infiltration<-read.csv("Infiltration.csv", header=T, dec=",", sep = ";")

ggplot(data=Infiltration, aes(x=Tiefe,y=Pixel))+
  theme_bw()+
  
  
  geom_path(linetype=1, size=1,color="orange")+
  geom_point(size=2,color="orange")+
    scale_x_continuous("Anteil der gefärbten Fläche in %",breaks=seq(0,50,by=10),limits=c(0,50))+
  scale_y_reverse("Tiefe in cm",breaks=seq(0,60,by=10),limits=c(60,0))
  

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

Infiltration<-read.csv("Infiltration.csv", header=T, dec=",", sep = ";")
ggplot(data=Infiltration,aes(x=Pixel,y=Tiefe))+
theme_bw()+
geom_path(linetype=1, size=1,color="orange")+
geom_point(size=2,color="orange")+
scale_x_continuous("Anteil der gefärbten Flächen in %")+
scale_y_reverse("Tiefe in cm",breaks=seq(0,60,by=10),limits=c(60,0))

