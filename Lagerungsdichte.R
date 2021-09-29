library(ggplot2)
library(extrafont)
library(hrbrthemes)
loadfonts(device="win")

Lagerungsdichte<-read.csv("Lagerungsdichte.csv", header=T, dec=",", sep = ";")
ggplot(data=Lagerungsdichte, aes(x=Tiefe,y=Lagerungsdichte))+
geom_line(linetype=1, size=1, color="orange")+
geom_point(size=2,color="orange")+
coord_flip()+
scale_x_reverse("Tiefe in cm",breaks=seq(0,80,by=10),limits=c(80,0))+
scale_y_continuous(position="top",limits=c(1.3,1.60))+
labs(y="Lagerungsdichte",title="Lagerungsdichte ?ber die Tiefe")+
theme(plot.title = element_text(size=11, family="sans",hjust=0.5))+
theme(text=element_text(size=11, family=""))

library(ggplot2)
library(extrafront)
loadfonts(device="win")
Lagerungsdichte<-read.csv("Lagerungsdichte.csv",header=T,dec=",",sep=";")
ggplot(data=Lagerungsdichte, aes(x=Lagerungsdichte,y=Tiefe))+
geom_path(linetype=1,size=1,color="orange")+
geom_point(size=2,color="orange")+
scale_x_continuous(position="top",limits=c(1.3,1.60))+
scale_y_reverse("Tiefe in cm",breaks=seq(0,80,by=10),limits=c(80,0))+
labs(x="Lagerungsdichte in"~g/cm^3,title="Lagerungsdichte ?ber die Tiefe")+
theme(plot.title=element_text(size=11, family="sans",hjust=0.5))


library(ggplot2)
library(ggthemes)
library(hrbrthemes)
Lagerungsdichte<-read.csv("Lagerung.csv",header=T,dec=",",sep=";")
ggplot(data=Lagerungsdichte, aes(y=Tiefe))+
  theme(text = element_text(size=30))+
geom_point(aes(x=A,color="1"),size=2)+
geom_point(aes(x=B,color="1"),size=2)+
geom_path(aes(x=C,color="2"), linetype=1, size=1)+
geom_point(aes(x=C,color="2"),shape=3,stroke=1.5)+
scale_x_continuous(position="top",limits=c(1.2,1.60))+
scale_y_reverse("Tiefe in cm",breaks=seq(0,80,by=10),limits=c(80,0))+
labs(x="Lagerungsdichte in"~g/cm^3)+
#theme(plot.title=element_text(size=11, family="sans",hjust=0.5))+
scale_color_manual(guide= "legend", values=c("chocolate3","goldenrod3"),name="", labels=c(" Messwert","Mittelwert"))+
guides(color = guide_legend(override.aes = list(linetype = c(0, 1),
                                                 shape = c(16, 3))))+
  theme(text = element_text(size=30))+
 
theme_bw() 


library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)
library(gridExtra)
library(tidyr)

sht = excel_sheets("Lagerung.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("Lagerung.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")

ggplot(data=df, aes(y=Tiefe))+
  geom_point(aes(x=A,color="1"),size=2)+
  geom_point(aes(x=B,color="1"),size=2)+
  geom_path(aes(x=C,color="2"), linetype=1, size=1)+
  geom_point(aes(x=C,color="2"),shape=3,stroke=1.5)+
  scale_x_continuous(position="top",limits=c(1.2,1.60))+
  scale_y_reverse("Tiefe in cm",breaks=seq(0,80,by=10),limits=c(80,0))+
  labs(x="Lagerungsdichte in"~g/cm^3)+
  #theme(plot.title=element_text(size=11, family="sans",hjust=0.5))+
  scale_color_manual(guide= "legend", values=c("chocolate3","goldenrod3"),name="", labels=c(" Messwert","Mittelwert"))+
  guides(color = guide_legend(override.aes = list(linetype = c(0, 1),
                                                  shape = c(16, 3))))+
  theme(text = element_text(size=30))+
  theme_bw() 




      