library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

#Tiefe 60cm

TS<-read.csv("KU_Boxplot_60.csv",header=T,dec=",",sep=";")
TS$Kategorie <- factor(TS$Kategorie,
                       levels = c('Ksat','KU','KS'),ordered = TRUE)

ggplot(data=TS,aes(y=KS,group=Kategorie,x=Kategorie,fill=Kategorie))+
  theme_bw()+
  
  #geom_point(position = "jitter") +
 stat_boxplot(geom="errorbar",width=0.5)+
  geom_boxplot(outlier.size = 2,outlier.shape = 8,show.legend = T)+
  #geom_jitter(position = position_jitter(0.5), aes(colour = Kategorie))+
  geom_jitter(color="black", size=1, alpha=0.5) +  
  scale_fill_manual(name="",values=c("#56B4E9", "#E69F00","#999999"),labels=c("KS ", "Ksat", "KU"))+
 # ggtitle("6cm Tiefe")+
  theme(legend.position="bottom")+
  labs(x="",y=expression(logK~"in"~m/s))+
  scale_y_continuous(breaks=seq(-8,-3,by=1),limits=c(-8,-3))+
  theme(text = element_text(size=18))
  

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

#Tiefe 31cm

TS<-read.csv("KU_Boxplot_31.csv",header=T,dec=",",sep=";")
TS$Kategorie <- factor(TS$Kategorie,
                       levels = c('Ksat','KU','KS'),ordered = TRUE)


ggplot(data=TS,aes(y=KS,group=Kategorie,x=Kategorie,fill=Kategorie))+
  theme_bw()+
  
  #geom_point(position = "jitter") +
  stat_boxplot(geom="errorbar",width=0.5)+
  geom_boxplot(outlier.size = 2,outlier.shape = 8,show.legend = T)+
  geom_jitter(color="black", size=1, alpha=0.5) +  
  scale_fill_manual(name="",values=c("#56B4E9","#E69F00","#999999"),labels=c("Kx", "Ksat ", "KU "))+
  #ggtitle("31cm Tiefe")+
 theme(legend.position="bottom")+
  labs(x="",y=expression(logK~"in"~m/s))+
  scale_y_continuous(breaks=seq(-8,-3,by=1),limits=c(-8,-3))+
  theme(text = element_text(size=18))

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

#Tiefe 17cm

TS<-read.csv("KU_Boxplot_17.csv",header=T,dec=",",sep=";")
TS$Kategorie <- factor(TS$Kategorie,
                       levels = c('Ksat','KU','KS'),ordered = TRUE)


ggplot(data=TS,aes(y=KS,group=Probe,x=Kategorie,fill=Kategorie))+
  theme_bw()+
  
  #geom_point(position = "jitter") +
  stat_boxplot(geom="errorbar",width=0.5)+
  geom_boxplot(outlier.size = 2,outlier.shape = 8,show.legend = T)+
  geom_jitter(color="black", size=1, alpha=2) +  
  scale_fill_manual(name="",values=c("#56B4E9","#E69F00","#999999"),labels=c("KS ", "Ksat ", "KU"))+
  #ggtitle("17cm Tiefe")+
  theme(legend.position="bottom")+
  labs(x="",y=expression(logK~"in"~m/s))+
  scale_y_continuous(breaks=seq(-8,-3,by=1),limits=c(-8,-3))+
  theme(text = element_text(size=18))

#Tiefe 6cm
library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)



TS<-read.csv("KU_Boxplot_6.csv",header=T,dec=",",sep=";")
TS$Kategorie <- factor(TS$Kategorie,
                       levels = c('Ksat','KU','KS'),ordered = TRUE)

ggplot(data=TS,aes(y=KS,group=Kategorie,x=Kategorie,fill=Kategorie))+
  theme_bw()+
  
  #geom_point(position = "jitter") +
  stat_boxplot(geom="errorbar",width=0.5)+
  geom_boxplot(outlier.size = 2,outlier.shape = 8,show.legend = T)+
  geom_jitter(color="black", size=1, alpha=0.5) +  
  scale_fill_manual(name="",values=c("#56B4E9","#E69F00","#999999"),labels=c("KS (Bodenmatrix)", "Ksat inklusive Makroporen", "KU inklusive Makroporen",size=20))+
  #ggtitle("6cm Tiefe")+
  theme(legend.position="bottom")+
    labs(X="",y=expression(logK~"in"~m/s))+
  scale_y_continuous(breaks=seq(-8,-3,by=1),limits=c(-8,-3))+
  guides(shape = guide_legend(override.aes = list(size = 0)))+
  theme(text = element_text(size=18))
  
