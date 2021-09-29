#Bodenfeuchtedifferenz

library(ggplot2)

Bodenfeuchte<-read.csv("Beregnung_Tabelle.csv", header=T, dec=",", sep = ";")
ggplot(data= Bodenfeuchte, aes(x=Uhrzeit,y=Tiefe)) + 
  theme_bw()+
  geom_raster(aes(fill = Wert), interpolate = TRUE) + 
  scale_fill_gradientn(colours=c("#ead911","#f5f3fb","#6abab1","#007fc4","#074560"), name = "Bodenfeuchte-\nänderung in Vol.-%", limits = c(-10, 55)) + 
  scale_y_reverse(breaks=seq(0,80,by=10), limits=c(75,-5))+
  geom_segment(aes(x = 1, y = -5, xend = 1, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 16, y = -5, xend = 16, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 24, y = -5, xend = 24, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 30, y = -5, xend = 30, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 46, y = -5, xend = 46, yend = 75),color="grey26",size=1)+
  labs(x="Uhrzeit",y="Tiefe in cm")+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks = c("11:00:00","12:00:00","14:00:00"))+
  theme(text = element_text(size=20))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Bodenfeuchte
library(ggplot2)

Bodenfeuchte<-read.csv("Bodenfeuchte_normal.csv", header=T, dec=",", sep = ";")
ggplot(data= Bodenfeuchte, aes(x=Uhrzeit,y=Tiefe)) + 
theme_bw()+
geom_raster(aes(fill = Wert), interpolate = TRUE) + 
scale_fill_gradientn(colours=c("#f5f3fb","#6abab1","#007fc4","#074560"), name = "Bodenfeuchte\n in Vol.-%", limits = c(0, 55)) + 
scale_y_reverse(breaks=seq(0,80,by=10), limits=c(75,-5))+
geom_segment(aes(x = 1, y = -5, xend = 1, yend = 75),color="grey26",size=1)+
geom_segment(aes(x = 26, y = -5, xend = 26, yend = 75),color="grey26",size=1)+
geom_segment(aes(x = 30, y = -5, xend = 30, yend = 75),color="grey26",size=1)+ 
geom_segment(aes(x = 46, y = -5, xend = 46, yend = 75),color="grey26",size=1)+
labs(x="Uhrzeit",y="Tiefe in cm")+
theme(plot.title = element_text(hjust=0.5))+
scale_x_discrete(breaks = c("11:00:00","12:00:00","14:00:00"))+
  theme(text = element_text(size=18))
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))#

#Bodenfeuchteänderung

library(ggplot2)

Bodenfeuchte<-read.csv("Beregnungaend.csv", header=T, dec=",", sep = ";")
ggplot(data= Bodenfeuchte, aes(x=Uhrzeit,y=Tiefe)) + 
  theme_bw()+
  geom_raster(aes(fill = A), interpolate = TRUE) + 
  scale_fill_gradientn(colours=c("#ead911","#f5f3fb","#6abab1","#007fc4","#074560"), name = "Bodenfeuchte-\nänderung in Vol%", limits = c(-10, 16)) + 
  scale_y_reverse(breaks=seq(0,80,by=10), limits=c(75,-5))+
  geom_segment(aes(x = 1, y = -5, xend = 1, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 26, y = -5, xend = 26, yend = 75),color="grey26",size=1)+
  labs(x="Uhrzeit",y="Tiefe in cm")+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks = c("11:00:00","12:00:00","14:00:00"))

##Vorfeuchte


Bodenfeuchte<-read.csv("Vorfeuchte_1.csv", header=T, dec=",", sep = ";")

ggplot(data= Bodenfeuchte, aes(x=Uhrzeit,y=Tiefe)) + 
  theme_bw()+
  geom_raster(aes(fill = Wert), interpolate = TRUE) + 
  scale_fill_gradientn(colours=c("#f5f3fb","#6abab1","#007fc4","#074560"), name = "Vorfeuchte\n in Vol-%", limits = c(0, 50)) + 
  scale_y_reverse(breaks=seq(0,80,by=10), limits=c(75,-5))+
    labs(x="Uhrzeit",y="Tiefe in cm")+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks = c("10:30:00","10:40:00"))

#rote Linien markieren Start bzw. Endpunkt der Beregnung. Mit dem Einfügen der Linien werden allerdings die ersten Werte entfernt!?

  
#library(ggplot2)
#library(lubridate)
#Bodenfeuchte<-read.csv("Beregnung_Tabelle.csv", header=T, dec=",", sep = ";")
#Bodenfeuchte_time<-Bodenfeuchte
#date_time<-as.POSIXct(strptime(Bodenfeuchte$Uhrzeit,format="%H:%M:%S"),tz="")
#Bodenfeuchte_time$Uhrzeit<-date_time

#ggplot(data= Bodenfeuchte_time, aes(x=Uhrzeit,y=Tiefe)) + 
#geom_raster(aes(fill = Wert, linejoin = 'mitre'), interpolate = TRUE) + 
#scale_fill_gradientn(colours=c("#f4ec2a","#f5f3fb","#83079a")) + 
#scale_y_reverse()+
#labs(x="Uhrzeit in h",y="Tiefe in cm",title="Bodenfeuchte TDR Sonde")+
#theme(plot.title = element_text(hjust=0.5))+



