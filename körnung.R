#library(ggplot2)
#library(readxl)
#library(scales)

#Koernung<-read_excel("K?rnungsanalyse1.xlsx", sheet = "0-10")
#ggplot(data=Koernung, aes(x=Korngr??e,y=Entnahmetiefe))+
#geom_path(linetype=1,size=1,color="orange")+
#geom_point(size=2,color="orange")+
#scale_x_continuous(trans='log2')+
#scale_x_continuous(trans = log2_trans(),breaks = trans_breaks("log2", function(x) 5^x)) +
#scale_y_continuous("Anteil an der Kornverteilung [M.%]",breaks=seq(0,120,by=20),limits=c(0,120))+
#labs(title="K?rnung 0-10")+
#theme(plot.title=element_text(size=11, family="sans",hjust=0.5)
      
      
library(ggplot2)
Koernung<-read.csv("Korn.csv", header=T, dec=",", sep = ";")

ggplot(data=Koernung, aes(x=Korngr))+
  theme_bw()+
geom_path(aes(y=A,color="1"), linetype=1, size=0.5)+
geom_point(aes(y=A,color="1"),size=2)+
geom_path(aes(y=B,color="2"), linetype=1, size=0.5)+
geom_point(aes(y=B,color="2"),size=2)+
  geom_path(aes(y=C,color="3"), linetype=1, size=0.5)+
  geom_point(aes(y=C,color="3"),size=2)+
  geom_path(aes(y=D,color="4"), linetype=1, size=0.5)+
  geom_point(aes(y=D,color="4"),size=2)+
  geom_path(aes(y=E,color="5"), linetype=1, size=0.5)+
  geom_point(aes(y=E,color="5"),size=2)+
  geom_path(aes(y=G,color="6"), linetype=1, size=0.5)+
  geom_point(aes(y=G,color="6"),size=2)+
  geom_path(aes(y=H,color="7"), linetype=1, size=0.5)+
  geom_point(aes(y=H,color="7"),size=2)+
  geom_path(aes(y=I,color="8"), linetype=1, size=0.5)+
  geom_point(aes(y=I,color="8"),size=2)+
  scale_x_continuous("Korngröße in µm",breaks = c(0,1,2,3), labels = c("2", "20","200","2000"))+
  scale_y_continuous("Anteil an der Kornfraktion in M%",breaks= seq(0,100,by=20),limits=c(0,103))+#labs(x="KorngrÃ¶ÃŸe",y="Anteil an der Kornverteilung [M%]",titel="Körnung")
scale_color_manual(values=c("yellow2","bisque3","grey30","chocolate3","cadetblue","hotpink3","darkorange1","yellowgreen"),name="Entnahmetiefe:",labels=c("0-10 cm","10-20 cm","20-30 cm","30-40 cm","40-50 cm","50-60 cm","60-70 cm","70-80 cm"))+
  theme(text = element_text(size=15))+
 # guides(colour = guide_legend(override.aes = list (size=c(2,2,2,2,2,2,1,2))))                                                size=c(2,2,2,1,2))),

library(ggplot2)
Koernung<-read.csv("Korn.csv", header=T, dec=",", sep = ";")

ggplot(data=Koernung, aes(x=Korngr))+
  geom_path(aes(y=A,color="1"), linetype=1, size=1)+
  geom_point(aes(y=A,color="1"),size=2)+
  geom_path(aes(y=B,color="2"), linetype=1, size=1)+
  geom_point(aes(y=B,color="2"),size=2)+
  geom_path(aes(y=D,color="3"), linetype=1, size=1)+
  geom_point(aes(y=D,color="3"),size=2)+
  geom_path(aes(y=H,color="4"), linetype=1, size=1)+
  geom_point(aes(y=H,color="4"),size=2)+
  scale_x_continuous(breaks = c(0,1,2,3), labels = c("2", "20","200","2000"))+
  labs(x="KorngrÃ¶ÃŸe [Âµm]",y="Anteil an der Kornverteilung [M%]")+
  scale_color_manual(values=c("yellow","bisque3","chocolate3","darkorange1"), name="Entnahmetiefe:", labels=c(" 0-10 cm","10-20 cm","30-40 cm","60-70 cm"))+
theme_linedraw()

#Bodenfeuchtedifferenz

library(ggplot2)

Bodenfeuchte<-read.csv("Beregnung_Tabelle.csv", header=T, dec=",", sep = ";")
ggplot(data= Bodenfeuchte, aes(x=Uhrzeit,y=Tiefe)) + 
  theme_bw()+
  geom_raster(aes(fill = Wert), interpolate = TRUE) + 
  scale_fill_gradientn(colours=c("#ead911","#f5f3fb","#6abab1","#007fc4","#074560"), name = "Bodenfeuchte-\nänderung in Vol.-%", limits = c(-10, 55)) + 
  scale_y_reverse(breaks=seq(0,80,by=10), limits=c(80,0))+
  geom_segment(aes(x = 1, y = -5, xend = 1, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 16, y = -5, xend = 16, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 24, y = -5, xend = 24, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 30, y = -5, xend = 30, yend = 75),color="grey26",size=1)+
  geom_segment(aes(x = 46, y = -5, xend = 46, yend = 75),color="grey26",size=1)+
  labs(x="Uhrzeit",y="Tiefe in cm")+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_discrete(breaks = c("11:00:00","12:00:00","14:00:00"))+
  theme(text = element_text(size=20))
