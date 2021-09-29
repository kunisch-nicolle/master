library(ggplot2)
library(readxl)
#library(dplyr)
library(hrbrthemes)
#library(lubridate)

WB <-read_excel("Wasserbilanz_NK_r.xlsx", sheet="Rohdaten")

#Zeitstempel korrigieren:
WB$Uhrzeit <- WB$Uhrzeit+(as.Date('2020-11-27') - as.Date(WB$Uhrzeit[1]))
WB$Time <- WB$Time+(as.Date('2020-11-27') - as.Date(WB$Time[1]))
WBx <- data.frame(WB[,c(1,2,3)])

WBb <- na.omit(data.frame(WB[,c(6,7)]))
WBb$Tiefe <- 0
colnames(WBb)[1] <- "Uhrzeit"
#WBb$Beregnung <- WBb$Beregnung*10.
# für die skalierte Darstellung:
WBb$Beregnung <- WBb$Beregnung*(10.*(25./80.))
scoeff <- 80./25.

#Tiefen korrigieren:
WBx$Tiefe_mitte <- WBx$Tiefe + 10

#Anfangswert abziehen für ???theta
#und Wasserbilanz berechnen
WBx$Dtheta <- NA
WBx$Dtheta_mm <- NA
for (ti in c(0,10,20,30,40,50,60,70)){
  WBx$Dtheta[WBx$Tiefe==ti] = 0.01* (WBx$theta[WBx$Tiefe==ti] - mean(WBx$theta[WBx$Tiefe==ti][1:3]))
  if (ti == 0 || ti == 70) {mfx <- 150.} else {mfx <- 100.}
  WBx$Dtheta_mm[WBx$Tiefe==ti] = WBx$Dtheta[WBx$Tiefe==ti] * mfx
}


## Tiefe als Factor, wobei die Levels umgekehrt sortiert werden:
WBx$Tiefe <- factor(WBx$Tiefe , levels=c("70", "60", "50", "40", "30", "20", "10","0") )


p <- ggplot(data=WBx)+
  geom_area(aes(x = Uhrzeit,y = Dtheta_mm, fill = Tiefe), alpha=0.6 , size=.5, colour="white") +
  scale_fill_discrete(type=rev(c("lightgoldenrod4","lightgoldenrod3",
                                 "lightgoldenrod1","lightgoldenrod2",
                                 "pink3","plum3","orchid","purple4"))) +
  
  labs(x="Uhrzeit",fill="Tiefe in cm")+
  theme_bw()+
  guides(fill = guide_legend(reverse=T))+
  theme(text = element_text(size=18))


p + geom_line(data = WB, aes(x = Uhrzeit, y=Beregnung, colour = "Niederschlag")) +
  scale_y_continuous(
    # Features of the first axis
    name = "Bodenwasser in mm",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*scoeff, name="Beregnung in mm")
  ) +
  
  scale_color_manual(values = "blue", name = "")