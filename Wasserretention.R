
#pf-Kurve 6 cm Tiefe"

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

sht = excel_sheets("pF_Tiefe6cm_n.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("pF_Tiefe6cm_n.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")


ggplot(data=df, aes(x=pF))+
  theme_bw()+
  geom_point(aes(y=A,color="1"),size=2,shape=1)+
  geom_point(aes(y=B,color="2"),size=2,shape=1)+
  geom_path(aes(y=C,color="3"), linetype=1, size=1)+
  scale_y_continuous(breaks=seq(0,50,by=10),limits = c(0,50))+
   scale_x_continuous(breaks=seq(0,7,by=1),limits = c(0,7))+
  geom_segment(aes(x = 1.8, y = 0, xend = 1.8, yend = 50),color="grey66",size=1)+
  geom_segment(aes(x = 4.2, y = 0, xend = 4.2, yend = 50),color="grey66",size=1)+
labs(x="pF",y="volumetrischer Wassergehalt in %",title="")+
scale_color_manual(values=c("red2","yellowgreen","skyblue2"), name="", labels=c("Probennummer 17","Probennummer 1517", "van Genuchten - \n m=1-1/n "))+
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0,1),
                                                  shape = c(1, 1,NA))))+
  theme(text = element_text(size=15), legend.position = "bottom")
#legend.text=element_text(size=14)
#pF-Kurve 17 cm Tiefe"


library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

sht = excel_sheets("pF_Tiefe17cm_n.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("pF_Tiefe17cm_n.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")


ggplot(data=df, aes(x=pF))+
  theme_bw()+
  geom_point(aes(y=A,color="1"),size=2,shape=1)+
  geom_point(aes(y=B,color="2"),size=2,shape=1)+
  geom_point(aes(y=C,color="3"),size=2,shape=1)+
  geom_path(aes(y=D,color="4"), linetype=1, size=1)+
  scale_y_continuous(breaks=seq(0,50,by=10),limits = c(0,50))+
  scale_x_continuous(breaks=seq(0,7,by=1),limits = c(0,7))+
  geom_segment(aes(x = 1.8, y = 0, xend = 1.8, yend = 50),color="grey66",size=1)+
  geom_segment(aes(x = 4.2, y = 0, xend = 4.2, yend = 50),color="grey66",size=1)+
  labs(x="pF",y="volumetrischer Wassergehalt in %",title="")+
  scale_color_manual(values=c("red2","yellowgreen","mediumpurple1","chocolate2"), name="", labels=c("Probennummer 15466","Probennummer 15718", "Probennummer 15720", "van Genuchten - \n m=1-1/n "))+
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0,0,1),
                                                  shape = c(1, 1,1,NA))))+
  theme(text = element_text(size=15),legend.position = "bottom")

#Tiefe 31 cm

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

sht = excel_sheets("pF_Tiefe31cm_n.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("pF_Tiefe31cm_n.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")



ggplot(data=df, aes(x=pF))+
  theme_bw()+
  geom_point(aes(y=A,color="1"),size=2,shape=1)+
  geom_point(aes(y=B,color="2"),size=2,shape=1)+
  geom_point(aes(y=C,color="3"),size=2,shape=1)+
  geom_path(aes(y=D,color="4"), linetype=1, size=1)+
  scale_y_continuous(breaks=seq(0,50,by=10),limits = c(0,50))+
  scale_x_continuous(breaks=seq(0,7,by=1),limits = c(0,7))+
  geom_segment(aes(x = 1.8, y = 0, xend = 1.8, yend = 50),color="grey66",size=1)+
  geom_segment(aes(x = 4.2, y = 0, xend = 4.2, yend = 50),color="grey66",size=1)+
  labs(x="pF",y="volumetrischer Wassergehalt in %",title="")+
  scale_color_manual(values=c("red2","yellowgreen","mediumpurple1","hotpink3"), name="", labels=c("Probennummer 15721","Probennummer 15722", "Probennummer 15724",  "van Genuchten - \n m=1-1/n "))+
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0,0,1),
                                                  shape = c(1, 1,1,NA))))+
  theme(text = element_text(size=15),legend.position = "bottom")




#Tiefe 60 cm

library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

sht = excel_sheets("pF_Tiefe60cm.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("pF_Tiefe60cm.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")


ggplot(data=df, aes(x=pF))+
  theme_bw()+
  geom_point(aes(y=A,color="1"),size=2,shape=1)+
  geom_point(aes(y=B,color="2"),size=2,shape=1)+
  geom_point(aes(y=C,color="3"),size=2,shape=1)+
  geom_path(aes(y=D,color="4"), linetype=1, size=1)+
  scale_y_continuous(breaks=seq(0,50,by=10),limits = c(0,50))+
  scale_x_continuous(breaks=seq(0,7,by=1),limits = c(0,7))+
  geom_segment(aes(x = 1.8, y = 0, xend = 1.8, yend = 50),color="grey66",size=1)+
  geom_segment(aes(x = 4.2, y = 0, xend = 4.2, yend = 50),color="grey66",size=1)+
  labs(x="pF",y="volumetrischer Wassergehalt in %",title="")+
  scale_color_manual(values=c("red2","yellowgreen","mediumpurple1","yellow2"), name="", labels=c("Probennummer 5","Probennummer 15716", "Probennummer 15725", "van Genuchten - \n m=1-1/n "))+
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0,0,1),
                                                  shape = c(1, 1,1,NA))))+
  theme(text = element_text(size=15),legend.position = "bottom")
#620,400


library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)

sht = excel_sheets("Fitting.xlsx")
df = lapply(setNames(sht, sht), function(s) read_excel("Fitting.xlsx", sheet=s))
df = bind_rows(df, .id="Sheet")


ggplot(data=df, aes(x=pF))+
  theme_bw()+
  geom_path(aes(y=A,color="1"),size=1,)+
  geom_path(aes(y=B,color="2"),size=1)+
  geom_path(aes(y=C,color="3"),size=1)+
  geom_path(aes(y=D,color="4"),size=1)+
    scale_y_continuous(breaks=seq(0,50,by=10),limits = c(0,50))+
  scale_x_continuous(breaks=seq(0,7,by=1),limits = c(0,7))+
  labs(x="pF",y="volumetrischer Wassergehalt in %",title="")+
  geom_segment(aes(x = 1.8, y = 0, xend = 1.8, yend = 50),color="grey66",size=1)+
  geom_segment(aes(x = 4.2, y = 0, xend = 4.2, yend = 50),color="grey66",size=1)+
  scale_color_manual(values=c("skyblue2","chocolate2","mediumorchid3","yellow2"), name="", labels=c("6 cm Tiefe ","17 cm Tiefe", "31 cm Tiefe", "60 cm Tiefe"))+
    theme(text = element_text(size=15),legend.position = "bottom")