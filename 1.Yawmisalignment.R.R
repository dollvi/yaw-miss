###########################  1. Moldovo...################################
##########################################################################

load("C:/Users/Swati Singh/Desktop/BDB_MODEL.RData 2017-09-25")

setwd('D:/yaw')

rm(data_moldova_model,data_dovilan_model,data_penaforcada_model,data_salbatica1_model,data_viravento_model)
d1<-subset(data_corugea_model,select = c(TIMESTAMP,ASSETID,ACTIVEPOWER,WINDSPEED,GENERATORSPEED,NACELLEPOSITION,WINDDIRECTION))

library(lubridate)
library(MASS)
library(e1071) ########statistics#########

d1$m<-month(d1$TIMESTAMP)
d1$NP_WD<-d1$NACELLEPOSITION-d1$WINDDIRECTION

d1<-na.omit(d1)

######Select Month###########
d2<-subset(d1,d1$m=='8')
d3<-subset(d2,d2$ACTIVEPOWER>0)
d3$NP_WD1<-ifelse(d3$NP_WD>180,360-d3$NP_WD,d3$NP_WD)
d3$NP_WD2<-ifelse(d3$NP_WD<= -180,d3$NP_WD+360,d3$NP_WD1)

df<-d3

df$NP_WD<-df$NP_WD2

#########Graph Plot########
pdf('NP_WD_Coru_swati.pdf')

op <- par(mfrow = c(2,2),
          oma = c(1,1,1,1) + 2.1,
          mar = c(2,2,1,1) + 0.6)

Assets<- unique(df$ASSETID)

for (i in 1:length(Assets))
{
  TT<- subset(df,ASSETID == Assets[i])

 x <- TT$NP_WD
 TT$Mean<-round(mean(TT$NP_WD),2)
 TT$Skewness<-round(skewness(TT$NP_WD),2)
 TT$Kurtosis<-round(kurtosis(TT$NP_WD),2)
 w<-unique(TT$Mean)
 y<-unique(TT$Skewness)
 z<-unique(TT$Kurtosis)
 
 b1<-nrow(TT)
 b2<-144*31
 
 b3<-round((((b2-b1)/b2)*100),2)
 
 
 s1<-paste("Mean =")
 s2<-paste("Skewness =")
 s3<-paste("kurtosis =")
 s4<-paste("DA% =")
 
 
 h<-hist(x, col="lightgrey",ylab="Density",xlab="NP-WD (Deg)",main=Assets[i],prob=TRUE,
         xlim=c((min(TT$NP_WD)-1),max(TT$NP_WD)+1),ylim=c(0,2))
 lines(density(x))
 text((max(TT$NP_WD)+0.45),1.75,s1,cex=0.8)
 text((max(TT$NP_WD)+0.42),1.65,s2,cex=0.8)
 text((max(TT$NP_WD)+0.45),1.55,s3,cex=0.8)
 text((max(TT$NP_WD)+0.45),1.45,s4,cex=0.8)
 
 text((max(TT$NP_WD)+.85),1.75,w,cex=0.8)
 text((max(TT$NP_WD)+.85),1.65,y,cex=0.8)
 text((max(TT$NP_WD)+.85),1.55,z,cex=0.8)
 text((max(TT$NP_WD)+.85),1.45,b3,cex=0.8)
 
 }

dev.off()
###########################  2. Corugea...################################
##########################################################################


load("C:/Users/Swati Singh/Desktop/BDB_MODEL.RData 2017-09-25")

setwd('D:/yaw')

rm(data_moldova_model,data_dovilan_model,data_penaforcada_model,data_salbatica1_model,data_viravento_model)
d1<-subset(data_corugea_model,select = c(TIMESTAMP,ASSETID,ACTIVEPOWER,WINDSPEED,GENERATORSPEED,NACELLEPOSITION,WINDDIRECTION))

library(lubridate)
library(MASS)
library(e1071) ########statistics#########

d1$m<-month(d1$TIMESTAMP)
d1$NP_WD<-d1$NACELLEPOSITION-d1$WINDDIRECTION

d1<-na.omit(d1)

######Select Month###########
d2<-subset(d1,d1$m=='8')
d3<-subset(d2,d2$ACTIVEPOWER>0)
d3$NP_WD1<-ifelse(d3$NP_WD>180,360-d3$NP_WD,d3$NP_WD)
d3$NP_WD2<-ifelse(d3$NP_WD<= -180,-d3$NP_WD-360,d3$NP_WD1)

df<-d3

df$NP_WD<-df$NP_WD2

#########Graph Plot########
pdf('NP_WD_Coru_Tab.pdf')

op <- par(mfrow = c(2,2),
          oma = c(1,1,1,1) + 2.1,
          mar = c(2,2,1,1) + 0.6)

Assets<- unique(df$ASSETID)

for (i in 1:length(Assets))
{
  TT<- subset(df,ASSETID == Assets[i])
  
  x <- TT$NP_WD
  TT$Mean<-round(mean(TT$NP_WD),2)
  TT$Skewness<-round(skewness(TT$NP_WD),2)
  TT$Kurtosis<-round(kurtosis(TT$NP_WD),2)
  w<-unique(TT$Mean)
  y<-unique(TT$Skewness)
  z<-unique(TT$Kurtosis)
  
  b1<-nrow(TT)
  b2<-144*31
  
  b3<-round((((b2-b1)/b2)*100),2)
  
  
  s1<-paste("Mean =")
  s2<-paste("Skewness =")
  s3<-paste("kurtosis =")
  s4<-paste("DA% =")
  
  
  h<-hist(x, col="lightgrey",ylab="Density",xlab="NP-WD (Deg)",main=Assets[i],prob=TRUE,
          xlim=c((min(TT$NP_WD)-1),max(TT$NP_WD)+1),ylim=c(0,0.5))
  lines(density(x))
  text((max(TT$NP_WD-20)),0.33,s1,cex=0.8)
  text((max(TT$NP_WD-20)),0.28,s2,cex=0.8)
  text((max(TT$NP_WD-20)),0.23,s3,cex=0.8)
  text((max(TT$NP_WD-20)),0.18,s4,cex=0.8)
  
  text((max(TT$NP_WD)-3),0.33,w,cex=0.8)
  text((max(TT$NP_WD)-3),0.28,y,cex=0.8)
  text((max(TT$NP_WD)-3),0.23,z,cex=0.8)
  text((max(TT$NP_WD)-3),0.18,b3,cex=0.8)
  
}

dev.off()
  