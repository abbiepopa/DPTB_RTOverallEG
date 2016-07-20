###Stats for Dx###
setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

ClusDataWide<-read.csv("AllDataClus outliers removed 1.2.csv")
library(reshape)
ClusDataLong<-melt(ClusDataWide, id=c("Subj","Cluster"))

library(nlme)
colnames(ClusDataLong)<-c("Subj","Cluster","Condition","LookingTime")
ClusDataLong$centeredLookingTime<-ClusDataLong$LookingTime - (1/3)

#Angry Trials#
dataAngry<-subset(ClusDataLong, Condition=="PercAngry" | Condition=="PercAngryNeutral")
fitac<-lme(centeredLookingTime~factor(Condition)*Cluster, random=~1|Subj, data=dataAngry)

dataAngry1<-subset(dataAngry, Cluster==1)
dataAngry2<-subset(dataAngry, Cluster==2)

fitac1<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataAngry1)
fitac2<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataAngry2)

summary(fitac)
summary(fitac1)
summary(fitac2)

#Happy Trials#
dataHappy<-subset(ClusDataLong, Condition=="PercHappy" | Condition=="PercHappyNeutral")
fithc<-lme(centeredLookingTime~factor(Condition)*Cluster, random=~1|Subj, data=dataHappy)

dataHappy1<-subset(dataHappy, Cluster==1)
dataHappy2<-subset(dataHappy, Cluster==2)

fithc1<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataHappy1)
fithc2<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataHappy2)

summary(fithc)
summary(fithc1)
summary(fithc2)

#angry versus happy#
DataEmo<-subset(ClusDataLong, Condition=="PercHappy" | Condition == "PercAngry")
fitemoc<-lme(centeredLookingTime~factor(Condition)*Cluster, random=~1|Subj, data=DataEmo)

DataEmo1<-subset(DataEmo, Cluster==1)
DataEmo2<-subset(DataEmo, Cluster==2)

fitemoc1<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=DataEmo1)
fitemoc2<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=DataEmo2)

summary(fitemoc)
summary(fitemoc1)
summary(fitemoc2)

###Desc###
library(psych)
d1<-describeBy(ClusDataWide, group=ClusDataWide$Cluster, digits=4)[[1]]
d2<-describeBy(ClusDataWide, group=ClusDataWide$Cluster, digits=4)[[2]]

DG1<-d1[c("PercHappy","PercAngry","PercHappyNeutral","PercAngryNeutral"), c("n","mean","sd","se")]

DG2<-d2[c("PercHappy","PercAngry","PercHappyNeutral","PercAngryNeutral"), c("n","mean","sd","se")]

DG1perc<-DG1*100
DG2perc<-DG2*100