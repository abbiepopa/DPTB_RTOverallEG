###Stats for Dx###
setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

DataWide<-read.csv("EG for reanalysis - outliers removed 1.1.csv")
Dx <- read.csv("DxCodes.csv")
DxDataWide<-merge(DataWide,Dx)
oldDxDataWide<-DxDataWide
DxDataWide<-oldDxDataWide
###remove outliers before reshaping###
outliers<-c()
for (i in 2:10) {
	curr_mean<-mean(DxDataWide[,i])
	curr_sd<-sd(DxDataWide[,i])
	curr_mean_plus2<-curr_mean+(2*curr_sd)
	curr_mean_minus2<-curr_mean-(2*curr_sd)
	
	curr_outliers<-c(colnames(DxDataWide)[i],DxDataWide[which(DxDataWide[,i]<curr_mean_minus2),1])
#	print(curr_outliers)
DxDataWide[which(DxDataWide[,i]<curr_mean_minus2),i]<-NA
	curr_outliers<-c(curr_outliers, DxDataWide[which(DxDataWide[,i]>curr_mean_plus2),1])
#	print(curr_outliers)
DxDataWide[which(DxDataWide[,i]>curr_mean_plus2),i]<-NA
	outliers<-c(outliers,curr_outliers)
}
write.csv(outliers, "OverallEGDx_outliers.csv")
library(reshape)
DxDataLong<-melt(DxDataWide, id=c("CABIL_ID","Dx"))

library(nlme)
colnames(DxDataLong)<-c("Subj","Dx","Condition","LookingTime")

#Analyze Angry Trials
dataAngry<-subset(DxDataLong, Condition=="PercAngry" | Condition=="PercAngryNeutral")
fita<-lme(LookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=dataAngry, na.action=na.omit)

dataAngry22q<-subset(dataAngry, Dx=="22q")
dataAngryTD<-subset(dataAngry, Dx=="TD")

fit22qa<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataAngry22q, na.action=na.omit)
fitTDa<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataAngryTD, na.action=na.omit)

summary(fita)
summary(fit22qa)
summary(fitTDa)

#Analyze Happy Trials
DataHappy<-subset(DxDataLong, Condition=="PercHappy" | Condition=="PercHappyNeutral")
DataHappy<-na.omit(DataHappy)
fith<-lme(LookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=DataHappy, na.action=na.omit)

dataHappy22q<-subset(DataHappy, Dx=="22q")
dataHappyTD<-subset(DataHappy,Dx=="TD")

fit22qh<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataHappy22q, na.action=na.omit)
fitTDh<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataHappyTD, na.action=na.omit)

summary(fith)
summary(fit22qh)
summary(fitTDh)

###Centered Looking Time###

DxDataLong$centeredLookingTime<-DxDataLong$LookingTime - (1/3)


#Analyze Angry Trials
dataAngry<-subset(DxDataLong, Condition=="PercAngry" | Condition=="PercAngryNeutral")
fitac<-lme(centeredLookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=dataAngry)

fit22qac<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataAngry22q)
fitTDac<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataAngryTD)

summary(fitac)
summary(fit22qac)
summary(fitTDac)

#Analyze Happy Trials
DataHappy<-subset(DxDataLong, Condition=="PercHappy" | Condition=="PercHappyNeutral")
DataHappy<-na.omit(DataHappy)
fithc<-lme(centeredLookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=DataHappy)

fit22qhc<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataHappy22q)
fitTDhc<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=dataHappyTD)

summary(fithc)
summary(fit22qhc)
summary(fitTDhc)

###angry versus happy###
DataEmo<-subset(DxDataLong, Condition=="PercHappy" | Condition == "PercAngry")
fitemoc<-lme(centeredLookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=DataEmo, na.action=na.omit)

DataEmo22q <- subset(DataEmo, Dx=="22q")
DataEmoTD <- subset(DataEmo, Dx=="TD")

fit22qemoc<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=DataEmo22q, na.action=na.omit)

fitTDemoc<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=DataEmoTD, na.action=na.omit)

summary(fitemoc)
summary(fit22qemoc)
summary(fitTDemoc)

library(psych)
d22qdesc<-describeBy(DxDataWide, group=DxDataWide$Dx, digits=4)[[1]]
TDdesc<-describeBy(DxDataWide, group=DxDataWide$Dx, digits=4)[[2]]

DG22q<-d22qdesc[c("PercHappy","PercAngry","PercHappyNeutral","PercAngryNeutral"), c("n","mean","sd","se")]


DGTD<-TDdesc[c("PercHappy","PercAngry","PercHappyNeutral","PercAngryNeutral"), c("n","mean","sd","se")]

DG22qperc<-DG22q*100
DGTDperc<-DGTD*100

