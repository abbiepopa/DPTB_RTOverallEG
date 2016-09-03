rm(list=ls())
setwd("/Users/abbiepopa/Documents/Lab/DPTB")
e<-read.csv("everyone_demo.csv")

###Stats for Dx###
setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

DataWide<-read.csv("EG for reanalysis - outliers removed 1.2.csv")
Dx <- read.csv("DxCodes.csv")
DxDataWide<-merge(DataWide,Dx)

DxDataWide <- merge(DxDataWide, e, all.x=T, all.y=F)

library(reshape)
DxDataLong<-melt(DxDataWide, id=c("CABIL_ID","Dx", "Dx_Code","Age","Gender_Code","Cluster.Analysis.","Overall.EG","Spence_Total_T_Score_Parent1","ABAS_GAC_Composite_Parent1","WISCIV_FullScale_C"))

library(nlme)
colnames(DxDataLong)<-c("Subj","Dx","Dx_Code","Age","Gender_Code","Cluster.Analysis.","Overall.EG","Spence_Total_T_Score_Parent1","ABAS_GAC_Composite_Parent1","WISCIV_FullScale_C","Condition","LookingTime")

#Analyze Angry Trials
dataAngry<-subset(DxDataLong, Condition=="PercAngry" | Condition=="PercAngryNeutral")
fita<-lme(LookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=dataAngry)

dataAngry22q<-subset(dataAngry, Dx=="22q")
dataAngryTD<-subset(dataAngry, Dx=="TD")

fit22qa<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataAngry22q)
fitTDa<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataAngryTD)

summary(fita)
summary(fit22qa)
summary(fitTDa)

#Analyze Happy Trials
DataHappy<-subset(DxDataLong, Condition=="PercHappy" | Condition=="PercHappyNeutral")
DataHappy<-na.omit(DataHappy)
fith<-lme(LookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=DataHappy)

dataHappy22q<-subset(DataHappy, Dx=="22q")
dataHappyTD<-subset(DataHappy,Dx=="TD")

fit22qh<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataHappy22q)
fitTDh<-lme(LookingTime~factor(Condition), random=~1|Subj, data=dataHappyTD)

summary(fith)
summary(fit22qh)
summary(fitTDh)

###Centered Looking Time###

DxDataLong$centeredLookingTime<-DxDataLong$LookingTime - (1/3)


dataAngry22q<-subset(dataAngry, Dx=="22q")
dataAngryTD<-subset(dataAngry, Dx=="TD")


dataHappy22q<-subset(DataHappy, Dx=="22q")
dataHappyTD<-subset(DataHappy,Dx=="TD")


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
fitemoc<-lme(centeredLookingTime~factor(Condition)+factor(Dx)+factor(Condition)*factor(Dx), random=~1|Subj, data=DataEmo)

DataEmo22q <- subset(DataEmo, Dx=="22q")
DataEmoTD <- subset(DataEmo, Dx=="TD")

fit22qemoc<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=DataEmo22q)

fitTDemoc<-lme(centeredLookingTime~factor(Condition), random=~1|Subj, data=DataEmoTD)

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

###including other factors (i.e., age, gender, IQ) in models###

#Age - sig Angry & Happy
summary(lme(LookingTime~factor(Condition)+factor(Dx)+Age, random=~1|Subj, data=dataAngry))
 plot(dataAngry[which(dataAngry$Condition == "PercAngryNeutral"),"Age"], dataAngry[which(dataAngry$Condition == "PercAngryNeutral"),"LookingTime"])
abline(lm(LookingTime~Age, data=dataAngry[which(dataAngry$Condition == "PercAngryNeutral"),]))

summary(lme(LookingTime~factor(Condition)+factor(Dx)+Age, random=~1|Subj, data=DataHappy))

 plot(DataHappy[which(DataHappy$Condition == "PercHappyNeutral"),"Age"], DataHappy[which(DataHappy$Condition == "PercHappyNeutral"),"LookingTime"])
abline(lm(LookingTime~Age, data=DataHappy[which(DataHappy$Condition == "PercHappyNeutral"),]))

summary(lme(LookingTime~factor(Condition)+Age, random=~1|Subj, data=dataAngry22q))
#age p = 0.0019
summary(lme(LookingTime~factor(Condition)+Age, random=~1|Subj, data=dataAngryTD))
#age p = 0.0669

summary(lme(LookingTime~factor(Condition)+Age, random=~1|Subj, data=dataHappy22q))
#age p = 0.0280
summary(lme(LookingTime~factor(Condition)+Age, random=~1|Subj, data=dataHappyTD))
#age p = 0.1566

#Gender = NS
summary(lme(LookingTime~factor(Condition)+factor(Dx)+factor(Gender_Code), random=~1|Subj, data=dataAngry))

summary(lme(LookingTime~factor(Condition)+factor(Dx)+factor(Gender_Code), random=~1|Subj, data=DataHappy))

#IQ = NS
summary(lme(LookingTime~factor(Condition)+factor(Dx)+WISCIV_FullScale_C, random=~1|Subj, data=dataAngry, na.action = na.omit))

summary(lme(LookingTime~factor(Condition)+factor(Dx)+WISCIV_FullScale_C, random=~1|Subj, data=DataHappy, na.action = na.omit))

#SCAS = NS
summary(lme(LookingTime~factor(Condition)+factor(Dx)+Spence_Total_T_Score_Parent1, random=~1|Subj, data=dataAngry, na.action = na.omit))

summary(lme(LookingTime~factor(Condition)+factor(Dx)+Spence_Total_T_Score_Parent1, random=~1|Subj, data=DataHappy, na.action = na.omit))

#ABAS = NS
summary(lme(LookingTime~factor(Condition)+factor(Dx)+ABAS_GAC_Composite_Parent1, random=~1|Subj, data=dataAngry, na.action = na.omit))

summary(lme(LookingTime~factor(Condition)+factor(Dx)+ABAS_GAC_Composite_Parent1, random=~1|Subj, data=DataHappy, na.action = na.omit))

###Only age is significant. May actually be useful, since that may lend credence to our "children show different biases than adults" statements
###stronger in 22q, could be power effect, could be that developmental delay puts them more in the window for this change

