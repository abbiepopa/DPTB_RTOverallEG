#Set-Up
setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")
AllData<-read.csv("AllData.csv",header=TRUE)
AllData<-AllData[2:11]
library(psych)

# import the data for the current participant
setwd("/Users/abbiepopa/Documents/Lab/DPTB/ParticipantData")
CurrPart<-read.csv("785.csv", header=TRUE)

setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")
# trim the columns to the ones you need
myCols<-c("Subject","ValidityLeftEye","ValidityRightEye","ProcCode","Display","AOI")
CurrPartCols<-CurrPart[myCols]

# trim the rows to the ones you need; here we remove all the epochs for which the facepair isn't on display and for which they have an emotional face. doing this before calculating validity means validity is a measure of how much data we are missing, not a measure of how well the participants do the task, since that validity measure could include all epochs
CurrPartColsRows<-subset(CurrPartCols, Display=="FacePair")
CurrPartColsRows<-subset(CurrPartColsRows, ProcCode!="NNL" )
CurrPartColsRows<-subset(CurrPartColsRows, ProcCode!="NNR" )

#Calculate % Invalid, we MUST include blank AOIs, as these could include areas of the screen lacking fixation or either of the faces
CurrPartColsRowsVal<-subset(CurrPartColsRows, ValidityLeftEye==0 & ValidityRightEye==0)
PercInvalid<-(nrow(CurrPartColsRows)-nrow(CurrPartColsRowsVal))/nrow(CurrPartColsRows)

# calculate %right, %left, %fix
NRgtLftFix<-table(CurrPartColsRowsVal$AOI)
PercLeft<-NRgtLftFix[2]/nrow(CurrPartColsRowsVal)
PercRight<-NRgtLftFix[3]/nrow(CurrPartColsRowsVal)
PercFix<-NRgtLftFix[4]/nrow(CurrPartColsRowsVal)

# split into angry trials and happy trials
CurrPartAngry<-subset(CurrPartColsRowsVal, ProcCode=="ANI" | ProcCode=="ANV" | ProcCode=="NAI" | ProcCode=="NAV")
CurrPartHappy<-subset(CurrPartColsRowsVal, ProcCode=="HNI" | ProcCode=="HNV" | ProcCode=="NHI" | ProcCode=="NHV")

# calculate % Angry
CurrPartAngry_AngryLeft<-subset(CurrPartAngry, AOI=="FaceLeft")
CurrPartAngry_AngryLeft<-subset(CurrPartAngry_AngryLeft, ProcCode=="ANI" | ProcCode=="ANV")
CurrPartAngry_AngryRight<-subset(CurrPartAngry, AOI=="FaceRight")
CurrPartAngry_AngryRight<-subset(CurrPartAngry_AngryRight, ProcCode=="NAI" | ProcCode=="NAV")
PercAngry_Angry<-(nrow(CurrPartAngry_AngryLeft)+nrow(CurrPartAngry_AngryRight))/nrow(CurrPartAngry)

# calculate % Neutral on Angry
CurrPartAngry_Left<-subset(CurrPartAngry, AOI=="FaceLeft")
CurrPartAngry_NeutralLeft<-subset(CurrPartAngry_Left, ProcCode=="NAI" | ProcCode=="NAV")
CurrPartAngry_Right<-subset(CurrPartAngry, AOI=="FaceRight")
CurrPartAngry_NeutralRight<-subset(CurrPartAngry_Right, ProcCode=="ANI" | ProcCode=="ANV")
PercAngry_Neutral<-(nrow(CurrPartAngry_NeutralLeft)+nrow(CurrPartAngry_NeutralRight))/nrow(CurrPartAngry)

# calculate % NonFace on Angry
PercAngry_NonFace<-1-PercAngry_Angry-PercAngry_Neutral

# calculate % Happy
CurrPartHappy_HappyLeft<-subset(CurrPartHappy, AOI=="FaceLeft")
CurrPartHappy_HappyLeft<-subset(CurrPartHappy_HappyLeft, ProcCode=="HNI" | ProcCode=="HNV")
CurrPartHappy_HappyRight<-subset(CurrPartHappy, AOI=="FaceRight")
CurrPartHappy_HappyRight<-subset(CurrPartHappy_HappyRight, ProcCode=="NHI" | ProcCode=="NHV")
PercHappy_Happy<-(nrow(CurrPartHappy_HappyLeft)+nrow(CurrPartHappy_HappyRight))/nrow(CurrPartHappy)

# calculate % Neutral on Happy
CurrPartHappy_Left<-subset(CurrPartHappy, AOI=="FaceLeft")
CurrPartHappy_NeutralLeft<-subset(CurrPartHappy_Left, ProcCode=="NHI" | ProcCode=="NHV")
CurrPartHappy_Right<-subset(CurrPartHappy, AOI=="FaceRight")
CurrPartHappy_NeutralRight<-subset(CurrPartHappy_Right, ProcCode=="HNI" | ProcCode=="HNV")
PercHappy_Neutral<-(nrow(CurrPartHappy_NeutralLeft)+nrow(CurrPartHappy_NeutralRight))/nrow(CurrPartHappy)

# calculate % NonFace on Happy
PercHappy_NonFace<-1-PercHappy_Happy-PercHappy_Neutral

# output values -- Initial SetUp
#RowNames<-c("Subj","PercHappy","PercAngry","PercLeft","PercRight","PercHappyNeutral","PercAngryNeutral","PercInvalid","PercHappyNonFace","PercAngryNonFace")
CurrPartRow<-c(CurrPartColsRowsVal[1,1], PercHappy_Happy,PercAngry_Angry,PercLeft,PercRight,PercHappy_Neutral,PercAngry_Neutral,PercInvalid,PercHappy_NonFace,PercAngry_NonFace)
#AllData<-as.data.frame(CurrPartRow)
#AllData<-t(AllData)
#colnames(AllData)<-RowNames

#Add New Part
AllData<-rbind(AllData,CurrPartRow)

AllData

#Export Master File and Stats for Dx
write.csv(AllData,"AllData.csv")
DxCodes<-read.csv("DxCodes.csv",header=TRUE)
colnames(DxCodes)<-c("Subj","Dx")
AllDataDx<-merge(AllData,DxCodes)
write.csv(AllDataDx,"AllDataDx.csv")
AllDataDxStats<-describeBy(AllDataDx, group=AllDataDx$Dx)
stats22q<-AllDataDxStats$'22q'[c("PercAngry","PercAngryNeutral","PercAngryNonFace","PercHappy","PercHappyNeutral","PercHappyNonFace"),c("n","mean","sd","se")]
statsTD<-AllDataDxStats$TD[c("PercAngry","PercAngryNeutral","PercAngryNonFace","PercHappy","PercHappyNeutral","PercHappyNonFace"),c("n","mean","sd","se")]
#AllDataDxStatsExp<-do.call("rbind",AllDataDxStats)
#write.csv(AllDataDxStatsExp,"AllDataDxStats.csv")
write.csv(stats22q,"stats22q.csv")
write.csv(statsTD,"statsTD.csv")

#Export Master File and Stats for Cluster

setwd("/Users/abbiepopa/Documents/Lab/DPTB")
KData <- read.csv("Clustering_Database_150722.csv",header=T,na.strings="-9999")
SubjNeeded<-AllData$Subj
SubjNeeded<-as.data.frame(SubjNeeded)
colnames(SubjNeeded)<-"CABIL_ID"
KDataCorr<-merge(SubjNeeded,KData)
KDataCorrClip<-KDataCorr[1:16]
#KDataCorrClipUni<-unique(KDataCorrClip)

BigKRowNamesStep<-KDataCorrClip["CABIL_ID"]
BigKRowNames<-t(BigKRowNamesStep)
BigKRowNames<-as.vector(BigKRowNames)

rownames(KDataCorrClip)<-BigKRowNames

BigKData<-KDataCorrClip[2:16]

#BigKData[BigKData == "N/A"] <- NA

BigKDataCleaned <-na.omit(BigKData)
BigKDataCleaned <-scale(BigKDataCleaned)

###ALL the data!!###

#determine number of clusters
wss <-(nrow(BigKDataCleaned)-1)*sum(apply(BigKDataCleaned,2,var))
 for(i in 2:15) wss[i] <- sum(kmeans(BigKDataCleaned, centers=1)$withinss)
quartz()
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#do kmeans clustering
 fit <- kmeans(BigKDataCleaned, 2)
 aggregate(BigKDataCleaned,by=list(fit$cluster),FUN=mean)
BigKDataCleaned<-data.frame(BigKDataCleaned,fit$cluster)

#plot clusters
 library(cluster)
quartz()
 clusplot(BigKDataCleaned,fit$cluster,col.clus="green",shade=TRUE,labels=2,lines=0) #labels=4 only the ellipses are labelled in the plot; #labels=2 points and ellipses are labelled in the plot

#to find what the components are use princomp
pcfitBigK <- princomp(BigKDataCleaned)
loadings(pcfitBigK)

#Make the cluster assignments file
ClusAssg<-cbind(rownames(BigKDataCleaned),BigKDataCleaned$fit.cluster)
ClusAssg<-as.data.frame(ClusAssg)
colnames(ClusAssg)<-c("Subj","Cluster")
AllDataClus<-merge(AllData,ClusAssg)
#setwd("/Users/ampopa/Desktop/Abbie Projects/DPTB/Conversion to R")
write.csv(AllDataClus,"AllDataClus.csv")
AllDataClusStats<-describeBy(AllDataClus,group=AllDataClus$Cluster)
AllDataClusStatsExp<-do.call("rbind",AllDataClusStats)
write.csv(AllDataClusStatsExp,"AllDataClusStats.csv")