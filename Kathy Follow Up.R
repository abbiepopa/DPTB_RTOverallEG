setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")
d22q<-read.csv("DPTB_RT_9-17-14_22qKoralyOut.csv")
TD<-read.csv("DPTB_RT_9-17-14_TDKoralyOut.csv")
SCAS<-read.csv("Spence03-11-15.csv")

d<-merge(rbind(d22q,TD),SCAS)

d1<-d[,c(1,15,40, 3,9,10,seq(43,53,2))]

d1$primary1<-colnames(d1[7:12])[max.col(d1[7:12],ties.method="first")]

d1$primary2<-colnames(d1[7:12])[max.col(d1[7:12],ties.method="last")]

d1$tie<-d1$primary1!=d1$primary2

write.csv(d1,"primaryDx.csv")

pd<-read.csv("primaryDx.csv")

library(psych)

describeBy(pd, group=pd$primary1)

ang_means<-c(mean(pd[which(pd$primary1=="Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"),"bias_ang"], na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_PanicAgorophobia_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_SeparationAnxiety_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_ObsessiveCompulsive_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_PhysicalInjuryFears_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_SocialPhobia_T_Score_Parent1"),"bias_ang"],na.rm=T))
	 
	 
hap_means<-c(mean(pd[which(pd$primary1=="Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"),"bias_hap"], na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_PanicAgorophobia_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_SeparationAnxiety_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_ObsessiveCompulsive_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_PhysicalInjuryFears_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 mean(pd[which(pd$primary1=="Spence_SocialPhobia_T_Score_Parent1"),"bias_hap"],na.rm=T))
	 

ang_sds<-c(sd(pd[which(pd$primary1=="Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"),"bias_ang"], na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_PanicAgorophobia_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_SeparationAnxiety_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_ObsessiveCompulsive_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_PhysicalInjuryFears_T_Score_Parent1"),"bias_ang"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_SocialPhobia_T_Score_Parent1"),"bias_ang"],na.rm=T))
	 
	 
hap_sds<-c(sd(pd[which(pd$primary1=="Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"),"bias_hap"], na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_PanicAgorophobia_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_SeparationAnxiety_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_ObsessiveCompulsive_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_PhysicalInjuryFears_T_Score_Parent1"),"bias_hap"],na.rm=T),
	 sd(pd[which(pd$primary1=="Spence_SocialPhobia_T_Score_Parent1"),"bias_hap"],na.rm=T))	 
	 
	 

ang_lengths<-c(length(pd[which(pd$primary1=="Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"),"bias_ang"]),
	 length(pd[which(pd$primary1=="Spence_PanicAgorophobia_T_Score_Parent1"),"bias_ang"]),
	 length(pd[which(pd$primary1=="Spence_SeparationAnxiety_T_Score_Parent1"),"bias_ang"]),
	 length(pd[which(pd$primary1=="Spence_ObsessiveCompulsive_T_Score_Parent1"),"bias_ang"]),
	 length(pd[which(pd$primary1=="Spence_PhysicalInjuryFears_T_Score_Parent1"),"bias_ang"]),
	 length(pd[which(pd$primary1=="Spence_SocialPhobia_T_Score_Parent1"),"bias_ang"]))
	 
	 
hap_lengths<-c(length(pd[which(pd$primary1=="Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"),"bias_hap"]),
	 length(pd[which(pd$primary1=="Spence_PanicAgorophobia_T_Score_Parent1"),"bias_hap"]),
	 length(pd[which(pd$primary1=="Spence_SeparationAnxiety_T_Score_Parent1"),"bias_hap"]),
	 length(pd[which(pd$primary1=="Spence_ObsessiveCompulsive_T_Score_Parent1"),"bias_hap"]),
	 length(pd[which(pd$primary1=="Spence_PhysicalInjuryFears_T_Score_Parent1"),"bias_hap"]),
	 length(pd[which(pd$primary1=="Spence_SocialPhobia_T_Score_Parent1"),"bias_hap"]))	 
	 
d2rownames<-c("GAD","PncAgr","SepAnx","OCD","PhyInj","SocPhob")	 
	 
d2<-data.frame(ang_means,hap_means,ang_sds,hap_sds,ang_lengths, hap_lengths)

d2$ang_ses<-d2$ang_sds/sqrt(ang_lengths)
d2$hap_ses<-d2$ang_sds/sqrt(hap_lengths)

rownames(d2)<-d2rownames

write.csv(d2,"kathyfuforDG.csv")