###Make Primary Dx Document
setwd("~/Documents/Lab/DPTB/Spences I entered 150724")
ax<-read.csv("newSCAS_NOdups.csv")

ax2<-ax[,c(1,3,4,seq(from = 7, to = 17, by =2), 18, seq(from=23, to = 35, by = 2))]

ax_parent<-ax2[,c(1,11:16)]

ax_parent$primary1<-colnames(ax_parent[,2:7])[max.col(ax_parent[2:7],ties.method="first")]

ax_parent$primary2<-colnames(ax_parent[,2:7])[max.col(ax_parent[2:7],ties.method="last")]

ax_parent$match<-(ax_parent$primary1==ax_parent$primary2)

ax_parent[which(!ax_parent$match),]
#300 is tied on Spence Physical Injury and Spence Social Phobia

write.csv(ax_parent, "primary_dx_new.csv")

pd<-ax_parent[,c("CABIL","primary1","primary2","match")]

###RT Analysis
setwd("/Users/abbiepopa/Documents/Lab/DPTB")
allRT<-read.csv("DPTB_calcmat_150724_outliers removed.csv")

RT_pd<-merge(allRT, pd)

library(psych)

RT_pd_stats<-describeBy(RT_pd[,c("bias_ang","bias_hap")], group=RT_pd$primary1)

RT_pd_stats_DG<-rbind(
	RT_pd_stats$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1[,c("n","mean","sd","se")],
	RT_pd_stats$Spence_ObsessiveCompulsive_T_Score_Parent1[,c("n","mean","sd","se")],
	RT_pd_stats$Spence_PanicAgorophobia_T_Score_Parent1[,c("n","mean","sd","se")],
	RT_pd_stats$Spence_PhysicalInjuryFears_T_Score_Parent1[,c("n","mean","sd","se")],
	RT_pd_stats$Spence_SeparationAnxiety_T_Score_Parent1[,c("n","mean","sd","se")],
	RT_pd_stats$Spence_SocialPhobia_T_Score_Parent1[,c("n","mean","sd","se")])

RT_pd_stats_DG<-data.frame(RT_pd_stats_DG)	
	
RT_pd_stats_DG$prim_dx<-c("GAD","GAD","OCD","OCD","Panic_Agora","Panic_Agora","PhysInj","PhysInj","SepAnx","SepAnx","SocPhob","SocPhob")

write.csv(RT_pd_stats_DG, "Kathy_RT.csv")

###Overall EG Analysis

setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

overall_EG<-read.csv("EG for reanalysis - outliers removed 1.2.csv")

colnames(overall_EG)[1]<-"CABIL"

overall_EG_pd<-merge(overall_EG,pd)

overall_EG_pd_stats<-describeBy(overall_EG_pd[,c("PercHappy","PercAngry","PercHappyNeutral","PercAngryNeutral")], group=overall_EG_pd$primary1)

overall_EG_pd_stats_DG<-rbind(
	overall_EG_pd_stats$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1[,c("n","mean","sd","se")],
	overall_EG_pd_stats$Spence_ObsessiveCompulsive_T_Score_Parent1[,c("n","mean","sd","se")],
	overall_EG_pd_stats$Spence_PanicAgorophobia_T_Score_Parent1[,c("n","mean","sd","se")],
	overall_EG_pd_stats$Spence_PhysicalInjuryFears_T_Score_Parent1[,c("n","mean","sd","se")],
	overall_EG_pd_stats$Spence_SeparationAnxiety_T_Score_Parent1[,c("n","mean","sd","se")],
	overall_EG_pd_stats$Spence_SocialPhobia_T_Score_Parent1[,c("n","mean","sd","se")])

overall_EG_pd_stats_DG<-data.frame(overall_EG_pd_stats_DG)
overall_EG_pd_stats_DG$mean<-overall_EG_pd_stats_DG$mean*100
overall_EG_pd_stats_DG$sd<-overall_EG_pd_stats_DG$sd*100
overall_EG_pd_stats_DG$se<-overall_EG_pd_stats_DG$se*100	
	
overall_EG_pd_stats_DG$prim_dx<-c("GAD","GAD","GAD","GAD","OCD","OCD","OCD","OCD","Panic_Agora","Panic_Agora","Panic_Agora","Panic_Agora","PhysInj","PhysInj","PhysInj","PhysInj","SepAnx","SepAnx","SepAnx","SepAnx","SocPhob","SocPhob","SocPhob","SocPhob")

write.csv(overall_EG_pd_stats_DG, "Kathy_overall_EG.csv")

###Time Course EG Analysis


setwd("/Users/abbiepopa/Documents/Lab/DPTB/Time Course Eye Gaze/Data")

TC<-read.csv("AllDataTC25_ToUse_1.1.csv")

colnames(TC)[1]<-"CABIL"

TC_pd<-merge(TC,pd)

TC_pd_angry_stats<-describeBy(TC_pd[,c("PercAngryChange25_Neutral", "PercAngryChange25_NonFace","PercAngryChange25_Angry")],group=TC_pd$primary1)

TC_pd_happy_stats<-describeBy(TC_pd[,c("PercHappyChange25_Neutral", "PercHappyChange25_NonFace","PercHappyChange25_Happy")],group=TC_pd$primary1)


TC_pd_angry_stats_DG<-rbind(
	TC_pd_angry_stats$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_angry_stats$Spence_ObsessiveCompulsive_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_angry_stats$Spence_PanicAgorophobia_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_angry_stats$Spence_PhysicalInjuryFears_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_angry_stats$Spence_SeparationAnxiety_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_angry_stats$Spence_SocialPhobia_T_Score_Parent1[,c("n","mean","sd","se")])
	

TC_pd_happy_stats_DG<-rbind(
	TC_pd_happy_stats$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_happy_stats$Spence_ObsessiveCompulsive_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_happy_stats$Spence_PanicAgorophobia_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_happy_stats$Spence_PhysicalInjuryFears_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_happy_stats$Spence_SeparationAnxiety_T_Score_Parent1[,c("n","mean","sd","se")],
	TC_pd_happy_stats$Spence_SocialPhobia_T_Score_Parent1[,c("n","mean","sd","se")])
	
TC_pd_angry_stats_DG<-data.frame(TC_pd_angry_stats_DG)
TC_pd_happy_stats_DG<-data.frame(TC_pd_happy_stats_DG)


TC_pd_angry_stats_DG$prim_dx<-c("GAD","GAD","GAD","OCD","OCD","OCD","Panic_Agora","Panic_Agora","Panic_Agora","PhysInj","PhysInj","PhysInj","SepAnx","SepAnx","SepAnx","SocPhob","SocPhob","SocPhob")


TC_pd_happy_stats_DG$prim_dx<-c("GAD","GAD","GAD","OCD","OCD","OCD","Panic_Agora","Panic_Agora","Panic_Agora","PhysInj","PhysInj","PhysInj","SepAnx","SepAnx","SepAnx","SocPhob","SocPhob","SocPhob")

TC_pd_angry_stats_DG$Trial<-c("Neutral","NonFace","Angry","Neutral","NonFace","Angry","Neutral","NonFace","Angry","Neutral","NonFace","Angry","Neutral","NonFace","Angry","Neutral","NonFace","Angry")

TC_pd_happy_stats_DG$Trial<-c("Neutral","NonFace","Happy","Neutral","NonFace","Happy","Neutral","NonFace","Happy","Neutral","NonFace","Happy","Neutral","NonFace","Happy","Neutral","NonFace","Happy")

write.csv(TC_pd_angry_stats_DG,"TC_angry_kathy.csv")
write.csv(TC_pd_happy_stats_DG,"TC_happy_kathy.csv")

###Pupil Overall

setwd("/Users/abbiepopa/Documents/Lab/DPTB/Pupilometry/Data")

pupil<-read.csv("pupilalldata_paper1.1.csv")

colnames(pupil)[1]<-"CABIL"

pupil_pd<-merge(pupil,pd)

pupil_pd_stats<-describeBy(pupil_pd[,c("AngryDil","HappyDil")], group=pupil_pd$primary1)

pupil_pd_stats_DG<-rbind(
	pupil_pd_stats$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1[,c("n","mean","sd","se")],
	pupil_pd_stats$Spence_ObsessiveCompulsive_T_Score_Parent1[,c("n","mean","sd","se")],
	pupil_pd_stats$Spence_PanicAgorophobia_T_Score_Parent1[,c("n","mean","sd","se")],
	pupil_pd_stats$Spence_PhysicalInjuryFears_T_Score_Parent1[,c("n","mean","sd","se")],
	pupil_pd_stats$Spence_SeparationAnxiety_T_Score_Parent1[,c("n","mean","sd","se")],
	pupil_pd_stats$Spence_SocialPhobia_T_Score_Parent1[,c("n","mean","sd","se")])
	
pupil_pd_stats_DG<-data.frame(pupil_pd_stats_DG)

pupil_pd_stats_DG$prim_dx<-c("GAD","GAD","OCD","OCD","Panic_Agora","Panic_Agora","PhysInj","PhysInj","SepAnx","SepAnx","SocPhob","SocPhob")

write.csv(pupil_pd_stats_DG, "pupil_overall_kathy.csv")

### Pupil TC 

pupilTC<-read.csv("AllTC25_pupil_paper1.1.csv")

colnames(pupilTC)[1]<-"CABIL"

pupilTC_pd<-merge(pupilTC,pd)

pupilTC_pd_stats<-describeBy(pupilTC_pd[,c("angry_begin_dil","angry_end_dil","happy_begin_dil","happy_end_dil")],group=pupilTC_pd$primary1)

pupilTC_pd_stats_DG<-rbind(
	pupilTC_pd_stats$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1[,c("n","mean","sd","se")],
	pupilTC_pd_stats$Spence_ObsessiveCompulsive_T_Score_Parent1[,c("n","mean","sd","se")],
	pupilTC_pd_stats$Spence_PanicAgorophobia_T_Score_Parent1[,c("n","mean","sd","se")],
	pupilTC_pd_stats$Spence_PhysicalInjuryFears_T_Score_Parent1[,c("n","mean","sd","se")],
	pupilTC_pd_stats$Spence_SeparationAnxiety_T_Score_Parent1[,c("n","mean","sd","se")],
	pupilTC_pd_stats$Spence_SocialPhobia_T_Score_Parent1[,c("n","mean","sd","se")])
	
pupilTC_pd_stats_DG<-data.frame(pupilTC_pd_stats_DG)

pupilTC_pd_stats_DG$prim_dx<-c("GAD","GAD","GAD","GAD","OCD","OCD","OCD","OCD","Panic_Agora","Panic_Agora","Panic_Agora","Panic_Agora","PhysInj","PhysInj","PhysInj","PhysInj","SepAnx","SepAnx","SepAnx","SepAnx","SocPhob","SocPhob","SocPhob","SocPhob")

write.csv(pupilTC_pd_stats_DG, "pupil_TC_kathy.csv")