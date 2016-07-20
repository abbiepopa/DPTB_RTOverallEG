rm(list=ls())
setwd("/Users/abbiepopa/Documents/Lab/DPTB")
e<-read.csv("everyone_demo.csv")

setwd("/Users/abbiepopa/Documents/Lab/DPTB")

all<-read.csv("DPTB_calcmat_150724_outliers removed.csv")

#Dx_Code 2 is 22q, 1 is TD

TD<-subset(all, Dx_Code==1)
d22q<-subset(all, Dx_Code==2)

###Export Descriptives for DataGraph
library(psych)
stats<-describeBy(all, group=all$Dx_Code)
DGstats<-rbind(stats$`1`[c("bias_ang","bias_hap"), c("mean","sd","n","se")],stats$`2`[c("bias_ang","bias_hap"),c("mean","sd","n","se")])

rownames(DGstats)<-c("TD_RT_bias_ang","TD_RT_bias_hap","22q_RT_bias_ang","22q_RT_bias_hap")

write.csv(DGstats,"traditional_RT_stats.csv")

###T-Tests
t.test(all$bias_ang, all$bias_hap)
#p=0.2829

t.test(TD$bias_ang, TD$bias_hap)
#p=0.0506

t.test(d22q$bias_ang, d22q$bias_hap)
#p=0.2387

t.test(TD$bias_ang, d22q$bias_ang)

t.test(TD$bias_hap, d22q$bias_hap)

t.test(all$bias_ang)
#p=0.9474

t.test(all$bias_hap)
#p=0.1175

t.test(TD$bias_ang)
#p=0.4935

t.test(TD$bias_hap)
#p=0.01055

t.test(d22q$bias_ang)
#p=0.4551

t.test(d22q$bias_hap)
#p=0.6206

###Correlations with Anxiety###
setwd("~/Documents/Lab/DPTB/Spences I entered 150724")
ax<-read.csv("newSCAS_NOdups.csv", na.strings=-999)

ax2<-ax[,c(1,3,4,seq(from = 7, to = 17, by =2), 18, seq(from=23, to = 35, by = 2))]

p<-c()

for ( i in 4:17){
	print(colnames(ax2)[[i]])
	print(cor.test(ax2$bias_ang, ax2[,i]))
	print(cor.test(ax2$bias_hap, ax2[,i]))
	p<-c(p,cor.test(ax2$bias_ang, ax2[,i])$p.value, cor.test(ax2$bias_hap, ax2[,i])$p.value)
}

p.adjust(p, method="fdr")
which(p.adjust(p,method="fdr")<0.05)

#Social Phobia T-Score Parent vs. Happy Bias RT p = 0.047 r = 0.24
#Total T-Score Child vs. Angry Bias RT p = 0.083 r = -0.21
#Generalized Anxiety Disorder T-Score Child vs. Angry Bias RT p = 0.035 r = -0.25
#Obsessive Compulsive Disorder T-Score Childe vs. Angry Bias RT p = 0.035 r =-0.25
#Social Phobia T-Score Child vs. Angry Bias RT p = 0.035 r = -0.26
#Physical Injury Fears T-Score Child vs. Angry Bias RT p = 0.032 r = -0.26 
#Separation Anxiety T-Score Child vs. Angry Bias RT p = 0.032 r = -0.26
#Panic Agorophobia T-Score Child vs. Angry Bias RT p = 0.029 r = -0.26
### need to do FDR correction for multiple comparisons, 13? tests

###Age, gender, etc.###

#Age = NS
summary(lm(bias_ang~as.factor(Dx_Code)*Age, data=all))

summary(lm(bias_hap~as.factor(Dx_Code)*Age, data=all))

#Gender = NS
summary(lm(bias_ang~as.factor(Dx_Code)*as.factor(Gender_Code), data=all))
summary(lm(bias_hap~as.factor(Dx_Code)*as.factor(Gender_Code), data=all))

#IQ = NS
a<-merge(all, e, by.x="CABIL", by.y="CABIL_ID", all.x=T, all.y=F)
summary(lm(bias_ang~as.factor(Dx_Code.x)*WISCIV_FullScale_C, data=a))
summary(lm(bias_hap~as.factor(Dx_Code.x)*WISCIV_FullScale_C, data=a))

#SCAS = NS
summary(lm(bias_ang~as.factor(Dx_Code.x)*Spence_Total_T_Score_Parent1, data=a))
summary(lm(bias_hap~as.factor(Dx_Code.x)*Spence_Total_T_Score_Parent1, data=a))

#ABAS = NS
summary(lm(bias_ang~as.factor(Dx_Code.x)*ABAS_GAC_Composite_Parent1, data=a))
summary(lm(bias_hap~as.factor(Dx_Code.x)*ABAS_GAC_Composite_Parent1, data=a))