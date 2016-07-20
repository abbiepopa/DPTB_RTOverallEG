setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")

TD<-read.csv("DPTB_RT_9-17-14_TDKoralyOut.csv")
d22q<-read.csv("DPTB_RT_9-17-14_22qKoralyOut.csv")

all<-rbind(TD,d22q)

t.test(all$bias_ang, all$bias_hap)

t.test(TD$bias_ang, TD$bias_hap)

t.test(d22q$bias_ang, TD$bias_hap)

t.test(all$bias_ang)

t.test(all$bias_hap)

t.test(TD$bias_ang)
t.test(TD$bias_hap)
t.test(d22q$bias_ang)
t.test(d22q$bias_hap)

SCAS <- read.csv("Spence03-11-15.csv", na.strings="-999")

ax<-merge(all, SCAS, by="CABIL_ID")

ax2<-ax[,c(1,3, 9, 10, seq(from = 27, to = 37, by = 2), 38,  seq(from = 43, to = 55, by = 2))]

ax3<-ax2[which(ax2$Spence_PanicAgorophobia_T_Score_Child != -999), ]

for (i in 5:18){
	print(colnames(ax3)[[i]])
	print(cor.test(ax3$bias_ang, ax3[,i]))
	print(cor.test(ax3$bias_hap, ax3[,i]))
}