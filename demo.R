setwd("/Users/abbiepopa/Downloads")

ABAS<-read.csv("ABAS 7-10-2015.csv")

gender<-ABAS[,c("CABIL_ID","CABIL_ABAS..Gender")]

setwd("/Users/abbiepopa/Documents/Lab/DPTB")
e4demo<-read.csv("everyonefordemo.csv")

e4demo<-e4demo[,c("CABIL_ID","Dx_Code","Age")]

demo<-merge(e4demo,gender)

library(psych)

describe(e4demo)


###IQ###

setwd("/Users/abbiepopa/Documents/Lab/DPTB/IQ for paper")

WASI<-read.csv("WASI.csv")

WISCIV<-read.csv("WISCIV.csv")

WASI_abr<-WASI[,c("CABIL_ID","Study_ID","WASI_Validity","WASI_Full4_IQ")]

WISCIV_abr<-WISCIV[,c("CABIL_ID","Study_ID","WISCIV_Validity","STUDY..Age","STUDY..DateTest","STUDY..Study","Test_Date","WISCIV_FullScale_C")]

WISCIVdemo<-merge(e4demo, WISCIV_abr)

WASIdemo<-merge(e4demo, WASI_abr)

write.csv(WISCIVdemo, "WISCIV_repeats.csv")
write.csv(WASIdemo, "WASI_repeats.csv")
write.csv(e4demo, "tocount.csv")

new_WASI <- read.csv("WASI_repeats.csv")
new_WISCIV <- read.csv("WISCIV_nodups.csv")

IQ_part<-c(new_WASI$CABIL_ID, new_WISCIV$CABIL_ID)

#so 280 and 808 have duplicates

write.csv(sort(IQ_part), "IQs.csv")
