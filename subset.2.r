#=========================================================================================================#
#--------------------------------------Subset for 4-year-olds Graders-------------------------------------#
#=========================================================================================================#
library(plyr)
library(MplusAutomation)
#---------------------------------Select out 4-year-olds--------------------------------------------------#
s1.a<-subset(FACES2006_missing, COHORT==4)
row.names(s1.a) <- NULL
#--------------------------Select out English speakers based on Engish Home Language----------------------#
s1.b<-subset(s1.a, P1HHLAN2==0)
row.names(s1.b) <- NULL
s2<-subset(s1.b, P3PRGTYP>=4)
row.names(s2) <- NULL
#s2b<- s1[s1$P1RHHLNG == 0,] #n=881 Using simple language majority has same sample size
long.invariance.2 <- subset(s2, 
select = c(A1PPVT4W, A1WJAPW, A1WJLWW, A1CSR, A1ECMIRT, A1WJSW,
	R1ATTUDE, R1MOTIVE, R1PRSIST,
	A2PPVT4W, A2WJAPW, A2WJLWW, A2CSR, A2ECMIRT, A2WJSW,
	R2ATTUDE, R2MOTIVE, R2PRSIST,
	A3PPVT4W, A3WJAPW, A3WJLWW, A3CSR, A3ECMIRT, A3WJSW,
	KR3ATUDE, KR3MOTIV, KR3PRSST,
	R1TPLBS, R2TPLBS, KR3TPLBS,
	STRAT, PSU, PRA12OTW, CHGENDER, P1RMOMED, P1RCAGE, CRACE, 
	P1PRGTYP, P2PRGTYP, P3PRGTYP,
	O2CLSSCD, O2CLSSIS, O2CLSSLM, O2CLSSQF, CLS1_ID,
    CLS2_ID, CLS3_ID,
    R1BAGGR,  R1BWITH, R1BHYPER ,R1ATTUDE,
    R2BAGGR, R2BWITH, R2BHYPER,
    KR3BAGGR, KR3BHYPE, KR3BWITH))
#---------------------------------Make Binary Variables---------------------------------------------------#
#for problem BeH
long.invariance.2$BINAGGR1<-ifelse(long.invariance.2$R1BAGGR>0,1,0)
long.invariance.2$BINWITH1<-ifelse(long.invariance.2$R1BWITH>0,1,0)
long.invariance.2$BINHYP1<-ifelse(long.invariance.2$R1BHYPER>0,1,0)
long.invariance.2$BINAGGR2<-ifelse(long.invariance.2$R2BAGGR>0,1,0)
long.invariance.2$BINWITH2<-ifelse(long.invariance.2$R2BWITH>0,1,0)
long.invariance.2$BINHYP2<-ifelse(long.invariance.2$R2BHYPER>0,1,0)
long.invariance.2$BINAGGR3<-ifelse(long.invariance.2$KR3BAGGR>0,1,0)
long.invariance.2$BINWITH3<-ifelse(long.invariance.2$KR3BWITH>0,1,0)
long.invariance.2$BINHYP3<-ifelse(long.invariance.2$KR3BHYPE>0,1,0)
long.invariance.2<-long.invariance.2[complete.cases(long.invariance.2$PRA12OTW),]
row.names(long.invariance.2) <- NULL
#-------------------------------Write the Data-----------------------------------------------------------#
s2.b<-long.invariance.2[complete.cases(long.invariance.2$PRA12OTW),]
prepareMplusData(s2.b, "moderation.4.dat")

