#Prepare data for MPlus analysis
library(MplusAutomation) 
#strip out CASEID
colnames(long.invariance)
row.names(long.invariance) <- NULL
long.invariance <- long.invariance[2:50]
prepareMplusData(long.invariance, "mplus-scripts/invariance-weights.dat") #creates appropriately formatted .dat for Mplus
#PREPARE PROBLEM BEH
long.invariance.2 <- long.invariance.2[2:59]
prepareMplusData(long.invariance.2, "mplus-scripts/invariance-prob.dat.dat") #creates appropriately formatted .dat for Mplus


# 
# 
# row.names(long.invariance.2) <- NULL
# long.invariance.2 <- long.invariance.2[2:79]
# prepareMplusData(long.invariance.2, "mplus-scripts/invariance-weights.2.dat") #creates appropriately formatted .dat for Mplus
# #write.csv(long.invariance.2,"invariance-count.csv",na="-9999")
# test <-long.invariance.2[is.na(long.invariance.2)] <- -9999 
# test <-data.frame(test)
# change.na <- function(x){
#   x[is.na(x)] <- -999
#   x
# }
# Dat2 <- apply(long.invariance.2, 2, change.na)
# Dat2<-data.frame(Dat2)
# prepareMplusData(Dat2, "mplus-scripts/invariance-count.dat") #creates appropriately formatted .dat for Mplus
