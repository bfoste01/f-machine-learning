#Taking some unsupervised learning approaches to discover interesting patterns in the data
#Let's look at the means
#library(plyr)
#ddply(trees1, 2, mean)
library(psych)
describe(trees1) # means are all over the place
#drop ID
trees1$CHILDID <- NULL
#now standardize
#write as a function
ls(trees1)
trees1$A3CAGE = scale(trees1$A3CAGE)
trees1$A3PPVT4R = scale(A3PPVT4R)
trees1$A3WJAPR = scale(A3WJAPR)
trees1$A3WJLWR = scale(A3WJLWR)
trees1$KR3ATUDE = scale(KR3ATUDE)
trees1$KR3BAGGR = scale(KR3BAGGR)
trees1$KR3BHYPE = scale(KR3BHYPE)
trees1$KR3BWITH = scale(KR3BWITH)
trees1$KR3MOTIV = scale(KR3MOTIV)
trees1$KR3PRSST = scale(KR3PRSST)
describe(trees1) #looks good
#-------------------------------------
# Principal Components
#-------------------------------------
pr.out <- prcomp(trees1, scale=TRUE)
#oops, we need to drop non numeric items
trees1$CHGENDER <- NULL
trees1$P1RMOMED <- NULL
pr.out <- prcomp(trees1, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$rotation
dim(pr.out$x)
biplot(pr.out, scale=0)
#looking for variance explained by PCs
pr.var <- pr.out$sdev^2
#computing proportion of variance explained by each PC
pve <- pr.var/sum(pr.var)
pve
plot(pve)
#------------------------------------
# kmeans
#-----------------------------------
km.out1 <- kmeans(trees1, 2, nstart = 50)
