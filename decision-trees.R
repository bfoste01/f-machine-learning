#Fun with Decision Trees
#SUBSET SOME DATA
trees <- subset(s2, 
  select = c(CHILDID, 
  A3PPVT4R, A3WJAPR, A3WJLWR,  
  KR3ATUDE, KR3MOTIV, KR3PRSST, KR3BAGGR, KR3BHYPE, KR3BWITH,
  A3CAGE, CHGENDER,P1RMOMED))
row.names(trees) <- NULL
trees1<-na.omit(trees)
row.names(trees1) <- NULL
str(trees1)
trees1$CHGENDER <- factor(trees1$CHGENDER)
trees1$P1RMOMED <- ordered(trees1$P1RMOMED)
trees1$CHGENDER <- factor(trees1$CHGENDER)
trees1$P1RMOMED <- ordered(trees1$P1RMOMED)
str(trees1)
#[Check this out for a guide](http://scikit-learn.org/stable/modules/tree.html)
#------------------------------
# Using tree, as in the introduction to statistical learning book
#------------------------------
library(tree)
library(MASS)
set.seed(25)
train <- sample(1:nrow(trees1), nrow(trees1)/2)
#model
tree1.ppvt <- tree(A3PPVT4R ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH + A3CAGE + CHGENDER + P1RMOMED,
  trees1, subset=train)
summary(tree1.ppvt)
plot(tree1.ppvt)
text(tree1.ppvt, pretty = 0)
tree1.ppvt
#seeing if prunnin the tree will help with cvtree
cv1.ppvt <- cv.tree(tree1.ppvt)
plot(cv1.ppvt$size, cv1.ppvt$dev, type = 'b')
#predict
yhat <- predict(tree1.ppvt, newdata=trees1[-train,])
ppvt1.test <- trees1[-train, "A3PPVT4R"]
plot(yhat, ppvt1.test)
abline(0,1)
mean((yhat-ppvt1.test)^2)
sqrt(360.1458) #result means that we come within 18.98 points of true PPVT scores
#----------------------------------
#Bagging
#trick here is that mtry or m = p or number of predictors
#----------------------------------
library(randomForest)
bag.ppvt <- randomForest(A3PPVT4R ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= trees1, subset=train, mtry=8, ntrees = 10000, importance = TRUE)
print(bag.wjap) #very poor model
#predict
yhat.bag <- predict(bag.ppvt, newdata=trees1[-train,])
plot(yhat.bag, ppvt1.test)
abline(0,1)
mse <- mean((yhat.bag-ppvt1.test)^2)
sqrt(mse) # we come within 17.68 points true scores
#------------------------------------
#Random Forests
#going to use a smaller value of mtry, that is first = to rounded sqrt(p), which is 3
#------------------------------------
library(randomForest)
rf1.ppvt <- randomForest(A3PPVT4R ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= trees1, subset=train, mtry=3, importance = TRUE)
print(rf1.ppvt)
yhat.rf1 <- predict(rf1.ppvt, newdata=trees1[-train,])
plot(yhat.rf1, ppvt1.test)
abline(0,1)
mse <- mean((yhat.rf1-ppvt1.test)^2)
sqrt(mse) # we come within 16.58 points  true score
importance(rf1.ppvt)
varImpPlot(rf1.ppvt)
#for fun, I'm wondering if other cognitive measures will trump social emotional stuff
rf2.ppvt <- randomForest(A3PPVT4R ~ A3WJAPR + A3WJLWR + KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= trees1, subset=train, mtry=3, importance = TRUE)
print(rf2.ppvt)
yhat.rf2 <- predict(rf2.ppvt, newdata=trees1[-train,])
plot(yhat.rf2, ppvt1.test)
abline(0,1)
mse <- mean((yhat.rf2-ppvt1.test)^2)
sqrt(mse) # we come within 16.58 points of true score
importance(rf2.ppvt) we get within 15.31 pts of PPVT score
varImpPlot(rf2.ppvt) #as expected the other cognitive measures get us a more accurate prediction, but the 'attitude' and 'motivation' still contribute to the model
#----------------------------------------
# boosting
#----------------------------------------
library(gbm)
set.seed(25)
boost.ppvt1 <- gbm(A3PPVT4R ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= trees1[train,], distribution = "gaussian", n.trees = 10000, interaction.depth = 4)
summary(boost.ppvt1)
#looking at the partial dependence plot
#marginal effect of the 2 most important variables after integrating out other predictors
par(mfrow=c(1,2))
plot(boost.ppvt1,i="KR3MOTIV")
plot(boost.ppvt1,i="KR3PRSST")
#prediction
yhat.boost <- predict(boost.ppvt1, newdata=trees1[-train,],
  n.trees = 10000)
mse <- mean((yhat.boost-ppvt1.test)^2)
sqrt(mse) # we come within 17.45 points of true score
#future. changing 'shrinkage' function from .001 to something higher in gbm command will probably perform better
#------------------------------
# Using rpart
#-----------------------------
library(rpart)
# grow tree 
fit.1 <- rpart(A3PPVT4R ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH + A3CAGE + CHGENDER + P1RMOMED,
  method="anova", data=long.invariance)
printcp(fit.1) # display the results 
plotcp(fit.1) # visualize cross-validation results 
summary(fit.1) # detailed summary of splits

# plot tree 
plot(fit.1, uniform=TRUE, 
  main="Classification Tree for Kyphosis")
text(fit.1, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree 
pfit<- prune(fit.1, cp=0.01160389) # from cptable   
# plot the pruned tree 
plot(pfit, uniform=TRUE, 
  main="Pruned Regression Tree for Mileage")
#----------------------------------------------
# using the party package
#----------------------------------------------
library(party)
train <- sample(1:nrow(trees1), nrow(trees1)/2)
# Conditional Inference Tree for Mileage
fit2.a <- ctree(A3PPVT4R ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data=trees1, subset=train)
plot(fit2.a, main="Predicting PPVT Scores with AtL and Problem BeH")
#WJAP scores
fit2.b <- ctree(A3WJAPR ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= test)
plot(fit2.b, main="Predicting WJAP Scores with AtL and Problem BeH")
fit2.c <- ctree(A3WJLWR ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= test)
plot(fit2.c, main="Predicting WJLW Scores with AtL and Problem BeH")
#------------------------------------------
# Random Forest
#------------------------------------------
library(randomForest)
#create training data
#[see this](http://caret.r-forge.r-project.org/splitting.html)
#[see this](http://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function-in-r-program)
library(caret)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(test))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(test)), size = smp_size)
train3 <- test[train_ind, ]
test3 <- test[-train_ind, ]
str(train3) #check variable types
#train3$CHGENDER <- factor(train3$CHGENDER)
#train3$P1RMOMED <- ordered(train3$P1RMOMED)
#test3$CHGENDER <- factor(test3$CHGENDER)
#test3$P1RMOMED <- ordered(test3$P1RMOMED)
#Run the model
set.seed(1)
#bagging
#m = p, wich is 8
bag.wjap <- randomForest(A3WJAPR ~ KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= train3, mtry=8, importance = TRUE)
print(bag.wjap) #very poor model
#build random forest
#taking the rounded sqrt(#ofpredictors), which in our model = 3
#predictors are correlated, so mtry will probably end up being smaller
fit3.a <- randomForest(A3WJAPR ~ A3WJAPR + A3WJLWR + KR3ATUDE + KR3MOTIV + KR3PRSST + KR3BAGGR + KR3BHYPE + KR3BWITH  + CHGENDER + P1RMOMED, 
  data= train3,
  importance = TRUE,
  keep.forest = TRUE)
print(fit3.a)
importance(fit3.a)
varImpPlot(fit3.a, type=1)
#Boosting
#growing the trees sequentially, with new information coming from previous trees

