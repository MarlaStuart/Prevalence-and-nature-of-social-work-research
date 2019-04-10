###################################################################################################
###################################################################################################
# Clear enviornment
rm(list=ls())

### Unsupervised machine learning to determine if an abstract is a research article
###http://will-stanton.com/machine-learning-with-r-an-irresponsibly-fast-tutorial/
#install.packages("caret", dependencies = TRUE) # Classification And REgression Training
#install.packages("randomForest")
library(caret)
library(randomForest)
library(binda)
library(crossval)

##################################################################
# predict using top ranked Tokens picked through cross validation THIS IS WHAT I ACTUALLY DID.
######## ALL THE OTHER STUFF I WAS TRYING
setwd("C:/Users/marlastuart/Dropbox/Journal Articles Analysis/Data/Final Set of Journal Data 3-8-16")
Xtrain <- read.table("TrainFull.csv", sep = ",", header = TRUE)
Ytrain <- factor(Xtrain$MarlaStudy, levels = c(0, 1), labels = c("Not a Study", "Study"))
myvars <- names(Xtrain) %in% c("ID", "MarlaStudy") # delete the ID and MarlaStudy variables
Xtrain <- Xtrain[!myvars]
Xtrain <- as.matrix(Xtrain)

# Identify most discriminating variables
br = binda.ranking(Xtrain, Ytrain)
pdf(file="fig2-ranking.pdf")
plot(br, top=59, arrow.col="black", zeroaxis.col="black", ylab="Tokens",
     main = "59 Most Discriminating Tokens")
dev.off()

## crossvalidation analysis

# predict using specified Tokens
predfun1 = function(Xtrain, Ytrain, Xtest, Ytest, selTokens)
{
  binda.out = binda(Xtrain[, selTokens, drop=FALSE], Ytrain, verbose=FALSE)
  ynew = predict.binda(binda.out, Xtest[, selTokens, drop=FALSE], verbose=FALSE)$class 
  
  cm = confusionMatrix(Ytest, ynew, negative="Not a Study") 
  
  return(cm)  
}

# predict using top ranked Tokens
predfun2 = function(Xtrain, Ytrain, Xtest, Ytest, numTokens)
{
  bir = binda.ranking(Xtrain, Ytrain, verbose=FALSE)
  selTokens = bir[,"idx"][1:numTokens]
  
  cm = predfun1(Xtrain, Ytrain, Xtest, Ytest, selTokens)
  
  return(cm)  
}

#' We use 5-fold cross-validation (i.e. divide into 5 folds of 4 samples each )
#' and repeat 20 times

K=5
B=20


#' Find optimal number of Tokens (we compute errors for various numbers
#' of top ranked genes)

set.seed(12345)
numTokens = c(1:744)
simFun = function(i)
{
  cat("Number of Tokens:", i, "/n")
  cvp = crossval(predfun2, Xtrain, Ytrain, K=K, B=B, numTokens = i, verbose=FALSE)
  return( diagnosticErrors(cvp$stat) )
}

#' this may take some time:
cvsim = lapply(numTokens, simFun)
cvsim = do.call(rbind, cvsim)
binda.sim = cbind(numTokens,cvsim)

#' This shows accuracy, sensitivity, specificity, positive predictive value,
#' negative predictive value, and log-odds ratio 
#' (for definitions see help page ?diagnosticErrors)
binda.sim

save(binda.sim, file="binda.sim.rda")
write.table(binda.sim, file = "binda.sim.csv", col.names = TRUE, row.names = FALSE, sep = ",")
write.table(br, file = "br.csv", col.names = TRUE, row.names = FALSE, sep = ",")

#The diagnostic errors are computed as follows:
#  acc = (TP+TN)/(FP+TN+TP+FN)
# sens = TP/(TP+FN)
# spec = TN/(FP+TN)
# ppv = TP/(FP+TP)
# npv = TN/(TN+FN)
# lor = log(TP*TN/(FN*FP))


