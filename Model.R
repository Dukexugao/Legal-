#install.packages('caret')
#install.packages('e1071')
#install.packages('DMwR')
#install.packages('mboost')
#install.packages('pROC')
#install.packages('gbm')
#install.packages('fastAdaboost')
#install.packages('monomvn')

library(dplyr)
library(caret)
library(e1071)
library(pROC)
library(DMwR)
library(randomForest)

# for classifiers
library(mboost)
library(gbm)
library(fastAdaboost) 
library(monomvn)

# Data Input
setwd("C:/Local/Legal Scoring/Version1")
payers <- read.table("data.csv", header=TRUE, row.names = 1, sep = ',')

payers_num <- select_if(payers, is.numeric)
payers <- payers_num


# Delete columns with containing all zeros
payers <- payers[, which(colSums(payers)!= 0)]
#str(payers)

# Replace NA with zeors
payers[is.na(payers)] <- 0





# Feature Selecton

# calculate correlation matrix
correlationMatrix <- cor(payers$Payer, payers[,-1])
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- which(correlationMatrix > 0.1 & correlationMatrix < 0.99)


# print indexes of highly correlated attributes
print(highlyCorrelated)
print(correlationMatrix[highlyCorrelated])

reduced = payers[c(1, highlyCorrelated)]
reduced$Payer <- factor(reduced$Payer, levels = c(0,1), labels = c(0,1))


# remove the suspicious columns
#reduced <- reduced[ , -which(names(reduced) %in% c("PAYER_IND.Missing.PAYER_IND"))]
#reduced <- reduced[ , -which(names(reduced) %in% c("salesman.CCASSIDY"))]

reduced <- reduced[, -c(grep("^trustbal", names(reduced), value = FALSE))]
reduced <- reduced[, -c(grep("^order_of_Date.Last.Paid", names(reduced), value = FALSE))]

names <- colnames(reduced)
print(names)

#library(FSelector)

# Calculate weights for each attributes using Chi-squared
#weights <- chi.squared(Payer~., payers) 
#print(weights)

#select a subset of 5 features with the lowest weight
#subset <- cutoff.k(weights, 5)

#f <- as.simple.formula(subset, "Payer")
#print(f)

# Principal components analysis of payers
#library(psych)
#pc <- principal(payers[,-1], nfactors = 1)
#pc


# Use 10 fold cross validation to train and evaluate the train set
# TODO: tune the model parameters based on the 10-fold CV results
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(Payer ~ .,  data= reduced, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)
print(mod_fit)


#payers.rose <- ROSE(Payer ~ collect.N02+solstate.NC+creditoro.RICHLAND.STATE.BANK+forwlist.317, data= payers, seed=123)$data


#payers <- lapply(payers, as.numeric)
#payers.rose <- ROSE(Payer ~ ., data= payers, seed=123)$data


#mod_fit <- train(Payer ~ collect.N02+solstate.NC+creditoro.RICHLAND.STATE.BANK+forwlist.317,  data=payers.rose, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)


# Under sampling
#### SMOTE
#library(pROC)

set.seed(1234)
splitIndex <- createDataPartition(reduced$Payer, p = .70,
                                  list = FALSE,
                                  times = 1)
trainSplit <- reduced[ splitIndex,]
testSplit <- reduced[-splitIndex,]
 
prop.table(table(trainSplit$Payer))

ctrl <- trainControl(method = "cv", number = 10)

tbmodel <- train(Payer ~ ., data = trainSplit, method = "rf", trControl = ctrl)                 

predictors <- names(trainSplit)[names(trainSplit) != 'Payer']
pred <- predict(tbmodel, testSplit[,predictors]) 

cf <- confusionMatrix(pred, testSplit$Payer)
print(cf)

testSplit$Payer <- as.numeric(testSplit$Payer)
auc <- roc(pred, testSplit$Payer)

print(auc)



trainSplit <- reduced[ splitIndex,]
testSplit <- reduced[-splitIndex,]
 
prop.table(table(trainSplit$Payer))

trainSplit$Payer <- as.factor(trainSplit$Payer)
trainSplit <- SMOTE(Payer ~ ., trainSplit, perc.over = 100, perc.under=200)
trainSplit$Payer <- as.numeric(trainSplit$Payer)
prop.table(table(trainSplit$Payer))

# estimate variable importance
#importance <- varImp(mod_fit, scale=FALSE)
#print(importance)

#trainSplit$Payer <- as.factor(trainSplit$Payer)

trainSplit$Payer <- factor(trainSplit$Payer, levels = c(1,2), labels = c(0,1))

tbmodel <- train(Payer ~ ., data = trainSplit, method = "rf",
                 trControl = ctrl)                 

predictors <- names(trainSplit)[names(trainSplit) != 'Payer'] 
pred <- predict(tbmodel, testSplit[,predictors])

testSplit$Payer <- as.numeric(testSplit$Payer)
auc <- roc(pred, testSplit$Payer)
print(auc)


testSplit$Payer <- factor(testSplit$Payer, levels = c(1,2), labels = c(0,1))
cf <- confusionMatrix(pred, testSplit$Payer)
print(cf)


plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))












