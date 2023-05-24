library(dplyr)
library(pROC)
library(tree)
library(e1071)
library(adabag)
library(rpart)
library(randomForest)

getwd()
setwd("~/Uni Mods/Sem 3/FIT 3152 Data Analytics/Assignment2")
rm(list = ls())

WAUS = read.csv("WarmerTomorrow2022.csv")
L = as.data.frame(c(1:49))
set.seed(3197123) # Your Student ID is the random seed
L = L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS = WAUS[(WAUS$Location %in% L),]
WAUS = WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

#Cleaning Data
WAUS.FullyClean = WAUS[complete.cases(WAUS),]


#Q1
#Count number of Warmer and not Warmer
WarmTmr = WAUS.FullyClean %>%
  select(Location,WarmerTomorrow) %>%
  count(WarmerTomorrow != is.na(WarmerTomorrow))

#Find proportion
isWarmer = round(proportions(WarmTmr$n)*100)
isWarmer 



#Analysis of Warmer and Cooler Days
Warmer = WAUS.FullyClean %>%
  select(MinTemp,MaxTemp,WarmerTomorrow) %>%
  filter(WarmerTomorrow == 1) %>%
  group_by(WarmerTomorrow == 1) 
Warmer$WarmerTomorrow[Warmer$WarmerTomorrow == 1] <- "Warmer Tomorrow"
  
summary(Warmer)
  
Cooler = WAUS.FullyClean %>%
  select(MinTemp,MaxTemp,WarmerTomorrow) %>%
  filter(WarmerTomorrow == 0) %>%
  group_by(WarmerTomorrow == 0) 
Cooler$WarmerTomorrow[Cooler$WarmerTomorrow == 0] <- "Warmer Tomorrow"

summary(Cooler)



#Q3
train.row = sample(1:nrow(WAUS.FullyClean),0.7*nrow(WAUS.FullyClean))
WAUS.train = WAUS.FullyClean[train.row,]
WAUS.test = WAUS.FullyClean[-train.row,]

#Q4, Q5 & Q6
detach("package:neuralnet", unload = TRUE)
#Decision Tree
WAUS.train$WarmerTomorrow[WAUS.train$WarmerTomorrow == 1] <- "Warmer Tomorrow"
WAUS.train$WarmerTomorrow[WAUS.train$WarmerTomorrow == 0] <- "Cooler Tomorrow"
WAUS.train$WarmerTomorrow = as.factor(WAUS.train$WarmerTomorrow)

DecTreeFit = tree(WarmerTomorrow~. ,data = WAUS.train)
plot(DecTreeFit)
text(DecTreeFit, pretty = 0)
print(DecTreeFit)

# do predictions as classes and draw a table
WAUS.predtree = predict(DecTreeFit,WAUS.test, type = "class")
t1 = table(Predicted_Class = WAUS.predtree,Actual_Class = WAUS.test$WarmerTomorrow)
cat("\n#Decision Tree Confusion\n")
print(t1)

#Calculate accuracy
DecTreeAccuracy = (t1[1,1]+t1[2,2])/sum(t1[,])
DecTreeAccuracy

# do predictions as probabilities and draw ROC
WAUS.pred.tree = predict(DecTreeFit, WAUS.test, type="vector")

# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class 
WAUSPred = prediction( WAUS.pred.tree[,2], WAUS.test$WarmerTomorrow)
WAUSperf = performance(WAUSPred,"tpr","fpr")
plot(WAUSperf)
abline(0,1)

#Calculate AUC
DecTreeAUC = performance(WAUSPred,"auc")
cat("\n#Decision Tree AUC\n")
print(as.numeric(DecTreeAUC@y.values))



#Naive Bayes
WAUS.bayes = naiveBayes(WarmerTomorrow~., data=WAUS.train)
WAUS.predBayes = predict(WAUS.bayes, WAUS.test)
tn = table( predicted=WAUS.predBayes, actual = WAUS.test$WarmerTomorrow)
cat("\n#Naives Bayes Confusion\n")
print(tn)

#Calculate accuracy
NaiveBayesAccuracy = (tn[1,1]+t1[2,2])/sum(tn[,])
NaiveBayesAccuracy

#Plot ROC Curve
WAUSpred.bayes = predict(WAUS.bayes,WAUS.test,'raw')
WAUSbayespred = prediction(WAUSpred.bayes[,2],WAUS.test$WarmerTomorrow)
WAUSperf = performance(WAUSbayespred,"tpr","fpr")
plot(WAUSperf, add=TRUE, col= "blueviolet")

#Calculate AUC
NaiveBayesAUC = performance(WAUSbayespred,"auc")
cat("\n#Naive Bayes AUC\n")
print(as.numeric(NaiveBayesAUC@y.values))


#Bagging
WAUS.bag = bagging(WarmerTomorrow~., data = WAUS.train, mfinal = 10)
WAUSpred.bag = predict.bagging(WAUS.bag, WAUS.test)
WAUSbagpred = prediction(WAUSpred.bag$prob[,2],WAUS.test$WarmerTomorrow)
WAUSbagperf = performance(WAUSbagpred,"tpr","fpr")
plot(WAUSbagperf, add = TRUE, col = "blue")
cat("\n#Bagging Confusion\n")
print(WAUSpred.bag$confusion)

#Calculate accuracy
BaggingAccuracy = (WAUSpred.bag$confusion[1,1]+WAUSpred.bag$confusion[2,2])/sum(WAUSpred.bag$confusion[,])
BaggingAccuracy

#Calculate AUC
BaggingAUC = performance(WAUSbagpred,"auc")
cat("\n#Bagging AUC\n")
print(as.numeric(BaggingAUC@y.values))

#Boosting
WAUS.boost = boosting(WarmerTomorrow ~. , data = WAUS.train, mfinal=10) 
WAUSpred.boost = predict.boosting(WAUS.boost, newdata=WAUS.test)

WAUSboostpred = prediction( WAUSpred.boost$prob[,2], WAUS.test$WarmerTomorrow) 
WAUSboostperf = performance(WAUSboostpred,"tpr","fpr") 
plot(WAUSboostperf, add=TRUE, col = "red")
cat("\n#Boosting Confusion\n")
print(WAUSpred.boost$confusion)

#Calculate accuracy
BoostingAccuracy = (WAUSpred.boost$confusion[1,1]+WAUSpred.boost$confusion[2,2])/sum(WAUSpred.boost$confusion[,])
BoostingAccuracy

#Calculate AUC
BoostingAUC = performance(WAUSboostpred,"auc")
cat("\n#Boosting AUC\n")
print(as.numeric(BoostingAUC@y.values))

#Random Forest
WAUS.rf = randomForest(WarmerTomorrow ~. , data = WAUS.train, na.action = na.exclude)
WAUSpredrf = predict(WAUS.rf, WAUS.test)
t3 = table(Predicted_Class = WAUSpredrf, Actual_Class = WAUS.test$WarmerTomorrow)
cat("\n#Random Forest Confusion\n")
print(t3)
WAUSpred.rf = predict(WAUS.rf, WAUS.test, type="prob")

#Plot ROC Curve
WAUSRFpred = prediction( WAUSpred.rf[,2], WAUS.test$WarmerTomorrow) 
WAUSRFperf = performance(WAUSRFpred,"tpr","fpr") 
plot(WAUSRFperf, add=TRUE, col = "darkgreen")

#Calculate accuracy
RandomForestAccuracy = (t3[1,1]+t3[2,2])/sum(t3[,])
RandomForestAccuracy

#Calculate AUC
RandomForestAUC = performance(WAUSRFpred,"auc")
cat("\n#Random Forest AUC\n")
print(as.numeric(RandomForestAUC@y.values))


#Q7
#Create Table

ComparisonData = matrix(c(DecTreeAccuracy,DecTreeAUC@y.values,
                          NaiveBayesAccuracy, NaiveBayesAUC@y.values,
                          BaggingAccuracy, BaggingAUC@y.values,
                          BoostingAccuracy,BoostingAUC@y.values,
                          RandomForestAccuracy, RandomForestAUC@y.values),ncol = 2, byrow = TRUE)

colnames(ComparisonData) = c('Accuracy', 'AUC')
rownames(ComparisonData) = c("Decision Tree", "Naive Bayes",
                             "Bagging", "Boosting", "Random Forest")

ComparisonTable = as.data.frame(ComparisonData)


#Q8
#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(DecTreeFit))
cat("\n#Baging Attribute Importance\n")
print(WAUS.bag$importance)
cat("\n#Boosting Attribute Importance\n") 
print(WAUS.boost$importance)
cat("\n#Random Forest Attribute Importance\n") 
print(WAUS.rf$importance)


#Q9
#Simple and Basic 
#Grab Variables of Min/MaxTemp
#Date,Temps Data
LocTemp = WAUS %>%
  select(MinTemp, MaxTemp,WarmerTomorrow) %>%
  filter(MinTemp != is.na(MinTemp),MaxTemp != is.na(MaxTemp)) %>%
  filter(WarmerTomorrow == 0 |WarmerTomorrow == 1 )


LocTemp.train = LocTemp[train.row,]
LocTemp.test = LocTemp[-train.row,]

LocTemp.train$WarmerTomorrow[LocTemp.train$WarmerTomorrow == 1] <- "Warmer Tomorrow"
LocTemp.train$WarmerTomorrow[LocTemp.train$WarmerTomorrow == 0] <- "Cooler Tomorrow"
LocTemp.train$WarmerTomorrow = as.factor(LocTemp.train$WarmerTomorrow)

SimpleDecTree = tree(WarmerTomorrow~. ,data = LocTemp.train)
plot(SimpleDecTree)
text(SimpleDecTree, pretty = 0)
print(SimpleDecTree)

# do predictions as classes and draw a table
LocTemp.predtree = predict(SimpleDecTree,LocTemp.test, type = "class")
t4 = table(Predicted_Class = LocTemp.predtree,Actual_Class = LocTemp.test$WarmerTomorrow)
cat("\n#Decision Tree Confusion\n")
print(t4)

#Calculate accuracy
SimpleDecTreeAccuracy = (t4[1,1]+t4[2,2])/sum(t4[,])
cat("\n#Decision Tree Accuracy\n")
SimpleDecTreeAccuracy

# do predictions as probabilities and draw ROC
LocTemp.pred.tree = predict(SimpleDecTree, LocTemp.test, type="vector")

# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class 
LocTempPred = prediction( LocTemp.pred.tree[,2], LocTemp.test$WarmerTomorrow)
LocTempPerf = performance(LocTempPred,"tpr","fpr")
plot(LocTempPerf)
abline(0,1)

#Calculate AUC
SimpleDecTreeAUC = performance(LocTempPred,"auc")
cat("\n#Decision Tree AUC\n")
print(as.numeric(SimpleDecTreeAUC@y.values))


#Q10
#Grab the most important Variables and use it in a Decision Tree
#Location,Date,Temps Data
ImpData = WAUS %>%
  select(Cloud9am,Humidity3pm, MinTemp, MaxTemp,WindGustDir,WindDir9am,WindDir3pm,Pressure9am,WarmerTomorrow) %>%
  filter(Cloud9am != is.na(Cloud9am),Humidity3pm != is.na(Humidity3pm)) %>%
  filter(MinTemp != is.na(MinTemp),MaxTemp != is.na(MaxTemp)) %>%
  filter(WindGustDir != is.na(WindGustDir),WindDir9am != is.na(WindDir9am), WindDir3pm != is.na(WindDir3pm)) %>%
  filter(Pressure9am != is.na(Pressure9am)) %>%
  filter(WarmerTomorrow == 0 |WarmerTomorrow == 1 )


ImpData.train = ImpData[train.row,]
ImpData.test = ImpData[-train.row,]

ImpData.train$WarmerTomorrow[ImpData.train$WarmerTomorrow == 1] <- "Warmer Tomorrow"
ImpData.train$WarmerTomorrow[ImpData.train$WarmerTomorrow == 0] <- "Cooler Tomorrow"
ImpData.train$WarmerTomorrow = as.factor(ImpData.train$WarmerTomorrow)

ImpDecTree = tree(WarmerTomorrow~. ,data = ImpData.train)
plot(ImpDecTree)
text(ImpDecTree, pretty = 0)
print(ImpDecTree)

# do predictions as classes and draw a table
ImpData.predtree = predict(ImpDecTree,ImpData.test, type = "class")
t4 = table(Predicted_Class = ImpData.predtree,Actual_Class = ImpData.test$WarmerTomorrow)
cat("\n#Decision Tree Confusion\n")
print(t4)

#Calculate accuracy
ImpDecTreeAccuracy = (t4[1,1]+t4[2,2])/sum(t4[,])
cat("\n#Decision Tree Accuracy\n")
ImpDecTreeAccuracy

# do predictions as probabilities and draw ROC
ImpData.pred.tree = predict(ImpDecTree, ImpData.test, type="vector")

# computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
# labels are actual values, predictors are probability of class 
ImpDataPred = prediction( ImpData.pred.tree[,2], ImpData.test$WarmerTomorrow)
ImpDataPerf = performance(ImpDataPred,"tpr","fpr")
plot(ImpDataPerf)
abline(0,1)

#Calculate AUC
ImpDecTreeAUC = performance(ImpDataPred,"auc")
cat("\n#Decision Tree AUC\n")
print(as.numeric(ImpDecTreeAUC@y.values))


#Q11
library(neuralnet)
library(car)

WAUS.train = WAUS.FullyClean[train.row,]
WAUS.train$WarmerTomorrow = as.numeric(WAUS.train$WarmerTomorrow)

#Change which variables to use
#Binomial classification: predict the probability of belonging to class 1 and if the probability is less than 0.5 consider it predicted as class 0
WAUS.nn = neuralnet(WarmerTomorrow == 1 ~ MinTemp+MaxTemp+Humidity3pm+Cloud9am+Pressure9am+Sunshine, WAUS.train, hidden=3,linear.output = FALSE)
WAUS.prednn = compute(WAUS.nn, WAUS.test[c(5,6,17,20,18,9)])
prob = WAUS.prednn$net.result
pred = ifelse(prob>0.5,1,0)

#confusion matrix
t5 = table(observed = WAUS.test$WarmerTomorrow, predicted = pred)
t5

#Calculate accuracy
ANNaccuracy = (t5[1,1]+t5[2,2])/sum(t5[,])
cat("\n#ANN Accuracy\n")
ANNaccuracy






