dim(iris)

##########################################################
## separate the dataset into two parts:
## training data: 70%
## testing data: 30%

ind = sample(2,nrow(iris), replace= TRUE, prob = c(101/150,49/150))
trainData = iris[ind==1,]
testData = iris[ind==2,]

dim(trainData)
dim(testData)

##########################################################
## separate the dataset into two parts:
## training data: 109
## testing data: 41

train.indices<-sample(1:nrow(iris), 101, replace = FALSE)
test.indices<-setdiff(1:nrow(iris), train.indices)

trainData = iris[train.indices, ]
testData = iris[test.indices,]

dim(trainData)
dim(testData)


##########################################################
## randomForest
##########################################################

#loading library
#installing the package needs to be done only once
install.packages("randomForest")
#calling the package using the library function needs to be done each and every time
library(randomForest)

#creating the model
rf = randomForest(Species ~ . , data = trainData, ntree = 100, mtry = 3, proximity = TRUE, replace = TRUE, sampsize = if(replace){nrow(trainData)}else{ceiling(0.65*nrow(trainData))}, importance = TRUE )


# ntree denotes the number of trees to grow. This should not be set to too small number. 
# mtry = number of variables randomly sampled as candidates at each split. 
# You can use the following formula to set mtry:
# mtry=if (!is.null(Species) && !is.factor(Species)) max(floor(ncol(trainData)/3), 1) else floor(sqrt(ncol(trainData)))


### results- different functions to view results
print(rf)
attributes(rf)


### To access the error rate 
plot(rf)

rf$err.rate

rndF1.legend <- if (is.null(rf$TestData$err.rate)) {colnames(rf$err.rate)} else {colnames(rf$TestData$err.rate)}
legend("top", cex =0.5, legend=rndF1.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)


### Variable importance
importance(rf)
dim(importance(rf))
importance(rf)[2:4,5]
# include type = 1 in the importance function to get the important variables based on MeanDecreaseAccuracy for regression trees

#plot variable importance
varImpPlot(rf)

### get accuracy of prediction
table(predict(rf), trainData$Species)

irisPred = predict(rf, newdata = testData)
table(irisPred, testData$Species)

### margin
plot(margin(rf, testData$Species))
# The margin of a data point is defined as the proportion of votes for 
# the correct class minus maximum proportion of votes for the other classes. 
# Thus under majority votes, positive margin means correct classification.


### Extract individual tree
getTree(rf, k =2, labelVar = TRUE)
#Try this and check the results


### Proximity matrix
rf$proximity

dim(rf$proximity)
dim(trainData)


### Imputation
## artificially drop some data values. 
for (i in 1:4) iris [sample(150, sample(20)), i] = NA
iris.imputed = rfImpute(Species ~ ., data = iris, ntree = 500)
iris.rf = randomForest(Species ~., data = iris.imputed)


### Tuning parameter mtry
tuneRF(trainData[,-5], trainData[,5], stepFactor = 0.5)
# the first element is the matrix of predictors
# the second element is the target variable
# at each iteration, mtry is inflated or deflated by stepFactor value
  
##########################################################
## Bagging
##########################################################

#install.packages("caret")
#library("caret")

install.packages("stringi")
library("stringi")

install.packages("adabag")
library("adabag")
library("rpart")

iris.bagging = bagging(Species ~ ., data = trainData, mfinal = 10)
# mfinal is the number of trees to use. The default option is mfinal = 100 iterations
# you can use the control argument used in rpart tree.

# Error rates
sum(iris.bagging$class != trainData$Species)/nrow(trainData)
predict(iris.bagging, newdata = trainData)$error


iris.adaboost = boosting(Species ~ ., data = trainData, boos = TRUE, mfinal = 10)
# if boos = TRUE (default), a boostrap sample of the training data set is drawn using 
# the weights for each observation on that iteration. If FALSE, each observation is used with its weights.

#additional results
#if not installed
install.packages("caret")
#calling the package library
library(caret)
caret::confusionMatrix(irisPred, testData$Species)


