hmeq<-hmeq[complete.cases(hmeq),]

hmeq <- subset(hmeq, (hmeq$CLAG < 500)&(hmeq$VALUE <= 3*10^5))

hmeq$BAD = as.factor(hmeq$BAD)


dim(hmeq)

set.seed(1234)
ind = sample(2, nrow(hmeq), replace=TRUE, prob=c(0.6, 0.4))
trainData = hmeq[ind==1,]
testData = hmeq[ind==2,]

library(rpart)
myFormula = BAD~LOAN + MORTDUE + VALUE + REASON + JOB + YOJ + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC 
# Build the tree

hmeq_rpart = rpart(myFormula, data = trainData, control = rpart.control(minsplit = 10), parms = list(split = "gini"))


##### Confusion Matrix

# Method 1
table(predict(hmeq_rpart, newdata = testData[,2:13], type="class"), testData$BAD, dnn = c("Predictions", "Actual Values"))


# Method 2
library(caret)
confusionMatrix(predict(hmeq_rpart, newdata = testData[,2:13], type="class"), testData$BAD, dnn = c("Predictions", "Actual Values"))



######################
# Charts
######################


### Constructing Tree using "rpart"
### We assume that we will use the cleaned dataset of hmeq, which is generated in Lab 04
### We will also separate the dataset into training data (trainData) and test data (testData).

library(rpart)
myFormula = BAD~. 
# Build the tree

hmeq_rpart = rpart(myFormula, data = trainData, control = rpart.control(minsplit = 30), parms = list(split = "information"))


hmeq_rpart.minsplit2 = rpart(myFormula, data = trainData, control = rpart.control(minsplit = 2), parms = list(split = "information"))



hmeq.prediction<-predict(hmeq_rpart, newdata = testData[,2:13])[1:10, ][,2]
hmeq.prediction2<-predict(hmeq_rpart.minsplit2, newdata = testData[,2:13])[,2]
hmeq.actual<-testData$BAD

##### ROC Curve

# Install package
#install.packages("ROCR")
# Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
library(ROCR)

# Calculating the values for ROC curve
pred = prediction(hmeq.prediction, hmeq.actual)
pred2 = prediction(hmeq.prediction2, hmeq.actual)

perf = performance(pred,"tpr","fpr")
perf2 = performance(pred2,"tpr","fpr")

# Plotting the ROC curve
plot(perf, col = "blue", lty = 3, lwd = 3, main="ROC Curve of hmeq Dataset")
lines(unlist(slot(perf2, "x.values")), unlist(slot(perf2, "y.values")), col="red", lty=3,lwd=3)


# Calculating AUC
auc = performance(pred, "auc")
auc2 = performance(pred2, "auc")
# Now converting S4 class to a vector
auc = unlist(slot(auc, "y.values"))
auc2 = unlist(slot(auc2, "y.values"))

auc
auc2

# Adding AUCs to the plot
appr.auc = min(round(auc, digits = 2))
appr.auc2 = min(round(auc2, digits = 2))
auct = paste(c("AUC of min.split 30 = "), appr.auc, sep = "")
auct2 = paste(c("AUC of min.split 2 = "), appr.auc2, sep = "")
minsplit30 = c("Blue Dashed Line: min.split = 30")
minsplit2 = c("Red Dashed Line: min.split = 2")
legend(0.4, 0.7, c(minsplit30, minsplit2, auct, auct2, "\n"), cex = 0.7, box.col = "white")
abline(a= 0, b=1)


### Gain Chart

library(ROCR)
perf = performance(pred,"tpr", "rpp")
perf2 = performance(pred2,"tpr", "rpp")
plot(perf)

plot(x=c(0, 1),y=c(0, 1),type="l",col="red",lwd=2,ylab="True Positive Rate", xlab="Rate of
Positive Predictions", main="Gain Chart of hmeq Dataset")
#lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)
gain.x = unlist(slot(perf, 'x.values'))
gain.y = unlist(slot(perf, 'y.values'))
gain.x2 = unlist(slot(perf2, 'x.values'))
gain.y2 = unlist(slot(perf2, 'y.values'))

lines(x=gain.x, y=gain.y, col="orange", lwd=2)
lines(x=gain.x2, y=gain.y2, col="blue", lwd=2)


legend(0.5,0.3, # places a legend at the appropriate place 
       c("min.split = 30","min.split = 2", "Baseline"), # puts text in the legend
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2,2,2),col=c("orange","blue","red")) # gives the legend lines the correct color and width


## Lift Chart

perf = performance(pred,"lift","rpp")
perf2 = performance(pred2,"lift","rpp")
plot(perf, main="lift curve")


lift.x = unlist(slot(perf, 'x.values'))
lift.y = unlist(slot(perf, 'y.values'))
lift.x2 = unlist(slot(perf2, 'x.values'))
lift.y2 = unlist(slot(perf2, 'y.values'))


plot(x=lift.x, y=lift.y,type="l",col="red",lwd=2,ylab="Lift", xlab="Rate of Positive Predictions", main="Lift Chart of hmeq Dataset")
lines(x=lift.x2, y=lift.y2, col="blue", lwd=2)


legend(0.5,9, # places a legend at the appropriate place 
       c("min.split = 30","min.split = 2"), # puts text in the legend
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2,2),col=c("red","blue")) # gives the legend lines the correct color and width


install.packages("lift")
library(lift)

plotLift(hmeq.prediction, testData$BAD, n.buckets = 100)





