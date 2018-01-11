# We use very well-known data set (iris) in R to construct a decision tree
# This data set gives the measurenments in centimeters of the
# variable sepal length, width, and petal length and width for 150 flowers
# Flowers are from 3 species: Setosa, Versicolor, and Virginica
# Given the information about sepal and petal of the flowers we want to 
# classify them into three different species classes

# Read data 
data(iris)
# Lets look at the data structure
str(iris)

# To construct any classification/regression model, we need to partition
# data into training and testing data 

# We fix the seed so that every time we run the model we do not work with different samples
set.seed(1234)

# We need to randomly select instances for train and test data
# We select approximately 70% of the data for training and 30% for testing
index = sample(2, nrow(iris), replace = T, prob = c(0.7,0.3))
TrainData = iris[index == 1, ]
TestData = iris[index == 2,]

# You can also use createDataPartition function from "caret" package to partition data
library(caret)
trainIndex = createDataPartition(iris$Species, p = 0.7, times = 3, list = F)
trainData = iris[trainIndex,]
testData = iris[-trainIndex,]

# We first construct a decision tree using ctree() from "party" package
library(party)
# Similar to CHIAD decision tree that uses the chi-square test 
#to check the association between input variables and the target variable,
# ctree uses a statistical test (Bonferroni) to check the relationship between inputs and the target;
# But opposite of the CHAID, this model can handle both categorical and numerical variables
# that is why it is very popular

# Basic model
iris_ctree = ctree(Species ~ ., data = TrainData)
iris_ctree

# Statistic is the Bonferroni statistic. If the value is higher it shows tha relationship is stronger
# Criterion = 1 - p-value; So as criterion is closer to one, it means the corresponding variable is a better option to split

# Plot the decision tree
plot(iris_ctree)

# Look at the two-way table to check the performance of the mdoel on train data
table(predict(iris_ctree), TrainData$Species, dnn = c("Predicted", "Actual"))

# Predict the classes for test data
predict(iris_ctree, newdata = TestData)

# Predict the probability of prediction for test data
predict(iris_ctree, newdata = TestData, type = "prob")
# Summary of predictions on test data 
table(predict(iris_ctree, newdata = TestData), TestData$Species)

# Construct a decision tree model using rpart() from "rpart" package
library(rpart)
iris_rpart = rpart(Species~., data = TrainData, method = "class", control = rpart.control(minsplit = 10, cp = 0.01))
# In "control" we can control the pruning options. 
# To learn about the settings of rpart.control, use help (write "?rpart.control" in console) 


summary(iris_rpart)
print(iris_rpart)

# rpart runs cross-validation (will talk about this soon) to compute the best value of "cp"
# Given a value for cp, rpart computes the prediction errors on the validation set
# We can choose the value of cp which has the minimum estimated error
printcp(iris_rpart) # printcp displays different values of cp and their corresponding errors on the validation set

# We want to choose the cp value from "cptable" whose "xerror" is minimum
# To do so we can use the following code:
opt = which.min(iris_rpart$cptable[,"xerror"])
opt
# Which.min finds a minimum value in a vector
# We look at the "xerror" column of the cptable (to call the cptable we need to use rpart_model$cptable)
# and stores the index of the minimum value in this column


# We try to find the cp value of the row which contains the minimum error
cp = iris_rpart$cptable[opt, "CP"]
cp

# We can now prune the model based on the best value of cp
tree_prune = prune(iris_rpart, cp = cp)
tree_prune
