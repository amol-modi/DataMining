#packages required 
install.packages("ggvis")
library(ggvis)

data(iris)
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species)
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) #%>% layer_points() 

install.packages("class")
library(class)


# We do not need normalization for this data set but just for practice here is the formula
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)

#split data into training and testing
set.seed(1234)
ind = sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33)) 
iris.training = iris[ind==1, 1:4]
iris.test = iris[ind==2, 1:4] 

iris.trainLabels = iris[ind==1, 5]
iris.testLabels = iris[ind==2, 5] 

#build model on training data 
iris_pred = knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
iris_pred_prob = knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3, prob = T)
attributes(.Last.value)

# using knn() function from FNN package
install.packages("FNN")
library(FNN)
iris_pred_fnn = knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3, prob = T)
iris_pred_fnn
indices <- attr(iris_pred_fnn, "nn.index")
print(indices[20, ])
dist = attr(iris_pred_fnn,"nn.dist")
print(dist[20, ])

# analyze the prediction
table = table(iris_pred_fnn ,iris.testLabels)
table
caret::confusionMatrix(table)

library(caret)

model <- train(Species~., data=iris, method='knn',tuneGrid=expand.grid(k=1:25), metric='Accuracy',trControl=trainControl(method='cv', number=10))

model
plot(model)
confusionMatrix(model)

