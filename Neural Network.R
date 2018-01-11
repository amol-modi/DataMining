# install.packages("ISLR") # Data for An Introduction to Statistical Learning with Applications in R
library(ISLR)
data(College)
View(College)


# Create vector of column Max and Min values
maxs = apply(College[,2:18], 2, max) 
mins = apply(College[,2:18], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data = as.data.frame(scale(College[,2:18], center = mins, scale = maxs - mins))
head(scaled.data)
summary(scaled.data)

# Private = as.numeric(College$Private)-1
Private = College$Private
data = data.frame(Private, scaled.data)
head(data)

# Partitioning data to train and test sets
set.seed(1234)
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
TrainData = data[ind == 1, ]
TestData = data[ind == 2, ]

#Constructing neural network
library(nnet)
nn  = nnet(Private ~ ., data=TrainData, linout=F, size=10, decay=0.01, maxit=1000)
#linout stands for linear out-put and is used to determine whether the target variable is continuous or not
#size sets the number of units in hidden layer
#decay is the regularization parameter. You can use any value between between zero and one
#maxit denotes the maximum number of iterations taken by the algorithm that computes the weights

#Neural network model out put
summary(nn) #Summary gives you the number units in each layer in addition to all the weights


#To plot the neural network using nnet we need to use devtools
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)
# The darker lines are associated to the higher weights and gray lines are for small weights

nn$wts #if you only want to look at weights you can use wts
nn$fitted.values #This will provide the probability of predicted values for each instance

# Using nnet model on the test data:
nn.preds = predict(nn, TestData, type = "class")
table(TestData$Private, nn.preds)


# *********************************

data$Private = as.numeric(data$Private)
str(data)

n <- names(data)
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))


library(neuralnet)
nNetwork = neuralnet(f, data = data, hidden = c(3,2), err.fct ="sse", linear.output =FALSE)


#plot
plot(nNetwork)

nNetwork$net.result
nNetwork$weights
nNetwork$result.matrix

nNetwork$net.result[[1]]
#if probability greater than 50 % then 1 else 0
nn1 = ifelse(nNetwork$net.result[[1]]>0.5,2,1)

misClasificationError = mean(data$Private!=nn1)
#misclasification error
misClasificationError

