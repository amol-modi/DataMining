data(iris)
View(iris)

iris$SpeciesClass[iris$Species=="versicolor"] <- "TRUE"
iris$SpeciesClass[iris$Species!="versicolor"] <- "FALSE"
iris$Species <- NULL
View(iris)

library(e1071)
irissvm = svm(SpeciesClass ~ ., data = iris, cost = 100, gamma = 1, kernel = "linear", type = "C-classification") 
irissvm

irissvm$SV # returns support vectors
nrow(irissvm$SV) 

irissvm$index # returns index of support vectors

irissvm$coefs # returns alpha_i*y_i

w = t(irissvm$coefs) %*% as.matrix(iris[irissvm$index,1:4]) # Computes weights
w
irissvm$rho # negative of intercept


irissvm$decision.values # returns w*x + b for all x
