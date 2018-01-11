

###############################################
### Regression

# lm(formula, data) where formula is the model description such as ?? ~ ?? and data indicates which data set should be used.
fit = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data= iris)
summary(fit)


fit2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width - 1, data= iris)
summary(fit2)

# Other useful functions
coefficients(fit)
# returns model coefficients

as.numeric(coefficients(fit)[1])

confint(fit, level=0.95) # returns 95% Confidence Intervals for model parameters

fitted(fit) # yhat returns predicted values for each observation; you can also get this using fit$fitted

residuals(fit) # yhat - y: returns residuals of predictions

sum(residuals(fit))

sum(residuals(fit2))



hist(fit$residuals, main="Histogram of Residuals", xlab = "bf residuals")


anova(fit) # returns anova table

vcov(fit) # returns the covariance matrix for the model parameters

influence(fit) # regression diagnostics

# plot fit
layout(matrix(c(1,2,3,4),2,2)) #We use layout get all the four plots of fit in one picture
plot(fit) # To view the plots, hit 'return'


#compare models
fit3 = lm(Sepal.Length~Sepal.Width , data=iris)
anova(fit,fit3)

# forward selection
null = lm(Sepal.Length~Sepal.Width, data= iris) # only includes one variable
full = lm(Sepal.Length~Sepal.Width + Petal.Length + Petal.Width, data= iris) # includes all the variables
# We can perform forward selection using the command:
step(null, scope=list(lower=null, upper=full), direction="forward")

step(full, data=iris, direction="backward")

step(null, scope = list(upper=full), data=iris, direction="both")




