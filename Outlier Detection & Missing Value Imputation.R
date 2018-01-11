# Handling outliers

# We can use cutoff threshold to remove outliers
# The most popular method: IQR method. Any value outside of [Q1 - 1.5 IQR, Q3 + 1.5 IQR] is
# an outlier -- boxplot uses this method
# z-score: compute the z-score [(x-mean)/stdv] of a variable. 
 #If z-score exceeds 3, it is considered an outlier



# finding outliers visiaully using graphs
hist(hmeq$CLAGE)

# Cutting off the outliers using a threshold
Newdata = subset(hmeq, CLAGE < 600)
hist(Newdata$CLAGE)

# IQR method
outliers= boxplot(hmeq$CLAGE)$out
outliers
CLAGEnoOut = ifelse(hmeq$CLAGE %in% outliers, NA, hmeq$CLAGE)
boxplot(CLAGEnoOut)





# Handling missing values

is.na(hmeq$NINQ)
sum(is.na(hmeq$NINQ))
sum(complete.cases(hmeq$NINQ))
sum(!complete.cases(hmeq$NINQ))
which(!complete.cases(hmeq$NINQ))
summary(hmeq)

# replacing missing values with mean
# na.rm = T removes na values before the computation

hmeq$NINQ[is.na(hmeq$NINQ)] = mean(hmeq$NINQ, na.rm = T)
sum(is.na(hmeq$NINQ))

# replacing missing values with mice (Multivariate Imputation by Chained Equations) function

install.packages("mice")
library(mice)

NData = mice(hmeq, m = 5, maxit = 10, meth = 'pmm')
# m indicates the number of imputated dataset
# meth indicates the imputation method -- 'pmm' is using the mean to impute missing values

complete(NData,1)