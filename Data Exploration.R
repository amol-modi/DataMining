dim(DRUG)

names(DRUG)

str(DRUG)

attributes(DRUG)

DRUG$Age
DRUG[ , c(1,3)]
names(DRUG[, c(1,3)])
dim(DRUG[, 1:5])


DRUG[1:5, ]

DRUG[1:6,"BP"]

# How to generate the subset of data who has "Normal" Blood Pressure

# Step 1: generate the indices of the target subset:
DRUG$BP == "NORMAL"
# Step 2: extract the subset:

DRUG[DRUG$BP == "NORMAL",]
dim(DRUG[DRUG$BP == "NORMAL",])


summary(DRUG)

mean(DRUG$Age)

sd(DRUG$Na)
var(DRUG$Na)

sqrt(var(DRUG$Na))

summary(DRUG$Na)


install.packages("pastecs")
library(pastecs)
stat.desc(DRUG$Na,basic=TRUE)

vars<-cbind(DRUG$Na,DRUG$K)
stat.desc(vars,basic=FALSE)

stat.desc(DRUG[,1:5], basic = FALSE)

cor(DRUG$Na, DRUG$K)

cov(DRUG$Na, DRUG$K)

cov(DRUG$Na,DRUG$K)/sqrt(var(DRUG$Na)*var(DRUG$K))

# The mean Na of Males
mean(DRUG[DRUG$Sex == "M","Na"])
# The mean Na of Females
mean(DRUG[DRUG$Sex == "F", "Na"])

aggregate(DRUG$Na, by=list(DRUG$Sex), FUN = mean)



var(DRUG$Age)

hist(DRUG$Age, main="Histogram of Variable Age")

plot(density(DRUG$Age), main="Density Curve of Variable Age", ylab="Density", xlab="x")

# to set the title: main = "...."
# to set the x-axis label: xlab = "...."
# to set the y-axis label: ylab = "...."

plot(density(rnorm(100000)))



plot(density(DRUG$Age))

barplot(table(DRUG$BP), main="Bar plot for the variable BP", ylab = "Frequency", xlab = "BP")

# Normality Test

shapiro.test(DRUG$Na)

# add colors
colors = c("red", "yellow", "blue")

barplot(table(DRUG$BP), main="Bar plot for the variable BP", ylab = "Frequency", xlab = "BP", col = colors)

BP.relfreq = table(DRUG$BP)/nrow(DRUG)

### Side by side bar charts

barplot(table(DRUG$Cholesterol, DRUG$BP), beside=T)

## Using relative frequency + side-by-side bar charts

colors.relfre = c("red", "yellow")

sbs.relfre.matrix<-table(DRUG$Cholesterol, DRUG$BP)/nrow(DRUG) ## generating the relative frequency matrix

barplot(sbs.relfre.matrix, beside=T, col=colors.relfre, legend=c("Chol", "BP"))


# three variables plots

cor(DRUG$Na, DRUG$K)
cov(DRUG$Na, DRUG$K)

with(DRUG, plot(Na, K), pch=15)
plot(DRUG$Na, DRUG$K, pch =15)

plot(DRUG$Na, DRUG$K, col=c("red", "blue", "green", "yellow", "orange")[DRUG$Drug], pch=15, ylim=c(0.01, 0.12), xlim=c(0.45, 1.05))
legend(0.9, 0.12, c("", "Yellow"), pch=c(15,15), col=c("red", "yellow"))
boxplot(Na~Drug, data = DRUG, main="DRUG data", xlab="Drug type", ylab="The amount of Na")

shapiro.test(DRUG$Na)

# How to convert the type of variables

is.factor(DRUG$Age)

DRUG$Age<-as.factor(DRUG$Age)

is.factor(DRUG$Age)

is.numeric(DRUG$Age)
DRUG$Age<-as.numeric(DRUG$Age)
is.numeric(DRUG$Age)
a<-1:10
b<-letters[1:5]
save(a,b,file="./data/mydatafile.Rdata")
rm(a,b)
load("./data/mydatafile.Rdata")
print(a)
