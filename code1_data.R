library(corrplot)
library(lattice)
library(ggplot2)
library(caret)

mydata = read.csv("/Users/manvijain/Desktop/Bayesian Classification/red_1.csv",header = TRUE, fill = TRUE)

summary(mydata)
sapply(mydata,class)
par(mfrow=c(2,2))
colnames(mydata) <- c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")


hist(mydata$fixed.acidity,col="light blue",xlab="fixed.acidity",main=NA)
hist(mydata$volatile.acidity,col="light blue",xlab="Volatile Acidity",main=NA)
hist(mydata$citric.acid,col="light blue",xlab="Citric Acid",main=NA)
hist(mydata$residual.sugar ,col="light blue",xlab="Residual Suger",main=NA)

par(mfrow=c(2,2))

hist(mydata$chlorides,col="light blue",xlab="Chlorides",main=NA)
hist(mydata$free.sulfur.dioxide,col="light blue",xlab="Free Sulfur Dioxide",main=NA)
hist(mydata$total.sulfur.dioxide,col="light blue",xlab="Total Sulfur Dioxide",main=NA)
hist(mydata$density,col="light blue",xlab="Density",main=NA)

par(mfrow=c(2,2))

hist(mydata$pH,col="light blue",xlab="pH",main=NA)
hist(mydata$sulphates ,col="light blue",xlab="Sulphates",main=NA)
hist(mydata$alcohol,col="light blue",xlab="Alcohol",main=NA)
hist(mydata$quality,col="light blue",xlab="Quality",main=NA)

par(mfrow=c(1,6))

boxplot(mydata$fixed.acidity,col="light blue",xlab="Fixed Acidity",main=NA)
boxplot(mydata$volatile.acidity,col="light blue",xlab="Volatile Acidity",main=NA)
boxplot(mydata$citric.acid,col="light blue",xlab="Citric Acid",main=NA)
boxplot(mydata$residual.sugar ,col="light blue",xlab="Residual Suger",main=NA)
boxplot(mydata$chlorides,col="light blue",xlab="Chlorides",main=NA)
boxplot(mydata$free.sulfur.dioxide,col="light blue",xlab="Free Sulfur Dioxide",main=NA)

par(mfrow=c(1,6))

boxplot(mydata$total.sulfur.dioxide,col="light blue",xlab="Total Sulfur Dixoide",main=NA)
boxplot(mydata$density,col="light blue",xlab="Density",main=NA)
boxplot(mydata$pH,col="light blue",xlab="pH",main=NA)
boxplot(mydata$sulphates ,col="light blue",xlab="Sulphates",main=NA)
boxplot(mydata$alcohol,col="light blue",xlab="Alcohol",main=NA)
boxplot(mydata$quality,col="light blue",xlab="Quality",main=NA)

pairs(~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+quality,data=mydata,main="Scatter Plot")

k1 = cor(mydata,method="pearson")
corrplot(k1,method="number")

k2 = cor(mydata,method="spearman")
corrplot(k2,method="number")

pca = princomp(mydata,cor=T)
summary(pca)
pca$loadings
load <- with(pca,unclass(loadings))
load
screeplot(pca,type="line")

