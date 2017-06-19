install.packages(bnlearn)
install.packages(forecast)

library(bnlearn)                       
library(forecast)
library(xlsx)
library(caret)
#library(CORElearn)

wine_data <- read.csv("/Users/manvijain/Desktop/wine data/outlier_free.csv", header = TRUE)
colnames(wine_data) <- c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")
head(wine_data)


#changing category of data
wine_data[c(6,7,12)] <- lapply(wine_data[c(6,7,12)],as.double)
summary(wine_data)

#Preprocess data : discretize data to make it more suitable for Structure and parameter learning
bucket_data <- discretize(wine_data[,-12],method = "interval",breaks =3,ordered = FALSE ,debug = FALSE)
bucket_data$quality <-as.factor(wine_data[,12])
summary(bucket_data)

#Dividing data in training and test set
smp_size <- floor(0.75 * nrow(wine_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(wine_data)),size = smp_size)
wine_train <- bucket_data[train_ind,]
wine_test<- bucket_data[-train_ind,]

head(wine_train)
summary(wine_train)
lapply(wine_train, class)

##----MODEL1---##

#HILL CLIMBING

wine_res = hc(wine_train)
plot(wine_res)
fitted = bn.fit(wine_res, wine_train)     # learning of parameters
pred = predict(fitted, "quality" ,wine_test)  
cbind(pred, wine_test[, "quality"])           # compare the actual and predicted
mean(pred == wine_test$quality)
confusionMatrix(pred,wine_test$quality,positive = NULL,prevalence = NULL,)

#Naives Bayes

NB.net<- naive.bayes(wine_train, "quality")
plot(NB.net)
NB.fit <- bn.fit(NB.net,wine_train)
NB.pred = predict(NB.fit, wine_test)
summary(NB.pred)
#writeLines("\n Multinomial Naive Bayes \n")
mean(NB.pred == wine_test$quality)
confusionMatrix(NB.pred,wine_test$quality,positive = NULL,prevalence = NULL,)

#Tree Augmented Bayesian Network

TB.net <- tree.bayes(wine_train,"quality",whitelist = NULL, blacklist = NULL,
                     mi = NULL, root = NULL, debug = FALSE)
plot(TB.net)
TB.fit <- bn.fit(TB.net,wine_train)
TB.pred = predict(TB.fit,wine_test)
summary(TB.pred)
mean(TB.pred == wine_test$quality)
confusionMatrix(TB.pred,wine_test$quality,positive = NULL,prevalence = NULL,)



###----MODEL 2----##(trying with the feature selected variables)
wine_train2 <- wine_train[,c(2,3,7,9,10,11,12)]	# Model2
wine_test2 <- wine_test[,c(2,3,7,9,10,11,12)]

#HILL CLIMBING

wine_res2 = hc(wine_train2)
plot(wine_res2)
fitted2 = bn.fit(wine_res2, wine_train2)     # learning of parameters
pred2 = predict(fitted2, "quality" ,wine_test2)  
cbind(pred2, wine_test2[, "quality"])           # compare the actual and predicted
mean(pred2 == wine_test2$quality)
confusionMatrix(pred2,wine_test2$quality,positive = NULL,prevalence = NULL,)


#Naives Bayes

NB.net2<- naive.bayes(wine_train2, "quality")
plot(NB.net2)
NB.fit2 <- bn.fit(NB.net2,wine_train2)
NB.pred2 = predict(NB.fit2, wine_test2)
summary(NB.pred2)
#writeLines("\n Multinomial Naive Bayes \n")
mean(NB.pred == wine_test2$quality)
confusionMatrix(NB.pred2,wine_test2$quality,positive = NULL,prevalence = NULL,)

#Tree Augmented Bayesian Network

TB.net2 <- tree.bayes(wine_train2,"quality",whitelist = NULL, blacklist = NULL,
                     mi = NULL, root = NULL, debug = FALSE)
plot(TB.net2)
TB.fit2 <- bn.fit(TB.net2,wine_train2)
TB.pred2 = predict(TB.fit2,wine_test2)
summary(TB.pred2)
mean(TB.pred2 == wine_test2$quality)
confusionMatrix(TB.pred2,wine_test2$quality,positive = NULL,prevalence = NULL,)


