#FEATURE SELECTION

library(corrplot)
library(randomForest)
library(ggplot2)
library(lattice)


wine_data <- read.csv("/Users/manvijain/Desktop/wine data/outlier_free.csv")
colnames(wine_data) <- c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")


#stepwise regression

step_reserveModel <- step(lm(quality ~1, wine_data),scope=list(lower=~1,upper=~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol),direction="forward")
summary(step_reserveModel)
step_reserveModel_bck <- step(lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol+quality,wine_data), direction = "backward")
summary(step_reserveModel_bck)


#randomForest
randf <- randomForest(quality~. ,wine_data,importance =TRUE)
importance(randf)

par(mfrow = c(1,1))
varImpPlot(randf,sort =TRUE)


smp_size <- floor(0.75 * nrow(wine_data))

set.seed(123)
train_ind <- sample(seq_len(nrow(wine_data)),size = smp_size)
t1 <- wine_data[train_ind,]
t2 <- wine_data[-train_ind,]

x_data <- t1[,c(2,3,7,9,10,11,12)]	# Model 1
y_data <- t2[,c(2,3,7,9,10,11,12)]	# Model 1

#x_data <- t1[,c(2,8,10,11,12)]	# Model 2
#y_data <- t2[,c(2,8,10,11,12)]	# Model 2

rf <- randomForest(as.factor(quality) ~.,data=x_data)

pred <- predict(rf,newdata=y_data)

table(y_data$quality,pred)
summary(pred)
