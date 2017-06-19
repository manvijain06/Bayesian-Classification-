library(corrplot)

mydata = read.csv("/Users/manvijain/Desktop/Bayesian Classification/red_1.csv",1)

data_iqr <- mydata
colnames(data_iqr) <- c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")

vars <- c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")

outliers <- c()

for(i in vars)
{
	max <- quantile(data_iqr[,i],0.75,na.rm=TRUE) + (IQR(data_iqr[,i],na.rm=TRUE)*1.5)
	min <- quantile(data_iqr[,i],0.75,na.rm=TRUE) - (IQR(data_iqr[,i],na.rm=TRUE)*1.5)
	
	idx <- which(data_iqr[,i] < min | data_iqr[,i] > max)
	
	print(paste(i,length(idx),sep=''))
	
	outliers <- c(outliers,idx)
}

outliers <- sort(outliers)

dsbase <- data_iqr[-outliers,]


#write.csv(dsbase,"/Users/manvijain/Desktop/Bayesian Classification/outlier_free.csv",row.names = FALSE)

#testing outlier data for normal distribution

data_no_outlier = read.csv("/Users/manvijain/Desktop/wine data/outlier_free.csv")
colnames(data_no_outlier) <- c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")


hist(data_no_outlier$fixed.acidity,col="light blue",xlab="fixed.acidity",main=NA)
hist(data_no_outlier$volatile.acidity,col="light blue",xlab="Volatile Acidity",main=NA)
hist(data_no_outlier$citric.acid,col="light blue",xlab="Citric Acid",main=NA)
hist(data_no_outlier$residual.sugar ,col="light blue",xlab="Residual Suger",main=NA)

par(mfrow=c(2,2))

hist(data_no_outlier$chlorides,col="light blue",xlab="Chlorides",main=NA)
hist(data_no_outlier$free.sulfur.dioxide,col="light blue",xlab="Free Sulfur Dioxide",main=NA)
hist(data_no_outlier$total.sulfur.dioxide,col="light blue",xlab="Total Sulfur Dioxide",main=NA)
hist(data_no_outlier$density,col="light blue",xlab="Density",main=NA)

par(mfrow=c(2,2))

hist(data_no_outlier$pH,col="light blue",xlab="pH",main=NA)
hist(data_no_outlier$sulphates ,col="light blue",xlab="Sulphates",main=NA)
hist(data_no_outlier$alcohol,col="light blue",xlab="Alcohol",main=NA)
hist(data_no_outlier$quality,col="light blue",xlab="Quality",main=NA)

view(data_no_outlier)
trans <- preProcess(data_no_outlier, method = c("center","scale"))
transformed <- predict(trans, data_no_outlier)
par(mfrow=c(1,2))
hist(data_no_outlier$citric.acid, main="Original",xlab="dist")
hist(transformed$citric.acid    , main="Centered and Scaled",xlab="dist")

