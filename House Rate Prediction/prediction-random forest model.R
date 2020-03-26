#GETTING DATA
housing.df <- read.table("housing.data", header = FALSE, sep = "")
names1 <- read.fwf("housing.names", skip = 30, n = 17,widths = c(-7,8,-60))

#Extract the column names alone from the relevent lines, remove spaces
names2 <- as.character(names1[-c(3,6,15),])
names2 <- gsub(" ", "", names2)
names(housing.df) <- names2

#Exploratory data analysis
any(is.na(housing.df))  #checking for null values
summary(housing.df)

library(ggplot2)
ggplot(housing.df,aes(x=MEDV)) + geom_histogram(bins = 20 , alpha = 0.5 ,fill ="blue") #since we're predicting MEDV


#Split into train-test data
library(caTools)
set.seed(12345)
sample <- sample.split(housing.df$MEDV,SplitRatio = 0.7)
train <- subset(housing.df,sample == TRUE)
test<-subset(housing.df,sample == FALSE)

#Building the Model
library(randomForest)
model <- randomForest(formula = MEDV ~ ., data = train) #(MEDV was skewed to the right hence a log transformation would normalize the distribution of MEDV.)
summary (model)

#Predict on test set
pred.rf <- predict(model,test)
rmse.rf <- sqrt(sum(((pred.rf) - test$MEDV)^2)/
                  length(test$MEDV))
c(RMSE = rmse.rf, pseudoR2 = mean(model$rsq))

plot(pred.rf,test$MEDV, xlab = "Predicted Price", ylab = "Actual Price", pch = 3)