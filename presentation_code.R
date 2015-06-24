

# Data dredging example

set.seed(12345)
aLotOfRandomData <- sapply(1:30, function(i) { rnorm(10000) })
randomDataFrame <- data.frame(t(aLotOfRandomData))

correlations <- cor(randomDataFrame)

max(correlations[abs(correlations) < 1])

which.max(colMeans(randomDataFrame))
which.min(colMeans(randomDataFrame))

with(randomDataFrame, t.test(X9681, X23))

# Overfitting

# Advertising data
advertising <- read.csv("Advertising.csv")
advertising <- arrange(advertising, TV)

set.seed(1000)
inTrain <- createDataPartition(advertising$Sales, p = 0.8)[[1]]
advertisingTrain <- advertising[inTrain,]
advertisingTest <- advertising[-inTrain,]

plot(advertising$TV, advertising$Sales, pch = 19, col = rgb(0, 0.3, 1, 0.8),
     xlab = "TV Advertising Budget", ylab = "Sales", main = "TV Advertising vs Sales")

baselineModel <- lm(Sales ~ 1, data = advertisingTrain)
simpleModel <- lm(Sales ~ TV, data = advertisingTrain)
complexModel <- approxfun(advertisingTrain$TV, advertisingTrain$Sales, method = "linear")

trainPredictionsBaseline <- predict(baselineModel)
trainPredictionsSimple <- predict(simpleModel)
trainPredictionsComplex <- complexModel(advertisingTrain$TV)

testPredictionsBaseline <- predict(baselineModel, newdata = advertisingTest)
testPredictionsSimple <- predict(simpleModel, newdata = advertisingTest)
testPredictionsComplex <- complexModel(advertisingTest$TV)

# Plot the predictions for the training data
plot(advertisingTrain$TV, advertisingTrain$Sales, pch = 19, col = rgb(0, 0.3, 1, 0.8),
     xlab = "TV Advertising Budget", ylab = "Sales", main = "TV Advertising vs Sales")
lines(advertisingTrain$TV, trainPredictionsBaseline, lwd = 2, type = "l", col = "orange")
lines(advertisingTrain$TV, trainPredictionsSimple, lwd = 2, type = "l", col = "darkgreen")
lines(advertisingTrain$TV, trainPredictionsComplex, lwd = 2, type = "l", col = "red")

# Plot the predictions for the test data
plot(advertisingTest$TV, advertisingTest$Sales, pch = 19, col = rgb(0, 0.3, 1, 0.8),
     xlab = "TV Advertising Budget", ylab = "Sales", main = "TV Advertising vs Sales")
lines(advertisingTest$TV, testPredictionsBaseline, lwd = 2, type = "l", col = "orange")
lines(advertisingTest$TV, testPredictionsSimple, lwd = 2, type = "l", col = "darkgreen")
lines(advertisingTest$TV, testPredictionsComplex, lwd = 2, type = "l", col = "red")

# Quantifying the results
RMSE <- function(predictions, observed) sqrt(mean((predictions - observed)^2))

trainRMSEBaseline <- RMSE(trainPredictionsBaseline, advertisingTrain$Sales)
trainRMSESimple <- RMSE(trainPredictionsSimple, advertisingTrain$Sales)
trainRMSEComplex <- RMSE(trainPredictionsComplex, advertisingTrain$Sales)

testRMSEBaseline <- RMSE(testPredictionsBaseline, advertisingTest$Sales)
testRMSESimple <- RMSE(testPredictionsSimple, advertisingTest$Sales)
testRMSEComplex <- RMSE(testPredictionsComplex, advertisingTest$Sales)

c("baseline" = trainRMSEBaseline, "simple" = trainRMSESimple, "complex" = trainRMSEComplex)
c("baseline" = testRMSEBaseline, "simple" = testRMSESimple, "complex" = testRMSEComplex)


linearModelRSS <- function(beta0, beta1, independentVariable, observedValues) {
    predictions <- beta0 + beta1 * independentVariable
    residuals <- observedValues - predictions
    sum(residuals ^ 2)
}

# Visualizing the Linear Regression error function for the previous example
vectorizedRSS <- Vectorize(linearModelRSS, vectorize.args = c("beta0", "beta1"))

library(manipulate)
beta0s <- seq(0, 17, 0.2)
beta1s <- seq(0.01, 0.08, 0.005)
RSSValues <- outer(beta0s,
                   beta1s, 
                   FUN = function(x, y) {
                       vectorizedRSS(x, y, advertising$TV, advertising$Sales) 
                   })

manipulate({
    persp(beta0s,
          beta1s,
          RSSValues,
          ,col =rgb(0, 0.3, 1, 0.5),
          xlab = "Beta0", ylab = "Beta1", zlab = "RSS",
          theta = angle1,
          phi = angle2) -> res
    points(trans3d(7.032594,
                   0.047537,
                   linearModelRSS(7.032594, 0.047537, advertising$TV, advertising$Sales),
                   pmat = res),
           pch = 19, col= "red")
}, 
angle1 = slider(0, 360),
angle2 = slider(0, 360))

# Cross-validation

library(caret)
library(AppliedPredictiveModeling)
library(randomForest)

data(concrete)
summary(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

crossValidation <- trainControl(method = "cv", number = 5)
rfGrid <- expand.grid(.mtry = seq(2, 5, 1))
rfModel <- train(CompressiveStrength ~ ., data = training, method = "rf",
                 trControl = crossValidation, tuneGrid = rfGrid)
print(rfModel)

linearModel <- train(CompressiveStrength ~ ., data = training, method = "lm",
                     trControl = crossValidation)
print(linearModel)
