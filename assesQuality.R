library(caret)
library(randomForest)
library(party)
library(rpart)

# get all accelerometer data by grepping on 'accel_' in colnames 

initD <- read.csv("pml-training.csv")
accelNames <- grep("accel_", names(initD))
accelNames <- c(accelNames, 160)
baseD <- initD[, accelNames]

# remove fields which have NA
naFields <- grep("var_", names(baseD))
baseD <- baseD[,-naFields]

# slice data
inTrain <- createDataPartition(y=baseD$classe, p=0.7, list=FALSE)
trainD  <- baseD[inTrain,]
testingD <- baseD[-inTrain,]

dim(trainD); dim(testingD)

paramD <- trainD
testParamD <- testingD


# model using decision tree
modFit <-train(classe ~ .,method="rpart",data=paramD)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE,
    main="Classification Tree")
    text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
pred <- predict(modFit,newdata=testParamD)
decisionTreeCm <- confusionMatrix(pred, testParamD$classe)
decisionTreeCm

# model using bag algo
predictors = data.frame(paramD[,1:16])
#install.packages("party")
treebag <- bag(predictors, paramD$classe, B = 10, vars = ncol(predictors),
            bagControl = bagControl(fit = ctreeBag$fit,
            predict = ctreeBag$pred,
            aggregate = ctreeBag$aggregate))
bagPred <- predict(treebag,testParamD[,1:16])
bagres <- (bagPred==testParamD$classe)*1
bagCorrect <- sum(bagres)/length(bagres)
bagCorrect

# model using random forest algo
rfFit <-train( classe ~ .,data=paramD,method="rf",prox=TRUE, eval=FALSE)

saveRDS(rfFit, file="rfModel.rds")
rfFit = readRDS("rfModel.rds")

#Run the prediction on test data
valpred <- predict(rfFit,newdata=testParamD)
valpred

confusionMatrix(valpred, testParamD$classe)

#preProcess validation data run validation test for random forest
valinitD <- read.csv("pml-testing.csv")
valbaseD <- valinitD[accelNames]
valbaseD <- valbaseD[,-naFields]
valpred <- predict(rfFit,newdata=valbaseD)
valpred



