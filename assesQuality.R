library(caret)
library(randomForest)
library(party)
library(rpart)

# get all accelerometer data by grepping on 'accel_' in colnames 


initD <- read.csv("pml-training.csv")
accelNames <- grep("accel_", names(initD))
cnames <- names(initD)[accelNames]

# add 'classe' field in column 160 to the data
accelNames <- c(1:7, accelNames, 160)
baseD <- initD[accelNames]

#preprocess
baseD$new_window <- (baseD$new_window=="yes")*1

# remove fields which have NA
naFields <- grep("var_", names(baseD))
baseD <- baseD[,-naFields]

# slice data
inTrain <- createDataPartition(y=baseD$classe, p=0.7, list=FALSE)
trainD <- baseD[inTrain,]
testingD <- baseD[-inTrain,]

dim(trainD); dim(testingD)

# create 3 set of features
# (1) parameters only,(2) UserName+timestamp+parameters,(3)timestamp+parameters
paramD <- trainD[, 8:24]
UsDuParamD <- trainD[,2:24]
duParamD <- trainD[,3:24]

testParamD <- testingD[, 8:24]
testUsDuParamD <-  testingD[,2:24]
testduParamD <- testingD[,3:24]

# model using decision tree - params without timestamp
modFit <-train(classe ~ .,method="rpart",data=paramD)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE,
    main="Classification Tree")
    text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
pred <- predict(modFit,newdata=testParamD)
res <- (pred==testParamD$classe)*1
correctPred <- sum(res)/length(res)

# params include timestamp
dumodFit <-train(classe ~ .,method="rpart",data=duParamD)
dupred<- predict(dumodFit,newdata=testduParamD)
dures <- (dupred==testduParamD$classe)*1
ducorrectPred <- sum(dures)/length(dures)

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

# model using random forest algo
rfFit <-train( classe ~ .,data=paramD,method="rf",prox=TRUE, eval=FALSE)

saveRDS(rfFit, file="rfModel.rds")
rfFit = readRDS("rfModel.rds")

#preProcess validation data run validation test for random forest
valinitD <- read.csv("pml-testing.csv")
valbaseD <- valinitD[accelNames]
valbaseD$new_window <- (valbaseD$new_window=="yes")*1
valbaseD <- valbaseD[,-naFields]
valparamD <- valbaseD[, 8:24]
valpred <- predict(rfFit,newdata=valparamD)
valpred


