---
title: "Prediction of how well barbell lifts is performed using data from accelerometers on the belt, forearm, arm, and dumbell"
author: "Ghada Abu Sheasha"
date: "Sunday, October 25, 2015"
output: html_document
---


# Synopsis

The aim of the current work is to predit how well barbell lifts is performed from accelerometers on the belt, forearm, arm and dumbell.
After loading the needed libraries and collected data, I used random Forest model to predict the different classes of performing the barbell lifts. I used two strategies to evaluate the performance of the mdoel. First, I split the data set into training and testing sets. Second, I used cross-validation to accurately evaluate the performance of the models.
I developed two models, the first one containing all the predictors. The secnod one contains the first 30 important predictors identified in the first model. 
Whiel the accuracy was 100% in the training sets for the two models, the accuracy of the concise model was better on the testing set. So, I chose the concise model and used it in predicting the test set.
#Loading required libraries

# Loading the data

```r
if (!file.exists("pml-training.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",method = "auto",destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",method = "auto",destfile = "pml-testing.csv")
}
training<-read.csv("pml-training.csv",stringsAsFactors=F,na.strings=c("","#DIV/0!","NA"))
testing<-read.csv("pml-testing.csv",stringsAsFactors=F,na.strings=c("","#DIV/0!","NA"))
```

# Preprocessing of the data file

## Cleaning of the data file
I cleaned the data file by removing all irrelevant and useless predictors
### Rememoving irrelevant variables
first I removed all the variables that were unrelated to the prediction. i.e., the first seven variables

```r
training<-training[,8:160]
testing<-testing[,8:160]
```


### Removing useless varibles
Then, I removed all variables with zero variance as they will of no value in prediction

```r
nsv<-nearZeroVar(training)
training<-(training[-nsv])
testing<-(testing[-nsv])
```
## Pre-processing of the data file
Before pre-processing of the data, I have to remove the outcome varialbe-after saving it in an object named outcome-and convert the variables into numeric variables

```r
#save the training$class into outcome 
outcome<-training$classe
#Remove the outcome variable
training$classe<-NULL
#Convert all variable into numeric
new<-sapply(training,as.numeric)
#convert the dataframe into notcentered
notcentered<-as.data.frame(new)
```
I imputed missing values using knn method

```r
preProc3<-preProcess(notcentered,method="knnImpute")
imputed<-predict(preProc3,notcentered)
```
# Fitting of a randomforest model
Before I applied the model, I splited  into training data set into train and test

```r
imputed$classe<-outcome
set.seed(123)
inTrain <- createDataPartition(y=imputed$classe,p=0.75, list=FALSE)
train <- imputed[inTrain,]
test <- imputed[-inTrain,]
rbind("original dataset" = dim(imputed),
      "training set" = dim(train),
      "testing set"=dim(test))
```

```
##                   [,1] [,2]
## original dataset 19622  118
## training set     14718  118
## testing set       4904  118
```


I trained the random forest model.

```r
#Train a randomForest model
y<-train$classe
y<-as.factor(y)
x<-train[,1:117]
# define training control
train_control <- trainControl(method="cv", number=3)
# train the model 
model <- train(x,y, trControl=train_control, method="rf")
# make predictions
predictionstraining <- predict(model, x)
# summarize results
confusionMatrix(predictionstraining, y)$overall[1]
```

```
## Accuracy 
##        1
```
I determine the important variables by arrangind them according to the number of trees that use them
The following plot shows the predictors arranged according to their importance in prediction

```r
##PLot used variables

varImpPlot(model$finalModel)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

I save the indices of the important pedictors in a vector named imp. I used to select the important predictors in the new model. Then, I trained the new model on the selected predictors only

```r
imp<-varImp(model)

a<-order(-imp$importance)[1:30]

x2<-train[,a]
modFit2 <- train(x2,y, method="rf",trControl=train_control)
# make predictions
predictionstraining2 <- predict(modFit2, x2)
# summarize results
confusionMatrix(predictionstraining2, y)$overall[1]
```

```
## Accuracy 
##        1
```
To evaluate the perfromance of the two models, the redudnant model- the model with all the predictor- and the concise model- the model containing the most important 30 predictors only. The performance of the concise model is a little higher than that of the redundant model althought it uses only 30 predictors.Thus, we will use the concise model

```r
#Predict the performance of the redundant model
predictions=predict(model,test)
#Predict the performance of the consice model
predictions2=predict(modFit2,test)
rbind("Accuracy of the redundant model"=confusionMatrix(test$classe,predictions)$overall[1], "Accuracy of the concise model"=confusionMatrix(test$classe,predictions2)$overall[1])
```

```
##                                  Accuracy
## Accuracy of the redundant model 0.9912316
## Accuracy of the concise model   0.9922512
```

# Predict the test set
First, we have to prepare the testing set to be used for prediction. We have to convert it into numeric variables before imputing the missing values


```r
testing$classe<-NULL
newTest<-sapply(testing,as.numeric)
notcenteredTest<-as.data.frame(newTest)
imputedTest<-predict(preProc3,notcenteredTest[,1:117])
```
I used the concise model to predict the outcome variable in the test set


```r
predictTest2<-predict(modFit2,imputedTest)
```

I used the following function to save the differet predicted values into 20 text file for submission

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictTest2)
```

Upon evaluating the model on the test set (n=20), all the resutls were correct
