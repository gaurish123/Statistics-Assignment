# k-Fold Cross Validation

# Importing the dataset
dataset = dfFinal


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$bad, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Kernel SVM to the Training set
library(rpart)
classifierTree = rpart(formula = bad ~ .,
                       data = training_set, control = rpart.control(minsplit = 10,cp = 0.02126))

# Predicting the Test set results
y_pred_tree = predict(classifierTree, newdata = test_set[-8], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 8], y_pred_tree)
"------------------------------------------------------------------------"
parameterTuning <- function(cp1){
  
# Importing the dataset
dataset = dfFinal


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$bad, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Kernel SVM to the Training set
library(rpart)
classifierTree = rpart(formula = bad ~ .,
                   data = training_set, control = rpart.control(minsplit = 10,cp = cp1))

# Predicting the Test set results
y_pred_tree = predict(classifierTree, newdata = test_set[-8], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 8], y_pred_tree)

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$bad, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifierTree = rpart(formula = bad ~ .,
                         data = training_fold, control = rpart.control(minsplit = 10, cp = cp1) )
  
  # Predicting the Test set results
  y_pred = predict(classifierTree, newdata = test_fold[-8], type = 'class')
  
  # Making the Confusion Matrix
  cm = table(test_fold[, 8], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracyTree = mean(as.numeric(cv))
return(accuracyTree)

}


#grid search
"""
library(caret)
classifierTree = train( form = bad ~ ., data = training_set ,method = 'rpart' )
classifierTree$bestTune
"""
value = list()
value[1] = parameterTuning(0.5)
value[2] = parameterTuning(0.25)
value[3] = parameterTuning(0.1)
value[4] = parameterTuning(0.05)
value[5] = parameterTuning(0.025)
value[6] = parameterTuning(0.024)
value[7] = parameterTuning(0.023)
value[8] = parameterTuning(0.022)
value[9] = parameterTuning(0.021)
value[10] = parameterTuning(0.020)

plotParams <- as.data.frame(c(0.5,0.25,0.1,0.05,0.025,0.024))

#Feature importance
#install.packages("mlbench")
library(mlbench)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(bad~., data=dataset, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

printcp(classifierTree)
plotcp(classifierTree)
summary(classifierTree)

