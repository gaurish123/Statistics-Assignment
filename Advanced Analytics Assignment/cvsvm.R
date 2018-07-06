# Importing the dataset
dataset = dfFinal

# Encoding categorical data
for(unique_value in unique(dataset$totDep)){
  
  
  dataset[paste("totDep", unique_value, sep = ".")] <- ifelse(dataset$totDep == unique_value, 1, 0)
}
dataset$totDep <- NULL
dataset$totDep.5 <-NULL

dataset$RES = factor(dataset$RES,
                     levels = c('F', 'N', 'O','P','U'),
                     labels = c(1, 2, 3,4,5))

for(unique_value in unique(dataset$RES)){
  
  
  dataset[paste("RES", unique_value, sep = ".")] <- ifelse(dataset$RES == unique_value, 1, 0)
}
dataset$RES <- NULL
dataset$RES.5 <-NULL


dataset$AES = factor(dataset$AES,
                     levels = c('B', 'E', 'M', 'N', 'P', 'R', 'T', 'U' ,'V', 'W' ,'Z'),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11))

for(unique_value in unique(dataset$AES)){
  
  
  dataset[paste("AES", unique_value, sep = ".")] <- ifelse(dataset$AES == unique_value, 1, 0)
}
dataset$AES <- NULL
dataset$AES.11 <-NULL

dataset$PHON = factor(dataset$PHON,
                      levels = c('0','1'),
                      labels = c(1,2))

for(unique_value in unique(dataset$PHON)){
  
  
  dataset[paste("PHON", unique_value, sep = ".")] <- ifelse(dataset$PHON == unique_value, 1, 0)
}
dataset$PHON <- NULL
dataset$PHON.2 <-NULL





# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)

split = sample.split(dataset$bad, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifierSVM = svm(formula = bad ~.,
                 data = training_set,
                 cost = 10,
                 type = 'C-classification',
                 kernel = 'sigmoid')

# Predicting the Test set results
y_pred_svm = predict(classifierSVM, newdata = test_set[-8])

# Making the Confusion Matrix
cm = table(test_set[, 8], y_pred_svm)
cm
# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$bad, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifierSVM = svm(formula = bad ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifierSVM, newdata = test_fold[-8])
  cm = table(test_fold[, 8], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracySVM = mean(as.numeric(cv))

yPredFinal <- sum(list(y_pred_LRM) +  list(y_pred_LRM) + list(y_pred_LRM) )

yPredAll$Final <- ifelse(((y_pred_LRM + y_pred_svm + y_pred_tree)/2)>1.5,1,0 )

cmFinal = table(test_set[, 8], yPredAll$Final)
