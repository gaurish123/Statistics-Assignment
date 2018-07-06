# Importing the dataset
dataset = dfFinal


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$bad, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

library(MASS)
qda.fit = lda(bad~ ., data = training_set)

# Predicting the Test set results
qda.pred = as.data.frame(predict(lda.fit,test_set[-8]))


# Making the Confusion Matrix
cm = table(test_set[,8], qda.pred$class)
cm

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$bad, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  set.seed(123)
  qda.fit = lda(bad~ ., data = training_fold)
  
  # Predicting the Test set results
  qda.pred = as.data.frame(predict(qda.fit,test_fold[-8]))
  
  
  # Making the Confusion Matrix
  cm = table(test_fold[,8], qda.pred$class)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracyqda = mean(as.numeric(cv))



