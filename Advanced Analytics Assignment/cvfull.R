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

library(MASS)
lda = lda(formula = bad ~ ., data = dfFinal)
training_set = as.data.frame(predict(lda, training_set))
training_set = training_set[c(4, 1)]
test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(4,1)]


# Applying k-Fold Cross Validation
# install.packages('caret')

folds = createFolds(training_set$class, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  
  classifier_full = svm(formula = class ~ .,
                        data = training_fold,
                        type = 'C-classification',
                        kernel = 'linear')
  
  # Predicting the Test set results
  y_pred = predict(classifier_full, newdata = test_fold[-2])
  
  #confusion Matrix
  cm = table(test_fold[,2],y_pred)
  
  
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracyTree = mean(as.numeric(cv))


