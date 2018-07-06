# Importing the dataset
dataset = dfFinal


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$bad, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-8],
                          y = training_set$bad,
                          ntree = 500,
                          mtry = 2)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-8])

# Making the Confusion Matrix
cm = table(test_set[, 8], y_pred)

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$bad, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  set.seed(123)
  classifier = randomForest(x = training_fold[-8],
                            y = training_fold$bad,
                            ntree = 500)
  
  # Predicting the Test set results
  y_pred = predict(classifier, newdata = test_fold[-8])
  
  # Making the Confusion Matrix
  cm = table(test_fold[, 8], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracyTree = mean(as.numeric(cv))

"""
library(caret)
classifierRFP = train( form = bad ~ ., data = training_set ,method = 'rf' )
classifierRFP$bestTune
"""
