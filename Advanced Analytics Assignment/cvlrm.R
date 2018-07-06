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

# Fitting LRM to the Training set


classifierLRM = glm(formula = bad ~ . - totDep -homeVal-AES-PHON,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifierLRM, type = 'response', newdata = test_set[-8])
y_pred_LRM = ifelse(prob_pred > 0.2636735, 1, 0)
# Making the Confusion Matrix
cm = table(test_set[, 8], y_pred_LRM > 0.5)
cm
# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$bad, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifierLRM = glm(formula = bad ~ . - totDep -homeVal-AES-PHON,
                      family = binomial,
                      data = training_set)
  prob_pred = predict(classifierLRM, type = 'response', newdata = test_fold[-8])
  y_pred = ifelse(prob_pred > 0.5, 1, 0)
  cm = table(test_fold[, 8], y_pred > 0.5)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracyLRM = mean(as.numeric(cv))
