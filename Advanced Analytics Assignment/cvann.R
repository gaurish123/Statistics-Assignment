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

# Fitting ANN to the Training set
#install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)
model = h2o.deeplearning(y = 'bad',
                         training_frame = as.h2o(training_set),
                         activation = 'Logistic',
                         hidden = c(50,50,50),
                         epochs = 200,
                         train_samples_per_iteration = -2)


# Predicting the Test set results
prob_pred = h2o.predict(model, newdata = as.h2o(test_set[-8]))
y_pred <- prob_pred[1]
y_pred <- as.vector(y_pred)
"""
y_pred = (prob_pred[2] > 0.5)
#y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)
"""

# Making the Confusion Matrix
cm = table(test_set[ ,8], y_pred)
cm
#h2o.shutdown()



