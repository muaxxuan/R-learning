#### IRIS DATASET ####
## Load Data
# Attach iris dataset to the environment
data(iris)
# Rename dataset
dataset <- iris

## Create Validation Dataset ##

# Create a list of 80% of the rows in the original dataset we can use 
# for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# Select 20% of the data for validation
validation_set <- dataset[-validation_index,]
# Use the remaining 80% of data for training and testing the models
dataset <- dataset[validation_index,]

## Summarize dataset ##

# Dimensions of dataset
dim(dataset)
# List types for each attribute
sapply(dataset, class)
# Take a peek at the first 5 rows of the data
head(dataset)
# List the levels for the class
levels(dataset$Species)
# Summarize the class distribution
percentage = prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species), percentage=percentage)
# Summarize attribute distribution
summary(dataset)

## Visualize Dataset ##

## Univariate plot
# Split input and output
x = dataset[,1:4]
y = dataset[,5]
# Boxplot for each attribute on one image
par(mfrow=c(1,4))
  for(i in 1:4)
  {
    boxplot(x[,i], main=names(iris)[i])
  }
# Barplot for class breakdown
plot(y)

## Multivariate plot
# Scatter plot matrix
featurePlot(x=x, y=y, plot="ellipse")
# Box and Whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")
# density plots for each attribute by class value
scales = list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

## Evaluate Some Algorithms ##

# Test Harness #
# Run algorithms using 10-fold cross validation
control = trainControl(method="cv", number=10)
metric = "Accuracy"

# Build Models #
# Let's evaluate 5 different algorithms:
# - Linear Discriminant (LDA)
# - Classification and Regression Trees (CART)
# - K-Nearest Neighbors (KNN)
# Support Vector Machines (SVM) with a linear kernel
# Random Forest (RF)

# Let's reset the random number seed before each run to ensure that the 
# evaluation of each algorithm is performed using exactly the same data splits.
# It ensures the results are directly comparable.

# Let's build our 5 models
# a) Linear algorithms
# LDA
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric,
                 trControl=control)
# b) Non-linear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric,
                 trControl=control)
# KNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric,
                 trControl=control)
# c) Advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric,
                 trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric,
                trControl=control)

# Select Best Model #

# Summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn,
                          svm=fit.svm, rf=fit.rf))
summary(results)
# Compare accuracy of models
dotplot(results)
# Summarize Best Models
print(fit.lda)

# Make Predictions #

# The LDA was the most accurate model. Now we want to get an idea of the 
# accuracy of the model on our validation set.

# This will give us an independent final check on the accuracy of the best 
# model. It is valuable to keep a validation set just in case you made a slip 
# during such as overfitting to the training set or a data leak. Both will 
# result in an overly optimistics result.

# Estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)

# We see that the accuracy is 100%. It was a small validation dataset(20%), but
# this result is wihthin our expected margin of 97% +- 4% suggesting we may 
# have an accurate and reliable model


















