### Estimate Model Accuracy using Caret ###
library(caret)# for sampling
library(caTools) # for train/test split
library(klaR) # for classification & visualization
# Load data
data(iris)
dataset <- iris
## Data Split
# Split data into training(80%) and test(20%) set 
split <- sample.split(dataset$Species, SplitRatio = 0.8)
train <- subset(dataset, split == TRUE)
test <- subset(dataset, split == FALSE)
# Train a naive bayes model
model <- NaiveBayes(Species ~., data = train)
# make predicitions
x_test <- test[, 1:4]
y_test <- test[, 5]
pred <- predict(model, newdata = x_test)
# summarize results
confusionMatrix(pred$class, y_test)
## Bootstrap
# define training model
train_control <- trainControl(method = "boot", number = 100)
# train the model
model <- train(Species ~., data = dataset, trControl = train_control,
               method = "nb")
# summarize results 
print(model)
## K-fold Cross-Validation (K=10 in this case)
train_control <- trainControl(method = "cv", number = 10)
mode <- train(Species ~., data = dataset, trControl = train_control,
              method = "nb")
print(model)
## LOOCV 
train_control <- trainControl(method = "LOOCV")
model <- train(Species ~., data = dataset, trControl = train_Control,
               method = "nb")
print(model)








