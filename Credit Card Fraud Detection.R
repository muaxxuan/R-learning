library(dplyr) # for data manipulation
library(stringr) # for data manipulation
library(caret) # for sampling
library(caTools) # for train/test split
library(ggplot2) # for data visualization
library(corrplot) # for correlations
library(Rtsne) # for tsne plotting
library(DMwR) # for smote implementation
library(ROSE)# for ROSE sampling
library(rpart)# for decision tree model
library(Rborist)# for random forest model
library(xgboost) # for xgboost model

# Function to set plot height and width
fig <- function(width, height){
  options(repr.plot.width = width, repr.plot.height = height)
}
# Load data
dataset <- read.csv("creditcard.csv") 
# Data exploration
dim(dataset)
head(dataset)
str(dataset)
summary(dataset)
# Check for missing values
colSums(is.na(dataset))# No variables are missing
# Check class imbalance
table(dataset$Class)
# Class imbalance in percentage
prop.table(table(dataset$Class))
# Plot the imbalance
fig(12, 8)
common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = dataset, aes(x = factor(Class), 
                      y = prop.table(stat(count)), fill = factor(Class),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_x_discrete(labels = c("no fraud", "fraud"))+
  scale_y_continuous(labels = scales::percent)+
  labs(x = 'Class', y = 'Percentage') +
  ggtitle("Distribution of class labels") +
  common_theme
# The dataset is clearly unbalance as 100% of cases are being non-fraudulent 
# transaction.

# Data Visualization
# Distribution of variable 'Time' by class
fig(14, 8)
dataset %>%
  ggplot(aes(x = Time, fill = factor(Class))) + 
  geom_histogram(bins = 100) +
  labs(x = 'Time in seconds since first transaction',
       y = 'Nb. of transacations') +
  ggtitle('Distribution of time of transaction by class') +
  facet_grid(Class~., scales = 'free_y') + common_theme
# The 'Time' feature looks pretty similar accross both types of transactions.
# One could argue that fraudualent transaction are more uniformly distributed,
# while normal transaction have a cyclical distribution.

fig(14, 8)
ggplot(dataset, aes(x = factor(Class), y = Amount)) + geom_boxplot() + 
  labs(x = 'Class', y = 'Amount') +
  ggtitle("Distribution of transaction amount by class") +
  common_theme
# There is clearly a lot more variability in the transaction values for non
# fraudulent transactions

# Correlation of anonymised variables and 'Amount'
fig(14, 8)
correlations <- cor(dataset[,-1], method = "pearson")
corrplot(correlations, number.cex = .9, 
         method = "circle", 
         type = "full",
         tl.cex = 0.8, tl.col = "black")
# We observe that most of the data features are not correlated.Because before
# publishing, most of the features were presented to a Principal Component 
# Analysis (PCA) algorithm.

# Visualization of transaction using T-SNE (T-distributed Stochastic Neighbor
# Embedding), a technique to reduce dimensionality
fig(16, 10)
# Use 10% of data to compute T-SNE
tsne_subset <- 1:as.integer(0.1*nrow(dataset))
tsne <- Rtsne(dataset[tsne_subset, -c(1, 31)], perplexity = 20, theta = 0.5,
              pca = F, verbose = F, max_iter = 500, check_duplicates = F)

classes <- as.factor(dataset$Class[tsne_subset])
tsne_mat <- as.data.frame(tsne$Y)

ggplot(tsne_mat, aes(x = V1, y = V2)) +
  geom_point(aes(color = classes)) +
  theme_minimal() +
  common_theme +
  ggtitle("T-SNE visualization of transactions") +
  scale_alpha_manual(values = c("#E69F00", "#56B4E9"))
# There appears to be a separation between the two classes as most fraudulent
# transactions seem to lie near the edge of the blob data

# Data Preparation
# Remove 'Time' variables
dataset <- dataset[, -1]
# Change 'Class' variables to factor
dataset$Class <- as.factor(dataset$Class)
levels(dataset$Class) <- c("Not fraud", "Fraud")
# Scale numeric variables
dataset[, -30] <- scale(dataset[, -30])
head(dataset)
# Split data into training and test sets
set.seed(123)
split <- sample.split(dataset$Class, SplitRatio = 0.7)
train <- subset(dataset, split == TRUE)
test <- subset(dataset, split == FALSE)

# Choosing sampling technique
# Class ratio initially
table(train$Class)
# Down sampling
set.seed(9560)
down_train <- downSample(x = train[, -ncol(train)],
                         y = train$Class)
table(down_train$Class)
# Up sampling
set.seed(9560) 
up_train <- upSample(x = train[, -ncol(train)],
                     y = train$Class)
table(up_train$Class)
# SMOTE
set.seed(9560)
smote_train <- SMOTE(Class~., data = train)
table(smote_train$Class)

# Decision Tress (CART)
# CART model performance on imbalanced data
set.seed(5627)
orig_fit <- rpart(Class~., data = train)
# Evaluate model performance on test set
pred_orig <- predict(orig_fit, newdata = test, method = "class")

roc.curve(test$Class, pred_orig[, 2], plotit = TRUE)
# We see that the auc core on the original dataset is 0.912

# Decision tree on various sampling techniques
set.seed(5627)
# Build down-sampled model
down_fit <- rpart(Class~., data = down_train)
# Build up-sampled model
set.seed(5627)
up_fit <- rpart(Class~., data = up_train)
# Build smote model
set.seed(5627)
smote_fit <- rpart(Class~., data = smote_train)

# AUC on down-sampled data
pred_down <- predict(down_fit, newdata = test)
print('Fitting model to down-sampled data')
roc.curve(test$Class, pred_down[, 2], plotit = FALSE)
# AUC on up-sampled data
pred_up <- predict(up_fit, newdata = test)
print('Fitting model to up-sampled data')
roc.curve(test$Class, pred_up[, 2], plotit = FALSE)

pred_smote <- predict(smote_fit, newdata = test)
print('Fitting model to smote data')
roc.curve(test$Class, pred_smote[,2], plotit = FALSE)
# We see that all the sampling techniques have yielded better auc scores than
# the simple imbalanced dataset. We will test different models now using the
# up sampling technique as that has given the highest auc score

# Models on up-sampled data
# Logistic Regression
glm_fit <- glm(Class~., data = up_train, family = 'binomial')
pred_glm <- predict(glm_fit, newdata = test, type = 'response')
roc.curve(test$Class, pred_glm, plotit = TRUE)
# Random Forest
x = up_train[, -30]
y = up_train[, 30]

rf_fit <- Rborist(x, y, ntree = 1000, minNode = 20, maxLeaf = 13)
rf_pred <- predict(rf_fit, test[, -30], ctgCensus = "prob")
prob <- rf_pred$prob

roc.curve(test$Class, prob[, 2], plotit = TRUE)
# XGBoost
# Convert class labels from factor to numeric
labels <- up_train$Class
y <- recode(labels, 'Not_Fraud' = 0, 'Fraud' = 1)
set.seed(42)

xgb <- xgboost(data = data.matrix(up_train[, -30]),
               label = y,
               theta = 0.1,
               gamma = 0.1,
               max_depth = 10,
               nrounds = 300,
               objective = "binary:logistics",
               colsample_bytree = 0.6,
               verbose = 0,
               nthread = 7,)

xgb_pred <- predict(xgb, data.matrix(test[, -30]) )
roc.curve(test$Class, xgp_pred, plotit = TRUE)

# We can also take a look at the important features here
names <- dimnames(data.matrix(up_train[, -30]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgp.plot.importance(importance_matrix[1:10,])
# With an auc score of 0.977  the XGboost model has performed the best 
# though both the random forest and the logistic regression models have 
# shown reasonable performance







