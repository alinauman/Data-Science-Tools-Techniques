# Load data
# install.packages('mlbench')
data(Ionosphere, package="mlbench")
ip <- Ionosphere[complete.cases(Ionosphere), ]  # keep complete rows

# convert to numeric
for(i in 1:9) {
  ip[, i] <- as.numeric(as.character(ip[, i]))
}

# Change Y values to 1's and 0's
ip$Class <- ifelse(ip$Class == "good", 1, 0)
ip$Class <- factor(ip$Class, levels = c(0, 1))

# Prep Training and Test data.
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(ip$Class, p=0.7, list = F)
trainData <- ip[trainDataIndex, ]
testData <- ip[-trainDataIndex, ]

# Class distribution of train data
table(trainData$Class)


# Down Sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

table(down_train$Class)


# Up Sample (optional)
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)

table(up_train$Class)

# Build full Logistic Model
fulllogitmod <- glm(Class ~., family = "binomial", data=down_train)
coef(fulllogitmod)

#Perform stepwise variable selection
library(MASS)
steplogitmod <- stepAIC(fulllogitmod, trace = FALSE)
coef(steplogitmod)


# Prediction accuracy of the full logistic regression model:
pred <- predict(fulllogitmod, newdata = testData, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
# Accuracy
mean(y_pred == y_act) #85.58

# Prediction accuracy of the stepwise logistic regression model:
pred <- predict(steplogitmod, newdata = testData, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
# Accuracy
mean(y_pred == y_act) #90.38
