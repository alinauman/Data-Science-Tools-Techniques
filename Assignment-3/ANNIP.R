# install.packages('mlbench')
data(Ionosphere, package="mlbench")
ip <- Ionosphere[complete.cases(Ionosphere), ]

# convert to numeric
for(i in 1:9) {
  ip[, i] <- as.numeric(as.character(ip[, i]))
}

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(ip[,1:34], ip[,35], sizes=c(1:34), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
# V5, V3, V27, V7, V8 - Top 5 Features out of 34

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

#Neural Network
library(neuralnet)
nn <- neuralnet(Class ~ V5+V3+V27+V7+V8, 
                data=trainData, hidden=c(2,1), linear.output=TRUE, threshold=0.01)
nn$result.matrix
plot(nn)

#Test the resulting output
temp_test <- subset(testData, select = c("V5","V3", "V27", "V7", "V8"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testData$Class, prediction = nn.results$net.result)
results
y_pred_num <- ifelse(results$prediction.1 > results$prediction.2, 0, 1)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_pred
y_act <- testData$Class
mean(y_pred == y_act)

table(y_act,y_pred)
