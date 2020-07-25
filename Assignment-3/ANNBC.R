data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]

# remove id column
bc <- bc[,-1]

for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(bc[,1:9], bc[,9], sizes=c(1:9), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))


library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

#Neural Network
library(neuralnet)
nn <- neuralnet(Class ~ Cl.thickness+Cell.size+Cell.shape, 
                data=trainData, hidden=c(2,1), linear.output=TRUE, threshold=0.01)
nn$result.matrix
plot(nn)

#Test the resulting output
temp_test <- subset(testData, select = c("Cl.thickness","Cell.size", "Cell.shape"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testData$Class, prediction = nn.results$net.result)
y_pred_num <- ifelse(results$prediction.1 > results$prediction.2, 0, 1)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_pred
y_act <- testData$Class
mean(y_pred == y_act)

table(y_act,y_pred)
