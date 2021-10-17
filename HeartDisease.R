dir <- '' 
setwd(dir)
getwd()

source('src/tools.R')
tools.load_sources()

file <- list.files(pattern = ".csv")
raw_data <- read.csv(file) 
dim(raw_data)
colnames(raw_data)

summary(raw_data)

nrow(raw_data[raw_data$RestingBP == 0.0 ,])
nrow(raw_data[raw_data$Cholesterol == 0.0 ,])
raw_data <- raw_data[raw_data$RestingBP != 0.0,]
raw_data <- raw_data[raw_data$Cholesterol != 0.0,]

library(ggplot2)
raw_data$HeartDisease = sapply(raw_data$HeartDisease, function(x) {
  ifelse(x == '0', "Healthy", "Sick")
})
raw_data$FastingBS = sapply(raw_data$FastingBS, function(x) {
  ifelse(x == '0', "Normal", "High")
})
cols_of_interest <- c('Sex', 'ChestPainType', 'FastingBS', 'RestingECG', 
                      'ExerciseAngina', 'ST_Slope')
dv.plot_multiple_bars_II(raw_data, cols_of_interest, 'HeartDisease', title = 'Number of healthy/sick patients per', xlabel = '', ylabel = 'count')


library(psych)
pairs.panels(raw_data[c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak')])

# Data Manipulation

raw_data <- dm.range_divide(raw_data, 'Age', 5, 1)
levels(raw_data$Age_range) <- c('28-37', '38-47', '48-57', '58-67', '68-77')

raw_data <- dm.range_divide(raw_data, 'RestingBP', 5, 1)

raw_data <- dm.range_divide(raw_data, 'MaxHR', 5, 1)

raw_data <- dm.range_divide(raw_data, 'Oldpeak', 5, .1)

raw_data <- dm.range_divide(raw_data, 'Cholesterol', 5, 1)

fac_cols <- c('Sex', 'ChestPainType', 'FastingBS', 'RestingECG', 
              'ExerciseAngina', 'ST_Slope', 'HeartDisease')

raw_data = dm.factorize_cols(raw_data, fac_cols)

cols_to_drop <- c('Sex', 'Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak')
data <- dm.drop_cols(raw_data, cols_to_drop)


# Building the Machine Learning model

library(randomForest)
Relevance <- randomForest(HeartDisease ~., data = data, ntree = 100,
                          nodesize = 10, importance = T)
varImpPlot(Relevance)

random_indexes <- de.get_random_row_indexes(data, 80)
trainSet <- data[random_indexes, ]
testSet <- data[-random_indexes, ]
model <- randomForest(HeartDisease ~., data = trainSet, 
                      ntree = 100, nodesize = 10)
model

library(caret)
prediction = predict(model, newdata = testSet)
comparisonTable = data.frame(observed = testSet$HeartDisease, 
                             predicted = prediction)
confusionMatrix(comparisonTable$observed, comparisonTable$predicted)

library(ROCR)
class1 <- predict(model, newdata = testSet, type = 'prob')
class2 <- testSet$HeartDisease
pred <- prediction(class1[,2], class2)
perf <- performance(pred, 'tpr', 'fpr')
auc <- dv.get_auc(pred)
plot(perf, col = rainbow(10))
print(auc)
