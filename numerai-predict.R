library(caret)
library(ROCR)
library(rpart)

setwd("/Users/robert/workspaces/numerai")

source("./src/submission.R")
source("./src/validate.R")

# download files
# download.file("http://datasets.numer.ai/57aada7/numerai_training_data.csv", "./data/numerai_training_data.csv")
# download.file("http://datasets.numer.ai/57aada7/numerai_tournament_data.csv", "./data/numerai_tournament_data.csv")

# load data
train <- read.csv("./data/numerai_training_data.csv")
test <- read.csv("./data/numerai_tournament_data.csv")
sample <- read.csv("./data/example_predictions.csv")

# explore
dim(train)
dim(test)
dim(sample)

str(train)
str(test)

# prepare data
train$target <- as.factor(train$target)

# split data into train and validation
set.seed(1234)
trainIndex <- createDataPartition(train$target, p = .8, list = FALSE, times = 1)

train.tmp <- train
train <- train.tmp[ trainIndex,]
validate  <- train.tmp[-trainIndex,]
rm(train.tmp); rm(trainIndex)


row.names(test) <- test$t_id
test$t_id <- NULL






# benchmarks

nrowTest <- nrow(test)

pred <- rep(0, nrowTest)
submission(pred, 'subm-benchmark-0.csv')

pred <- sample(0:1, nrowTest, replace = T)
submission(pred, 'subm-benchmark-random.csv')

pred <- rep(1, nrowTest)
submission(pred, 'subm-benchmark-1.csv')

# formulas

formula.basic <- target ~ .
formula <- formula.basic

# rpart (logloss = 0.69294)
fit <- rpart(formula, train)
acc <- validate(fit, validate)
print(acc)

pred <- predict(fit, test, type = "prob")
submission(pred[,2], 'simple-rpart.csv')









