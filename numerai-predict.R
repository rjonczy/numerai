library(caret)
library(pROC)
library(rpart)
library(randomForest)
library(MASS)
library(Metrics)

setwd("/Users/robert/workspaces/numerai")

source("./src/submission.R")
# source("./src/validation.R")


# Load --------------------------------------------------------------------

# download files
# download.file("http://datasets.numer.ai/57aada7/numerai_training_data.csv", "./data/numerai_training_data.csv")
# download.file("http://datasets.numer.ai/57aada7/numerai_tournament_data.csv", "./data/numerai_tournament_data.csv")

# load data
train <- read.csv("./data/numerai_training_data.csv")
test <- read.csv("./data/numerai_tournament_data.csv")
sample <- read.csv("./data/example_predictions.csv")

# Explore -----------------------------------------------------------------


# explore
dim(train)
dim(test)
dim(sample)

str(train)
str(test)


# Prepare -----------------------------------------------------------------

train$target <- as.factor(train$target)

# split data into train and validation
set.seed(1234)
trainIndex <- createDataPartition(train$target, p = .8, list = FALSE, times = 1)

train.tmp <- train
train <- train.tmp[ trainIndex,]
validate  <- train.tmp[-trainIndex,]
rm(train.tmp); rm(trainIndex)

row.names(test) <- test$t_id
test$t_id <- NULL                       # remove t_id from test dataset

# Benchmarks --------------------------------------------------------------

nrowTest <- nrow(test)

pred <- rep(0, nrowTest)
submission(pred, 'subm-benchmark-0.csv')

pred <- sample(0:1, nrowTest, replace = T)
submission(pred, 'subm-benchmark-random.csv')

pred <- rep(1, nrowTest)
submission(pred, 'subm-benchmark-1.csv')



# Formulas ----------------------------------------------------------------
formula.basic <- target ~ .
formula <- formula.basic





# model - rpart -----------------------------------------------------------

# (valid logloss 0.6926833 / numerai logloss 0.69294)
rpartModel <- rpart(formula, train, method = "class")                   # train model

rpartPredValid.prob <- predict(rpartModel, validate, type = "prob")
rpartPredValid.class <- predict(rpartModel, validate, type = "class")

logLoss(as.numeric(as.character(validate$target)), 
        rpartPredValid.prob[,2])                                        # calculate logloss metrics

confusionMatrix(data = rpartPredValid.class,                            # calculate confusion matrix
                reference = validate$target,
                positive = '1')



# printcp(rpartModel) # display the results 
# plotcp(rpartModel) # visualize cross-validation results 
# summary(rpartModel) # detailed summary of splits

rpartPredTest <- predict(rpartModel, test, type = "prob")
submission(rpartPredTest[,2], 'simple-rpart.csv')        # save for submission


# model - qda -------------------------------------------------------------

# (valid logloss 0.6943223 / numerai logloss 0.69657)
qdaModel <- qda(formula, data = train)                   # train model
qdaPredValid <- predict(qdaModel, validate)           # predict on validation dataset

names(qdaPredValid)

qdaPredValid.prob <- qdaPredValid$posterior        # propabilities
qdaPredValid.class <- qdaPredValid$class           # predicted target

head(qdaPredValid.prob)
head(qdaPredValid.class)

logLoss(as.numeric(as.character(validate$target)), 
        qdaPredValidate.prob[,2])                                        # calculate logloss metrics

confusionMatrix(data = rpartPredValid.class,                             # calculate confusion matrix
                reference = validate$target,
                positive = '1')


qdaPredTest <- predict(qdaModel, test)
submission(qdaPredTest$posterior[,2], 'simple-qda.csv') # we are interested in probabilities of target 1











# model - random forest ---------------------------------------------------












# model - logistic regression ---------------------------------------------


