# Project 3 Random Forest


rm(list=ls())

# Libraries
library(DescTools)
library(glmnet)
library(stringr)
library(scales)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(kernlab)
library(pracma)
library(pROC)

# Set working directory
setwd('C:/Users/conno/Documents/School work/STA 5900/Titanic Survival/Data')

titanicPlus <- read.csv('new_train.csv')

titanicPlus$Survived[892:nrow(titanicPlus)] <- 'NA'

# ------ Changing variable to categorical ------

#titanicPlus$Sex <- as.factor(titanicPlus$Sex)
titanicPlus$Survived <- as.factor(titanicPlus$Survived)
titanicPlus$Embarked <- as.factor(titanicPlus$Embarked)
titanicPlus$Pclass <- as.factor(titanicPlus$Pclass)
titanicPlus$Letter <- as.factor(titanicPlus$Letter)
titanicPlus$Prefix <- as.factor(titanicPlus$Prefix)

# ---------- Model for Filling in Cabin Letters ----------

# Random Forest Model for predicting Cabin Letters
#loop.vector = c(1:1)
#for (i in loop.vector) {
  
# Cabin.Forest <- randomForest(Letter~Fare+
#                                Survived+
#                                SibSp+
#                                Parch+
#                                SibSp*Parch+
#                                Survived*SibSp+
#                                Pclass*Survived+
#                                Pclass+
#                                Occur,
#                              method='class',
#                              na.action=na.omit,
#                              data=titanicPlus,
#                              proximity=TRUE
#                              )
  
Cabin.Forest <- randomForest(Letter~Pclass+FPP+Occur+
                               Lastname+
                               Cabin,
                               method='class',
                               na.action=na.omit,
                               data=titanicPlus,
                               proximity=TRUE
)

if (length(which(is.na(titanicPlus$Letter)))>0){
  cabin.na <-which(is.na(titanicPlus$Letter))
}

predict.Letter <- predict(Cabin.Forest,titanicPlus[cabin.na,],
                         type='class')

titanicPlus$Letter[cabin.na] <- predict.Letter


# Lm Model for Predicting Age

  # Creating Age.Na column for Ages not entered

if (length(which(is.na(titanicPlus$Age)))>0){
  Age.na <- which(is.na(titanicPlus$Age))
}

# Age.lm <- lm(Age~Sex+
#                SibSp+
#                Parch+
#                Prefix*Fare+
#                SibSp*Parch+
#                Pclass*Fare+
#                Pclass*Parch+
#                Pclass+
#                Occur+
#                Occur*SibSp+
#                Survived+
#                Embarked+
#                Letter+
#                Prefix+
#                Fare,
#              na.action=na.omit,
#              data=titanicPlus
#              )

# Age.lm <- lm(Age~SibSp+
#                Parch+
#                Prefix+
#                Sex,
#              na.action=na.omit,
#              data=titanicPlus
# )

Age.x <- model.matrix(Age~(SibSp+
                        Parch+
                        Prefix+
                        Sex+
                        Fare+
                        Letter+
                        Embarked+
                        Occur+
                        Pclass)^3,
                      na.action=na.omit,
                      data=titanicPlus
                      )

y <- titanicPlus$Age[-Age.na]

grid <- 10^seq(3,-3,length=200)

Age.lasso <- cv.glmnet(Age.x,y,
                       alpha=1,
                       lambda=grid,
                       folds=10)

print(Age.lasso)

plot(Age.lasso)

lambda_opt <- Age.lasso$lambda.min

train_control <- trainControl(method='CV', number=10)

Age.model <- train(Age~(SibSp+
                          Parch+
                          Prefix+
                          Sex+
                          Fare+
                          Letter+
                          Embarked+
                          Survived+
                          Occur+
                          Pclass)^3+
                     (SibSp+
                        Parch+
                        Prefix+
                        Sex+
                        Fare+
                        Letter+
                        Embarked+
                        Survived+
                        Occur+
                        Pclass)^2,
                   na.action=na.omit,
                   data=titanicPlus,
                   method='glmnet',
                   tuneGrid = expand.grid(alpha=1,
                                          lambda=lambda_opt)
                   )


predict.lasso <- predict(Age.model,titanicPlus[-Age.na,])

RMSE.age <- sqrt(mean((predict.lasso - 
                         titanicPlus$Age[-Age.na])^2)); RMSE.age

predict.lasso <- predict(Age.model,titanicPlus[Age.na,])

titanicPlus$Age[Age.na] <- predict.lasso

############

predict.Age <- predict(Age.lm,titanicPlus[-Age.na,])

RMSE.age <- sqrt(mean((predict.Age - 
                         titanicPlus$Age[-Age.na])^2)); RMSE.age

predict.Age <- predict(Age.lm,titanicPlus[Age.na,])

titanicPlus$Age[Age.na] <- predict.Age

# ---------- Random Forest Prediction Model For Survival ----------
trainSet <- titanicPlus[-which(titanicPlus$Survived=='NA'),]

if (length(which(titanicPlus$Survived=='NA'))<1) {
  trainSet = titanicPlus
}

trainSet$Survived <- as.numeric(trainSet$Survived)
trainSet$Survived <- as.factor(trainSet$Survived)

levels(trainSet$Survived) <- c('No', 'Yes')

# model.forest <- randomForest(Survived~Pclass+
#                                Sex+
#                                Sex*Prefix+
#                                Pclass*SibSp+
#                                Age+
#                                Age*Sex+
#                                SibSp+
#                                Parch+
#                                Fare+
#                                Pclass*Fare+
#                                SibSp*Parch+
#                                Occur*Parch+
#                                Occur+
#                                Prefix+
#                                Age.Na+
#                                Letter+
#                                Embarked,
#                              na.action=na.omit,
#                              data=trainSet,
#                              proximity=TRUE,
#                              method='class'
# )



predictor <- predict(model.forest, trainSet, type='class')

length(which(predictor==trainSet$Survived))/nrow(trainSet)


predict.testSet <- predict(model.forest,titanicPlus[titanicPlus$PassengerId>891,],type='class')

titanicPlus$Survived[titanicPlus$PassengerId>891] <- predict.testSet

#}

length(which(predictor==trainSet$Survived))/nrow(trainSet)

predict.testSet <- as.numeric(predict.testSet)

predict.testSet[predict.testSet==1] <- 0
predict.testSet[predict.testSet==2] <- 1

df <- data.frame(titanicPlus$PassengerId[titanicPlus$PassengerId>891],predict.testSet)

colnames(df) <- c('PassengerId','Survived')

write.csv(df,file='predict_test3.csv',row.names=FALSE)


