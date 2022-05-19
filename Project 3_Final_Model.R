# Project 3 Lasso and Ridge


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
library(coefplot)
library(matrixStats)

# Set working directory
setwd('C:/Users/conno/Documents/School work/STA 5900/Titanic Survival/Data')

titanicPlus <- read.csv('new_train.csv')

titanicPlus$Survived[892:nrow(titanicPlus)] <- 'NA'

# ------ Changing variable to categorical ------

#titanicPlus$Sex <- as.factor(titanicPlus$Sex)
titanicPlus$Survived <- as.factor(titanicPlus$Survived)
titanicPlus$Embarked <- as.factor(titanicPlus$Embarked)
titanicPlus$Pclass <- as.factor(titanicPlus$Pclass)
#titanicPlus$Letter <- as.factor(titanicPlus$Letter)
titanicPlus$Prefix <- as.factor(titanicPlus$Prefix)

# ---------- Model for Filling in Cabin Letters ----------


#######################################################


#######################################################



# Random Forest Model for predicting Cabin Letters


cabin.na <-which(is.na(titanicPlus$Letter))

titanicPlus[cabin.na,]$Letter[which(titanicPlus[cabin.na,]$Pclass==1)] <- 'Up'
titanicPlus[cabin.na,]$Letter[which(titanicPlus[cabin.na,]$Pclass==2)] <- 'Middle'
titanicPlus[cabin.na,]$Letter[which(titanicPlus[cabin.na,]$Pclass==3)] <- 'Low'

titanicPlus$Letter <- as.factor(titanicPlus$Letter)

#######################################################


#######################################################


# Lm Model for Predicting Age

# Creating Age.Na column for Ages not entered

if (length(which(is.na(titanicPlus$Age)))>0){
  Age.na <- which(is.na(titanicPlus$Age))
}


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

Age.model <- train(Age~
                     # (SibSp+
                     #      Parch+
                     #      Prefix+
                     #      Sex+
                     #      Fare+
                     #      Letter+
                     #      Embarked+
                     #      Survived+
                     #      Occur+
                     #      Pclass)^3+
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
                   tuneGrid = expand.grid(alpha=0,
                                          lambda=lambda_opt)
)


predict.lasso <- predict(Age.model,titanicPlus[-Age.na,])

RMSE.age <- sqrt(mean((predict.lasso - 
                         titanicPlus$Age[-Age.na])^2)); RMSE.age

predict.lasso <- predict(Age.model,titanicPlus[Age.na,])

titanicPlus$Age[Age.na] <- predict.lasso


#######################################################


#######################################################



# ---------- Random Forest Prediction Model For Survival ----------
trainSet <- titanicPlus[-which(titanicPlus$Survived=='NA'),]

if (length(which(titanicPlus$Survived=='NA'))<1) {
  trainSet = titanicPlus
}

trainSet$Survived <- as.numeric(trainSet$Survived)
trainSet$Survived <- as.factor(trainSet$Survived)

levels(trainSet$Survived) <- c('No', 'Yes')

set.seed(100)


model.forest <- randomForest(Survived~Pclass+
                                         Sex+
                                         Age+
                                         SibSp+
                                         Parch+
                                         Fare+
                                         #Occur+
                                         Prefix+
                                         Age.Na+
                                         #FPP+
                                         #Family+
                                         Letter+
                                         Embarked,
                       na.action=na.omit,
                       data=trainSet,
                       proximity=TRUE,
                       method='class'
)


predict.survived <- predict(model.forest,
                            newx = trainSet,
                            type='class')


cm <- confusionMatrix(trainSet$Survived,
                      predict.survived,
                      positive='Yes'
                      )

cm$table

cm$overall["Accuracy"]


# ################################################
# 
# glm.fitall <- glm(Survived ~ .-Name-Ticket-Lastname-PassengerId-Cabin-Parch-Age.Na-Sex-Embarked+Age*Letter,
#                   data = trainSet,
#                   family=binomial
#                   )
# phat <- predict(glm.fitall, data=trainSet,type = "response")
# yhat <- rep("No", length.out=nrow(trainSet))
# yhat[which(phat>0.5)] <- "Yes"
# yhat <- as.factor(yhat)
# cm.log <- confusionMatrix(trainSet$Survived, yhat, positive='Yes')
# 
# cm.log$table
# 
# cm.log$overall["Accuracy"]
# 
# phat.test <- predict(glm.fitall,
#                      newdata=titanicPlus[titanicPlus$PassengerId>891,],
#                      type = "response"
#                      )
# 
# yhat.test <- rep("No", length.out=nrow(titanicPlus[titanicPlus$PassengerId>891,]))
# yhat.test[which(phat.test>0.45)] <- "Yes"
# yhat.test <- as.factor(yhat.test)
# 
# yhat.test <- as.numeric(yhat.test)
# 
# yhat.test[yhat.test==1] <- 0
# yhat.test[yhat.test==2] <- 1
# 
# df.Theo <- data.frame(titanicPlus$PassengerId[titanicPlus$PassengerId>891],
#                       yhat.test)
# 
# colnames(df.Theo) <- c('PassengerId','Survived')
# 
# write.csv(df.Theo,file='predicted_Theo.csv',row.names=FALSE)
# 
# 
# 
# 
# ################################################
# 






predict.testSet <- predict(model.forest,titanicPlus[titanicPlus$PassengerId>891,],type='class')


titanicPlus$Survived[titanicPlus$PassengerId>891] <- predict.testSet


predict.testSet <- as.numeric(predict.testSet)

predict.testSet[predict.testSet==1] <- 0
predict.testSet[predict.testSet==2] <- 1

df <- data.frame(titanicPlus$PassengerId[titanicPlus$PassengerId>891],predict.testSet)

colnames(df) <- c('PassengerId','Survived')

write.csv(df,file='predicted_Values.csv',row.names=FALSE)




