# Project 3 Random Forest


rm(list=ls())

# Libraries
library(DescTools)
library(stringr)
library(scales)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(kernlab)
library(pracma)

# Set working directory
setwd('C:/Users/conno/Documents/School work/STA 5900/Titanic Survival/Data')


titanic <- read.csv('train.csv')

titanic.test <- read.csv('test.csv')

titanic.test$Survived <- rep('NA',nrow(titanic.test))

titanicPlus <- rbind(titanic, titanic.test)

titanic.test$Survived <- rep(0,nrow(titanic.test))


# ---------- Preparing Variables For Models ----------

# Make Variables categorical

titanicPlus$Sex <- as.factor(titanicPlus$Sex)
titanicPlus$Survived <- as.factor(titanicPlus$Survived)
levels(titanicPlus$Survived) <- c('No', 'Yes', 'NA')
titanicPlus$Embarked <- as.factor(titanicPlus$Embarked)

# Fare price for Passenger 1044 is blank
titanicPlus$Fare[titanicPlus$PassengerId==1044] <- 7.5

# Split Name into First and Last

split.name <- strsplit(titanicPlus$Name, ',')

Lastname <- sapply(split.name, function(x) x[1])
Firstname <- sapply(split.name, function(x) x[2])

titanicPlus$Lastname <- Lastname

  # Adding Number of Occurance of Last Name to each person

Lastname.Occur <- Freq(titanicPlus$Lastname)

titanicPlus$Occur <- rep(1,length(Lastname))
for (i in Lastname.Occur$level) {
  titanicPlus$Occur[which(Lastname==i)] <- Lastname.Occur[Lastname.Occur$level==i,]$freq
}

  # Split Prefix of First Name

split.prefix <- strsplit(Firstname, '. ')

titanicPlus$Prefix <- sapply(split.prefix, function(x) x[1])

titanicPlus$Prefix <- as.factor(titanicPlus$Prefix)

# Separate Cabin Letter

split.cabin <- strsplit(titanicPlus$Cabin, '')

titanicPlus$Letter <- sapply(split.cabin, function(x) x[1])

# Adding Cabin Letter to Family members

Letters.notNA <- titanicPlus[-which(is.na(titanicPlus$Letter)),]

for (i in Letters.notNA$Lastname[Letters.notNA$Occur>1]) {
  titanicPlus$Letter[which(Lastname==i)] <- unique(Letters.notNA$Letter[Letters.notNA$Lastname==i])[1]
}

titanicPlus$Letter <- as.factor(titanicPlus$Letter)


# ---------- Model for Filling in Cabin Letters ----------

# Random Forest Model for predicting Cabin Letters
loop.vector = c(1:1)
for (i in loop.vector) {
  
Cabin.Forest <- randomForest(Letter~Fare+
                               Survived+
                               SibSp+
                               Parch+
                               SibSp*Parch+
                               Survived*SibSp+
                               Pclass*Survived+
                               Pclass+
                               Occur,
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

titanicPlus$Age.Na <- 0
titanicPlus$Age.Na[which(is.na(titanicPlus$Age))] <- 1

Age.lm <- lm(Age~Sex+
               SibSp+
               Parch+
               Prefix*Fare+
               SibSp*Parch+
               Pclass*Fare+
               Pclass*Parch+
               Pclass+
               Occur+
               Occur*SibSp+
               Survived+
               Embarked+
               Letter+
               Prefix+
               Fare,
             na.action=na.omit,
             data=titanicPlus
             )

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

model.forest <- randomForest(Survived~Pclass+
                               Sex+
                               Sex*Prefix+
                               Pclass*SibSp+
                               Age+
                               SibSp+
                               Parch+
                               Fare+
                               Pclass*Fare+
                               SibSp*Parch+
                               Occur*Parch+
                               Occur+
                               Prefix+
                               Age.Na+
                               Letter+
                               Embarked,
                             na.action=na.omit,
                             data=trainSet,
                             proximity=TRUE,
                             method='class'
)

predictor <- predict(model.forest, trainSet, type='class')

length(which(predictor==trainSet$Survived))/nrow(trainSet)


predict.testSet <- predict(model.forest,titanicPlus[titanicPlus$PassengerId>891,],type='class')

titanicPlus$Survived[titanicPlus$PassengerId>891] <- predict.testSet

}

length(which(predictor==trainSet$Survived))/nrow(trainSet)

predict.testSet <- as.numeric(predict.testSet)

predict.testSet[predict.testSet==1] <- 0
predict.testSet[predict.testSet==2] <- 1

df <- data.frame(titanicPlus$PassengerId[titanicPlus$PassengerId>891],predict.testSet)

colnames(df) <- c('PassengerId','Survived')

write.csv(df,file='predict_test3.csv',row.names=FALSE)


