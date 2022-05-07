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

# ------ Fixing N/A Ages ------

# There is a problem with the age column.
  # It is missing values

  # To fix this problem we will try to predict the age of the
  # passengers using linear regression

# Adjust variable to numeric or categorical
titanic$Sex <- as.factor(titanic$Sex)

titanic$Embarked <- as.factor(titanic$Embarked)
levels(titanic$Embarked)

titanic$Survived <- as.factor(titanic$Survived)
levels(titanic$Survived) <- c('No','Yes')

## Split name into first and last

split.name <- strsplit(titanic$Name, ',')

## Removes Name column from original data frame
titanic <- titanic[,-4]

## Adding occurance of Last name to data frame
Lastname <- sapply(split.name, function(x) x[1])
Firstname <- sapply(split.name, function(x) x[2])

# Split Prefix of first name
split.prefix <- StrSplit(Firstname, '. ')

titanic$prefix <- sapply(split.prefix, function(x) x[1])
titanic$prefix <- as.factor(titanic$prefix)

titanic$Lastname <- Lastname

titanic$nameOccur <- 0

for (i in Lastname) {
  titanic$nameOccur[which(Lastname==i)] <- 
    Freq(Lastname[Lastname==i])$freq
}

titanic$Lastname[titanic$nameOccur==1] <- 'different'
titanic$Lastname <- as.factor(titanic$Lastname)

## Split Cabin Number

split.cabin <- strsplit(titanic$Cabin, '')

cabinLetter <- sapply(split.cabin, function(x) x[1])

titanic$cabinletter <- cabinLetter

titanic$cabinletter <- as.factor(titanic$cabinletter)

# ------ Random Forest for Cabin letter------

cabin.tree <- randomForest(cabinletter~Fare+
                      Survived+
                      SibSp+
                      Parch+
                      Pclass+
                      nameOccur,
                    method='class',
                    na.action=na.omit,
                    data=titanic,
                    proximity=TRUE
                    )

predict.cabin <- predict(cabin.tree,titanic[-which(is.na(titanic$cabinletter)),],
        type='class')

actual.cabin <- titanic$cabinletter[-which(is.na(titanic$cabinletter))]

accuaracy <- c()
bluh <- c(1:length(actual.cabin))
for (i in bluh) {
  if (actual.cabin[i]==predict.cabin[i]) {
    accuaracy[i] <- TRUE
  }
  else{
    accuaracy[i] <- FALSE
  }
}

proportion.correct.cabin <- Freq(accuaracy[accuaracy==TRUE])$freq/
  length(actual.cabin); proportion.correct.cabin

predict.cabin <- predict(cabin.tree,titanic[which(is.na(titanic$cabinletter)),],
                         type='class')

titanic$cabinletter[which(is.na(titanic$cabinletter))] <- predict.cabin

# ------ Predicting Age ------

rows.na <- which(is.na(titanic$Age))

titanic.not.na <- titanic[-rows.na,]

mlr <- lm(Age~Sex+
            SibSp+
            Parch+
            Pclass+
            nameOccur+
            Survived+
            Sex*Survived+
            Embarked+
            cabinletter+
            prefix+
            Fare,
          data=titanic.not.na,
          )

titanic.na <- titanic[rows.na,]

predict.age <- predict(mlr,titanic.not.na)

RMSE.age <- sqrt(mean((predict.age - 
                         titanic$Age[-rows.na])^2)); RMSE.age

predict.age <- predict(mlr,titanic.na)

titanic$Age[rows.na] <- predict.age


# ------ Random Forest Model ------

proportion.correct.forest <- c()
loop.vector <- c(1:20)
for (j in loop.vector) {
  
trainSet <- titanic[sample(nrow(titanic), 713),] # Should have length of 713 which is 80%

testSet <- titanic[-trainSet$PassengerId,] # Should have length of 178 which is 20%

trainSet$Survived <- as.factor(trainSet$Survived)

model.forest <- randomForest(Survived~Pclass+
                               Sex+
                               Age+
                               SibSp+
                               Parch+
                               Fare+
                               Pclass*Fare+
                               SibSp*Parch+
                               nameOccur+
                               nameOccur*Parch+
                               prefix+
                               cabinletter+
                               Embarked,
                             data=trainSet,
                             proximity=TRUE,
                             method='class'
)

test.predict <- predict(model.forest,titanic,type='class')
test.actual <- titanic$Survived

accuaracy <- c()
bluh <- c(1:length(test.actual))
for (i in bluh) {
  if (test.actual[i]==test.predict[i]) {
    accuaracy[i] <- TRUE
  }
  else{
    accuaracy[i] <- FALSE
  }
}
proportion.correct.forest[j] <- Freq(accuaracy[accuaracy==TRUE])$freq/ 
  length(test.actual)

}


proportion.correct.forest
mean(proportion.correct.forest)
