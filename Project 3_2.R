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


## Split name into first and last

split.name <- strsplit(titanic$Name, ',')

## Removes Name column from original data frame
titanic.name <- titanic[,-4]

## Adding occurance of Last name to data frame
Lastname <- sapply(split.name, function(x) x[1])
Firstname <- sapply(split.name, function(x) x[2])

# Split Prefix of first name
split.prefix <- StrSplit(Firstname, '. ')

titanic.name$prefix <- sapply(split.prefix, function(x) x[1])
titanic.name$prefix <- as.factor(titanic.name$prefix)

titanic.name$nameOccur <- 0

for (i in Lastname) {
  titanic.name$nameOccur[which(Lastname==i)] <- 
    Freq(Lastname[Lastname==i])$freq
}

## Split Cabin Number

split.cabin <- strsplit(titanic$Cabin, '')

cabinLetter <- sapply(split.cabin, function(x) x[1])

titanic.name$cabinletter <- cabinLetter

#titanic.name$cabinletter[which(is.na(titanic.name$cabinletter))] <- 'm'

titanic.name$cabinletter <- as.factor(titanic.name$cabinletter)


# ------ Decision Tree for Cabin letter------

cabin.tree <- rpart(cabinletter~Fare+
                      Pclass+
                      Embarked,
                    method='class',
                    na.action=na.omit,
                    data=titanic.name,
                    )

predict.cabin <- predict(cabin.tree,titanic.name[which(is.na(titanic.name$cabinletter)),],
        type='class')

titanic.name$cabinletter[which(is.na(titanic.name$cabinletter))] <- predict.cabin

# actual.cabin <- titanic.name$cabinletter[-which(is.na(titanic.name$cabinletter))]
# 
# accuaracy <- c()
# bluh <- c(1:length(actual.cabin))
# for (i in bluh) {
#   if (actual.cabin[i]==predict.cabin[i]) {
#     accuaracy[i] <- TRUE
#   }
#   else{
#     accuaracy[i] <- FALSE
#   }
# }
# 
# proportion.correct.cabin <- Freq(accuaracy[accuaracy==TRUE])$freq/ 
#   length(actual.cabin); proportion.correct.cabin


# ------ Prediction Plot ------

titanic <- titanic.name

rows.na <- which(is.na(titanic$Age))

titanic.not.na <- titanic[-rows.na,]

mlr <- lm(Age~Sex+
            SibSp+
            Parch+
            Pclass+
            nameOccur+
            Survived+
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

#predict.age[predict.age<0] <- 0

titanic$Age[rows.na] <- predict.age

#titanic$Age[titanic$Age<=10] <- 'Young'
#titanic$Age[titanic$Age>10 & titanic$Age<=28] <- 'Middle'
#titanic$Age[titanic$Age>28 & titanic$Age<=53] <- 'Old'
#titanic$Age[titanic$Age>28] <- 'Ancient'

#titanic$Age <- as.factor(titanic$Age)


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
                               nameOccur+
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
