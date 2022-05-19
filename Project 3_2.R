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

titanic.test <- rbind(titanic[1, ] , titanic.test)
titanic.test <- titanic.test[-1,]

# Adjusting Test Set

titanic.test$Sex <- as.factor(titanic.test$Sex)

titanic.test$Embarked <- as.factor(titanic.test$Embarked)

split.name.test <- strsplit(titanic.test$Name, ',')

Lastname.test <- sapply(split.name.test, function(x) x[1])
Firstname.test <- sapply(split.name.test, function(x) x[2])

split.prefix.test <- StrSplit(Firstname.test, '. ')

titanic.test$prefix <- sapply(split.prefix.test, function(x) x[1])
titanic.test$prefix[415] <- ' Don'

titanic.test$prefix <- as.factor(titanic.test$prefix)

titanic.test$Lastname <- Lastname.test

titanic.test$nameOccur <- 0

for (i in Lastname.test) {
  titanic.test$nameOccur[which(Lastname.test==i)] <- 
    Freq(Lastname.test[Lastname.test==i])$freq
}

split.cabin.test <- strsplit(titanic.test$Cabin, '')

cabinLetter.test <- sapply(split.cabin.test, function(x) x[1])

titanic.test$cabinletter <- cabinLetter.test

titanic.cabin.na.test <- titanic[-which(is.na(titanic.test$cabinletter)),]
titanic.cabin.na.test[titanic.cabin.na.test$nameOccur>1,]

list.lastnames.test <- titanic.cabin.na.test$Lastname[titanic.cabin.na.test$nameOccur>1]

for (i in list.lastnames.test) {
  titanic.test$cabinletter[titanic.test$Lastname==i] <- unique(titanic.cabin.na.test$cabinletter[titanic.cabin.na.test$nameOccur>1 & 
                                                                                    titanic.cabin.na.test$Lastname==i])[1]
}

titanic.test$cabinletter <- as.factor(titanic.test$cabinletter)

titanic.test$Fare[153] <- 7.25



# ------ Fixing N/A Ages ------

# There is a problem with the age column.
  # It is missing values

  # To fix this problem we will try to predict the age of the
  # passengers using linear regression

# Adjust variable to numeric or categorical
titanic$Sex <- as.factor(titanic$Sex)

titanic$Embarked <- as.factor(titanic$Embarked)

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
levels(titanic$prefix) <- c('Capt', 'Col', 'Don', 'Dr', 
                            'Jonkheer', 'Lady', 'Major', 
                            'Master', 'Miss', 'Mlle', 
                            'Mme', 'Mr', 'Mrs', 'Ms', 
                            'Rev', 'Sir', 'th')

levels(titanic.test$prefix) <- c('Col', 'Don', 'Dr', 
                                 'Master', 'Miss', 'Mr', 
                                 'Mrs', 'Ms', 'Rev', 'Capt',
                                 'Jonkheer', 'Lady', 'Major',
                                 'Mlle', 'Mme', 'Sir', 'th')

levels(titanic.test$Embarked) <- c('C', 'Q', 'S', '')

titanic$Lastname <- Lastname

titanic$nameOccur <- 0

for (i in Lastname) {
  titanic$nameOccur[which(Lastname==i)] <- 
    Freq(Lastname[Lastname==i])$freq
}

## Split Cabin Number

split.cabin <- strsplit(titanic$Cabin, '')

cabinLetter <- sapply(split.cabin, function(x) x[1])

titanic$cabinletter <- cabinLetter

titanic.cabin.na <- titanic[-which(is.na(titanic$cabinletter)),]
titanic.cabin.na[titanic.cabin.na$nameOccur>1,]

list.lastnames <- titanic.cabin.na$Lastname[titanic.cabin.na$nameOccur>1]

for (i in list.lastnames) {
  titanic$cabinletter[titanic$Lastname==i] <- unique(titanic.cabin.na$cabinletter[titanic.cabin.na$nameOccur>1 & 
                                                                                    titanic.cabin.na$Lastname==i])[1]
}

titanic$cabinletter <- as.factor(titanic$cabinletter)

# ------ Random Forest for Cabin letter------

cabin.tree <- randomForest(cabinletter~Fare+
                      Survived+
                      SibSp+
                      Parch+
                      SibSp*Parch+
                      Survived*SibSp+
                      Pclass+
                      nameOccur,
                    method='class',
                    na.action=na.omit,
                    data=titanic,
                    proximity=TRUE
                    )

cabin.tree.test <- randomForest(cabinletter~Fare+
                             SibSp+
                             Parch+
                             SibSp*Parch+
                             Pclass+
                             nameOccur,
                           method='class',
                           na.action=na.omit,
                           data=titanic,
                           proximity=TRUE
)

#.cabin.tree.best <- cabin.tree: This model is the best right now and can't be removed from rm(list=ls())

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

predict.cabin.test <- predict(cabin.tree.test,titanic.test[which(is.na(titanic.test$cabinletter)),],
                         type='class')

levels(titanic.test$cabinletter) <- levels(titanic$cabinletter)

# Best Prediction
#predict.cabin <- predict(.cabin.tree.best,titanic[which(is.na(titanic$cabinletter)),],
#                         type='class')

titanic$cabinletter[which(is.na(titanic$cabinletter))] <- predict.cabin

titanic.test$cabinletter[which(is.na(titanic.test$cabinletter))] <- predict.cabin.test

# ------ Predicting Age ------

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

mlr.test <- lm(Age~Sex+
            SibSp+
            Parch+
            Pclass+
            nameOccur+
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

predict.age.test <- predict(mlr.test,titanic.test[which(is.na(titanic.test$Age)),])

titanic.test$Age[which(is.na(titanic.test$Age))] <- predict.age.test




# ------ Random Forest Model ------

proportion.correct.forest <- c()
loop.vector <- c(1:20)
for (j in loop.vector) {
  
trainSet <- titanic[sample(nrow(titanic), 713),] # Should have length of 713 which is 80%

testSet <- titanic[-trainSet$PassengerId,] # Should have length of 178 which is 20%

model.forest <- randomForest(Survived~Pclass+
                               Sex+
                               Sex*prefix+
                               Pclass*SibSp+
                               Age+
                               SibSp+
                               Parch+
                               Fare+
                               Pclass*Fare+
                               SibSp*Parch+
                               nameOccur*Parch+
                               nameOccur+
                               prefix+
                               cabinletter+
                               Embarked,
                             data=titanic,
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
median(proportion.correct.forest)



predict.titanic.test <- predict(model.forest,titanic.test,type='class'); predict.titanic.test

predicted.df <- data.frame(titanic.test$PassengerId,predict.titanic.test)

predicted.df$predict.titanic.test <- as.numeric(predicted.df$predict.titanic.test)

predicted.df$predict.titanic.test[which(predicted.df$predict.titanic.test==1)] <- 0
predicted.df$predict.titanic.test[predicted.df$predict.titanic.test==2] <- 1

colnames(predicted.df) <- c('PassengerId','Survived')

write.csv(predicted.df,file='predict_test.csv',row.names=FALSE)
