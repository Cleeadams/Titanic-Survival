# Project 3: Preparing the dataset for analysis

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


# ---------- Preparing Variables For Models ----------

# Make Variables categorical

titanicPlus$Sex[titanicPlus$Sex=='female'] <- 1
titanicPlus$Sex[titanicPlus$Sex=='male'] <- 2

titanicPlus$Sex <- as.numeric(titanicPlus$Sex)
titanicPlus$Survived <- as.factor(titanicPlus$Survived)
levels(titanicPlus$Survived) <- c('No', 'Yes', 'NA')
titanicPlus$Embarked <- as.factor(titanicPlus$Embarked)
titanicPlus$Pclass <- as.factor(titanicPlus$Pclass)

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

titanicPlus$Prefix[which(titanicPlus$Prefix %in% c(' Capt',' Col',
                                               ' Don',
                                               ' Jonkheer',
                                               ' Major',' Rev'
                                               ))] <- ' Mr'

titanicPlus$Prefix[which(titanicPlus$Prefix==' Dr' &
                           titanicPlus$Sex=='male')] <- ' Mr'

titanicPlus$Prefix[which(titanicPlus$Prefix %in% c(' Dona',' Mme'
                                               ))] <- ' Mrs'

titanicPlus$Prefix[which(titanicPlus$Prefix %in% c(' Ms',' Mlle'
                                               ))] <- ' Miss'

titanicPlus$Prefix[which(titanicPlus$Prefix==' Dr' &
                           titanicPlus$Sex=='female')] <- ' Mrs'

titanicPlus$Prefix <- as.factor(titanicPlus$Prefix)
levels(titanicPlus$Prefix)
# Separate Cabin Letter

split.cabin <- strsplit(titanicPlus$Cabin, '')

titanicPlus$Letter <- sapply(split.cabin, function(x) x[1])

# Adding Cabin Letter to Family members

Letters.notNA <- titanicPlus[-which(is.na(titanicPlus$Letter)),]

for (i in Letters.notNA$Lastname[Letters.notNA$Occur>1]) {
  titanicPlus$Letter[which(Lastname==i)] <- unique(Letters.notNA$Letter[Letters.notNA$Lastname==i])[1]
}

titanicPlus$Letter <- as.factor(titanicPlus$Letter)

# Adding a Family Size Column

titanicPlus$Family <- titanicPlus$Parch + titanicPlus$SibSp

# Adding a Fare per Person Column

titanicPlus$FPP <- titanicPlus$Fare / (titanicPlus$Family+1)

# Creating an Age.na column: represents with passengers had incomplete Age

titanicPlus$Age.Na <- 0
titanicPlus$Age.Na[which(is.na(titanicPlus$Age))] <- 1

# ------ Creating CSV for the new dataset ------

write.csv(titanicPlus,'new_train.csv',row.names=FALSE)

