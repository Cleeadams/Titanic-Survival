# Titanic Classification


rm(list=ls())

# Libraries
library(DescTools)
library(scales)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

# Set working directory
setwd('C:/Users/conno/Documents/School work/STA 5900/Titanic Survival/Data')


titanic.og <- read.csv('train.csv')

## titanic without names
titanic <- titanic.og[,-4]

## Change sex to factor
sex <- as.factor(titanic$Sex)

## Females -> 1 and Male -> 2
sex <- as.numeric(sex)
cor(titanic$Survived,sex)

## Females -> 0 and Male -> 1
sex[sex==1] <- 0
sex[sex==2] <- 1
cor(titanic$Survived,sex)

## Proportion of female that survived
surv_fem <- subset(titanic,
                   titanic$Sex=='female' & 
                     titanic$Survived==1)

total_fem <- subset(titanic,
                   titanic$Sex=='female')

prop_fem_surv <- nrow(surv_fem)/nrow(total_fem); prop_fem_surv


died_fem <- subset(titanic,
                    titanic$Sex=='female' &
                      titanic$Survived==0)

## Proportion of men that survived
surv_male <- subset(titanic,
                   titanic$Sex=='male' & 
                     titanic$Survived==1)

total_male <- subset(titanic,
                    titanic$Sex=='male')

prop_male_surv <- nrow(surv_male)/nrow(total_male); prop_male_surv


died_male <- subset(titanic,
                      titanic$Sex=='male' &
                        titanic$Survived==0)

# ---------- Male Plots ----------

### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 6),
     main='Male Passengers Who Died'
)


numdiedmalesib <- unique(died_male$SibSp)
numsurvmalesib <- unique(surv_male$SibSp)

## Creating Frequency plot for Male Passengers who died
## Based on 2 variables.

for (i in numdiedmalesib) {
    points(died_male$SibSp[died_male$SibSp==i],
           died_male$Parch[died_male$SibSp==i],
           type = 'n'
           )
    for (j in unique(died_male$Parch[died_male$SibSp==i])) {
      text(died_male$SibSp[died_male$SibSp==i],
           died_male$Parch[died_male$SibSp==i &
                             died_male$Parch==j],
           label=Freq(died_male$Parch[died_male$SibSp==i &
                                        died_male$Parch==j])$freq,
           col='black')
    }
}

### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 6),
     main='Male Passengers Who Survived'
)


## Creating Frequency plot for Male Passengers who survived
## Based on 2 variables.

for (i in numsurvmalesib) {
  points(surv_male$SibSp[surv_male$SibSp==i],
         surv_male$Parch[surv_male$SibSp==i],
         type = 'n'
  )
  for (j in unique(surv_male$Parch[surv_male$SibSp==i])) {
    text(surv_male$SibSp[surv_male$SibSp==i],
         surv_male$Parch[surv_male$SibSp==i &
                           surv_male$Parch==j],
         label=Freq(surv_male$Parch[surv_male$SibSp==i &
                                      surv_male$Parch==j])$freq,
         col='black')
  }
}


### Same plots as above but relative frequency

### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 6),
     main='Precentage of Male Passengers Who Died'
)

## Creating Relative Frequency plot for Male Passengers 
## who died based on 2 variables.

for (i in numdiedmalesib) {
  points(died_male$SibSp[died_male$SibSp==i],
         died_male$Parch[died_male$SibSp==i],
         type = 'n'
  )
  for (j in unique(died_male$Parch[died_male$SibSp==i])) {
    text(died_male$SibSp[died_male$SibSp==i],
         died_male$Parch[died_male$SibSp==i &
                           died_male$Parch==j],
         label=sprintf('%0.2f%%',round(Freq(died_male$Parch[died_male$SibSp==i &
                                      died_male$Parch==j])$freq/nrow(total_male)*100,2)),
         col='black')
  }
}

### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 6),
     main='Percentage of Male Passengers Who Survived'
)


## Creating Frequency plot for Male Passengers who survived
## Based on 2 variables.

for (i in numsurvmalesib) {
  points(surv_male$SibSp[surv_male$SibSp==i],
         surv_male$Parch[surv_male$SibSp==i],
         type = 'n'
  )
  for (j in unique(surv_male$Parch[surv_male$SibSp==i])) {
    text(surv_male$SibSp[surv_male$SibSp==i],
         surv_male$Parch[surv_male$SibSp==i &
                           surv_male$Parch==j],
         label=sprintf('%0.2f%%',round(Freq(surv_male$Parch[surv_male$SibSp==i &
                                      surv_male$Parch==j])$freq/nrow(total_male)*100,2)),
         col='black')
  }
}

 




# ---------- Female Subset -----------


### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 7),
     main='Female Passengers Who Died'
)


numdiedfemalesib <- unique(died_fem$SibSp)
numsurvfemalesib <- unique(surv_fem$SibSp)

## Creating Frequency plot for Female Passengers who died
## Based on 2 variables.

for (i in numdiedfemalesib) {
  points(died_fem$SibSp[died_fem$SibSp==i],
         died_fem$Parch[died_fem$SibSp==i],
         type = 'n'
  )
  for (j in unique(died_fem$Parch[died_fem$SibSp==i])) {
    text(died_fem$SibSp[died_fem$SibSp==i],
         died_fem$Parch[died_fem$SibSp==i &
                           died_fem$Parch==j],
         label=Freq(died_fem$Parch[died_fem$SibSp==i &
                                      died_fem$Parch==j])$freq,
         col='black')
  }
}

### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 6),
     main='Female Passengers Who Survived'
)


## Creating Frequency plot for Female Passengers who survived
## Based on 2 variables.

for (i in numsurvfemalesib) {
  points(surv_fem$SibSp[surv_fem$SibSp==i],
         surv_fem$Parch[surv_fem$SibSp==i],
         type = 'n'
  )
  for (j in unique(surv_fem$Parch[surv_fem$SibSp==i])) {
    text(surv_fem$SibSp[surv_fem$SibSp==i],
         surv_fem$Parch[surv_fem$SibSp==i &
                           surv_fem$Parch==j],
         label=Freq(surv_fem$Parch[surv_fem$SibSp==i &
                                      surv_fem$Parch==j])$freq,
         col='black')
  }
}


### Same plots as above but relative frequency

### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 7),
     main='Precentage of Female Passengers Who Died'
)

## Creating Relative Frequency plot for Female Passengers 
## who died based on 2 variables.

for (i in numdiedfemalesib) {
  points(died_fem$SibSp[died_fem$SibSp==i],
         died_fem$Parch[died_fem$SibSp==i],
         type = 'n'
  )
  for (j in unique(died_fem$Parch[died_fem$SibSp==i])) {
    text(died_fem$SibSp[died_fem$SibSp==i],
         died_fem$Parch[died_fem$SibSp==i &
                          died_fem$Parch==j],
         label=sprintf('%0.2f%%',round(Freq(died_fem$Parch[died_fem$SibSp==i &
                                     died_fem$Parch==j])$freq/nrow(total_fem)*100,2)),
         col='black')
  }
}

### Creating a blank plot
plot(1, type = "n", xlab = "Number of Siblings & Spouses",
     ylab = "Parents & Children", xlim = c(-1, 9), 
     ylim = c(-1, 6),
     main='Percentage of Female Passengers Who Survived'
)


## Creating Relative Frequency plot for Female Passengers who survived
## Based on 2 variables.

for (i in numsurvfemalesib) {
  points(surv_fem$SibSp[surv_fem$SibSp==i],
         surv_fem$Parch[surv_fem$SibSp==i],
         type = 'n'
  )
  for (j in unique(surv_fem$Parch[surv_fem$SibSp==i])) {
    text(surv_fem$SibSp[surv_fem$SibSp==i],
         surv_fem$Parch[surv_fem$SibSp==i &
                          surv_fem$Parch==j],
         label=sprintf('%0.2f%%',round(Freq(surv_fem$Parch[surv_fem$SibSp==i &
                                     surv_fem$Parch==j])$freq/nrow(total_fem)*100,2)),
         col='black')
  }
}






# ---------- Survivors by age ----------

titanic.survived <- subset(titanic, titanic$Survived==1)
hist(titanic.survived$Age)




# ---------- Death by age ----------

titanic.death <- titanic[titanic$Survived==0,]
hist(titanic.death$Age)

row.age.na <- which(is.na(titanic$Age))
titanic.age.na <-titanic[row.age.na,]
Freq(titanic.age.na$Survived)
titanic.age.na.surv <- titanic.age.na[titanic.age.na$Survived==1,]
titanic.age.na.dead <- titanic.age.na[titanic.age.na$Survived==0,]
Freq(titanic.age.na.surv$Pclass)
Freq(titanic.age.na.dead$Pclass)




# ---------- Split First and Last Name ----------

## Split name into first and last

split.name <- strsplit(titanic.og$Name, ',')

## Removes Name column from original data frame
titanic.name <- titanic.og[,-4]

## Adding occurance of Last name to data frame
Lastname <- sapply(split.name, function(x) x[1])
titanic.name$nameOccur <- 0

for (i in Lastname) {
  titanic.name$nameOccur[which(Lastname==i)] <- 
    Freq(Lastname[Lastname==i])$freq
}

## Changing NA to 0
Age.NA <- which(is.na(titanic.name$Age))

titanic.name[is.na(titanic.name)] <- 0

## Adding an Age NA classification column
titanic.name$AgeNA <- 0
titanic.name$AgeNA[Age.NA] <- 1


## Change variables class appropriately

titanic.name$Sex <- as.factor(titanic.name$Sex) # Sex is Categorical

titanic.name$Pclass <- as.factor(titanic.name$Pclass)

# titanic.name$SibSp <- as.factor(titanic.name$SibSp)

# titanic.name$Parch <- as.factor(titanic.name$Parch)

titanic.name$Embarked <- as.factor(titanic.name$Embarked)

titanic.name$AgeNA <- as.factor(titanic.name$AgeNA)

## randomly selecting training set
loop.vector <- c(1:10)
proportion.correct.mlr <- c()
for (j in loop.vector) {
  
trainSet <- titanic.name[sample(nrow(titanic.name), 713),] # Should have length of 713 which is 80%

trainSet.ID <- trainSet$PassengerId

testSet <- titanic.name[-trainSet$PassengerId,] # Should have length of 178 which is 20%

TestSet.ID <- testSet$PassengerId


# ---------- Creating Model with Training Set ----------

  # ------ Linear Regression Model ------
model.mlr <- lm(Survived~Pclass+
                 Sex+
                 Age+
                 SibSp+
                 Parch+
                 Fare+
                 Embarked+
                 nameOccur+
                 AgeNA,
               data=trainSet
)

test.predict <- round(predict(model.mlr,titanic.name),0)
test.actual <- titanic.name$Survived

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
proportion.correct.mlr[j] <- Freq(accuaracy[accuaracy==TRUE])$freq/ 
  length(test.actual); proportion.correct.mlr[j]
}
proportion.correct.mlr
mean(proportion.correct.mlr)
median(proportion.correct.mlr)


  # ------ Logisitc Regression Model ------

titanic.name$Survived <- as.factor(titanic.name$Survived)
levels(titanic.name$Survived) <- c('No','Yes')

proportion.correct.log <- c()
for (j in loop.vector) {
  
  trainSet <- titanic.name[sample(nrow(titanic.name), 713),] # Should have length of 713 which is 80%
  
  trainSet.ID <- trainSet$PassengerId
  
  testSet <- titanic.name[-trainSet$PassengerId,] # Should have length of 178 which is 20%
  
  TestSet.ID <- testSet$PassengerId
  
model.log <- glm(Survived~Pclass+
                 Sex+
                 Age+
                 SibSp+
                 Parch+
                 Fare+
                 Embarked+
                 nameOccur+
                 AgeNA,
               data=trainSet,
               family = 'binomial'
)

test.predict <- round(predict(model.log,titanic.name,type='response'),0)
test.predict[test.predict==0] <- 'No'
test.predict[test.predict==1] <- 'Yes'
test.actual <- titanic.name$Survived

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

proportion.correct.log[j] <- Freq(accuaracy[accuaracy==TRUE])$freq/ 
  length(test.actual)
}

proportion.correct.log
mean(proportion.correct.log)
median(proportion.correct.log)


  # --------- Decision Tree Model ----------

proportion.correct.tree <- c()
for (j in loop.vector) {
  
  trainSet <- titanic.name[sample(nrow(titanic.name), 713),] # Should have length of 713 which is 80%
  
  trainSet.ID <- trainSet$PassengerId
  
  testSet <- titanic.name[-trainSet$PassengerId,] # Should have length of 178 which is 20%
  
  TestSet.ID <- testSet$PassengerId
  
model.tree <- rpart(Survived~Pclass+
              Sex+
              Age+
              SibSp+
              Parch+
              Fare+
              Embarked+
              nameOccur+
              AgeNA,
            data=trainSet,
            method='class'
            )

rpart.plot(model.tree,extra = 106)

test.predict <- predict(model.tree,titanic.name,type='class')
test.actual <- titanic.name$Survived

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

proportion.correct.tree[j] <- Freq(accuaracy[accuaracy==TRUE])$freq/ 
  length(test.actual)
}

proportion.correct.tree
mean(proportion.correct.tree)
median(proportion.correct.tree)

  # ------ Random Forest Regression Model ------

proportion.correct.forest <- c()
for (j in loop.vector) {
  
  trainSet <- titanic.name[sample(nrow(titanic.name), 713),] # Should have length of 713 which is 80%
  
  trainSet.ID <- trainSet$PassengerId
  
  testSet <- titanic.name[-trainSet$PassengerId,] # Should have length of 178 which is 20%
  
  TestSet.ID <- testSet$PassengerId
  
model.forest <- randomForest(Survived~Pclass+
                 Sex+
                 Age+
                 SibSp+
                 Parch+
                 Fare+
                 Embarked+
                 nameOccur+
                 AgeNA,
               data=trainSet,
               proximity=TRUE,
               method='class'
)

test.predict <- predict(model.forest,titanic.name,type='class')
test.actual <- titanic.name$Survived

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
