##Gender-Class Model
##based on tutorial from http://www.trevorstephens.com

##loading training and test data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

##exploring passenger Sex vs Survival Rate
summary(train$Sex)
prop.table(table(train$Sex, train$Survived), 1) #1 tells command to give proportions in the 1st dimension (rows), 2 would be 2nd dimension (columns)

##predicting all females survived and all males did not
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1

##building .csv for Kaggle submission
PassengerId <- test$PassengerId
Survived <- test$Survived
genderclass1 <- data.frame(PassengerId, Survived)
write.csv(genderclass1, file = "genderclass1.csv", row.names = FALSE)

##exploring passenger Age vs Surivival
summary(train$Age)

##creating new variable 'Child'
train$Child <- 0
train$Child[train$Age < 18] <- 1

##exploring 'Age' and 'Sex' together
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

##exploring class and ticket pay
##changing fare from a continuous variable into bins
train$Fare2 <- "30+"
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- "20-30"
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- "10-20"
train$Fare2[train$Fare < 10] <- "<10"
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {sum(x)/length(x)}) #indicates that most class 3 women who paid more than $20 for their ticket miss out on a lifeboat

##making prediction
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0

##building .csv for Kaggle submission
PassengerId <- test$PassengerId
Survived <- test$Survived
genderclass2 <- data.frame(PassengerId, Survived)
write.csv(genderclass2, file = "genderclass2.csv", row.names = FALSE)
