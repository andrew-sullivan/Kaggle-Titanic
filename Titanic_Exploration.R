##loading training and test data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

##INITIAL VARIABLES
##survival - Survived (0 = No; 1 = Yes)
##pclass - Passenger Class (1 = 1st ~ Upper; 2 = 2nd ~ Middle; 3 = 3rd ~ Lower)
##name - Name
##sex - Sex
##age - Age
##sibsp - Number of Siblings/Spouses Aboard
##parch - Number of Parents/Children Aboard
##ticket - Ticket Number
##fare - Passenger Fare
##cabin - Cabin
##embarked - Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)

##changing numeric variables to factors
train$Survived <- factor(train$Survived)

##logistic regression with a few variables
log.fit1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch, data = train, family = binomial)
summary(log.fit1)

##log.fit1 predictions
log.fit1.probs <- predict(log.fit1, test, type = "response") #probabilities of survival (ie, P(Y=1|X))
log.fit1.pred <- rep(0, length(test$PassengerId))
log.fit1.pred[log.fit1.probs > .5] <- 1
table(log.fit1.pred)

##building .csv for Kaggle submission
PassengerId <- test$PassengerId
Survived <- log.fit1.pred
log.fit1.df <- data.frame(PassengerId, Survived)
write.csv(log.fit1.df, file = "log.fit1.csv", row.names = FALSE)

##refining logistic regression
log.fit2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked, data = train, family = binomial) #logistic regression with all variables
summary(log.fit2)
log.fit2 <- glm(Survived ~ Pclass + Sex + Sex:Age + Sex:Pclass + SibSp, data = train, family = binomial) #dropping factors with high p-values adding interactions
summary(log.fit2)

##log.fit2 predictions
log.fit2.probs <- predict(log.fit2, test, type = "response") #probabilities of survival (ie, P(Y=1|X))
log.fit2.pred <- rep(0, length(test$PassengerId))
log.fit2.pred[log.fit2.probs > .5] <- 1
table(log.fit2.pred)

##building .csv for Kaggle submission
PassengerId <- test$PassengerId
Survived <- log.fit2.pred
log.fit2.df <- data.frame(PassengerId, Survived)
write.csv(log.fit2.df, file = "log.fit2.csv", row.names = FALSE)
