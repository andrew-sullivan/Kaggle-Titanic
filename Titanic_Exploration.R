#loading training data
train <- read.csv("titanic_train.csv")

##INITIAL VARIABLES
#survival - Survived (0 = No; 1 = Yes)
#pclass - Passenger Class (1 = 1st ~ Upper; 2 = 2nd ~ Middle; 3 = 3rd ~ Lower)
#name - Name
#sex - Sex
#age - Age
#sibsp - Number of Siblings/Spouses Aboard
#parch - Number of Parents/Children Aboard
#ticket - Ticket Number
#fare - Passenger Fare
#cabin - Cabin
#embarked - Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)

#logistic regression with a few variables
log.fit1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch, data = train, family = binomial)
summary(log.fit1)
