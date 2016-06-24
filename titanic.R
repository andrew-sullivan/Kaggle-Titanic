#reading in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#VARIABLES
#survival - Survival (0 = No; 1 = Yes)
#pclass - Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd)
#name - Name
#sex - Sex
#age - Age
#sibsp - Number of Siblings/Spouses Aboard
#parch - Number of Parents/Children Aboard
#ticket - Ticket Number
#fare - Passenger Fare
#cabin - Cabin
#embarked - Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)

#recoding numerical to factor/categorical variables
train$Survived <- factor(train$Survived)

#creating tables of sex vs survival
sex_table <- table(train$Sex, train$Survived)
sex_table_prop <- prop.table(sex_table, margin=1)

#creating new child category
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0
train$Child <- factor(train$Child)
child_table <- table(train$Child, train$Survived)
child_table_prop <- prop.table(child_table, margin=1)

#loading rpart package
library(rpart)

#decision tree
my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
plot(my_tree)
text(my_tree)

#loading fancy tree plot packages
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#fancy plot of tree
fancyRpartPlot(my_tree)

#predict using decision tree
tree_prediction <- predict(my_tree, test, type = "class")
tree_solution <- data.frame(PassengerId = test$PassengerId, Survived = tree_prediction)
nrow(tree_solution)

#write csv using decision tree predictions
write.csv(tree_solution, file = "tree_solution.csv", row.names = FALSE)

#changing decision tree parameters, BEWARE OF OVERFITTING
#cp - determines when splitting of decision tree stops
#minsplit - determines minimum amount of observations in a leaf of the tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
fancyRpartPlot(my_tree_two)

#adding new variable family_size
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method = "class")
fancyRpartPlot(my_tree_three)

#random forest analysis
str(train)
str(test)

#load in the package
library(randomForest)

#apply the random forest algorithm
#NOTE because there is no argument class, Survived must be turned into a factor
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, importance = TRUE, ntree = 1000)
