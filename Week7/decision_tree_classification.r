# Template for Classification Tree  
# Exercise using Titanic Dataset 


install.packages("e1071")
library(foreach)
library(MASS)
library(ggplot2)
library(caret)
library(titanic)
library(rpart)
library(party)


# Create a dataframe with selected variables from the titanic dataset 

dat <- titanic_train[,colnames(titanic_train) %in% 
                       c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]

# Factorize the binary variables 

dat$Survived <- factor(dat$Survived, labels=c("Died", "Survived"))
dat$Sex <- factor(dat$Sex)
# Create a Sex binary variable, and call it Male 
dat$Male <- as.numeric(dat$Sex) - 1

# omit missing variables 

dat <- na.omit(dat)


# Split data

set.seed(1230596) # For reproducibility purpose 

trainIndex <- createDataPartition(dat$Survived, p=0.75, list = FALSE, times = 1)

train <- dat[trainIndex,]
test <- dat[-trainIndex,]

#
# Fit the classification tree
#
fit <- rpart(Survived ~ Age + Male + Pclass, data = train,
  control = rpart.control(minsplit = 20, minbucket = 5))
#
# Plot tree
plot(fit, uniform=TRUE, main="Classification Tree for Titanic Data")
text(fit, use.n=TRUE, all=TRUE, cex=.7)
#
table(dat$Age, dat$Survived, dat$Male)
#
table(dat$Pclass, dat$Survived, dat$Male)
#
train.pred <- predict(fit, train, type="class")
test.pred <- predict(fit, test, type="class")
#
confusionMatrix(train$Survived, train.pred)
confusionMatrix(test$Survived, test.pred)
#
# Prune the tree
#
plotcp(fit)
pfit <- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
#
# Plot the pruned tree
plot(pfit, uniform=TRUE, main="Pruned Classification Tree for Titanic Data")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#
#
# Conditional inference tree
#
cfit <- ctree(Survived ~ Age + Male + Pclass, data=train,
  controls = ctree_control(mincriterion=0.95))
#
plot(cfit, main="Conditional Inference Tree for Titanic Data")
#
train.cpred <- predict(cfit, train, type="response")
test.cpred <- predict(cfit, test, type="response")
#
confusionMatrix(train$Survived, train.cpred)
confusionMatrix(test$Survived, test.cpred)
#
