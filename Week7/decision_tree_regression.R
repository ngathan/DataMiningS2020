# Decision Tree Regression Template 

# Template adapted from: 
# James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). 
# An introduction to statistical learning (Vol. 112, pp. 3-7). New York: springer.
# Book is available for free: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.158.8831&rep=rep1&type=pdf
# Importing the dataset

dataset <- read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

set.seed(123) # Reproducibility 


# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)
# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)
# Fitting Decision Tree Regression to the dataset


install.packages('rpart')
library(rpart)

regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, data.frame(Level = 6.5))
print(y_pred)

# Visualising the Decision Tree Regression results (higher resolution)
# install.packages('ggplot2')

library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')

# Plotting the tree

library(rpart.plot)
plot(regressor)
text(regressor)
rpart.plot(regressor, roundint = TRUE, box.palette="RdBu", shadow.col="gray", nn=TRUE)



