#====================================================
# Logistic Regression
#====================================================

# 1. Importing the dataset
dataset <- read.csv("Social_Network_Ads.csv", header = TRUE)

# 2. Summary of the dataset
str(dataset)
summary(dataset)

# 3. Splitting randomly the dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set  = subset(dataset, split == FALSE) 

# 4.
training_set[c(3,4)] = scale(training_set[c(3,4)])
test_set[c(3,4)] = scale(test_set[c(3,4)])

# 5.
model <- glm(Purchased ~ Age, family=binomial, data = training_set)
summary(model)

# 7.

# 8. Is Age significant to the model ?
# p-value < 0.05 => Age is significant to the model.

# 9. AIC value ? The model with lower value of AIC is better
model$aic
# 256.109

# 10. Plotting Purchased in fuction of Age
plot(training_set$Age, training_set$Purchased)
curve(predict(model, data.frame(Age=x), type="response"), add=TRUE)
# better plot
library(ggplot2)
ggplot(training_set, aes(x=Age, y=Purchased)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

# 11.
secondModel <- glm(Purchased ~ Age + EstimatedSalary, family=binomial, data = training_set)

# 12.
summary(secondModel)
# both pvalues are < 0.05 => Both predictors are significant to the model.

# 13.
secondModel$aic
# 205.7819
# Aic model 1 > Aic model 2 => This model is indeed better.

# 14.
y_predict = predict(secondModel, test_set[3:4])
y_predict = predict(secondModel, data.frame(Age=test_set[3], EstimatedSalary=test_set[4]))
y_predict
test_set[3:4]