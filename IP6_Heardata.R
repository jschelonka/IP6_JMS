# Importing the dataset
dataset = read.csv('heart.data.csv')

#Displaying the count of null values per column
colSums(is.na(dataset))


# Missing data
#na. rm = TRUE to exclude missing values
dataset$biking[is.na(dataset$biking)] <- mean(dataset$biking, na.rm = TRUE)
dataset$smoking[is.na(dataset$smoking)] <- mean(dataset$smoking, na.rm = TRUE)
dataset$heart.disease[is.na(dataset$heart.disease)] <- mean(dataset$heart.disease, na.rm = TRUE)
colSums(is.na(dataset))
#Create multiple copies of the dataset with no missing data
dataset1 <- dataset
dataset2 <- dataset
dataset3 <- dataset
dataset4 <- dataset

##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)


# Fitting Multiple Linear Regression to the Training set
split = sample.split(dataset$heart.disease, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

Regressor_MLR <- lm(formula = heart.disease~., data = training_set)
y_pred = predict(Regressor_MLR, newdata = testing_set)


# Predicting the Validation set results

new <- data.frame(biking = 45.0972, smoking = 21.38562)
predict(Regressor_MLR, newdata = new)

new2 <- data.frame(biking = 8.279743, smoking = 6.42372)
predict(Regressor_MLR, newdata = new2)

new3 <- data.frame(biking = 42.34586, smoking = 20.74133)
predict(Regressor_MLR, newdata = new3)

new4 <- data.frame(biking = 30.77425, smoking = 23.61017)
predict(Regressor_MLR, newdata = new4)

#RMSE
library(caret)
RMSE(testing_set$heart.disease, y_pred)

########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set

# Fitting SVR to the dataset
library(e1071)

regressor_SVR = svm(formula = heart.disease~., data = training_set,
                    type = 'eps-regression',
                    kernel = 'radial')

y_pred = predict(regressor_SVR, newdata = testing_set)

# Predicting the Validation set results
new5 <- data.frame(biking = 45.0972, smoking = 21.38562)
predict(regressor_SVR, newdata = new5)

new6 <- data.frame(biking = 8.279743, smoking = 6.42372)
predict(regressor_SVR, newdata = new6)

new7 <- data.frame(biking = 42.34586, smoking = 20.74133)
predict(regressor_SVR, newdata = new7)

new8 <- data.frame(biking = 30.77425, smoking = 23.61017)
predict(regressor_SVR, newdata = new8)

#RMSE
RMSE(testing_set$heart.disease, y_pred)
#sqrt(mean((dataset$y_test-y_pred)^2))

########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(rpart)

regressor_DT = rpart(formula = heart.disease~., data = training_set)

y_pred = predict(regressor_DT, newdata = testing_set)

# Predicting the Validation set results

new9 <- data.frame(biking = 45.0972, smoking = 21.38562)
predict(regressor_DT, newdata = new9)

new10 <- data.frame(biking = 8.279743, smoking = 6.42372)
predict(regressor_DT, newdata = new10)

new11 <- data.frame(biking = 42.34586, smoking = 20.74133)
predict(regressor_DT, newdata = new11)

new12 <- data.frame(biking = 30.77425, smoking = 23.61017)
predict(regressor_DT, newdata = new12)

#RMSE
RMSE(testing_set$heart.disease, y_pred)
#sqrt(mean((dataset$y_test-y_pred)^2))

########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(randomForest)
set.seed(1234)

regressor_RF = randomForest(x=training_set[,1:2],
                            y = training_set$heart.disease,
                            ntree = 20)

y_pred = predict(regressor_RF, newdata = testing_set)

# Predicting the Validation set results

new13 <- data.frame(biking = 45.0972, smoking = 21.38562)
predict(regressor_RF, newdata = new13)

new14 <- data.frame(biking = 8.279743, smoking = 6.42372)
predict(regressor_RF, newdata = new14)

new15 <- data.frame(biking = 42.34586, smoking = 20.74133)
predict(regressor_RF, newdata = new15)

new16 <- data.frame(biking = 30.77425, smoking = 23.61017)
predict(regressor_RF, newdata = new16)

#RMSE
RMSE(testing_set$heart.disease, y_pred)
#sqrt(mean((dataset$y_test-y_pred)^2))

