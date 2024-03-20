

##### week 13 in-class exercise #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 

# install packages
install.packages("earth")
install.packages("kknn")
install.packages("nnet")


#load the packages we need
library(broom)
library(earth)
library(kknn)
library(nnet)


# load the dataset
data_vg = read.csv("data_video_game.csv", header = T, sep = ",",
                   stringsAsFactors = T)

# observe the dataset
str(data_vg)

# create index to randomly split the data into 80% and 20%
set.seed(1)
is_training = runif(nrow(data_vg)) < 0.8


# split the data into training and validation sets
data_train = subset(data_vg, is_training)
data_valid = subset(data_vg, !is_training)

# try three models with different interactions
model1 = lm(month.4 ~ month.1 + month.2 + month.3, data = data_train)
model2 = lm(month.4 ~ (month.1 + month.2 + month.3)^2, data = data_train)
model3 = lm(month.4 ~ (month.1 + month.2 + month.3)^3, data = data_train)

# we don't check the coefficients because we don't care about coefficient estimates.
# let's look at predictions.

# calculate RMSE for each predictive model
# first get the difference btw observed and predicted values USING validation set
dif_model1 = data_valid$month.4 - predict(model1, data_valid)
dif_model2 = data_valid$month.4 - predict(model2, data_valid)
dif_model3 = data_valid$month.4 - predict(model3, data_valid)

# second take the average of the squared difference, then take the square root
mean(dif_model1^2)^0.5
mean(dif_model2^2)^0.5
mean(dif_model3^2)^0.5

# which model is the best? -- the second model: because it has lowest RMSE

# estimate the full dataset using the best model

final_model = lm(month.4 ~ (month.1 + month.2 + month.3)^2, data = data_vg)
summary(final_model)

### K-Fold Cross Validation

# set seeds for repeatable results from random number generators
set.seed(1)

# set the number of folds K
nFold = 10

# step 1: Randomly assign which fold each row is in 
valNum = floor(runif(nrow(data_vg))*nFold)+1
head(valNum)

# create a matrix where we store prediction error 
model_performance = matrix(NA, nFold, 3)

# loop through each fold for cross validation
for(i in 1:nFold){
  
  # step 2i: Get the training and validation data for this fold
 trainData = subset(data_vg, valNum != i)
 validData = subset(data_vg, valNum == i)
  
  # step 2ii: Estimate the model for this training data
 model1 = lm(month.4 ~ month.1 + month.2 + month.3, data = trainData)
 model2 = lm(month.4 ~ (month.1 + month.2 + month.3)^2, data = trainData)
 model3 = lm(month.4 ~ (month.1 + month.2 + month.3)^3, data = trainData)
  
  # step 2iii: Calculate out of sample RMSE for this validData
 valid1 = mean((validData$month.4 - predict(model1, validData))^2)^0.5
 valid2 = mean((validData$month.4 - predict(model2, validData))^2)^0.5
 valid3 = mean((validData$month.4 - predict(model3, validData))^2)^0.5
  
  # store model performance
 model_performance[i, ] = c(valid1, valid2, valid3)
 
}

# step 3: Check Average Model Performance
model_performance
colMeans(model_performance)

# step 4: Now which one is the best? 
min(colMeans(model_performance))     # check which column mean is the smallest

# the first model is the best in this case - has the smallest RMSE
finalModel = lm(month.4 ~ month.1 + month.2 + month.3, data = data_vg)
summary(finalModel)

##### predictive models #####

# MARS model
mars_model1 = earth(month.4 ~ month.1 + month.2 + month.3, data = data_train)

#Trace and thres allow some control over when new variables are added
mars_model2 = earth(month.4 ~ month.1 + month.2 + month.3, data = data_train, trace = 2, thres = 0.1)

# trace = 2 means forward pass
# thres = forward stepping threshold. default is 0.001

#Degree allows for more interactions 
mars_model3 = earth(month.4 ~ month.1 + month.2 + month.3, data = data_train, degree = 2)


#Plot it - MARS is more interpretable than other models.  You can see the non-linear effects 
plotmo(mars_model1)
plotmo(mars_model2)
plotmo(mars_model3)


#Check RMSE 
mean((data_valid$month.4 - predict(mars_model1, data_valid))^2)^0.5
mean((data_valid$month.4 - predict(mars_model2, data_valid))^2)^0.5
mean((data_valid$month.4 - predict(mars_model3, data_valid))^2)^0.5

#All of these models beat linear regression!
mars_full = earth(month.4 ~ month.1 + month.2 + month.3, data = data_vg, degree = 2)

### K-Nearest Neighbor
#Default uses square distance 
kknnModel1 = kknn(month.4 ~ month.1 + month.2 + month.3, data_train, data_valid, k = 5)

#linear distance 
kknnModel2 = kknn(month.4 ~ month.1 + month.2 + month.3, data_train, data_valid, k = 5, distance = 1)

#More k
kknnModel3 = kknn(month.4 ~ month.1 + month.2 + month.3, data_train, data_valid, k = 10, distance = 1)

# evaluate the models
mean((data_valid$month.4 - kknnModel1$fitted.values)^2)^0.5
mean((data_valid$month.4 - kknnModel2$fitted.values)^2)^0.5
mean((data_valid$month.4 - kknnModel3$fitted.values)^2)^0.5

### neural network

nnetFit1 = nnet(month.4 ~ month.1 + month.2 + month.3, data = data_train, linout = 1, size = 2, maxit = 10000)
nnetFit2 = nnet(month.4 ~ month.1 + month.2 + month.3, data = data_train, linout = 1, size = 3, maxit = 10000)
nnetFit3 = nnet(month.4 ~ month.1 + month.2 + month.3, data = data_train, linout = 1, size = 6, maxit = 10000, skip = T)

# linout -- =1 means linear output units
# size -- number of units in the hidden layer
# maxit -- maximum number of iterations
# skip -- add skip-layer connections from input to output

#Check RMSE number of units in the hidden layer.
mean((data_valid$month.4 - predict(nnetFit1, data_valid))^2)^0.5
mean((data_valid$month.4 - predict(nnetFit2, data_valid))^2)^0.5
mean((data_valid$month.4 - predict(nnetFit3, data_valid))^2)^0.5


