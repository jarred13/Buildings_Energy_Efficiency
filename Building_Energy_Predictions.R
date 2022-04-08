#Predicting Buildings Energy Efficiency

#loading libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(elasticnet)
library(knitr)
library(matrixStats)

#loading the data
energy <- read.csv("../Building_Energy_Efficiency/ENB2012_data.csv")

#looking at the data
head(energy)
summary(energy)
class(energy)
str(energy)
names(energy)

#renaming column names to match the data set description
colnames(energy) <- c('Relative_Compactness', 'Surface_Area', 'Wall_Area',
                      'Roof_Area', 'Overall_Height', 'Orientation',
                      'Glazing_Area', 'Glazing_Area_Distribution',
                      'Heating_Load', 'Cooling_Load')

#Data cleaning

#finding NAs in the data
colSums(is.na(energy))

#checking to see if there are any blank observations
colSums(energy == "")

#scaling the features
energy_scaled <- scale(energy[,1:8])

energy[,1:8] <- scale(energy[,1:8])

#Data Visualization

#boxplot of the variables
boxplot(energy)

#Density of heating load
energy %>% ggplot(aes(Heating_Load)) +
  geom_density(aes(fill = "red", color = "red")) +
  xlab("heating lab") +
  ggtitle("Density of Heating Load") +
  theme_economist() +
  theme(legend.position = "none")

#Density of Cooling load
energy %>% ggplot(aes(Cooling_Load)) +
  geom_density(aes(fill = "blue", color = "blue")) +
  xlab("cooling lab") +
  ggtitle("Density of Cooling Load") +
  theme_economist() +
  theme(legend.position = "none")


#scatter plot of surface area and heating load
energy %>% ggplot(aes(Surface_Area,Heating_Load)) +
                    geom_point(aes(color = "red")) +
                    xlab("surface area") +
                    ylab("heating load")+
                    ggtitle("Surface area and heat") +
                    theme_economist() +
                    theme(legend.position = "none")
  
#scatter plot of roof area and heating load
energy %>% ggplot(aes(Roof_Area,Heating_Load)) +
  geom_point(aes(color = "red")) +
  xlab("roof area") +
  ylab("heating load")+
  ggtitle("Roof area and heat") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of compactness and heating load
energy %>% ggplot(aes(Relative_Compactness,Heating_Load)) +
  geom_point(aes(color = "red")) +
  xlab("relative compactness") +
  ylab("heating load") +
  ggtitle("Relative Compactness and Heating Load") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of surface area and cooling load
energy %>% ggplot(aes(Surface_Area,Cooling_Load)) +
  geom_point(aes(color = "blue")) +
  xlab("surface area") +
  ylab("cooling load")+
  ggtitle("Surface area and cooling") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of roof area and cooling load
energy %>% ggplot(aes(Roof_Area,Cooling_Load)) +
  geom_point(aes(color = "blue")) +
  xlab("roof area") +
  ylab("cooling load")+
  ggtitle("Roof area and cooling") +
  theme_economist() +
  theme(legend.position = "none")

#scatter plot of compactness and cooling load
energy %>% ggplot(aes(Relative_Compactness,Cooling_Load)) +
  geom_point(aes(color = "blue")) +
  xlab("relative compactness") +
  ylab("cooling load") +
  ggtitle("Relative Compactness and Cooling Load") +
  theme_economist() +
  theme(legend.position = "none")  

#Models

#first we will be predicting the heat load

#setting seed
set.seed(1, sample.kind = "Rounding")

#splitting data into test and train data sets
test_index <- createDataPartition(energy$Heating_Load, times = 1, p = 0.2, list = FALSE)
test <- energy[test_index,]
train <- energy[-test_index,]

#checking to see if the test and train have similar outcomes
mean(train$Heating_Load)
mean(test$Heating_Load)

#Will be using k-fold cross validation on all the algorithms
#creating the k-fold parameters, k is 10
set.seed(7, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)

#linear regression for heating load

#training the model using train set
set.seed(9, sample.kind = "Rounding")
train_lm <- train(Heating_Load ~ .,
                  data = train,
                  method = "lm",
                  tuneGrid = data.frame(intercept = seq(-10,10,2)), 
                  trControl = control)

#veiwing traing results
train_lm

#ploting training results
plot(train_lm)

#creating predictions
lm_preds <- predict(train_lm, test)

#calulating the RMSE for the linear regression model
lm_rmse_hl <- RMSE(lm_preds,test$Heating_Load)

#ridge regression for heating load

#training the model using train set
set.seed(10, sample.kind = "Rounding")
train_ridge <- train(Heating_Load ~ .,
                  data = train,
                  method = "ridge",
                  tuneGrid = data.frame(lambda = seq(.001,.005,.001)), 
                  trControl = control)

#viewing training results
train_ridge

#plotting training results
plot(train_ridge)

#creating predictions
ridge_preds <- predict(train_ridge,test)

#creating RMSE for ridge regression model
ridge_rmse_hl <- RMSE(ridge_preds, test$Heating_Load)

#Random Forest for heating load

#training the model using training set
set.seed(12, sample.kind = "Rounding")
train_rf <- train(Heating_Load ~ .,
                     data = train,
                     method = "rf",
                     tuneGrid = data.frame(mtry = seq(2,10,2)), 
                     trControl = control)

#veiwing training results
train_rf

#plotting training results
plot(train_rf)

#creating predictions
rf_preds <- predict(train_rf,test)

#creating RMSE for random forest model
rf_rmse_hl <- RMSE(rf_preds,test$Heating_Load)

#Ensemble for heating load

heating_preds <- data.frame("lm" = lm_preds,
                            "ridge" = ridge_preds,
                            "rf" = rf_preds)

ensemble_preds <- rowMeans(heating_preds)                            

heating_preds$ensemble <- ensemble_preds

ensemble_rmse_hl <- RMSE(ensemble_preds,test$Heating_Load)

#Now we will make predictions for the cooling load

#linear regression for cooling load

#training the model using train set
set.seed(9, sample.kind = "Rounding")
train_lm <- train(Cooling_Load ~ .,
                  data = train,
                  method = "lm",
                  tuneGrid = data.frame(intercept = seq(-10,10,2)), 
                  trControl = control)

#veiwing traing results
train_lm

#ploting training results
plot(train_lm)

#creating predictions
lm_preds <- predict(train_lm, test)

#calulating the RMSE for the linear regression model
lm_rmse_cl <- RMSE(lm_preds,test$Cooling_Load)

#ridge regression for cooling load

#training the model using train set
set.seed(10, sample.kind = "Rounding")
train_ridge <- train(Cooling_Load ~ .,
                     data = train,
                     method = "ridge",
                     tuneGrid = data.frame(lambda = seq(.001,.005,.001)), 
                     trControl = control)

#viewing training results
train_ridge

#plotting training results
plot(train_ridge)

#creating predictions
ridge_preds <- predict(train_ridge,test)

#creating RMSE for ridge regression model
ridge_rmse_cl <- RMSE(ridge_preds, test$Cooling_Load)

#Random Forest for cooling load

#training the model using training set
set.seed(12, sample.kind = "Rounding")
train_rf <- train(Cooling_Load ~ .,
                  data = train,
                  method = "rf",
                  tuneGrid = data.frame(mtry = seq(2,10,2)), 
                  trControl = control)

#veiwing training results
train_rf

#plotting training results
plot(train_rf)

#creating predictions
rf_preds <- predict(train_rf,test)

#creating RMSE for random forest model
rf_rmse_cl <- RMSE(rf_preds,test$Cooling_Load)

#Ensemble for cooling load

cooling_preds <- data.frame("lm" = lm_preds,
                            "ridge" = ridge_preds,
                            "rf" = rf_preds)

ensemble_preds <- rowMeans(cooling_preds)                            

cooling_preds$ensemble <- ensemble_preds

ensemble_rmse_cl <- RMSE(ensemble_preds,test$Cooling_Load)

#Results

results_hl <- data.frame(Model = c("Linear Regression",
                                   "Ridge Regression",
                                   "Random Forest",
                                   "Ensemble"),
                         RMSE = c(lm_rmse_hl,
                                  ridge_rmse_hl,
                                  rf_rmse_hl,
                                  ensemble_rmse_hl)) 

kable(results_hl)

results_cl <- data.frame(Model = c("Linear Regression",
                                   "Ridge Regression",
                                   "Random Forest",
                                   "Ensemble"),
                         RMSE = c(lm_rmse_cl,
                                  ridge_rmse_cl,
                                  rf_rmse_cl,
                                  ensemble_rmse_cl)) 

kable(results_cl)
