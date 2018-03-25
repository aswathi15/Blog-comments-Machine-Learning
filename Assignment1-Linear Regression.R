# import libraries
library(ggplot2)
library(dplyr)
library(reshape2)
#install.packages("Hmisc")
#install.packages("Metrics")
library(Hmisc)
library(Metrics)

setwd("C:\\Users\\Aswathi\\Dropbox\\Spring 2018\\Machine Learning\\BlogFeedback")
getwd()
blog_data = read.csv("blogData_train.csv")
feb_01_df = read.csv("Feb_01.csv")
feb_02_df = read.csv("Feb_02.csv")
march_01_df = read.csv("March_01.csv")
march_02_df = read.csv("March_02.csv")

options(max.print=1000000)
#Experiment 1
#Train data
basic_df <- blog_data[,c(51:60,281)]
textual_df <- blog_data[,c(63:262,281)]

#Test data for basic features
b_feb_01_test <- feb_01_df[,c(51:60,281)]
b_feb_02_test <- feb_02_df[,c(51:60,281)]
b_march_01_test <- march_01_df[,c(51:60,281)]
b_march_02_test <- march_02_df[,c(51:60,281)]

#Test data for textual features
t_feb_01_test <- feb_01_df[,c(63:262,281)]
t_feb_02_test <- feb_02_df[,c(63:262,281)]
t_march_01_test <- march_01_df[,c(63:262,281)]
t_march_02_test <- march_02_df[,c(63:262,281)]

#Change the column names of the basic features and Textual features

basic_column_change <- function(data)
{
  colnames(data) <- c(51:60,"target")
  colnames(data) <- paste("B",colnames(data),sep="")
  return(data)
}

basic_df <- basic_column_change(basic_df)
b_feb_01_test <- basic_column_change(b_feb_01_test)
b_feb_02_test <- basic_column_change(b_feb_02_test)
b_march_01_test <- basic_column_change(b_march_01_test)
b_march_02_test <- basic_column_change(b_march_02_test)


textual_column_change <- function(data)
{
  colnames(data) <- c(63:262,"target")
  colnames(data) <- paste("T",colnames(data),sep="")
  return(data)
}

textual_df <- textual_column_change(textual_df)
t_feb_01_test <- textual_column_change(t_feb_01_test)
t_feb_02_test <- textual_column_change(t_feb_02_test)
t_march_01_test <- textual_column_change(t_march_01_test)
t_march_02_test <- textual_column_change(t_march_02_test)


#--------------Multiple Linear Regression for Basic Features----------------------

b_regressor = lm(formula = Btarget ~ .,data = basic_df)
summary(b_regressor)

# Let us keep the level of significance as 0.001
# We will use the backward elimination and check for the highest p-value, 
# and remove it from the model and refit hte model. 

b_regressor_1 = lm(formula = Btarget ~ .-B53-B55-B58-B60,
               data = basic_df)
summary(b_regressor_1)
#Now we have all the variables as significant , and regressor_1 is our final
#linear regression model. We can now use the test set to predict the results. 
b_predict_feb01 = predict(b_regressor_1,newdata = b_feb_01_test[,1:10])
b_predict_feb02 = predict(b_regressor_1,newdata = b_feb_02_test[,1:10])
b_predict_mar01 = predict(b_regressor_1,newdata = b_march_01_test[,1:10])
b_predict_mar02 = predict(b_regressor_1,newdata = b_march_02_test[,1:10])
print(b_predict_feb01)

b_mse_feb01 = mse(b_feb_01_test$Btarget,b_predict_feb01)
b_mse_feb02 = mse(b_feb_02_test$Btarget,b_predict_feb02)
b_mse_mar01 = mse(b_march_01_test$Btarget,b_predict_mar01)
b_mse_mar02 = mse(b_march_02_test$Btarget,b_predict_mar02)

#-----------------Multiple Regression for Textual Features ---------------------

t_regressor = lm(formula = Ttarget ~ ., data = textual_df)
summary(t_regressor)

# Let us keep the level of significance as 0.001
# We will only keep those features which are significant
# 69,72,77,102,125,137,154,170,184,191,194,195,210,219,228,232,241,251

t_regressor_1 = lm(formula = Ttarget ~ T69+T72+T77+T102+T125+T137+T154+T170+
                     T184+T191+T194+T195+T210+T219+T228+T232+T241+T251
                     ,data=textual_df)
summary(t_regressor_1)

#t_regressor_2 = lm(formula = Ttarget ~ T69-T72+T77+T102+T125-T137+T154+T170+
#                     T184+T191+T194+T195+T210+T219+T228+T232-T241+T251
#                   ,data=textual_df)

t_regressor_2 = lm(formula = Ttarget ~ T69+T77+T102+T125+T154+T170+T184+T191+T194+T195+T210+T219+T228+T232+T251
                   ,data=textual_df)
summary(t_regressor_2)

t_predict_feb01 = predict(t_regressor_2,newdata = t_feb_01_test)
t_predict_feb02 = predict(t_regressor_2,newdata = t_feb_02_test)
t_predict_mar01 = predict(t_regressor_2,newdata = t_march_01_test)
t_predict_mar02 = predict(t_regressor_2,newdata = t_march_02_test)
print(t_predict_feb01)

t_mse_feb01 = mse(t_feb_01_test$Ttarget,t_predict_feb01)
t_mse_feb02 = mse(t_feb_02_test$Ttarget,t_predict_feb02)
t_mse_mar01 = mse(t_march_01_test$Ttarget,t_predict_mar01)
t_mse_mar02 = mse(t_march_02_test$Ttarget,t_predict_mar02)

#-----------------------------------END---------------------------------------------