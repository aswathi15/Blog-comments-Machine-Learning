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


blog_data['Outcome'] <- ifelse(blog_data$X1.0.2 > 0 ,1,0)
feb_01_df['Outcome'] <- ifelse(feb_01_df$X4.0 > 0,1,0)
feb_02_df['Outcome'] <- ifelse(feb_02_df$X1.0.23 > 0,1,0)
march_01_df['Outcome'] <- ifelse(march_01_df$X0.0.239 > 0,1,0)
march_02_df['Outcome'] <- ifelse(march_02_df$X1.0.15 > 0 ,1,0)


#Logistic Regression for basic features
#Experiment 1
#Train data
log_basic_df <- blog_data[,c(51:60,282)]
log_textual_df <- blog_data[,c(63:262,282)]

#Test data for basic features
log_b_feb_01_test <- feb_01_df[,c(51:60,282)]
log_b_feb_02_test <- feb_02_df[,c(51:60,282)]
log_b_march_01_test <- march_01_df[,c(51:60,282)]
log_b_march_02_test <- march_02_df[,c(51:60,282)]

#Test data for textual features
log_t_feb_01_test <- feb_01_df[,c(63:262,282)]
log_t_feb_02_test <- feb_02_df[,c(63:262,282)]
log_t_march_01_test <- march_01_df[,c(63:262,282)]
log_t_march_02_test <- march_02_df[,c(63:262,282)]

#Change column names for Textual and Basic features-------------------------------

basic_column_change <- function(data)
{
  colnames(data) <- c(51:60,"target")
  colnames(data) <- paste("B",colnames(data),sep="")
  return(data)
}

log_basic_df <- basic_column_change(log_basic_df)
log_b_feb_01_test <- basic_column_change(log_b_feb_01_test)
log_b_feb_02_test <- basic_column_change(log_b_feb_02_test)
log_b_march_01_test <- basic_column_change(log_b_march_01_test)
log_b_march_02_test <- basic_column_change(log_b_march_02_test)

textual_column_change <- function(data)
{
  colnames(data) <- c(63:262,"target")
  colnames(data) <- paste("T",colnames(data),sep="")
  return(data)
}

log_textual_df <- textual_column_change(log_textual_df)
log_t_feb_01_test <- textual_column_change(log_t_feb_01_test)
log_t_feb_02_test <- textual_column_change(log_t_feb_02_test)
log_t_march_01_test <- textual_column_change(log_t_march_01_test)
log_t_march_02_test <- textual_column_change(log_t_march_02_test)

#Applying PCA
library(caret)
#install.packages('e1071', dependencies=TRUE)
library(e1071)
#------------Applying PCA for Basic Features ------------------------------
pca = preProcess(x = log_basic_df[-11],method='pca',pcaComp = 2)
log_basic_df <- predict(pca,log_basic_df)
log_basic_df <- log_basic_df[c(2,3,1)]

log_b_feb_01_test <- predict(pca,log_b_feb_01_test)
log_b_feb_01_test <- log_b_feb_01_test[c(2,3,1)]

log_b_feb_02_test <- predict(pca,log_b_feb_02_test)
log_b_feb_02_test <- log_b_feb_02_test[c(2,3,1)]

log_b_march_01_test <- predict(pca,log_b_march_01_test)
log_b_march_01_test <- log_b_march_01_test[c(2,3,1)]

log_b_march_02_test <- predict(pca,log_b_march_02_test)
log_b_march_02_test <- log_b_march_02_test[c(2,3,1)]

#------------Applying PCA for Textual Features ------------------------------
pca_1 = preProcess(x = log_textual_df[-201],method='pca',pcaComp = 2)
log_textual_df <- predict(pca_1,log_textual_df)
log_textual_df <- log_textual_df[c(2,3,1)]

log_t_feb_01_test <- predict(pca_1,log_t_feb_01_test)
log_t_feb_01_test <- log_t_feb_01_test[c(2,3,1)]

log_t_feb_02_test <- predict(pca_1,log_t_feb_02_test)
log_t_feb_02_test <- log_t_feb_02_test[c(2,3,1)]

log_t_march_01_test <- predict(pca_1,log_t_march_01_test)
log_t_march_01_test <- log_t_march_01_test[c(2,3,1)]

log_t_march_02_test <- predict(pca_1,log_t_march_02_test)
log_t_march_02_test <- log_t_march_02_test[c(2,3,1)]

#------------------Logistic Regression for Basic Features----------------------------

logregressor = glm(formula = Btarget ~.,family = binomial(link='logit'),
                   data = log_basic_df)
summary(logregressor)


log_b_predict_probfeb01 = predict(logregressor,type= 'response',newdata = log_b_feb_01_test[-3])
log_b_predict_probfeb02 = predict(logregressor,type= 'response',newdata = log_b_feb_02_test[-3])
log_b_predict_probmar01 = predict(logregressor,type= 'response',newdata = log_b_march_01_test[-3])
log_b_predict_probmar02 = predict(logregressor,type= 'response',newdata = log_b_march_02_test[-3])


log_b_pred_feb_01 = ifelse(log_b_predict_probfeb01 > 0.5,1,0)
log_b_pred_feb_02 = ifelse(log_b_predict_probfeb02 > 0.5,1,0)
log_b_pred_mar_01 = ifelse(log_b_predict_probmar01 > 0.5,1,0)
log_b_pred_mar_02 = ifelse(log_b_predict_probmar02 > 0.5,1,0)

#install.packages("caret")
#install.packages('e1071', dependencies=TRUE)

cm_feb01 = confusionMatrix(log_b_pred_feb_01,log_b_feb_01_test$Btarget)
cm_feb02 = confusionMatrix(log_b_pred_feb_02,log_b_feb_02_test$Btarget)
cm_mar01 = confusionMatrix(log_b_pred_mar_01,log_b_march_01_test$Btarget)
cm_mar02 = confusionMatrix(log_b_pred_mar_02,log_b_march_02_test$Btarget)

print(cm_feb01)
print(cm_feb02)
print(cm_mar01)
print(cm_mar02)

#------------------Logistic Regression for Textual Features----------------------------

logregressor = glm(formula = Ttarget ~.,family = binomial(link='logit'),
                   data = log_textual_df)
summary(logregressor)

logregressor_1 = glm(formula = Ttarget ~ PC2,
                     family = binomial(link ='logit'),
                     data = log_textual_df)
summary(logregressor_1)


log_t_predict_probfeb01 = predict(logregressor_1,type= 'response',newdata = log_t_feb_01_test[-3])
log_t_predict_probfeb02 = predict(logregressor_1,type= 'response',newdata = log_t_feb_02_test[-3])
log_t_predict_probmar01 = predict(logregressor_1,type= 'response',newdata = log_t_march_01_test[-3])
log_t_predict_probmar02 = predict(logregressor_1,type= 'response',newdata = log_t_march_02_test[-3])

print(log_t_predict_probfeb01)

log_t_pred_feb_01 = ifelse(log_t_predict_probfeb01 > 0.5,1,0)
log_t_pred_feb_02 = ifelse(log_t_predict_probfeb02 > 0.5,1,0)
log_t_pred_mar_01 = ifelse(log_t_predict_probmar01 > 0.5,1,0)
log_t_pred_mar_02 = ifelse(log_t_predict_probmar02 > 0.5,1,0)

#install.packages("caret")

library(caret)
cm_feb01 = confusionMatrix(log_t_pred_feb_01,log_t_feb_01_test$Ttarget)
cm_feb02 = confusionMatrix(log_t_pred_feb_02,log_t_feb_02_test$Ttarget)
cm_mar01 = confusionMatrix(log_t_pred_mar_01,log_t_march_01_test$Ttarget)
cm_mar02 = confusionMatrix(log_t_pred_mar_02,log_t_march_02_test$Ttarget)

print(cm_feb01)
print(cm_feb02)
print(cm_mar01)
print(cm_mar02)

#--------------------------------END-----------------------------------------------


