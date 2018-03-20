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

#Column names(Make a function)
colnames(basic_df) <- c(51:60,"target")
colnames(basic_df) <- paste("B",colnames(basic_df),sep="")

colnames(b_feb_01_test) <- c(51:60,"target")
colnames(b_feb_01_test) <- paste("B",colnames(b_feb_01_test),sep="")

colnames(b_feb_02_test) <- c(51:60,"target")
colnames(b_feb_02_test) <- paste("B",colnames(b_feb_02_test),sep="")

colnames(b_march_01_test) <- c(51:60,"target")
colnames(b_march_01_test) <- paste("B",colnames(b_march_01_test),sep="")

colnames(b_march_02_test) <- c(51:60,"target")
colnames(b_march_02_test) <- paste("B",colnames(b_march_02_test),sep="")

colnames(textual_df) <- c(63:262,"target")
colnames(textual_df) <- paste("T",colnames(textual_df),sep="")

colnames(t_feb_01_test) <- c(63:262,"target")
colnames(t_feb_01_test) <- paste("T",colnames(t_feb_01_test),sep="")

colnames(t_feb_02_test) <- c(63:262,"target")
colnames(t_feb_02_test) <- paste("T",colnames(t_feb_02_test),sep="")

colnames(t_march_01_test) <- c(63:262,"target")
colnames(t_march_01_test) <- paste("T",colnames(t_march_01_test),sep="")

colnames(t_march_02_test) <- c(63:262,"target")
colnames(t_march_02_test) <- paste("T",colnames(t_march_02_test),sep="")



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

t_regressor = lm(formula = Ctarget ~ ., data = textual_df)
summary(t_regressor)

# Let us keep the level of significance as 0.001
# We will only keep those features which are significant
# 69,72,77,102,125,137,154,170,184,191,194,195,210,219,228,232,241,251

t_regressor_1 = lm(formula = Ctarget ~ C69+C72+C77+C102+C125+C137+C154+C170+
                     C184+C191+C194+C195+C210+C219+C228+C232+C241+C251
                     ,data=textual_df)
summary(t_regressor_1)

t_regressor_2 = lm(formula = Ttarget ~ T69-T72+T77+T102+T125-T137+T154+T170+
                     T184+T191+T194+T195+T210+T219+T228+T232-T241+T251
                   ,data=textual_df)
summary(t_regressor_2)

t_predict_feb01 = predict(t_regressor_2,newdata = t_feb_01_test)
t_predict_feb02 = predict(t_regressor_2,newdata = t_feb_02_test)
t_predict_mar01 = predict(t_regressor_2,newdata = t_march_01_test)
t_predict_mar02 = predict(t_regressor_2,newdata = t_march_02_test)


t_mse_feb01 = mse(t_feb_01_test$Ttarget,t_predict_feb01)
t_mse_feb02 = mse(t_feb_02_test$Ttarget,t_predict_feb02)
t_mse_mar01 = mse(t_march_01_test$Ttarget,t_predict_mar01)
t_mse_mar02 = mse(t_march_02_test$Ttarget,t_predict_mar02)

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#                     ***LOGISTIC REGRESSION***  
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

blog_data['Outcome'] <- ifelse(blog_data$X1.0.2 > 50,1,0)
feb_01_df['Outcome'] <- ifelse(feb_01_df$X4.0 > 50,1,0)
feb_02_df['Outcome'] <- ifelse(feb_02_df$X1.0.23 > 50,1,0)
march_01_df['Outcome'] <- ifelse(march_01_df$X0.0.239 > 50,1,0)
march_02_df['Outcome'] <- ifelse(march_02_df$X1.0.15 > 50,1,0)

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

#Column names (Make a Function)
colnames(log_basic_df) <- c(51:60,"target")
colnames(log_basic_df) <- paste("B",colnames(log_basic_df),sep="")

colnames(log_b_feb_01_test) <- c(51:60,"target")
colnames(log_b_feb_01_test) <- paste("B",colnames(log_b_feb_01_test),sep="")

colnames(log_b_feb_02_test) <- c(51:60,"target")
colnames(log_b_feb_02_test) <- paste("B",colnames(log_b_feb_02_test),sep="")

colnames(log_b_march_01_test) <- c(51:60,"target")
colnames(log_b_march_01_test) <- paste("B",colnames(log_b_march_01_test),sep="")

colnames(log_b_march_02_test) <- c(51:60,"target")
colnames(log_b_march_02_test) <- paste("B",colnames(log_b_march_02_test),sep="")

colnames(log_textual_df) <- c(63:262,"target")
colnames(log_textual_df) <- paste("T",colnames(log_textual_df),sep="")

colnames(log_t_feb_01_test) <- c(63:262,"target")
colnames(log_t_feb_01_test) <- paste("T",colnames(log_t_feb_01_test),sep="")

colnames(log_t_feb_02_test) <- c(63:262,"target")
colnames(log_t_feb_02_test) <- paste("T",colnames(log_t_feb_02_test),sep="")

colnames(log_t_march_01_test) <- c(63:262,"target")
colnames(log_t_march_01_test) <- paste("T",colnames(log_t_march_01_test),sep="")

colnames(log_t_march_02_test) <- c(63:262,"target")
colnames(log_t_march_02_test) <- paste("T",colnames(log_t_march_02_test),sep="")


#Logistic Regression for Basic Features
logregressor = glm(formula = Btarget ~.,family = binomial(link='logit'),
                   data = log_basic_df)
summary(logregressor)

logregressor_1 = glm(formula = Btarget ~ B51+B52+B54,family = binomial(link ='logit'),
                     data = log_basic_df)
summary(logregressor_1)
#-----------------
log_b_predict_probfeb01 = predict(logregressor_1,type= 'response',newdata = log_b_feb_01_test)
log_b_predict_probfeb02 = predict(logregressor_1,type= 'response',newdata = log_b_feb_02_test)
log_b_predict_probmar01 = predict(logregressor_1,type= 'response',newdata = log_b_march_01_test)
log_b_predict_probmar02 = predict(logregressor_1,type= 'response',newdata = log_b_march_02_test)

log_b_pred_feb_01 = ifelse(log_b_predict_probfeb01 > 0.5,1,0)
log_b_pred_feb_02 = ifelse(log_b_predict_probfeb02 > 0.5,1,0)
log_b_pred_mar_01 = ifelse(log_b_predict_probmar01 > 0.5,1,0)
log_b_pred_mar_02 = ifelse(log_b_predict_probmar02 > 0.5,1,0)

install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(caret)
cm_feb01 = confusionMatrix(log_b_pred_feb_01,log_b_feb_01_test$Btarget)
cm_feb02 = confusionMatrix(log_b_pred_feb_02,log_b_feb_02_test$Btarget)
cm_mar01 = confusionMatrix(log_b_pred_mar_01,log_b_march_01_test$Btarget)
cm_mar02 = confusionMatrix(log_b_pred_mar_02,log_b_march_02_test$Btarget)

precision <- posPredValue(log_b_pred_feb_01,log_b_feb_01_test$Btarget,positive="1")
recall <- sensitivity(log_b_pred_feb_01,log_b_feb_01_test$Btarget,positive = "1")
