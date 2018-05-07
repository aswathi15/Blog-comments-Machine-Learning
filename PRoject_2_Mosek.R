require(caTools)
set.seed(101)

setwd("C:\\Users\\Aswathi\\Dropbox\\Spring 2018\\Machine Learning\\BlogFeedback")
blog_data <- read.csv("blogData_train.csv",header = TRUE)
blog_data <- blog_data[sample(nrow(blog_data),5000),]

split = sample.split(blog_data$X1.0.2, SplitRatio = 0.8)
train <- subset(blog_data, split == TRUE)
test <- subset(blog_data, split == FALSE)

train_x <- train[,51:60]
train_y <- train[,281]

test_x <- test[,51:60]
test_y <- test[,281]

train_scaled_x <- scale(train_x)

test_scaled_x <- scale(test_x)

train_blog <- as.data.frame(cbind(train_scaled_x,train_y))
test_blog <- as.data.frame(cbind(test_scaled_x,test_y))
colnames(train_blog) <- c(51:60,'Target')
colnames(test_blog) <- c(51:60,'Target')
#--------------------------------------------------------------------------------------

fit1 <- lm(formula = Target ~., data = train_blog )
summary(fit1)

y_pred = predict(fit1,newdata = test_blog[-11])
library(Metrics)
mean_error = mse(test_y,y_pred)

#--------------------------------------------------------------------------------------
install.packages('matlib')
library(matlib)
blog<-list()

memory.limit()
memory.limit(size=56000)

#----------------------------------------------------------------------------------


p <- dim(train_scaled_x)[2] # number of parameters of interest

#correspondence between OLS and the Quadratic Program
xx<-crossprod(train_scaled_x) # X'X=Q variable in the Quadratic program
c<--crossprod(train_scaled_x,train_y) # X'y=c variable in the Quadratic program
xx2<-xx
xx2[upper.tri(xx)]<-0 #mosek needs Q to be  triangular
idx <- which(xx2 != 0, arr.ind=TRUE) #index of the nonzero elements of Q

#problem definition in Mosek
blog$sense <- 'min'
blog$c <- as.vector(c)
blog$qobj <- list(i = idx[,1],
                  j = idx[,2],
                  v = xx2[idx] ) #the Q matrix is imputed by the row indexes i, the col indexes j and the values v that define the Q matrix


require(Rmosek)
#sparse matrix
blog$A <- Matrix (rep(1,50000),nrow = 5000,ncol=10)
#lower bound of the target : number of comments
blc<-rep(0,5000)
#upper bound of the target variable : number of comments
buc<-rep(Inf,5000)
blog$bc<-rbind(blc,buc)

#lower bound of all the constraints of columns
blx<-c(min(train_blog[,1]),min(train_blog[,2]),min(train_blog[,3]),min(train_blog[,4]),
       min(train_blog[,5]),min(train_blog[,6]),min(train_blog[,7]),min(train_blog[,8]),
       min(train_blog[,9]),min(train_blog[,10]))
#upper bound of all the constraints of columns
bux<-c(max(train_blog[,1]),max(train_blog[,2]),max(train_blog[,3]),max(train_blog[,4]),
       max(train_blog[,5]),max(train_blog[,6]),max(train_blog[,7]),max(train_blog[,8]),
       max(train_blog[,9]),max(train_blog[,10]))

blog$bx<-rbind(blx,bux)

blog$bx


r<-mosek(blog)
r$sol$itr$xx


w <- c(-0.2526137 ,14.5153876,  8.9540881, -0.3698086, 11.6885269, -0.3189680, -0.1328503,-0.1363324,  0.4594937, -1.0339606)
w=as.matrix(w)
test_scaled_x=as.matrix(test_scaled_x)
yhat1 = test_scaled_x %*% w
#yhat1 = scale(yhat1)
mean_error_1 = mse(test_y,yhat1)


