#installing packages
install.packages("ggplot2", "plyr", "caret","dplyr")

#loading the library
library("ggplot2")
library("plyr")
library("dplyr")
library("caret")

#reading the dataset
d1=read.table("student-mat.csv",sep=";",header=TRUE)

#checking for nulls
is.na(d1)

str(d1)
dim(d1)
sapply(d1, class)
head(d1)
summary(d1)

#Removing data that isnt used
d1$G2 <- NULL
d1$G1 <- NULL

#Looking deeper into the family size to grades
d1%>%
  group_by(famsize)%>%
  ggplot(aes(x=G3, fill=famsize))+
  geom_density( alpha=0.5)

#Looking deeper into mothers job to grades
d1%>%
  group_by(Mjob)%>%
  ggplot(aes(x=G3, fill=Mjob))+
  geom_density( alpha=0.5)

#Looking deeper into fathers job to grades
d1%>%
  group_by(Fjob)%>%
  ggplot(aes(x=G3, fill=Fjob))+
  geom_density( alpha=0.5)


#Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv", number = 10)
metric <- "RMSE"

#Training models
set.seed(7)
fit.cart <-train(G3~., data=d1, method="rpart", metric=metric, trControl=control)

set.seed(7)
fit.knn <-train(G3~., data=d1, method="knn", metric=metric, trControl=control)

set.seed(7)
fit.svm <-train(G3~., data=d1, method="svmRadial", metric=metric, trControl=control)

set.seed(7)
fit.rf <-train(G3~., data=d1, method="rf", metric=metric, trControl=control)

#summarize accuracy of models
results <- resamples(list( cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)


#creating data frame to compare the predictions to actual results
d2 <- data.frame(d1$G3)
d2$G3 <- d1$G3 

#predicting the grade from the cart model
d2$cart_G3 <- predict(fit.cart, d1)


#predicting the grade from the knn model
d2$knn_G3 <- predict(fit.knn, d1)


#predicting the grade from the svm model
d2$svm_G3 <- predict(fit.svm, d1)


#predicting the grade from the rf model
d2$rf_G3 <- predict(fit.rf, d1)
str(d2)

#looking inside model results to find the most important vari 
ggplot(varImp(fit.cart))

#looking inside model results to find the most important vari 
ggplot(varImp(fit.knn))

#looking inside model results to find the most important vari 
ggplot(varImp(fit.svm))

#looking inside model results to find the most important vari 
ggplot(varImp(fit.rf))

#create j1 to predict own grade for curiosity
j1=read.table("jonny.csv",sep="," ,header=TRUE)
#predicting the grade from the rf model
j1$G1 <- NULL
j1$G2 <- NULL
j1$Pstatus <-as.character(j1$Pstatus)
j1$Pstatus <- as.factor(revalue(j1$Pstatus, c("TRUE" = "T", "FALSE" = "A")))
j1$rf_G3 <- predict(fit.rf, j1)

