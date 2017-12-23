

 
 '''
1. Make a program to perform the 3-class classification using Naïve Bayes using 5-fold cross validation,and 
report the prediction accuracy and confusion matrix. 
 '''
str(iris)
summary(iris)
table(names(iris))
set.seed(50)  # setting a seed of random number
pairs(iris[,-5])
#scatterplotMatrix(iris[,-5])
#featurePlot(iris[,-5], plot = "ellipse")
pairs.panels(iris[1:4],gap = 0,bg = c("red","green", "blue")[iris$Species],pch = 21)

#To make a naiive bayes classification we will first assign iris features as x and species which is class as Y.
x <- iris[,-5]
y <- iris[,5]
head(x)
head(y)

# To run model we need to install a package e1071,caret, MASS and load
#install.packages(c("e1071"),("caret"),("MASS"))
library(e1071)
library(caret)
library(MASS)
#library(ggplot2)
library(caTools)
library(car)
library(psych)
install.packages("psych")

 #For 5-fold cross validation we need to create training and testing data sets.We will split the data 70-30 i.e 70% for training and 30% for testing. 
#is.na(x)
trn_data <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
dim(trn_data)
#head(trn_data)
train_data <- iris[trn_data,]
test_data <- iris[-trn_data,]
#dim(train_data)
#dim(test_data)

#checking the prior probability distributions of species in training and testing datasets
prop.table(table(train_data$Species))
prop.table(table(test_data$Species))

#target variable is species and predictictors are rest of the data


NBmodel <- train(Species~.,data = train_data,'nb',trControl=trainControl(method='cv',number=5))
NBmodel
prediction <- predict(NBmodel,test_data)

#prediction
NB_pred <- table(predictions = prediction, actual = test_data$Species)
NB_pred


tc <- tune.control(cross = 5)
alt_model <- naiveBayes(Species~.,data = train_data,tune.control(cross = 5))
alt_pred <- predict(alt_model,test_data)
table(predict(alt_model,test_data[,-5]),test_data[,5],dnn=list('predicted','actual'))
alt_pred


#Sepal Length
alt_model$tables$Sepal.Length

plot(function(x)dnorm(x,5.005, 0.3802496),0,8,col = 'red', main = "Sepal length distribution")
curve(dnorm(x, 5.930, 0.5445299), add=TRUE, col= 'blue')
curve(dnorm(x, 6.660, 0.6121840), add=TRUE, col='green')

#Petal Length
pl <-alt_model$tables$Petal.Length
#pl[1,2]
plot(function(x)dnorm(x,pl[1,1], pl[1,2]),0,8,col = 'red', main = "Petal length distribution", ylab = "", xlab= "x petal Length")
curve(dnorm(x, pl[2,1], pl[2,2]), add=TRUE, col= 'blue')
curve(dnorm(x, pl[3,1], pl[3,2]), add=TRUE, col='green')



"""
2. Make a program to perform the 3-class classification using the discriminant function that derived based on the 
assumption that the observation feature vector x follows multivariate normal distribution. The general case the covariance matrix of each 
class Si = arbitrary. The decision is to decide the class that maximizes the discriminant function  :  
 
Use 5-fold cross-validation, report the prediction accuracy and confusion matrix. 

"""
lda_1 <- lda(Species~.,data = iris)
lda_1


cv.k =5
lin_discrim <- lda(Species~.,data = train_data,estimator="cv",est.para=control.errorest(k=cv.k),prior = rep(1, 3)/3)
#lin_discrim
p_l <- predict(lin_discrim,train_data)$class
p_l2 <- predict(lin_discrim,train_data)
train_lda_matrix<- table(predicted=p_l,Actual= train_data$Species)
KJJ <-lin_discrim$scaling
attributes(lin_discrim)
#Histograms

ldahist(p_l2$x[,2], g = train_data$Species)      #Histogram for LD1
ldahist(p_l2$x[,2],g = train_data$Species)      #Histogram for LD2

#Bi-Plot
library(devtools)
install_github("fawda123/ggord")
library(ggord)
ggord(lin_discrim,train_data$Species)

ggord(lda(Species ~ ., train_data, prior = rep(1, 3)/3), train_data$Species,ylim = c(-10,10))

#partition plot
library(klaR)
partimat(Species~.,data = train_data, method = 'lda')
partimat(Species~.,data = train_data, method = 'qda') #for quadratic discriminant analysis

#lin_discrim2 <- lda(Species~.,data = train_data,CV = TRUE)

#LD1 = percentage saperation achieved by the first model

lda_pred <-predict(lin_discrim,test_data)$class
lda_confMatrix <- table(predicted=lda_pred,Actual= test_data$Species)
lda_confMatrix
lda_Accuracy <- sum(diag(lda_confMatrix))/sum(lda_confMatrix)
lda_Accuracy


attributes(lin_discrim)
lin_discrim$counts
lin_discrim$scaling

vlda = function(v,formula,data,cl){
  require(MASS)
  grps = cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
  pred = lapply(1:v,function(i,formula,data){
    omit = which(grps == i)
    z = lda(formula,data=data[-omit,])
    predict(z,data[omit,])
  },formula,data)
  
  wh = unlist(lapply(pred,function(pp)pp$class))
  table(wh,cl[order(grps)])
}
vlda(5,Species~.,train_data,train_data$Species)
vlda(5,Species~.,test_data,test_data$Species) 
#predict(ggj,test_data)
ggj

"""
Binary classification and ROC analysis: select two types of Iris flower versicolor and virginica for binary classification; 
you can denote versicolor samples by 1 and virginica samples by -1. The binary classification is based on the discriminant functions 
that developed for the general case: the covariance matrix of each class Si = arbitrary.  

1)	Combine the two discriminant functions into one decision function: g(x) = g1(x) - g2(x). Write the explicit form of
the new discriminant function g(x), and the corresponding decision rule. 
2)	Make a program to perform binary classification using the discriminant function   derived in 1).  For each class, take the first 40 samples for 
training and the last 10 samples for testing. Report sensitivity and specificity of your
classification. (Define: sensitivity = prediction accuracy for the class labeled as 1, specificity = prediction accuracy for the class labeled as -1.)

3)	ROC analysis is an important criterion to evaluate the overall performance of a prediction model. Shift the decision boundary to generate ROC curve 
as demonstrated in the following two plots. In this problem, shift the decision boundary from -75 to 75 with a step size of 1.  For each step, calculate
the sensitivity and specificity for the testing dataset. Plot the ROC curve and calculate the AUC value for the following two cases, and discuss the
results briefly.  
.	Training and testing only using the features 1, 2 
.	Training and testing using all the four features 1, 2, 3, 4.  
"""























