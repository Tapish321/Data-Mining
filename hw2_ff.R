str(iris)
set.seed(5)
#install.packages("cvTools")
#library(cvTools)
library(e1071)
library(caret)
library(MASS)
#library(ggplot2)
library(caTools)
library(car)
library(psych)
library(cvTools)

#lets split data into 70% for trainig and 30% for testing.
trn <-createDataPartition(iris$Sepal.Length, p = 0.7,list = FALSE) 
training_iris_data <- iris[trn,]
Testing_iris_data <- iris[-trn,]
n <- nrow(iris)
ttt <- sample(1:n,replace = FALSE, size =  round(0.7*n))
trrr_data <- iris[ttt,]
tsss_data <- iris[-ttt,]



pairs.panels(iris[1:4],gap = 0,bg = c("red","green", "blue")[iris$Species],pch = 21)




nrFolds <- 5
folds1 <- rep_len(1:nrFolds, nrow(iris))
folds <- cvFolds(NROW(iris),K = nrFolds)


myData<-iris[sample(nrow(iris)),]

#Create 5 equally size folds
folds <- cut(seq(1,nrow(iris)),breaks=5,labels=FALSE)

#Perform 5 fold cross validation
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- iris[testIndexes, ]
  trainData <- iris[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}




Trained_Dta <- list()
Test_Dta <- list()
Conf_Matrix <- list()
Accuracy_NB <- list()

set.seed(5)
for (k in (1:nrFolds)){
  train.iris <- iris[folds$subsets[folds$which != k],]
  
  
  test.iris <- iris[folds$subsets[folds$which ==k],]
  Trained_Dta[[k]] = train.iris
  Test_Dta[[k]] = test.iris
  NB_model <- train(Species~.,data = train.iris,'nb',na.action = na.pass)
  #NB_model2 <- naiveBayes(Species~.,data = train.iris)
  NB_prediction <-  predict(NB_model,train.iris)
  NB_ConMatrix <- table(predictions = NB_prediction, actual = train.iris$Species)
  Conf_Matrix[[k]] = NB_ConMatrix
  Accuracy_NB[[k]] = NB_model$results$Accuracy
  #print(Conf_Matrix)
  #print(NB_ConMatrix)
  #print (NB_model$results$Accuracy)
  #test_test <- replicate(k,data.frame(), simplify=FALSE)
  #test_test['k'] <- NB_ConMatrix
  
}
(Conf_Matrix[[1]]+Conf_Matrix[[2]]+Conf_Matrix[[3]]+Conf_Matrix[[4]]+Conf_Matrix[[5]])
(Accuracy_NB[[1]]+Accuracy_NB[[2]]+Accuracy_NB[[3]]+Accuracy_NB[[4]]+Accuracy_NB[[5]])/5

set.seed(555)
Naive_Bayes <- function(data,nrFolds){
  Trained_Dta <- list()
  Test_Dta <- list()
  Conf_Matrix <- list()
  Accuracy_NB <- list()
  myData<-data[sample(nrow(data)),]
  folds <- cut(seq(1,nrow(data)),breaks=nrFolds,labels=FALSE)
  #folds1 <- rep_len(1:nrFolds, nrow(iris))
  #folds <- cvFolds(NROW(iris),K = nrFolds)
  for (k in (1:nrFolds)){
    testIndexes <- which(folds==k,arr.ind=TRUE)
    test.iris <- data[testIndexes, ]
    train.iris <- data[-testIndexes, ]
    #print((test.iris))
    
    #train.iris <- data[folds$subsets[folds$which != k],]
    
    #test.iris <- data[folds$subsets[folds$which ==k],]
    Trained_Dta[[k]] = train.iris
    Test_Dta[[k]] = test.iris
    str(Trained_Dta)
    NB_model <- train(Species~.,data = train.iris,'nb',na.action=na.pass)
    #NB_model2 <- naiveBayes(Species~.,data = train.iris)
    NB_prediction <-  predict(NB_model,train.iris)
    NB_ConMatrix <- table(predictions = NB_prediction, actual = train.iris$Species)
    Conf_Matrix[[k]] = NB_ConMatrix
    Accuracy_NB[[k]] = NB_model$results$Accuracy
    #print(Conf_Matrix)
  
    #print(NB_ConMatrix)
    #print (NB_model$results$Accuracy)
    #test_test <- replicate(k,data.frame(), simplify=FALSE)
    #test_test['k'] <- NB_ConMatrix
    
    
  }
  #print(Accuracy_NB[[1]])
  #print(Conf_Matrix[[5]])
  #print(Conf_Matrix[[4]])
  #print(Conf_Matrix[[2]])
  #print(Conf_Matrix[[3]])
  #print(Test_Dta[[2]])
  Mean_ofMatrices = (Conf_Matrix[[1]]+Conf_Matrix[[2]]+Conf_Matrix[[3]]+Conf_Matrix[[4]]+Conf_Matrix[[5]])
  Accuracy_ofNB=(Accuracy_NB[[1]]+Accuracy_NB[[2]]+Accuracy_NB[[3]]+Accuracy_NB[[4]]+Accuracy_NB[[5]])/5
  return (list(Mean_ofMatrices,Accuracy_ofNB))
  
}  

#str(training_iris_data)
Naive_Bayes(iris,5)


op_test <- function(data){
  sss<- length(data[,-2])
  lyyy <- str(data[1,1])
  return(sss)
  return(lyyy)
}
op_test(iris)


'''

for (i in 1:k) {
    
  assign(paste0("Accuracy", i), i + 1)
  #Accuracy("i") = NB_model("k")
  
  
  }
  if (k == 1){
    train1 <- train.iris
    test1 <- test.iris
    Accuracy1 <- NB_model$results$Accuracy
    #print(Accuracy1)
    
  }
  if (k ==2){
    train2 <- train.iris
    test2 <- test.iris
    Accuracy2 <- NB_model$results$Accuracy
    #print(Accuracy2)
  }
  if (k ==3){
    train3 <- train.iris
    test3 <- test.iris
    Accuracy3 <- NB_model$results$Accuracy
    #print(Accuracy3)
  }
  if (k ==4){
    train4 <- train.iris
    test4 <- test.iris
    Accuracy4 <- NB_model$results$Accuracy
  }
  if (k ==5){
    train5 <- train.iris
    test5 <- test.iris
    Accuracy5 <- NB_model$results$Accuracy
  }
  
  '''


str(train.iris)
NB_model$results$Accuracy
NB_model
NB_ConMatrix

#test_model = naiveBayes(Species~.,data = train.iris)
#test2_mod <- train(Species~.,data = train.iris,'nb')
#test2_pred <-  predict(test2_mod,test.iris)
#table(predictions = test2_pred, actual = test.iris$Species)


#test_pred <- predict(test_model,train.iris)
#table(predictions = test_pred, actual = test.iris$Species)
#table(predict(test_model,train.iris[,-5]),train.iris[,-5], dnn = list("predicted", "actual"))
#table(predict(alt_model, test_data[,-5]),test_data[,5],dnn=list('predicted','actual'))


for (i in 1:5) {
  assign(paste0("x", i), 0)
  i+1
  print("x",i) 
  
}

for (i in 1:5){
  
} 

iterations = 10
variables = 2

output <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  output[i,] <- runif(2)
  
}

output


n <- 5
lst <- replicate(n,data.frame(), simplify=FALSE)

lst

x1 = function(x){
  mu = mean(x)
  l1 = list(s1=table(x),std=sd(x))
  return(list(l1,mu))
}

library(Ecdat)
data("iris")
x1(iris$Sepal.Length)
str(iris)
'''

2. Make a program to perform the 3-class classification using the discriminant function that derived based on
the assumption that the observation feature vector x follows multivariate normal distribution. 
The general case the covariance matrix of each class Si = arbitrary. The decision is to decide the class that maximizes
the discriminant function  :  
  
  Use 5-fold cross-validation, report the prediction accuracy and confusion matrix. 

'''


discriminant <- function(data,objec,obj_class){
  as.factor(objec)
  
  as.character(obj_class)
  x <- unique(data[objec])
 
  print(x)
  
  #print(data2)
  dat<-subset(data,objec==(obj_class))
  dat2 <- filter(data, as.character(objec) == as.character(obj_class))
  print(dat)
  #print(iris[data2,])
  #x2 <-as.character(x)

  len = length(x[,1])
  #print(n)
  one = list()
  two = list()
  wi_zero = list()
  w1 = list()
  w2 = list()
  g_x = list()
  D= list()
  jj2 = list()
  trans_x = list()
  kk = list()
  w_one = list()
  jj=list()
  #len_cl <- length(class_list)
  for (i in 1:len){
    #print(i)
    
    #class =i
    one <-data[which(data[objec] == as.character(x[i,])),]
    print(one)
    two[[i]] = as.data.frame(one[,which(names(one)!= objec)])
    
    #print(i)
    #iris[,which(names(iris)!= "Species"))]
    #D[[i]] = two[,which(names(two) != 'objec')]
    #print (D)
    #mu = colMeans(two[[i]][,1:4])
    #print(mu)
    mu = colMeans(two[[i]])
   
    Sigma <- cov(two[[i]])
    pri_prob <- 1/len
    wi_zero <- (-0.5*t(mu)%*%solve(Sigma)%*%mu)-(0.5%*%det(Sigma))+(log(pri_prob))
    #print(wi_zero)
    inv <- solve(Sigma)
    #print (inv)
    w1 <- (inv * (-0.5))
    #print(typeof(w1))
    #w2[[i]] <- solve(Sigma) %*% mu    ############
    w <- solve(Sigma) %*% mu 
    w2 <- as.matrix(w)
    #print(w2)
   # print(t(w2))
    
    #print(t(as.matrix(w2)))
    two_2 <- two[[i]]
    #print(typeof(c(w1)))
    #print (as.matrix(two[[i]][1,1:4]))
    #print(typeof(t(two[[i]][k,1:4])))
    
    #gx <- (t(two[[i]][,1:4]) %*% w1 %*% two[[i]][,1:4]) + (t(w2) %*% two[[i]][,1:4]) + wi_zero
    w_two <- list()
    n = nrow(two[[i]])
    #print (n)
    kk = as.matrix(w1)
   
    
    for (k in (1:n)){
      #print(k)
     # print(typeof(t(w2)))
      #ii = as.matrix(w2)
      
      jj = (two[[i]][k,])
      kkl = as.matrix(jj)
      tt <- t(kkl)
      
      #print(jj)
      #jj2[[k]] = (as.matrix(jj))
      #jj2 <- do.call(rbind,jj[k,])
     
      #output <- matrix(unlist(z), ncol = 10, byrow = TRUE)
      #jj2[k] = jj
      
      #print(typeof(jj2))
      #trans_x[[k]] <- as.matrix(t(jj))
      #print(dim(as.matrix(trans_x[[k]])))
      #llo = (unlist(trans_x))
      #pak_gaya <- (matrix(llo, ncol = 1,nrow = 4, byrow = TRUE))
      #print(w1 * pak_gaya)
    
      #print(length(pak_gaya))
      #print(jj2)
      #print(trans_x)
      #print(Map('*', jj, trans_x))
      #print (dim(ii))
      #print(w1)
      
      #print((jj2) %*% (as.matrix(trans_x)))
      #print (dim(tt))
      #)
      #print (w2)
      trans_w2 <- as.matrix(t(w2))
      #print(typeof(trans_w2))
      #print ((trans_w2))
      #print ((kkl))
      w_one <-  kkl %*% kk  %*% tt
      #
      w_two <-  kkl %*%  w2 
      #w_three <- as.matrix(wi_zero)
      #w_two <- Map('*',t(w2),jj)
      #print(dim(w_one))
      #print((w_one))
      #print((w_two))
      #print(w_three)#############
      #print(dim(w_three))
      
      #print(dim(w_one))
      g_x[[k]] <- w_one+w_two+wi_zero
      
      #print (dim(w_two))
      #print(w_two)
      #g[[k]]= w_two 
      #as.matrix(g)
      #print ((g))
     
      #print(length(g_x))
      #gx[[k]] <-  as.matrix(two[[i]][k,1:4]) %*% (as.matrix(w1[[i]]) %*% t(two[[i]][k,1:4]))  + 
        #(jj %*% ii) + wi_zero
      
    }    
    
    
  
  }

  
  
}


discriminant(iris,'Species','versicolor')






xm<- unique(iris['Species'])
xm
as.character(xm[1,])
head(xm)
hh <- xm[2,]
hh2<-as.character(xm[1,])
hh3<-as.character(xm[2,])
hh4<-as.character(xm[3,])
hh2
for (k in 1:3){
  ll <- list()
  yy <-iris[which(iris['Species'] == hh2),]
  gg <- iris[which(iris['Species'] == hh3),]
  ii <- iris[which(iris['Species'] == hh4),]
  ek<- yy
  do <-gg
  teen <-ii
} 
Wi <- (-0.5*t(colMeans(ek[,-5])%*%solve(cov(ek[,-5]))%*%colMeans(ek[,-5])))


dattta <- iris
shak <-"Species"
#as.call(shak)
as.factor(shak)
ooooo=noquote(shak)
lak <- "setosa"
subset(dattta,Species==(as.character(lak)))
subset(dattta,Species==(as.character(lak)))







