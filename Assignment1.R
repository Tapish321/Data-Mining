library(ggplot2)
head(iris)
str(iris)
typeof(iris)

iris2 = iris
iris2$Species = as.character(iris$Species)
iris2$Species[iris2$Species == "setosa"]=1
iris2$Species[iris2$Species == "versicolor"] = 2
iris2$Species[iris2$Species == "virginica"] = 3
#Scatter Plots
ggplot(iris, aes(x=Petal.Length, y = Sepal.Length,ylab = "Sepal Length in cm",xlab="Petal Length in cm", colour = Species))+
  geom_point()+ggtitle("Iris-Length Scatter")
ggplot(iris, aes(x=Petal.Width, y = Sepal.Width,ylab = "Sepal Width in cm",xlab="Petal Width in cm", colour = Species))+
  geom_point()+ggtitle("Iris-Width Scatter")

#Visualisation of Feature Matrix
ir_matrix <-as.matrix(iris[,1:4])

filled.contour(z =ir_matrix,plot.title =main("Iris Feature Visualization"),color = terrain.colors)


#Corealtion Plots

irisCor_Plot <- pairs(~Sepal.Length + Petal.Length + Sepal.Width + Petal.Width,lower.panel = NULL,upper.panel=upper.panel, data = iris)
irisCor_Plot



#3D Scater Plots (2-2)
#install.packages(c("car","rgl","scatterplot3d"))
library(c("car","rgl","scatterplot3d"))
scatterplot3d::scatterplot3d(x = iris[,1], y = iris$Petal.Length, z = iris$sepal.Length, grid=TRUE,color = colors,pch = 20,angle = 40, main = "3D Scatter plot Lengths")+grid(nx=,ny =nx)

colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]

scatterplot3d::scatterplot3d(iris[,c(1,2,4)],main= "3D Scatter Plot",xlab = "Sepal Length in cm",ylab="Petal Length in cm",zlab = "Petal
                             Width in cm",pch=16,color=colors,angle = 60)




#Histograms
Hist_SL <- ggplot(iris,aes(x= Sepal.Length))+geom_histogram(binwidth = 0.2,color = 'black',aes(fill=Species))+ xlab("Sepal Length in cm")+
  ylab("Frequency")+ ggtitle("Histogram of Sepal Length")+ geom_vline(data=iris, aes(xintercept = mean(Sepal.Length)),linetype="dashed",color="black")

Hist_PL <- ggplot(iris,aes(x= Petal.Length))+geom_histogram(binwidth = 0.2,color = 'black',aes(fill=Species))+ xlab("Petal Length in cm")+
  ylab("Frequency")+ ggtitle("Histogram of Petal Length")+ geom_vline(data=iris, aes(xintercept = mean(Petal.Length)),linetype="dashed",color="black")

Hist_SW <- ggplot(iris,aes(x= Sepal.Width))+geom_histogram(binwidth = 0.2,color = 'black',aes(fill=Species))+ xlab("Sepal Width in cm")+
  ylab("Frequency")+ ggtitle("Histogram of Sepal Width")+ geom_vline(data=iris, aes(xintercept = mean(Sepal.Width)),linetype="dashed",color="black")

Hist_PW <- ggplot(iris,aes(x= Petal.Width))+geom_histogram(binwidth = 0.2,color = 'black',aes(fill=Species))+ xlab("Petal Width in cm")+
  ylab("Frequency")+ ggtitle("Histogram of Petal Width")+ geom_vline(data=iris, aes(xintercept = mean(Petal.Width)),linetype="dashed",color="black")
Hist_PL
Hist_SL
Hist_PW
Hist_SW
#Box Plots
#boxplot(Sepal.Length~Species,color=c("blue","green","red"), data = iris)
BP_SL <- ggplot(iris,aes(Species,Sepal.Length,fill=Species))+geom_boxplot()+ scale_y_continuous("Sepal Length in cm", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")
BP_PL <- ggplot(iris,aes(Species,Petal.Length,fill=Species))+geom_boxplot()+ scale_y_continuous("Petal Length in cm", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")
BP_SW <- ggplot(iris,aes(Species,Sepal.Width,fill=Species))+geom_boxplot()+ scale_y_continuous("Sepal Width in cm", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")
BP_PW <- ggplot(iris,aes(Species,Petal.Width,fill=Species))+geom_boxplot()+ scale_y_continuous("Petal Width in cm", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")
BP_SL
BP_PL
BP_SW
BP_PW


#Co-relation 
COV <- cov(iris[,1:4])
COR <- cor(iris[,1:4])
as.table()
COR

#Corelation Plots
upper.panel<-function(x, y){
  points(x,y, pch=20, col=c("red", "green", "blue")[iris$Species])
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R-value=", r) 
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.4, 0.9, txt)
}
pairs(iris[,1:4],upper.panel = upper.panel, lower.panel = NULL)

#MASS::parcoord(iris, col=rainbow(length(iris[,1])), var.label=TRUE)
#ir <- rbind(iris[,,1], iris[,,2], iris[,,3])
#MASS::parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50,var.label = TRUE)
#iris == iris3
#iris3
#str(iris3)
#install.packages("GGally")


#Parellel Co-Ordinate Plots
parellel_plot <- ggparcoord(data = iris, columns = 1:4, groupColumn = 5, showPoints = TRUE, title = "Parallel Coordinate Plot", scale = "globalminmax")
parellel_plot2 <- ggparcoord(data = iris, columns = 1:4, groupColumn = 5, showPoints = TRUE, title = "Parallel Coordinate Plot")
parellel_plot
parellel_plot2




#Feature Matrix and corelaton plot (2-6)



#install.packages('corrplot')
library(corrplot)

library(reshape2)
library(gplots)
corrplot(COR,method = "color", title = "Vusualization of Correlation plot",addgrid.col = "gray",tl.col = "black")
iris_cormat <- melt(COR)
head(iris_cormat)

ggplot(data=iris_cormat,aes(Var1,Var2,fill=value,main="Feature Matrix",color= group_by(iris$Species))) + geom_tile(color = "gray")+
  scale_fill_gradient2(low="darkblue", high="darkgreen", guide="colorbar") 
#heatmap.2(t(iris[, 1:4]), trace="none", scale="row", key=TRUE, mar=c(2, 8), cexRow=1, ColSideColors=c("grey", "black", "yellow")[iris$Species])
#ggplot(data = iris, aes(x=iris[,1:4], y=iris$Species, fill=value)) +  geom_tile()


# to sacle the data set.
iris_norm <- scale(iris[,-5])
S = matrix(c(5.0000, 3.5000, 1.4600, 0.2540),nrow = 1,ncol = 4)
s2 <- cov(iris[,1:4],S)
S
library("cmna")
#Fuction for Minkowski Distance
set.seed(5)
Minkowski_Dist <- function(x,y,r){
  if (length(x)==length(y)){
    ans = 0
  
    for (n in 1:length(x)){
      ans = ans +(abs((x[n]-y[n]))^r)
    }
    final = ans^(1/r)
  }
  return (final)
}

Minkowski_Dist(iris[,1:4],S,1)
Minkowski_Dist(iris[,1:4],S,2)
Minkowski_Dist(iris[,1:4],S,100)

# T-statistics distance 
set.seed(5)
t_stat <- function(x,y){
  if (length(x) == length(y)){
    ans = 0
    num = abs(mean(as.matrix(x))-mean(as.matrix(y)))
    den = sd(x-y)
  }
  ans = (num/den)
  return(ans)
}


t_stat(ts_Data[,1],ts_Data[,2])


#function for Mahalanobis Distance
mahalanobis_Dist <- function(x,y,m){
  
  if (length(x)== length(y)){
    ans = 0
    for (n in 1:length(x)){
      ans = ans + (x[n]-y[n])*m^-1*(t(x[n]-y[n]))
    }
  return(ans)
  }

}


mahalanobis2_Dist <- function(x,y,m){
  post = as.matrix(as.matrix(x))
  present = as.double(as.matrix(y))
  m = as.matrix(m)
  p = 0
  
  for (n in nrow(post)){
    print (n)
    d[t] = (t(post[n]-present)) %*% (m^-1) %*% (post[n]-present)
    
    p = p+1
  }
  
  return (d)
  
}


mahalabonis3_dist=function(a,b,m){
  a1=as.matrix(as.matrix(a))
  print ("whats up")
  b1=as.double(as.matrix(b))
  m=as.matrix(m)
  j=0
  kk = vector()
  for (i in nrow(a1)){
    print ("555555555555")
    d[j]=(t(i-b1)) %*% (m^-1) %*% (i-b1)
    append(kk,j)
    j=j+1
  }
  print(a1)
  print(b1)
  return (kk)
}

typeof(as.matrix(as.matrix(features)))
ggg <- read.table("iris")
features <- iris[,1:4]
cov_var <- cov(features)
mahalabonis_dist(features,S,cov_var)
mahalabonis3_dist(iris[,1:4],S,cov(iris[,1:4]))

mahaDist <- mahalanobis(iris[,1:4],S,cov(iris[1:4]))
install.packages('microbenchmark')
library(microbenchmark)
benchmark(distance())
#Minkowski_Dist(c(2,2),c(3,5),2)
Mahalanobis_Dist(c(2,2),c(3,5),cov(c(2,2)))
# Assuming a new iris sample S has a feature vector as following
S

#test <- Minkowski_Dist(gr,gt,3)
#test

# Minkowski Distances for r = 1, 2, 100, respectively
Mdist_1<- Minkowski_Dist(iris[,1:4],S,1)
Mdist_2<- Minkowski_Dist(iris[,1:4],S,2)
Mdist_3<- Minkowski_Dist(iris[,1:4],S,100)
#Distances after Normalizing Iris data set
Minkowski_Dist(iris_norm,S,1)
Minkowski_Dist(iris_norm,S,2)
Minkowski_Dist(iris_norm,S,100)
#plot(c(r1,r2,r3),pch=20,main = "Distances plot", col = rainbow(3))
plot(Mdist_1,  type = "l")
plot(Mdist_1,pch=20,main = "Distances plot", col = rainbow(3),xlab = "observations")+grid(col = "lightgray", lty = "dotted") + lines(Mdist_1,pch = 16)

#lines(x[order(x)], r1[order(x)],  pch=16)
plot(Mdist_2,  type = "l")
plot(Mdist_2,pch=20,main = "Distances plot", col = rainbow(3), xlab = "observations")+grid(col = "lightgray", lty = "dotted",
                                                               lwd = par("lwd")) + lines(Mdist_2,pch = 16)

plot(Mdist_3,  type = "l")
plot(Mdist_3,pch=20,main = "Distances plot", col = rainbow(3),xlab = "observations")+grid(col = "lightgray", lty = "dotted",
                                                               lwd = par("lwd")) + lines(Mdist_3,pch = 16)



plot(mahaDist,  type = "l")
plot(mahaDist,pch=20,main = "Distances plot", col = rainbow(3),xlab = "observations")+grid(col = "lightgray", lty = "dotted",
                                                                                          lwd = par("lwd")) + lines(mahaDist,pch = 16)




#bb <- matrix(c(0.3,0.2),2,2)
library("MASS")
#mahalanobis(iris[,1:4],S,COV) 
#mahalanobis_Dist(iris[,1:4],S,COV)
#Sigma <- matrix(c(1,0.3,0.,1),2,2)
#X <- mvrnorm(n=100,rep(0,0),Sigma)
#X2 <- pmvnorm(lower= 0.3, upper = 1.0)
#mvrnorm(n = 10, rep(0, 2), Sigma)


#Time-Series Plot
#installed.packages('MASS')

mu = c(0,0)
sigma <- matrix(c(1,0.3,0.3,1),2,2)
ts_Data <- mvrnorm(100,mu,sigma)
head(ts_Data)
ts.plot(ts_Data,gpars= list(col="red", lwd=2,main = "Time-series plot"))
lines(ts_Data[,1],col="#27ccc0",lwd=2)


#T-statistcs for two series

ttest=t.test(ts_Data[,1],ts_Data[,2], var.equal=TRUE, paired=FALSE)
names(ttest)
TStatistics<-ttest$statistic
TStatistics
t_stat(ts_Data[,1],ts_Data[,2])
#tt<-replicate(1000,ttest)
#range(tt)



#correlation of the two time series
cor_r = ccf(ts_Data[,1],ts_Data[,2],main = "Co-relation", col= rainbow(1))
cor_r

#Coefficient of Corelation is strong when lag = 0
#names(corr)
#new_data <- scale(iris[1:4],scale = FALSE)
#head(new_data)
#normalized<-function(value, min,max) {
 # (value - min) / (max - min);
  
#}



#) Normalize the feature matrix of the IRIS dataset 
normalized <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
head(normalized(iris[,1:4]))


