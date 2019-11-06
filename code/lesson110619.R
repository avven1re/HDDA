#110619 browseURL("http://www.hmwu.idv.tw/web/R/B01-2-hmwu_R-MissingValuesOutliers.pdf")

#84/85
library(MASS)
cov(stackloss)

cov.mve(stackloss)$cov

par(mfrow=c(2,1))
library(MASS)
ccov <- cov(stackloss)
pca.scores <- as.matrix(stackloss) %*%
eigen(ccov)$vectors[,1:2]
plot(pca.scores, main="PCA", asp=1, type="n")
text(pca.scores, label=1:nrow(stackloss))
rcov <- cov.mve(stackloss)$cov
rpca.scores <- as.matrix(stackloss) %*%
  eigen(rcov)$vectors[,1:2]
plot(rpca.scores, main="Robust PCA", asp=1, type="n")
text(rpca.scores, label=1:nrow(stackloss))

#
par(mfrow=c(2,1))
library(MASS)

# stackloss.orig <- stackloss
# stackloss <- stackloss.orig
stackloss.s <- scale(stackloss)
heatmap(stackloss.s)
summary(stackloss.s)

set.seed(12345)
id <- sample(1:21, 5)
id
# [1] 16 18 15 21  8

stackloss.s[id[1:2], 1] <- stackloss.s[id[1:2], 1] + 
  2 * sqrt(qchisq(0.975, 4)) + 2*rnorm(2)

stackloss.s[id[3:5], 2] <- stackloss.s[id[3:5], 2] + 
  2 * sqrt(qchisq(0.975, 4)) + 2*rnorm(3)

heatmap(stackloss.s)
summary(stackloss.s)

ccov <- cov(stackloss.s)
pca.scores <- as.matrix(stackloss.s) %*% eigen(ccov)$vectors[,1:2]
plot(pca.scores, main="PCA", asp=1, type="n")
text(pca.scores, label=1:nrow(stackloss))
rcov <- cov.mve(stackloss.s)$cov
rpca.scores <- as.matrix(stackloss.s) %*%  
  eigen(rcov)$vectors[,1:2]
plot(rpca.scores, main="Robust PCA", asp=1, type="n")
text(rpca.scores, label=1:nrow(stackloss))

#browseURL("http://www.hmwu.idv.tw/web/R_AI_M/AI-M3-hmwu_R_DataManagement_v2.pdf")

# 10/70
myvector <- c(10, 20, NA, 30, 40)
myvector
mycountry <- c("Austria", "Australia", NA, NA, "Germany", "NA")
mycountry
is.na(myvector)
which(is.na(myvector))
x <- c(1, 4, 7, 10)
x[4] <- NA # sets the 4th element to NA
x
is.na(x) <- 1 # sets the first element to NA
x

set.seed(12345)
mydata <- matrix(round(rnorm(20), 2), ncol=5)
mydata[sample(1:20, 3)] <- NA
mydata 
which(colSums(is.na(mydata))>0)


# 11/70
x <- c(1, 4, NA, 10)
summary(x)
mean(x)
sd(x)
mean(x, na.rm=TRUE)
sd(x, na.rm=TRUE)
x[!is.na(x)]


# 12/70
mydata <- as.data.frame(matrix(sample(1:20, 8), ncol = 2))
mydata[4, 2] <- NA
names(mydata) <- c("y", "x")
mydata
lm(y~x, data = mydata)
lm(y~x, data = mydata, na.action = na.omit)
lm(y~x, data = mydata, na.action = na.fail)


# 13/70
x <- c(1, 0, 10)
x/x
is.nan(x/x)

1/x
is.finite(1/x)
-10/x
is.infinite(-10/x)

exp(-Inf)
0/Inf
Inf - Inf
Inf/Inf


# 16/70
methods(mice)
? mice


# 17/70
head(airquality)
dim(airquality)
mydata[4:10,3] <- rep(NA,7)
mydata[1:5,4] <- NA
summary(mydata)


# 18/70
library(mice)
md.pattern(mydata)

library(VIM)
mydata.aggrplot <- aggr(mydata, col=c('lightblue','red'), numbers=TRUE, prop = TRUE, sortVars=TRUE, labels=names(mydata), cex.axis=.7, gap=3)


# 19/70
matrixplot(mydata)


# 20/70
md.pairs(mydata)


# 21/70
marginplot(mydata[,c("Ozone", "Solar.R")], col = c("blue", "red"))


# 22/70
mdata <- matrix(rnorm(15), nrow=5)
mdata[sample(1:15, 4)] <- NA 
mdata <- as.data.frame(mdata)
mdata
(x1 <- na.omit(mdata))
(x2 <- mdata[complete.cases(mdata),])
mdata[!complete.cases(mdata),]


# 23/70
mdata
cov(mdata)
cov(mdata, use = "all.obs")
cov(mdata, use = "complete.obs")

cov(mdata, use = "na.or.complete")
cov(mdata, use = "pairwise")


# 24/70
mean.subst <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

mdata
mdata.mip <- apply(mdata, 2, mean.subst)
mdata.mip


# 26/70
names(airquality)
airquality.imp.median <- kNN(airquality[1:4], k=5)
head(airquality.imp.median)


# 27/70
matrixplot(airquality[1:4], interactive = F, main="airquality")
matrixplot(airquality.imp.median[1:4], interactive = F, main="imputed by median")

trim_mean <- function(x){
  mean(x, trim = 0.1)
}

airquality.imp.tmean <- kNN(airquality[1:4], k=5, numFun=trim_mean)


# 31/70
par(mfrow=c(1,4))
raw.data <- 0:100
pa.data <- ifelse(raw.data >= 84, 1, 0)
id <- which(pa.data==1)
plot(raw.data[id], pa.data[id], main="present-absent", 
     type="l", lwd=2, col="blue", ylim=c(-1, 2), xlim=c(0, 100))
points(raw.data[-id], pa.data[-id], type="l", lwd=2, col="blue")
log.data <- log(raw.data)
plot(raw.data, log.data, main="log", type="l", lwd=2, col="blue")
sqrt10.data <- sqrt(raw.data)*10
plot(raw.data, sqrt10.data, main="sqrt*10", type="l", lwd=2, col="blue", asp=1)
abline(a=0, b=1)
trun.data <- ifelse(raw.data >= 80, 80, ifelse(raw.data < 20, 20, raw.data))
plot(raw.data, trun.data, main="truncation", type="l", lwd=2, col="blue")


# 32/70
install.packages("R.matlab")
library('R.matlab')
data <- readMat("software.mat")
print(data)
str(data)


# 33/70
plot(data$prepsloc, data$defsloc, xlab="PrepTime(min)/SLOC", ylab="Defects/SLOC", main="Software Data")

plot(log(data$prepsloc), log(data$defsloc), xlab="Log PrepTime/SLOC", 
     ylab="Log Defects/SLOC", main="Software Data")

plot(log(data$prepsloc), log(data$defsloc), xlab="Log PrepTime/SLOC", 
     ylab="Log Defects/SLOC", main="Software Data", asp=1)


# 34/70
logx <- function(x){
  log(x + 1 - min(x)) 
}

x <- runif(80, min = -5, max = 5) # x <- rnorm(80) 
x <- c(x, rnorm(20, mean=20, sd=10))
par(mfrow=c(1, 3))
hist(x, main="x~runif")
plot(x, logx(x), main="x vs logx")
hist(logx(x), main="logx")


# 35/70
x <- seq(0.5, 2, length.out=100)
bc <- function(y, lambda){
  (y^lambda -1)/lambda
} 
lambda <- seq(-2, 3, 0.5)
plot(0, 0, type="n", xlim=c(0.5, 2), ylim=c(-2, 2.5), main="Box-Cox transformation")
for(i in 1:length(lambda)){
  points(x, bc(x, lambda[i]), type="l", col=i)
  points(2, bc(2, lambda[i]), col=i, pch=i)
}
legend(0.7, 2.5, legend=as.character(rev(lambda)), 
       lty=1, pch=length(lambda):1, col=length(lambda):1)


# 36/70
x <- rexp(1000)
bc <- function(y, lambda){
  (y^lambda -1)/lambda
} 
qqnorm(x); qqline(x, col="red")

bc1.x <- bc(x, 0.1)
qqnorm(bc1.x, main="lambda=0.1")
qqline(bc1.x, col="red")
bc3.x <- bc(x, 0.5)
qqnorm(bc3.x, main="lambda=0.5")
qqline(bc3.x, col="red")

bc2.x <- bc(x, 0.268)
qqnorm(bc2.x, main="lambda=0.268") 
qqline(bc2.x, col="red")

hist(x, main="rexp(1000)")
hist(bc2.x, main="lambda=0.268")


# 39/70
x <- rpois(500, lambda=1)
hist(x, main="rpois(500, lambda=1)"); z <- scale(x); hist(z, main="")


# 40/70
head(airquality )
r <- range(airquality[,1:4], na.rm = T)
hist(airquality$Ozone , xlim = r)
hist(airquality$Solar.R, xlim = r)
hist(airquality$Wind, xlim = r)
hist(airquality$Temp, xlim = r)
airquality.std <- as.data.frame(apply(airquality, 2, scale))
r.std <- c(-3, 3)
hist(airquality.std$Ozone, xlim = r.std)
hist(airquality.std$Solar.R, xlim = r.std)
hist(airquality.std$Wind, xlim = r.std)
hist(airquality.std$Temp, xlim = r.std)


# 42/70
cell.raw <- read.table("trad_alpha103.txt", row.names=1, header=T)
head(cell.raw)
cell.xdata <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))    
y.C <-  as.integer(cell.raw[,1])
table(y.C)
no.cluster <- length(unique(y.C))            
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
p <- ncol(cell.raw) -1
ycolors <- cellcycle.color[y.C+1]
my.pch <- c(1:no.cluster)[y.C+1]    
phase <- c("G1", "S", "S/G2", "G2/M", "M/G1")
matplot(t(cell.xdata), pch = 1:p, lty=1, type = "l", ylab="gene expression", 
        col=ycolors, xlab="time", main="Time series", xaxt="n")
time.label <- parse(text=paste("t[",0:p,"]",sep=""))        
axis(1, 1:(p+1), time.label)
legend("bottom", legend=phase, col=cellcycle.color, lty=1, horiz = T, lwd=2)


# 43/70
library(MASS)
data(crabs)

# 44/70
boxplot(crabs$FL~crabs$sp, main="FL", horizontal=T)

# 45/70
pairs(crabs[,4:8], pch=as.integer(crabs$sex)+1, 
      col=c("blue","orange")[as.integer(crabs$sp)])


# 46/70
par(mfrow=c(1,2))
mp <- as.integer(crabs$sex)+1
mc <- c("blue","orange")[as.integer(crabs$sp)]
isometric.size <- apply(crabs[,4:8], 1, mean)
plot(isometric.size,  log(crabs$BD/crabs$RW), pch=mp, col=mc)
plot(isometric.size, log(crabs$CL/crabs$CW), pch=mp, col=mc)


# 50/70
library(caTools)
set.seed(12345) 
id <- sample.split(1:nrow(iris), SplitRatio = 0.90)
iris.train <- subset(iris, id == TRUE)
iris.test <- subset(iris, id == FALSE)

require(caTools)
Y <- iris[,5] # extract labels from the data
msk <- sample.split(Y, SplitRatio=4/5)
msk
table(Y, msk)
iris.train <- iris[msk, ] 
iris.test <- iris[!msk, ]  
dim(iris.train) 
dim(iris.test)

library(caret)
id <- createDataPartition(y=iris$Species, p=0.9, list=FALSE)
iris.train <- iris[id, ]
iris.test <- iris[-id, ]

library(caret)
createFolds(iris$Species, k=3)


# 51/70
library(bootstrap)
x <- rnorm(20)               
theta <- function(x){mean(x)} 
(theta.hat <- theta(x))
results <- jackknife(x,theta)      
results

theta.hat.loo <- mean(results$jack.values)
(theta.hat.jack <- n * theta.hat - (n-1) * theta.hat.loo)
plot(results$jack.values, main="jackknife")


# 54/70
library(bootstrap)
set.seed(12345)
x <- rnorm(20)                
mean(x)
(x.bootstrap.mean <- bootstrap(x, 50, theta=mean))
mean(x.bootstrap.mean$thetastar)


# 57/70
library(rpart); library(mlbench); library(adabag)
data(Vehicle)
dim(Vehicle)
head(Vehicle)
table(Vehicle$Class)

n <- nrow(Vehicle)
sub <- sample(1:n, 2*n/3)
Vehicle.train <- Vehicle[sub, ]
Vehicle.test <- Vehicle[-sub, ]

mfinal <- 10 
maxdepth <- 5
Vehicle.rpart <- rpart(Class ~ ., data = Vehicle.train, maxdepth = maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart, newdata = Vehicle.test, type = "class")
(tb <- table(Vehicle.rpart.pred, Observed.Class=Vehicle.test$Class))
(error.rpart <- 1 - (sum(diag(tb)) / sum(tb)))


# 58/70
library(adabag)
Vehicle.adaboost <- boosting(Class ~., data = Vehicle.train, mfinal = mfinal, 
                             control = rpart.control(maxdepth=maxdepth))
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost, newdata = Vehicle.test)
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error
importanceplot(Vehicle.adaboost)
evol.train <- errorevol(Vehicle.adaboost, newdata = Vehicle.train)
evol.test <- errorevol(Vehicle.adaboost, newdata = Vehicle.test)
plot.errorevol(evol.test, evol.train)

sort(Vehicle.adaboost$importance, dec=T)[1:5]


# 59/70
Vehicle.boost.cv <- boosting.cv(Class ~., data = Vehicle, v = 10, mfinal = 5, 
                                control = rpart.control(maxdepth = maxdepth))
Vehicle.boost.cv$confusion
Vehicle.boost.cv$error


Vehicle.bag.cv <- bagging.cv(Class ~., data = Vehicle, v = 10, mfinal = 5, 
                             control = rpart.control(maxdepth = maxdepth))
Vehicle.bag.cv$confusion


# 65/70
library(unbalanced)
p <- ncol(ubIonosphere)
y <- ubIonosphere$Class
x <- ubIonosphere[ ,-p]
data <- ubBalance(X=x, Y=y, type="ubOver", k=0)
overData <- data.frame(data$X, Class=data$Y)
table(overData$Class)
data <- ubBalance(X=x, Y=y, type="ubUnder", perc=50, method="percPos")
underData <- data.frame(data$X, Class=data$Y)
table(underData$Class)
bdata <- ubBalance(X=x, Y=y, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
str(bdata)
table(bdata$Y)

data(ubIonosphere)
dim(ubIonosphere)
head(ubIonosphere)
table(ubIonosphere$Class)
     
     
# 66/70
set.seed(12345)
n <- nrow(ubIonosphere) # 351  
no.train <- floor(0.5*n) # 175, keep half for training and half for testing
id <- sample(1:n, no.train)
x.train  <- x[id, ]  # 175 x 32
y.train <- y[id]
x.test <- x[-id, ] # 176  32
y.test <- y[-id]
   
library(e1071)
model1 <- svm(x.train, y.train) 
y.pred1 <- predict(model1, x.test)
table(y.pred1, y.test)
balancedData <- ubBalance(X=x.train, Y=y.train, type="ubSMOTE", 
                       percOver=200, percUnder=150)
table(balancedData$Y)
   
model2 <- svm(balancedData$X, balancedData$Y)
y.pred2 <- predict(model2, x.test)
table(y.pred2, y.test)
   
     
# 67/70
set.seed(1234)
load("creditcard.Rdata")
str(creditcard)
table(creditcard$Class)
     
ubConf <- list(percOver=200, percUnder=200, k=2, perc=50, method="percPos", w=NULL)
results <- ubRacing(Class ~., creditcard, "randomForest", 
                   positive=1, metric="auc", ubConf=ubConf, ntree=5)
     
     
     
# 69/70
results
     
results <- ubRacing(Class ~., creditcard, "randomForest", positive=1, metric="auc", ubConf=ubConf, ncore=4)
library(e1071)
results <- ubRacing(Class ~., creditcard, "svm", positive=1, ubConf=ubConf)
library(rpart)
results <- ubRacing(Class ~., creditcard, "rpart", positive=1, ubConf=ubConf)
     
     
     
     
     
# 75/70
library(TTR)
data(ttrc)
dim(ttrc)
head(ttrc)

t <- 1:100
sma.20 <- SMA(ttrc[t, "Close"], 20)
ema.20 <- EMA(ttrc[t, "Close"], 20)
wma.20 <- WMA(ttrc[t, "Close"], 20)
   
plot(ttrc[t,"Close"], type="l", main="ttrc")
lines(sma.20, col="red", lwd=2)
lines(ema.20, col="blue", lwd=2)
lines(wma.20, col="green", lwd=2)
legend("topright", legend=c("sma.20", "ema.20", "wma.20"), col=c("red", "blue", "green"), lty=1, lwd=2)
     
     
# 77/70
data(cars)
dim(cars)
head(cars)
par(mfrow=c(1, 3))
for(i in c(0.1, 0.3, 0.5)){
 plot(cars$dist ~ cars$speed, main=paste0("lowess (f=", i,")"))
lines(lowess(cars$dist ~ cars$speed, f = i), col="red", lwd=2)
   }
     
     
# 81/70
plot(density(iris$Sepal.Length))
hist(iris$Sepal.Length, prob=T)
lines(density(iris$Sepal.Length), col="red")
     
# 83/70
library(jpeg)
ruddyduck.img <- readJPEG("ruddyduck.jpg")
plot(0, xlim=c(0, 14), ylim=c(-6, 4), type='n', xlab="", ylab="", main="Spline approximate to the top profile of the ruddy duck")
rasterImage(ruddyduck.img, 0.6, -6, 13.8, 3.3)
abline(v=1:14, h=-6:4, col="grey")
     
     
# 84/70
ruddyduck.dat <- read.table("ruddyduck.txt", header=T, sep="\t")
head(ruddyduck.dat)
points(ruddyduck.dat, col="blue", pch=16)
     
duck.spl <- smooth.spline(ruddyduck.dat$fx ~ ruddyduck.dat$x)
lines(duck.spl, col = "red", lwd=2)
     