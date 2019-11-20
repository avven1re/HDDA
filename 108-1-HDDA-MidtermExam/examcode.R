#HDDA Mid exam
#410578035-統計四-楊閔文

#1
#(a)
mamm <- read.table("108-1-HDDA-MidtermExam/mammographic_masses.data", sep = ",", na.strings = "?")
names(mamm) <- c("BI-RADS", "Age", "Shape", "Margin", "Density", "Severity")

summary(mamm)

library(mice)
library(VIM)

md.pattern(mamm)

mamm.aggrplot <- aggr(mamm, col = c("green", "red"), nubers = T, prop = T, sortVars = T, labels = names(mamm))
matrixplot(mamm)

#(b)
#Mean Substitution
mean.subst <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

mamm.m <- apply(mamm, 2, mean.subst)
matrixplot(mamm.m)

#kNN
mamm.kNN <- kNN(mamm, k = 5)
matrixplot(mamm.kNN[1 : 6])

#kNN with 自訂平均函數
t_mean <- function(x){
  mean(x, trim = 0.1)
}

mamm.kNNt <- kNN(mamm, k = 5, numFun = t_mean)
matrixplot(mamm.kNNt[1 : 6])

###
#Log
airf.log <- as.data.frame(apply(airf, 2, log))
airflog.lm <- lm(formula = Sspl ~ Frequency + Angle_of_Attack + Chord_length + Free_stream_velocity + Ssdt, data = airf.log)
plot(airflog.lm$residuals)

#SQRT*10
airf.sr <- as.data.frame(apply(airf, 2, function(x) sqrt(x)*10))
airfsr.lm <- lm(formula = Sspl ~ Frequency + Angle_of_Attack + Chord_length + Free_stream_velocity + Ssdt, data = airf.sr)
plot(airfsr.lm$fitted.values)
###

#2
airf <- read.table("108-1-HDDA-MidtermExam/airfoil_self_noise.dat")
names(airf) <- c("Frequency", "Angle_of_Attack", "Chord_length", "Free_stream_velocity", "Ssdt", "Sspl")

summary(airf)

attach(airf)


airf.lm <- lm(formula = Sspl ~ Frequency + Angle_of_Attack + Chord_length + Free_stream_velocity + Ssdt, data = airf)
plot(airf.lm$residuals)

#log
plot(airf$Sspl, log(airf$Sspl))
plot(airf$Ssdt, log(airf$Ssdt))
#SQRT*10
sqr <- function(x){
  sqrt(x) * 10
}

plot(airf$Sspl, sqr(airf$Sspl))
plot(airf$Ssdt, sqr(airf$Ssdt))

#Box-Cox
library(MASS)
airf.lm <- lm(formula = Sspl ~ Frequency + Angle_of_Attack + Chord_length + Free_stream_velocity + Ssdt, data = airf)

airfbc <- boxcox(Sspl ~ Frequency + Angle_of_Attack + Chord_length + Free_stream_velocity + Ssdt, data = airf, lambda = seq(-2, 2, 1/10))

#3
library(RgoogleMaps)

library(ggmap)

library(mapproj)

install.packages("BiocManager") 
BiocManager::install("EBImage")
library(EBImage)

library(plyr) 
library(maps)
library(maptools)
library(mapdata)
library(sf)

nt <- read.table("108-1-HDDA-MidtermExam/po0201a1ac2.csv", sep = ",", skip = 2)



























