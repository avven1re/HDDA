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
plot(airfsr.lm$residuals)
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
airfsq.lm <- lm(formula = Sspl ~ Frequency + Angle_of_Attack + Chord_length + Free_stream_velocity + Ssdt, data = sqr(airf))
plot(airfsq.lm$residuals)
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
library(jsonlite)

nt <- read.table("108-1-HDDA-MidtermExam/po0201a1ac2.csv", sep = ",", skip = 2)

ggmap()


#4
#地址轉經緯度
geoPoint <- function(address, key, verbose=FALSE) {
  #若未輸入地址, return錯誤
  if(verbose) cat(address,"\n")
  root = "https://maps.googleapis.com/maps/api/place/findplacefromtext/"
  #Google編碼為UTF8, 中文要轉碼後帶入URL才會正確
  address = iconv(address,"big5","utf8")
  #POI API型態(XML or JSON)
  return.call = "json"
  sensor = "false"
  #產生URL
  url_gen = paste(root, return.call, "?input=", address, "&inputtype=textquery&fields=geometry&key=", key, sep = "")
  #擷取網頁原始碼
  html_code = fromJSON(url_gen)
  #若status為OK抓取資料, 若不為OK return status
  if(html_code$status=="OK") {
    return(cbind(html_code$candidates$geometry$location, address))
  } else {
    return(paste("Status:", html_code$status, sep = " "))
  }
}

covst <- read.table("108-1-HDDA-MidtermExam/SanShia7-11.csv", sep = ",", header = T)
llpos <- matrix(NaN, length(covst$門市地址), 2)
k <- 1
for (i in 1 : length(covst$門市地址)) {
  templl <- geoPoint(covst$門市地址[i], "AIzaSyD7ExFH62Ni2cYE5Vk") #放入GoogleAPI金鑰
  
  if(sum(templl[1] == "Status: ZERO_RESULTS") == 1){
    llpos[i, 1] <- NA
    llpos[i, 2] <- NA
    cat("遺漏", k, "個", "\n")
    k <- k + 1
    next()
  }
  else{
    llpos[i, 1] <- templl[1, 1]
    llpos[i, 2] <- templl[1, 2]
    cat("完成,", i, "個", "\n")}
}

covst$lat <- llpos[, 1]
covst$lng <- llpos[, 2]

tw.newtaipei.zh <- get_map(location = c(lon = 121.375, lat = 24.943403), zoom = 13, language = "zh-TW" , maptype = "roadmap")

ggmap(tw.newtaipei.zh) + geom_point(data = covst, aes(x = covst$lng, covst$lat, color = "red"), size = 1)




















