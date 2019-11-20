#HDDA_exam1 
#unicode : UTF-8
#410578035-統計四-楊閔文

#1 input and summary
realestate <- read.table("D:\\Rcode\\HDDA_exam1\\realestatedata.csv", header = T, sep = ",")
realestate2 <- realestate

summary(realestate)

str(realestate)

#1.1 rps01交易標的轉換成英文類別
qdf <- data.frame(or = realestate$rps01, id = 1 : 4874)
ref <- data.frame(or = levels(realestate$rps01), ans = c("L", "P", "LB", "LBP", "B"))
m <- merge(qdf, ref, by = "or")
m[order(m$id), ]
realestate$rps01 <- m[order(m$id), 3]

#1.2 rps04都市土地使用分區轉換成英文類別
qdf2 <- data.frame(or2 = realestate$rps04, id2 = 1 : 4874)
ref2 <- data.frame(or2 = levels(realestate$rps04), ans2 = c(NA, "I", "R", "O", "B", "A"))
m2 <- merge(qdf2, ref2, by = "or2")
realestate$rps04 <- m2[order(m2$id2), 3]

#1.3 rps05非都市土地使用分區轉換成英文類別
levels(realestate$rps14)

qdf3 <- data.frame(or3 = realestate$rps05, id3 = 1 : 4874)
ref3 <- data.frame(or3 = levels(realestate$rps05), ans3 = c(NA, "N_A", "H", "I", "R", "S", "S_A", "F", "C"))
m3 <- merge(qdf3, ref3, by = "or3")
realestate$rps05 <- m3[order(m3$id3), 3]

#1.4 rps07 and rps14 轉換成日期格式yyyy-mm-dd
#install.packages("lubridate")
library(lubridate)
nchar(realestate$rps07[1])

datechange <- function(date){
  datelist <- NaN
  for (i in 1 : length(date)) {
    if(is.na(date[i]) == T){
      datelist[i] <- NA
      next()
    }
    
    if(nchar(date[i]) == 7){
      year <- substr(date[i], 1, 3)
      month <- substr(date[i], 4, 5)
      day <- substr(date[i], 6, 7)
    }

    else{      
    year <- substr(date[i], 1, 2)
    month <- substr(date[i], 3, 4)
    day <- substr(date[i], 5, 6)}
    
  year2 <- as.character(as.numeric(year) + 1911)
  t_date <- paste(year2, month, day, sep = "-")
  datelist[i] <- t_date
  }
datelist
}
realestate$rps07 <- datechange(realestate$rps07)
realestate$rps14 <- datechange(realestate$rps14)
realestate[1:5, c(8, 15)]

#1.5 rps08 項目拓展
sum(is.na(realestate$rps08))
x <- realestate$rps08
exprps08 <- function(x){
  Land <- NaN
  Build <- NaN
  Park <- NaN
  for (i in 1 : length(x)) {
    tempa <- strsplit(as.character(x[i]), "土地")
    tempb <- strsplit(tempa[[1]][2], "建物")
    Land[i] <- as.numeric(tempb[[1]][1])
    tempc <- strsplit(tempb[[1]][2], "車位")
    Build[i] <- as.numeric(tempc[[1]][1])
    Park[i] <- as.numeric(tempc[[1]][2])
  }
return(list(Land, Build, Park))
}
explist <- exprps08(realestate$rps08)

n_realestate <- cbind(realestate, rps08_L = explist[[1]], rps08_B = explist[[2]], rps08_P = explist[[3]])

#1.6 rps10 數值轉換
qdf4 <- data.frame(or4 = realestate$rps10, id4 = 1 : 4874)
ref4 <- data.frame(or4 = levels(realestate$rps10),
                   ans4 = as.character(c(NA, 1, 7, 9, 21, 27, 29, 22, 28, 23, 25, 26, 24, 20,
                  2, 8, 11, 17, 19, 12, 18, 13, 15, 16, 14, 10, 
                  31, 37, 32, 33, 35, 36, 34, 30, 3, 5, 6, 41, 43, 40, 4)))
m4 <- merge(qdf4, ref4, by = "or4")
n_realestate$rps10 <- m4[order(m4$id4), 3]

attach(n_realestate)
#2
#2.1 索引圖
par(mfrow = c(2, 3))
plot(rps03, main = "土地移轉總面積平方公尺索引圖")
plot(rps15, main = "建物移轉總面積平方公尺索引圖")
plot(rps21, main = "總價元索引圖")
plot(rps22, main = "單價每平方公尺索引圖")
plot(rps24, main = "車位移轉總面積平方公尺索引圖")
plot(rps25, main = "車位總價元索引圖")

#直方圖
par(mfrow = c(1, 2))
hist(rps22, main = "單價每平方公尺直方圖")
hist(rps25, main = "車位總價元直方圖")

#盒形圖
boxplot(rps22, main = "單價每平方公尺盒形圖")
boxplot(rps25, main = "車位總價元盒形圖")



#長條圖
par(mfrow = c(1, 1))
barplot(sort(table(rps04), decreasing = T), legend.text = names(sort(table(realestate2$rps04), decreasing = T))[-4], col = rainbow(5), main = "都市土地使用分區長條圖")
barplot(sort(table(rps05), decreasing = T), legend.text = names(sort(table(realestate2$rps05), decreasing = T))[2 : 9], col = rainbow(8), main = "非都市土地使用分區長條圖")
barplot(sort(table(rps06), decreasing = T), legend.text = names(sort(table(realestate2$rps06), decreasing = T))[-1], col = rainbow(11), axisnames = F, ylim = c(0, 1500), main = "非都市土地使用編定長條圖")

barplot(sort(table(rps11), decreasing = T), legend.text = names(sort(table(realestate2$rps11), decreasing = T)), col = rainbow(10), axisnames = F, main = "建物型態長條圖")
barplot(sort(table(rps13), decreasing = T), legend.text = names(sort(table(realestate2$rps13), decreasing = T))[-2], col = rainbow(7), axisnames = F, ylim = c(0, 1500), main = "主要建材長條圖")
barplot(table(rps19), main = "有無隔間長條圖", col = terrain.colors(5))
barplot(table(rps20), main = "有無管理組織長條圖", col = terrain.colors(5))
barplot(sort(table(rps23), decreasing = T), legend.text = names(sort(table(realestate2$rps23), decreasing = T))[-1], col = rainbow(7), axisnames = F, main = "車位類別長條圖")

#餅圖
par(mfrow = c(2, 2))
pie(sort(table(rps04), decreasing = T), labels = names(sort(table(realestate2$rps04), decreasing = T))[-4], col = rainbow(5), main = "都市土地使用分區圓餅圖")
pie(sort(table(rps05), decreasing = T), labels = names(sort(table(realestate2$rps05), decreasing = T))[2 : 9], col = rainbow(8), main = "非都市土地使用分區圓餅圖")
pie(sort(table(rps06), decreasing = T), labels = names(sort(table(realestate2$rps06), decreasing = T))[-1], col = rainbow(11), ylim = c(0, 1500), main = "非都市土地使用編定圓餅圖")

pie(sort(table(rps11), decreasing = T), labels = names(sort(table(realestate2$rps11), decreasing = T)), col = rainbow(10), main = "建物型態圓餅圖")
pie(sort(table(rps13), decreasing = T), labels = names(sort(table(realestate2$rps13), decreasing = T))[-2], col = rainbow(7), ylim = c(0, 1500), main = "主要建材圓餅圖")
pie(table(rps19), main = "有無隔間圓餅圖", col = terrain.colors(5))
pie(table(rps20), main = "有無管理組織圓餅圖", col = terrain.colors(5))
pie(sort(table(rps23), decreasing = T), labels = names(sort(table(realestate2$rps23), decreasing = T))[-1], col = rainbow(7), main = "主要建材圓餅圖")



#時間分布圖
par(mfrow = c(2, 2))
plot(as.Date(rps07), rps21, main = "交易年月之於總價元時間分布圖")
plot(as.Date(rps14), rps21, main = "建築完成年月之於總價元時間分布圖")
plot(as.Date(rps07), rps22, main = "交易年月之單價每平方公尺時間分布圖")
plot(as.Date(rps14), rps22, main = "建築完成年月之於單價每平方公尺時間分布圖")
plot(as.Date(rps07), rps24, main = "交易年月之車位移轉總面積平方公尺時間分布圖")
plot(as.Date(rps14), rps24, main = "建築完成年月之於車位移轉總面積平方公尺時間分布圖")
plot(as.Date(rps07), rps25, main = "交易年月之車位總價元時間分布圖")
plot(as.Date(rps14), rps25, main = "建築完成年月之於車位總價元時間分布圖")



library("plot3D")
library(plotrix)
library(survival)

#2D散佈圖
plot(rps22, rps25)

pairs(n_realestate[, c(22, 23, 25, 26)])
pairs(n_realestate[, 29 : 31])

par(mfrow = c(1, 1))
#3D散佈圖
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(rps08_L, rps08_B, rps08_P)
scatter3D(rps08_L, rps08_B, rps08_P)

#熱圖
library(fields)
heatmap(as.matrix(n_realestate[, c(22, 23, 25, 26)]))
heatmap(as.matrix(n_realestate[, 29 : 31]),  col = heat.colors(100))

#地圖
#GoogleMap

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

llpos <- matrix(NaN, length(rps02), 2)
k <- 1
for (i in 1 : length(rps02)) {
  templl <- geoPoint(rps02[i], "AIzaSyD7ExFH62Ni2cYE5Vp4939DVO6DeUKxupk") #放入GoogleAPI金鑰
  
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

n_realestate$lat <- llpos[, 1]
n_realestate$lng <- llpos[, 2]
write.table(n_realestate, file = "D:\\Rcode\\new_realestate.csv", sep = ",", row.names = T, na = "NA")

n_realestate <- read.csv("D:\\Rcode\\new_realestate.csv")
attach(n_realestate)
#標記
tw.newtaipei.zh <- get_map(location = c(lon = 121.55, lat = 25.043403), zoom = 11, language = "zh-TW" , maptype = "roadmap")
ggmap(tw.newtaipei.zh) + geom_point(data = n_realestate, aes(x = n_realestate$lng, n_realestate$lat, color = rps21), size = 1.5) + scale_color_continuous(
  low = "yellow",high = "red")+ 
  guides(size=FALSE)

ggmap(tw.newtaipei.zh) + geom_point(data = n_realestate, aes(x = n_realestate$lng, n_realestate$lat, color = rps22), size = 1.5) + scale_color_continuous(
  low = "yellow",high = "red")+ 
  guides(size=FALSE)

ggmap(tw.newtaipei.zh) + geom_point(data = n_realestate, aes(x = n_realestate$lng, n_realestate$lat, color = rps24), size = 1.5) + scale_color_continuous(
  low = "yellow",high = "red")+ 
  guides(size=FALSE)

ggmap(tw.newtaipei.zh) + geom_point(data = n_realestate, aes(x = n_realestate$lng, n_realestate$lat, color = rps25), size = 1.5) + scale_color_continuous(
  low = "yellow",high = "red")+ 
  guides(size=FALSE)