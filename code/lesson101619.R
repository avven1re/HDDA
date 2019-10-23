##101619 and 102319
#encoding : CP950
#E04

#8/47 ggplot & 頁面分割
install.packages("gridExtra")
library(gridExtra)
h1 <- ggplot(data=iris, aes(x=Sepal.Length)) + geom_histogram()
h2 <- ggplot(data=iris, aes(x=Sepal.Length)) + geom_histogram(binwidth=1)
h3 <- ggplot(data=iris, aes(x=Sepal.Length)) +
  geom_histogram(color="black", fill="blue", bins = 10)
h4 <- ggplot(data=iris, aes(x=Sepal.Length, color=Species)) + geom_histogram(binwidth = 1)
grid.arrange(h1, h2, h3, h4, nrow=1, ncol=4)

#9/47 geom_histogram
p <- ggplot(data=iris, aes(x=Sepal.Length))
p <- p + geom_histogram()
p + facet_grid(Species~.)

# geom_density
ggplot(iris, aes(x=Sepal.Length)) + geom_density()
ggplot(iris, aes(x=Sepal.Length, color=Species)) + geom_density()
?geom_density

#12~13/47 geom_bar
p <- ggplot(mtcars, aes(x= cyl)) + geom_bar()
p
ggplot(mtcars, aes(x= cyl)) + geom_bar() + coord_flip()
p + labs(title = "Motor Trend Car Road Tests Data",
           x = "Number of cylinders", y = "Number of cars")

iris.mean <- aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean)
iris.mean     #aggregate 以Species 為根據做平均
mydata <- cbind(stack(iris.mean[,-1]), Species = iris.mean$Species)
mydata
ggplot(mydata, aes(x=ind, y=values, fill = Species)) + geom_bar(position="dodge", stat="identity")

#14/47
p <- ggplot(data=mtcars, aes(x=wt, y=mpg, label = rownames(mtcars)))
p + geom_point()
p + geom_text(size=3)
p + geom_label()

#15/47 geom_boxplot
mtcars$cyl <- factor(mtcars$cyl)
ggplot(data=mtcars, aes(x=cyl, y=disp)) + geom_boxplot()

#16/47
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width,
                      shape=Species, color=Species)) + geom_point()
p <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species, color=Species))
p <- p + geom_point()
p
p + geom_line(aes(y=Sepal.Width))

#17/47 Google Map
install.packages("ggmap")
library(ggmap)
register_google(key = " API ", write = TRUE)  #put google API KEYS
houston_center <- geocode("Houston, TX")
houston_center

has_google_key()  #往後"先"用此指令判斷是否已經匯入API金鑰
google_key()
ggmap_show_api_key()

library(RgoogleMaps)
twmap <- get_map(location = 'Taiwan', zoom = 7, language = "zh-TW")
twmap

TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom =7, destfile = "Taiwan1.png")
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom = 10, destfile = "Taiwan2.png",
                    maptype = "terrain")

# 19/68
#library(ggmap)
#register_google(key = "API", write = TRUE) 
tw.xy <- geocode("Taiwan")
tw.xy
has_google_key()
google_key()

tw.map <- get_map(location = 'Taiwan', zoom = 7, language = "zh-TW")

# 21/68
library(RgoogleMaps)
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom =7, destfile = "pic/Taiwan1.png")
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom = 10, destfile = "pic/Taiwan2.png", maptype = "terrain")

# 22/68
my.lat <- c(25.175339, 25.082288, 25.042185, 25.046254)
my.lon <- c(121.450003, 121.565481, 121.614548, 121.517532)
bb = qbbox(my.lat, my.lon)
print(bb)
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "pic/my.png", maptype = "roadmap")
My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile = "my.png", cex=2.5, pch=20, col=1:4, add=F)

# 23/68 於地圖上標記
png("pic/my2.png", 640, 640)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], cex=2.5, pch=20, col=1:4, add=F)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], col="blue", add=T, FUN = lines, lwd = 2)

# 24/68 於地圖上加文字與圖片
my.lat <- c(25.175339, 25.14362, 24.942605)
my.lon <- c(121.450003, 121.501768, 121.368381)
bb = qbbox(my.lat, my.lon)
print(bb)
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "pic/my3.png", maptype = "roadmap")

My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <- PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], 
                       destfile = "my.png", cex=2.5, pch=18:10, col=1:3, add=F)
TextOnStaticMap(MyMap, lat = My.markers[,"lat"]+0.01,
                 lon = My.markers[,"lon"],
                 labels=c("我家", "復興高中", "國立臺北大學三峽校區"), add=T)

library(EBImage)
ntpu <- readImage("dataset/NTPUcolorlogo.jpg")
loc <- LatLon2XY.centered(MyMap, lat=My.markers[3, 1], lon=My.markers[3, 2])
rasterImage(ntpu, loc[[1]], loc[[2]]+30, loc[[1]]+50, loc[[2]]+80)
Fuxing <- readImage("data/Fuxinglogo.jpg")
loc <- LatLon2XY.centered(MyMap, lat=My.markers[2, 1], lon=My.markers[2, 2])
rasterImage(Fuxing, loc[[1]], loc[[2]]+30, loc[[1]]+50, loc[[2]]+80)

# 26/68 Spatial Visualization with ggplot2
library(maps); library(maptools); library(mapdata); library(mapproj)
layout(matrix(c(1,1,1,0,2,0), ncol=2), widths=c(10, 1), heights=c(1, 10, 1))
map("world2Hires", xlim=c(118, 123), ylim=c(21, 26))
data <- read.table("20140714-weather.txt", sep="\t", header=TRUE, row.names=1)
x <- data$TEMP
tm <- floor((100-1)/(max(x)-min(x))*(x-min(x)) + 1)
used.col <- heat.colors(100)[tm]
points(data$lon, data$lat, pch=15, col=used.col)
text(data$lon, data$lat, labels=row.names(data))
title("20140714, 天氣")
par(mar=c(1,1,1,1))
image(t(matrix(c(1:100), ncol=1)), 
      col=heat.colors(100), xaxt="n", yaxt="n")
axis(LEFT <- 2, at=tm/100, 
     labels=as.character(x), cex.axis=1)

#28/47
library(ggmap)
library(mapproj)
tw.map <- get_map(location = 'Taiwan', zoom = 7)
ggmap(tw.map)
tw.map.zh <- get_map(location = 'Taiwan', zoom = 7, language = "zh-TW")
ggmap(tw.map.zh)

tw.map.ntpu <- get_map(location = c(lon = 121.374925, lat = 24.943403),
                       zoom = 15, language = "zh-TW" , maptype = "terrain")
ggmap(tw.map.ntpu)

# 29/68
tw.map.ntpu <- get_map(location = c(lon = 121.374925, lat = 24.943403),
                       zoom = 10, language = "zh-TW", maptype = "terrain")
ggmap(tw.map.ntpu)

map <- get_map(location = c(lon = 121.374925, lat = 24.943403),
               zoom = 10, language = "zh-TW", maptype = "toner-lite")
ggmap(map)

# 30/68
uv <- read.csv("dataset/UV_20191016130309.csv")
head(uv)

# 經緯度(度分秒) => 度數
lon.deg <- sapply((strsplit(as.character(uv$WGS84Lon), ",")), as.numeric)
lon.deg
uv$lon <- lon.deg[1, ] + lon.deg[2, ]/60 + lon.deg[3, ]/3600
lat.deg <- sapply((strsplit(as.character(uv$WGS84Lat), ",")), as.numeric)
uv$lat <- lat.deg[1, ] + lat.deg[2, ]/60 + lat.deg[3, ]/3600

tw.map <- get_map(location = 'Taiwan', zoom = 7)
ggmap(tw.map) + 
  geom_point(data = uv, aes(x = lon, y = lat, size = UVI), color="purple")

# 31/68
ggmap(tw.map) + 
  geom_point(data = uv, aes(x = lon, y = lat, size = UVI, color = UVI)) +
  scale_color_continuous(low = "yellow", high = "red") +
  facet_grid(~PublishAgency) +
  guides(size = FALSE)
