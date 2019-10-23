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
ntpu <- readImage("data/NTPUcolorlogo.jpg")
loc <- LatLon2XY.centered(MyMap, lat=My.markers[3, 1], lon=My.markers[3, 2])
rasterImage(ntpu, loc[[1]], loc[[2]]+30, loc[[1]]+50, loc[[2]]+80)
Fuxing <- readImage("data/Fuxinglogo.jpg")
loc <- LatLon2XY.centered(MyMap, lat=My.markers[2, 1], lon=My.markers[2, 2])
rasterImage(Fuxing, loc[[1]], loc[[2]]+30, loc[[1]]+50, loc[[2]]+80)


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