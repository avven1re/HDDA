# 14/208
library(graphics)
demo(graphics)#常見圖形
demo(Hershey) #各種符號
demo(image) #image和contours
demo(Japanese) #日本字
demo(persp) #曲面圖
demo(plotmath) #數學符號


# 17/208
dev.list()

plot(iris[,1])
dev.list()

dev.cur()

windows()
dev.set(2)

dev.copy()


# 21/208
for(i in 1: 4){
  name <-paste("iris",i, ".jpg", sep="")
  jpeg(name, width=800, height=800)
  plot(iris[,i])
  dev.off()
}

pdf("myplot%03d.pdf", onefile = FALSE)
for(i in 1: 4){
  plot(iris[,i])
}
dev.off()

pdf("myplot.pdf", onefile = TRUE)
for(i in 1: 4){
  plot(iris[,i])
}
dev.off()


# 23/208
pdf("iris.pdf") 
plot(iris[, 1:2], xlab="花萼長", ylab="花萼寬", main="鳶尾花 散佈圖", col=iris[,5])
text(6.7, 4, "這是中文")
dev.off()

library(showtext)
font.add("msjh", "msjh.ttc") 
showtext.auto(enable=TRUE)


# 24/208
pdf("test.pdf", width=12, height=8)  
par(family="msjh") ## 微軟正黑體
par(mfrow=c(1,2), oma=c(2, 1, 2, 1))
hist(iris[, 1])
text(6, 20, "這裡有中文字", col="green", cex=2)
plot(iris[, 1], iris[, 2], xaxt="n", bty="n", xlab="", xlim=c(0, 8), ylim=c(0, 5))
axis(1, at=0:8 , labels=letters[1:9], tick=FALSE)
text(2, 3, "我是中文", col="red", cex=2)
title('主標題(family="fang")', family="fang", outer=TRUE,  line=-1, cex.main=2)
mtext("這裡也有文字喲!", side=1, line=6, at=6, col="blue")
mycolor <- rainbow(100, alpha=0.6)[1:80]
rasterImage(t(mycolor), 0, 0, 8, 1, interpolate=FALSE)
dev.off()


# 28/208
library(MASS)
data(gehan)
time <- gehan$time
windows()
par(mfrow=c(1,2))
boxplot(time, ylim=c(-5, 50))
title("Boxplot of time")
hist(time, xlim=c(-5, 50))
title("Histogram of time")


# 29/208
windows()
par(mfrow=c(2,1))
plot(time, ylim=c(-5, 50), main="Scatterplot of time")
boxplot(time, ylim=c(-5, 50), main="Boxplot of time")

#or
windows()
par(mfrow=c(2,1))
s.title <- "Scatterplot of time"
plot(time, ylim=c(-5, 50), main=s.title)
b.title <- "Boxplot of time"
boxplot(time, ylim=c(-5, 50), main=b.title)


# 31/208
y <- iris[,1]
plot(y, type="p") # points
plot(y, type="l") # lines
plot(y, type="b") # both
plot(y, type="h") # histogram-like
plot(y, type="n") # none


# 32/208
attach(iris)
plot(Sepal.Length, Petal.Length, xlim = c(-1, max(Sepal.Length)),
     ylim = c(-1, max(Petal.Length)))
abline(lm(Petal.Length ~ Sepal.Length), col = "black")
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
abline(h = 2, col = "red", lty = 2)
abline(v = 5.5, col = "blue", lty = 3)
abline(a = 1, b = 0.7,  col = "green", lty = 4, lwd = 2)
detach(iris)


# 33/208
x <- iris[, 1]
y <- iris[, 3]
plot(x, y, pch=16, col="red")
lines(x,y)

sequence <- order(x)
plot(x, y, pch=16, col="red")
lines(x[sequence], y[sequence])


# 34/208
x <- runif(12)
y <- rnorm(12)
plot(x, y, main="arrows and segments")
arrows(x[1], y[1], x[2], y[2], col= "black", length=0.2)
segments(x[3], y[3], x[4], y[4], col= "red")
segments(x[3:4], y[3:4], x[5:6], y[5:6], col= c("blue", "green"))


# 37/208
par(c("col", "lty"))
par(col="red", lty="dashed")
y <- rnorm(20)
plot(y, type="l") 
plot(y, type="l", lty="solid") 
plot(y, type="l") 

op <- par()
par(col="red")
plot(1,2)
par(op)


# 39/208
myplot <- function(n){
  for(i in 1:n){ 
    plot(1, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
    text(1, 1, labels=paste(i), cex=3)
  }
}
orig.par <- par(mai=c(0.1, 0.1, 0.1, 0.1), mfrow=c(3, 2))
myplot(6)
par(orig.par)
par(mai=c(0.1, 0.1, 0.1, 0.1), mfcol=c(2, 3))


# 40/208
orig.par <- par(mai=c(0.1, 0.1, 0.1, 0.1))
(mat1 <- matrix(c(1,2,1,3), 2, 2))
layout(mat1)
myplot(3)
par(orig.par)

layout(mat1, widths=c(3, 1), heights=c(2, 1))
myplot(3)

(mat2 <- matrix(c(2,1,0,3), 2, 2))

layout(mat2, widths=c(3, 1), heights=c(1, 3))
myplot(3)


# 42/208
plot(1:10, rep(1, 10), pch=20, col=1:10, cex=5, xlab="", ylab="")
text(1:10, rep(1.2, 10), labels=1:10)


# 43/208
colors()

rgb(1,0,0) 


# 44/208
col2rgb("peachpuff")

col2rgb(c(myblue = "royalblue", reddish = "tomato")) # names kept

col2rgb(paste("gold", 1:4, sep=""))

col2rgb("#08a0ff")

palette() #predefined set of colors  

col2rgb(1:8) # the ones from the palette()


# 46/208
par(mfrow=c(2,3))
n <- 24
pie(rep(1,n), col=rainbow(n))
pie(rep(1,n), col=heat.colors(n))
pie(rep(1,n), col=terrain.colors(n))
pie(rep(1,n), col=topo.colors(n))
pie(rep(1,n), col=cm.colors(n))
pie(rep(1,n), col=grey(1:n/n))


# 47/208
library(RColorBrewer)
brewer.pal.info

display.brewer.all()


# 48/208
display.brewer.pal(5, "BrBG")
display.brewer.pal(7, "BrBG")
display.brewer.pal(9, "BrBG")

attach(iris)
par(mfrow=c(1,6))
hist(Sepal.Length, breaks=10, col=brewer.pal(3, "Set3"), main="Set3 3 colors")
hist(Sepal.Length, breaks=3 ,col=brewer.pal(3, "Set2"), main="Set2 3 colors")
hist(Sepal.Length, breaks=7, col=brewer.pal(3, "Set1"), main="Set1 3 colors")
hist(Sepal.Length, breaks= 2, col=brewer.pal(8, "Set3"), main="Set3 8 colors")
hist(Sepal.Length, col=brewer.pal(8, "Greys"), main="Greys 8 colors")
hist(Sepal.Length, col=brewer.pal(8, "Greens"), main="Greens 8 colors")
detach(iris)


# 49/208
n <- 10
(col.a <- colorRamp(c("red", "green"))((0:n)/n)) #(x) , x in [0,1]

rgb(col.a/255)

col.b <- colorRampPalette(c("red", "green"))(n)  # (n)
col.b

col2rgb(col.b)

plot(1:n,  rep(0.5, n),  pch=20, col=rgb(col.a/255), cex=8, ylim=c(0, 3))
text(5, 1, 'colorRamp(c("red", "green"))((0:n)/n)')
points(1:n,  rep(2, n),  pch=20, col=col.b, cex=8)
text(5, 2.5, 'colorRampPalette(c("red", "green"))(n)')


# 50/208
library(fields)
par(mfcol=c(3,2))
x <- 1:20
y <- 1:20
z <- outer(x, rep(1,20), "+")
obj <- list(x=x, y=y, z=z)
image(obj, col=tim.colors(200), main="tim.colors(200)")
image(obj, col=two.colors(), main="two.colors()")
image(obj, col=two.colors(start="darkgreen", end="darkred", middle="black"), 
      main="two.colors()")

plot(x, y,  main="two.colors(alpha=.5)")
image(obj, col=two.colors(alpha=.5), add=TRUE)
image(obj, col=designer.colors(), main="designer.colors()")
coltab <- designer.colors(col=c("blue", "grey", "green", "red")) 
image(obj, col= coltab, main="designer.colors()")


# 51/208
library(fields)
cor.col <- two.colors(start="blue", middle="white", end="red") 
length(cor.col)
range(cor.col)

n <- 100
p <- 4

set.seed(12345)
x <- matrix(rnorm(n*p), ncol=p)
rx <- cor(x)
rx
#(-1, 0, 1) => (0, 1, 2) => (1, 128, 255)
range.col <- floor((1+range(rx))*127+1) 
range.col

par(mfrow=c(1, 2))
image(t(rx)[,p:1], main="cor(x)", col=cor.col, xaxt="n", yaxt="n")
axis(3, at=seq(0, 1, length.out=4), labels=paste0("x", 1:p))
axis(2, at=seq(0, 1, length.out=4), labels=paste0("x", p:1), las=1)

image(t(rx)[,p:1], main="cor(x)", col=cor.col[range.col[1]: range.col[2]], , xaxt="n", yaxt="n")
axis(3, at=seq(0, 1, length.out=4), labels=paste0("x", 1:p))
axis(2, at=seq(0, 1, length.out=4), labels=paste0("x", p:1), las=1)

x <- 1:20
y <- 1:20
z <- outer(x, rep(1,20), "+")
obj <- list(x=x, y=y, z=z)
image(obj, col=cor.col, xaxt="n", ylab="", yaxt="n")
axis(1, at=x, labels=round(seq(-1, 1, length.out=20), 2), las=2)


# 52/208
library(corrplot)
# par(mar = rep(0,4))
plot(0, xlim = c(0, 3), ylim = c(0, 1), type = "n")
colorlegend(rainbow(100), 0:9)

colorlegend(heat.colors(100), LETTERS[1:12], xlim = c(1, 2))
colorlegend(terrain.colors(100), 0:9, ratio.colbar = 0.6,
            lim.segment = c(0, 0.6), xlim = c(2, 3), align = "l")


# 53/208
par(mar = rep(0,4))
plot(0, xlim = c(3, 6), ylim = c(-0.5, 1.2), type = "n")
colorlegend(topo.colors(100), 0:9, lim.segment = c(0,0.6),
            xlim = c(3,4), align = "l", offset = 0)
colorlegend(cm.colors(100),1:5, xlim = c(4,5))
colorlegend(sample(rainbow(12)), labels = LETTERS[1:12],
            at = seq(0.05, 0.95, len = 12), xlim = c(5, 6), align = "r")

colorlegend(sample(rainbow(12)),
            labels = LETTERS[1:12], at = seq(0.05, 0.95, len = 12),
            xlim = c(3, 6), ylim = c(1.1, 1.2), vertical = FALSE)

colorlegend(colbar = grey(1:100 / 100), 1:10, col = "red", align = "l",
            xlim = c(3, 6), ylim = c(-0.5, -0.1), vertical = FALSE)


# 55/208
plot(iris[,3], iris[,4], type="n")
my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))
text(iris[,3], iris[,4], labels=my.label, cex=0.7)


# 56/208
plot(iris[,3], iris[,4], type="n")
my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))
text(iris[,3], iris[,4], my.label, cex=0.7, 
     col=ifelse(iris[,1] > median(iris[,1]), "red", "blue"))


# 57/208
plot(iris[,1], iris[,2], xlim=c(0, 10), ylim=c(0, 10))
text(2,8, "This is a test")
arrows(x0=3, y0=7, x1=5, y1=5, length = 0.15, col="red")
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))


# 59/208
plot(iris[,3], iris[,4], type="n")
my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))
my.color <- c(rep("red", 50), rep("blue", 50), rep("green", 50))
text(iris[,3], iris[,4], my.label, cex=0.7, col=my.color)
legend(5, 0.6, legend=c("setosa","versicolor", "virginica"), pch = "abc",       
       col=c("red","blue","green"))


# 60/208
x <- seq(-pi, pi, len = 65)
plot(x, sin(x), type = "l", ylim = c(-1.2, 1.8), col = 3, lty = 2)
points(x, cos(x), pch = 3, col = 4)
lines(x, tan(x), type = "b", lty = 1, pch = 4, col = 6)
legend(-1, 1.9, c("sin", "cos", "tan"), col = c(3,4,6), text.col = "green4",      
       lty = c(2, -1, 1), pch = c(-1, 3, 4), bg = 'gray90')


# 62/208
plot(1:7, rnorm(7), xaxt = "n", frame = FALSE)
axis(1, 1:7, LETTERS[1:7], col = "green")
axis(3, 1:7, paste("test", LETTERS[1:7]), col.axis = "blue", las=2)
axis(4, lty=2, lwd = 2, las=2)

plot(1:8, xaxt = "n",  xlab = "")
axis(1, labels = FALSE)
my.labels <- paste("Label", 1:8, sep = "-")
text(1:8, par("usr")[3] - 0.25, srt = 45, adj = 1, 
     labels = my.labels, xpd = TRUE)
mtext(1, text = "X Axis Label", line = 3)

par("usr")


# 63/208
plot(0, xlim=c(0,14), ylim=c(0, 14), type = "n", 
     xlab = "", ylab = "", main = "Rectangles")
rect(1, 2, 3, 6)
n <- 0:3
rect(5+n, 5+n, 6+2*n, 6+2*n, col = rainbow(4), border = n+1, lwd=4)

symbols(x = c(2, 6), y = c(2, 6), circles = c(1, 4), xlim=c(0, 10), ylim=c(0, 10), bg=c("red", "gray"), xlab="", ylab="")


# 64/208
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "")
i1 <- as.raster(matrix(0:1, ncol = 6, nrow = 7))
rasterImage(i1, 180, 380, 220, 430, interpolate = FALSE)

i2 <- as.raster(matrix(colors()[1:100], ncol = 5))
rasterImage(i2, 100, 300, 150, 400, interpolate = FALSE) 


# 65/208
install.packages(c("tiff", "jpeg", "png", "fftwtools"), repos="http://cran.csie.ntu.edu.tw")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")

library(EBImage) # (Repositories: BioC Software)
Transformers <- readImage("Transformers07.jpg")
(dims <- dim(Transformers))
Transformers

plot(c(0, dims[1]), c(0, dims[2]), type='n', xlab="", ylab="")
rasterImage(Transformers, 0, 0, dims[1], dims[2])


# 66/208
Transformers.f <- Image(flip(Transformers))
# convert RGB to grayscale
rgb.weight <- c(0.2989, 0.587, 0.114)
Transformers.gray <- rgb.weight[1] * imageData(Transformers.f)[,,1] + 
  rgb.weight[2] * imageData(Transformers.f)[,,2] + 
  rgb.weight[3] * imageData(Transformers.f)[,,3]
dim(Transformers.gray)

Transformers.gray[1:5, 1:5]
par(mfrow=c(1,2), mai=c(0.1, 0.1, 0.1, 0.1))
image(Transformers.gray, col = grey(seq(0, 1, length = 256)), xaxt="n", yaxt="n")
image(Transformers.gray, col = rainbow(256), xaxt="n", yaxt="n")


# 67/208
main.ex <- expression(paste("Math Symbols: ", list(alpha, theta)))
plot(1:10, 1:10, type="n", main=main.ex, xlab="", ylab="")
text(5, 9, expression(list({f * minute}(x), {f * second}(x))))
text(5, 7, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(5, 5, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
ex <- expression(f(x)==paste(frac(1, sigma*sqrt(2*pi)), " ",
                             plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}))
text(5, 3, labels = ex)


# 71/208
data <- iris[,1]
data[33] <- data[33]*10
plot(data)
ind <- which(data>15)
data[ind]
data[ind] <- 5.2
windows()
plot(data)


# 75/208
lab <- names(iris)[1]
title <- paste("Histogram of ", lab)
hist(iris[,1], main=title, xlab=lab)
range(iris[,1])
hist(iris[,1], breaks=seq(3.5, 8.5, length=50),main=title, xlab=lab)
hist(iris[,1], breaks=seq(3.5, 8.5, length=50),main=title, xlab=lab, pro=T)


# 76/208
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")
VADeaths

?VADeaths


# 77/208
attach(OrchardSprays)
names(OrchardSprays)

OrchardSprays[1:5,]
stripchart(decrease~treatment, xlab="decrease", ylab="treatment")


# 80/208
plot(density(iris$Sepal.Length))


# 81/208
hist(iris[,1], breaks=15, main=title, xlab=lab, col="green", pro=T)
lines(density(iris[,1], width=0.6, n=200))


# 84/208
x <- seq(-3, 3, 0.01)
plot(x, dnorm(x), main="standard normal", type="l", lwd=2, xaxt = "n")
p <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
q <- round(qnorm(p), 2)
rbind(p, q)

abline(v=q, col="blue")
abline(h=0, col="black")
text(q, -0.02, srt = 45, adj = 1, labels = q, xpd = TRUE)
y <- dnorm(x)
polygon(c(x[x <= qnorm(0.05)], qnorm(0.05)), c(y[x <= qnorm(0.05)], y[x==-4]), col="lightgreen")
text(-1.9, 0.03, "5%", col="red")
text(-1.6, 0.06, "10%", col="red")


# 87/208
par(mfrow = c(1, 2))
set.seed(12345); 
n <- 100; mu <- 0.5; sigma <- 0.15
x <- rnorm(n, mu, sigma)
hist(x, freq=FALSE, ylim=c(0, 3), main="")
y <- seq(0, 1, length = n)
lines(y, dnorm(y, mu, sigma), type = 'l')
qqnorm(x, main = "rnorm(mu=0.5, sigma=0.15)"); 
qqline(x)

qqplot(x, rnorm(300)) 
qqline(x, col = 2)
qqplot(scale(x), rnorm(300)) 
qqline(scale(x), col = 2)


# 89/208
qqnorm(iris[,1])
qqline(iris[,1])

qqnorm(scale(iris[,1]))
qqline(scale(iris[,1]))

my.qqplot(iris[,1])

my.qqplot <- function(x){
  x.mean <- mean(x)
  x.var <- var(x)
  n <- length(x)
  
  z <- (x-x.mean)/sqrt(x.var)
  z.mean <- mean(z)
  z.var <- var(z)
  z.sort <- sort(z)
  
  k <- 1:n
  p <- (k-0.5)/n
  q <- qnorm(p)
  
  plot(q, z.sort, xlim=c(-3, 3), ylim=c(-3, 3))
  title("QQ plot") 
  lines(q, q, col=2)
}


# 95/208
data(UKLungDeaths) # total, male, female death
ts.plot(ldeaths, mdeaths, fdeaths, xlab="year", ylab="deaths", lty=c(1:3))

data(sunspots)
plot(sunspots) # sunspots is ts class
class(sunspots)
is.ts(sunspots)


# 96/208
cell.raw <- read.table("trad_alpha103.txt", row.names=1, header=T)
head(cell.raw)
cell.xdata <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))    
y.C <-  as.integer(cell.raw[,1])
table(y.C)
no.cluster <- length(unique(y.C))            
p <- ncol(cell.raw) -1
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
ycolors <- cellcycle.color[y.C+1]
my.pch <- c(1:no.cluster)[y.C+1]    
phase <- c("G1", "S", "S/G2", "G2/M", "M/G1")
matplot(t(cell.xdata), lty=1, type = "l", ylab="gene expression", 
        col=ycolors, xlab="time", main="Time series", xaxt="n")
time.label <- parse(text=paste("t[",0:p,"]",sep=""))        
axis(1, 1:(p+1), time.label)
legend("bottom", legend=phase, col=cellcycle.color, lty=1, horiz = T, lwd=2)



# 97/208
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
sum(pie.sales)
names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")

pie(pie.sales) # default colours

pie(pie.sales, col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))
pie(pie.sales, clockwise=TRUE, main="pie with clockwise=TRUE")
pie(rep(1,200), labels="", col=rainbow(200), border=NA, main = "Rainbow Pie")


# 98/208
library(plotrix)
library(survival)
head(veteran)

slices <- summary(veteran$celltype)
p <- floor(100*slices/sum(slices))
pie3D(slices, labels=paste0(names(slices), " (",p, "%)"), explode=0.1)


# 100/208
xlab <- names(iris)[1]
ylab <- names(iris)[2]
title <- paste(ylab, "against", xlab, " of Iris Data")
x <- iris[,1]
y <- iris[,2]
plot(x, y, col="red", xlab=xlab, ylab=ylab, main=title)

range(x)
range(y)
plot(y~x, xlab=xlab, ylab=ylab, xlim=c(1.5,9), 
     ylim=c(1.5,9), type="n")
points(x[1:50], y[1:50], col="red")
points(x[51:100], y[51:100], col="blue")
points(x[101:150], y[101:150], col="green")
abline(lm(y~x))


# 102/208
data(airquality)
head(airquality, 3)

aq <- airquality[airquality$Month 
                 %in% c(7,8,9),]
aq$Month <- factor(aq$Month, 
                   labels = c("July", 
                              "August", 
                              "September"))
attach(aq)
radius <- sqrt(Wind/pi) 
symbols(Day, Ozone, circles=radius,
        inches=0.1, fg="black", bg=as.integer(Month)+1, 
        xlab="Day of month", ylab="Ozone (ppb)",
        main="Air quality in New York by Day",
        ylim=c(0, 210))

legend(-2, 200, legend=c("July", "August", "September"), 
       pch=21, pt.bg=2:4, col="black", pt.cex=2, horiz=T)
x.loc <- rep(30, 3)
y.loc <- seq(140, 180, length.out=3)
s <- summary(radius)[c(1, 4, 6)]
symbols(x.loc, y.loc, circles=s, inches=0.1, add = T)
text(x.loc+1, y.loc, labels=round(s,2), pos=4)
text(31, 200, labels="Wind (mph)")
detach(aq)


# 103/208
aq.complete <- airquality[complete.cases(airquality), ]
np <- dim(aq.complete) 
attach(aq.complete)
layout(matrix(c(1, 2), ncol=2), width=c(4, 1))
Ozone.col <- get_order_color(Ozone)
plot(Wind ~ Temp, xlab="Temperature (F)", ylab="Wind (mph)", pch=16, col= Ozone.col, cex=2)
at <- which(Ozone==24)
text(Temp[at], Wind[at], "24")
par("mai")
par.orig <- par(mai=c(1.02, 0, 0.82, 0.1))
mycolor <- terrain.colors(n)
plot(0, 0, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
rasterImage(rev(mycolor), -1, -1, 0, 1, interpolate=FALSE)
id <- rev(floor(seq(1, np[1], length.out=10)))
id.scale <- (id-min(id))/(max(id)-min(id))*2-1 
text(rep(0.1, 10), id.scale, label= sort(Ozone)[id])
mtext("Ozone (ppb)", side = 1)
par(par.orig)
detach(aq.complete)

get_order_color <- function(x){
  n <- length(x)
  mycol <- terrain.colors(n)
  sorted.x <- sort(x)
  order.x <- order(x)
  id <- 1:n
  sorted.id <- id[order.x]
  x.col <- mycol[order(sorted.id)]
  x.col
}


# 104/208
attach(aq.complete)
layout(matrix(c(1, 2), ncol=2), width=c(4, 1))
Ozone.col <- variable_to_color(Ozone)
plot(Wind ~ Temp, xlab="Temperature (F)", ylab="Wind (mph)", pch=16, col= Ozone.col, cex=2)
at <- which(Ozone==24)
text(Temp[at], Wind[at], "24")

par.orig <- par(mai=c(1.02, 0, 0.82, 0.1))
mycolor <- terrain.colors(n)
plot(0, 0, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
rasterImage(rev(mycolor), -1, -1, 0, 1, interpolate=FALSE)
id <- rev(floor(seq(1, np[1], length.out=10)))
lab <- rev(floor(seq(min(Ozone), max(Ozone), length.out=10)))
id.scale <- (id-min(id))/(max(id)-min(id))*2-1 # scale to (-1, 1)
text(rep(0.2, 10),  id.scale, label= lab)
mtext("Ozone (ppb)", side = 1)
par(par.orig)
detach(aq.complete)

variable_to_color <- function(x){
  n <- length(x)
  mycol <- terrain.colors(n)
  # scale x to (1, n)
  scale.x <- floor((x-min(x))/(max(x)-min(x))*(n-1))+1
  x.col <- mycol[scale.x]
  x.col
}


# 107/208
coplot(Sepal.Length ~ Petal.Width | Species, data = iris)
coplot(Wind ~ Temp | Ozone, data = airquality)


# 108/208
head(ChickWeight, 3)
coplot(weight ~ Time | Chick * Diet, type = "l", data = ChickWeight)

table(ChickWeight$Chick, ChickWeight$Diet)


# 109/208
n <- 1e+04
x1 <- rnorm(n, mean = -1, sd = 1)
y1 <- rnorm(n, mean = -1, sd = 1)
x2 <- rnorm(n, mean = 2, sd = 1) 
y2 <- rnorm(n, mean = 2, sd = 1)
par(mfrow=c(1, 2))
plot(x1, y1, main="black")
smoothScatter(x1, y1, col="black", colramp=colorRampPalette(c("white", "black")), 
              main='colorRampPalette(c("white", "black"))')


# 110/208
curve(x^3-3*x, -2, 2)
curve(sin, -2*pi, 2*pi)

x <- seq(-2, 2, 0.01)
y <- x^3-3*x
plot(x, y, type="l")

weibull <- function(alpha, beta, x){
  alpha * beta * (x^(alpha-1))
}

b <- c(1, 2, 4, 8)
for(i in 1:length(b)) {
  curve(weibull(0.5, b[i], x), from=0, to=2, 
        add=(i!=1), 
        col=i, ylim=c(0, 50), main="alpha=.5")
}

legend(1.5, 40, legend=b, col=1:length(b), lty=1)


# 111/208
xv <- 0:100
yA <- 482*xv*exp(-0.045*xv)
yB <- 518*xv*exp(-0.055*xv)
plot(c(xv, xv), c(yA, yB), xlab="stock", ylab="recruits", type="n")
lines(xv, yA, lty=1, col="blue")
lines(xv, yB, lty=2, col="red")


# 113/208
data(airquality)
id <- is.na(airquality$Ozone)  
Ozone <- airquality$Ozone[id==F]
Temp <- airquality$Temp[id==F]

plot(Ozone~Temp, main="non-linear parametric curves 1")
model1 <- nls(Ozone~a+b*Temp+c*Temp*Temp, start=list(a=1, b=1, c=1))
range(Temp)
xv <- seq(55, 100, 1)
lines(xv, predict(model1, list(Temp=xv)), col="red", lwd=2)

plot(Ozone~Temp, main="non-linear parametric curves 2")
yv <- 305.48577-9.55060*xv+0.07807*xv*xv
lines(xv, yv, col="red", lwd=2)


# 114/208
plot(Ozone~Temp, main="lowess")
lines(lowess(Ozone~Temp), col="red", lwd=2)

plot(Ozone~Temp, main="non-parametric linear model")
model2 <- lm(Ozone~Temp+(Temp^2)+(Temp^3))
lines(xv, predict(model2, list(Temp=xv)), col="red", lwd=2)


# 117/208
attach(iris)
plot(factor(Species), Sepal.Length, ylab="Sepal.Length", main="Boxplot") 


# 118/208
par(mfrow=c(1,4))
names(iris)
names(iris) <- c("SL", "SW", "PL", "PW", "Species")
boxplot(Sepal.Length, xlab="Sepal.Length")
boxplot(Sepal.Length~Species, ylab="Sepal.Length")
boxplot(iris[,which(sapply(iris, is.numeric))])
boxplot(iris[,which(sapply(iris, is.numeric))], horizontal=T, col=2:8)


# 119/208
ylab <- "decrease"
xlab <- " treatment"
boxplot(decrease ~ treatment, data=OrchardSprays, log="y", col="grey", xlab=xlab, ylab=ylab)

boxplot(decrease ~ treatment, data=OrchardSprays, log="y", col="grey", xlab=xlab, ylab=ylab, boxwex=0.5)


# 120/208
means <- tapply(iris$Sepal.Length, iris$Species, mean)
barplot(means, xlab="Species", ylab="Mean of Sepal.Length")
barplot(means, xlab="Species", ylab="Mean of Sepal.Length", density=20, horiz=TRUE)


# 121/208
barplot(VADeaths)
barplot(VADeaths, beside = TRUE, col=1:5)

barplot(VADeaths, beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(VADeaths), ylim = c(0, 140))
title(main = "Death Rates in Virginia", font.main = 4)


# 122/208
pairs(iris[,1:4], col=as.integer(iris[,5])+1)
pairs(iris[,1:4], col=as.integer(iris[,5])+1, panel=panel.smooth)


# 124/208
stem(iris[,1])


# 126/208
sines <- outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
dim(sines)
sines
matplot(sines, pch = 1:4, type = "o", col = rainbow(ncol(sines)), main="ex1")
matplot(sines, pch = 21:23, type = "b", col = 2:5, bg= 2:5, main="ex2")


# 127/208
locations <- locator(6)
polygon(location, col="lavender")

xv <- seq(-3, 3, 0.01)
yv <- dnorm(xv)
plot(xv, yv, type="l")

polygon(c(xv[xv <= -1]), c(yv[xv <= -1]), col="blue")

x11()
plot(xv, yv, type="l")
polygon(c(xv[xv <= -1], -1), c(yv[xv <= -1], yv[xv==-3]), col="red")


# 129/208
head(mtcars[, 1:7])


# 131/208
palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
      main = "Motor Trend Cars", draw.segments = TRUE)


# 132/208
stars(USJudgeRatings, draw.segments = TRUE, scale = FALSE, key.loc = c(13,1.5))


# 134/208
par(mfrow=c(1,2))
stars(USJudgeRatings, locations = c(0, 0), scale = FALSE, 
      radius  =  FALSE, col.stars = 1:10, key.loc = c(0, 0), 
      main = "US Judges rated")
# Same as above, but with colored lines instead of filled polygons.
stars(USJudgeRatings, locations = c(0, 0), scale = FALSE,
      radius  =  FALSE, col.lines = 1:10, key.loc = c(0, 0), 
      main = "US Judges rated")


# 136/208
stars(mtcars[, 1:7], locations = c(0, 0), radius = FALSE,
      key.loc = c(0, 0), main = "Motor Trend Cars", lty = 2)


# 137/208
stars(USJudgeRatings[1:10,], locations = 0:1, scale = FALSE,
      draw.segments = TRUE, col.segments = 0, col.stars = 1:10, key.loc =  0:1,
      main = "US Judges 1-10 ")
palette("default")
stars(cbind(1:16, 10*(16:1)), draw.segments = TRUE,
      main = "A Joke -- do *not* use symbols on 2D data!")


# 141/208
library(MASS)
parcoord(iris[,1:4], col=as.integer(iris[,5])+1, var.label = T)
library(GGally) # Extension to 'ggplot2'
ggparcoord(data = iris, columns = 1:4, groupColumn = 5)
ggparcoord(data = iris, columns = 1:4, groupColumn = 5, boxplot = T)
ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE)


# 142/208
library(iplots) 
ipcp(iris)


# 143/208
ploy <- function(x, y){x^2-x*y+y^2}
x.grid <- seq(-3, 3, length=50)
y.grid <- seq(-3, 3, length=50)
z.grid <- outer(x.grid, y.grid, FUN=ploy)
ploy.title <- paste("三維空間散圖\n", "f(x, y) =x^2-xy+y^2")
persp(x.grid, y.grid, z.grid, main= ploy.title)


# 145/208
library(scatterplot3d)
z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue", 
              col.grid="lightblue", main="scatterplot3d - 1", pch=20)


scatterplot3d(iris[,1:3], color=as.integer(iris[,5]))


# 146/208
temp <- seq(-pi, 0, length = 50)

x <- c(rep(1, 50) %*% t(cos(temp)))
y <- c(cos(temp) %*% t(sin(temp)))
z <- c(sin(temp) %*% t(sin(temp)))

scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", 
              main="scatterplot3d - 2.1", pch=20, cex.symbols=0.5)

scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", 
              main="scatterplot3d - 2.2", pch=20, cex.symbols=0.5, angle= 20)


# 147/208
temp <- seq(-10, 10, 0.01)
x <- c(x, cos(temp))
y <- c(y, sin(temp))
z <- c(z, temp)
color <- rep("green", length(temp))
color <- c(color, rep("red", length(temp)))

scatterplot3d(x, y, z, color, pch=20, zlim=c(-2, 10), main="scatterplot3d - 3")

my.mat <- matrix(runif(25), nrow=5)
dimnames(my.mat) <- list(LETTERS[1:5], letters[11:15])
my.mat
col(my.mat)
row(my.mat)
cols <- as.vector(col(my.mat))
rows <- as.vector(row(my.mat))
v <- as.vector(my.mat)

s3d.dat <- data.frame(cols=cols, rows=rows, value=v)
scatterplot3d(s3d.dat, type="h", lwd=5, pch=" ", x.ticklabs=colnames(my.mat), 
              y.ticklabs=rownames(my.mat), 
              color=grey(25:1/40), main="scatterplot3d - 4")


# 148/208
install.packages("plot3D")
library("plot3D")

x <- iris$Sepal.Length
y <- iris$Sepal.Width
z <- iris$Petal.Length
w <- iris$Petal.Width
s <- iris$Species

par(mfrow = c(1,3), mai = c(0.3, 0.3, 0.3, 0.3)) 
scatter3D(x, y, z)
scatter3D(x, y, z, pch = 18, clab = c("Sepal", "Width (cm)"), main = "Iris data",           xlab = "Sepal.Length", zlab = "Petal.Length", ylab = "Sepal.Width")
scatter3D(x, y, z, colvar = as.integer(s), col = "blue", pch = 19, cex = 1)


# 149/208
par(mfrow = c(1,3), mai = c(0.3, 0.3, 0.2, 0.3)) 
scatter3D(x, y, z, bty = "f", colkey = FALSE, ticktype = "detailed",
          theta=0, phi=60)
scatter3D(x, y, z, bty = "g", pch = 18, 
          col.var = as.integer(s), 
          col = c("red", "blue", "green"),
          pch = 18, ticktype = "detailed",
          colkey = list(at = c(2, 3, 4), side = 1, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("setosa", "versicolor", "virginica")))
text3D(x, y, z, labels = as.integer(s), colvar = w, bty = "b2")


# 150/208
library(rgl)
open3d()
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x,y)
plot3d(x, y, z, col=rainbow(1000), size=2)

M <- par3d("userMatrix")
play3d(par3dinterp(userMatrix=list(M, rotate3d(M, pi/2, 1, 0, 0), rotate3d(M, pi/2, 0, 1, 0) ) ), duration=4)


# 151/208
library(rgl)
swissroll <- function(n, sigma=0.05){
  
  angle <- (3*pi/2)*(1+2*runif(n)); 
  height <- runif(n);
  xdata <-  cbind(angle*cos(angle), height, angle*sin(angle))
  xdata <- scale(xdata) + matrix(rnorm(n*3, 0, sigma), n, 3)
  
  order.angle <- order(angle)
  sort.angle <- sort(order.angle, index.return=TRUE)	
  col.id <- rainbow(n)
  my.color <- col.id[sort.angle$ix]
  
  colnames(xdata) <- paste("x", 1:3, sep="")
  
  list(xdata=xdata, angle=angle, color=my.color)
}

swissdata <- swissroll(500)
xdata <- swissdata$xdata
x.color <- swissdata$color
open3d()
plot3d(xdata[,1], xdata[,2], xdata[,3], col=x.color, size=3, xlab="", ylab="", zlab="", axes = FALSE)	         


# 152/208
library(rgl)
open3d()  
o1 <- oh3d(tran = par3d("userMatrix"), color="red") # "o" objects
shade3d(translate3d(o1, -6, 0, 0))
o2 <- subdivision3d(o1)  # divides and refines a given mesh
shade3d(translate3d(o2, -2, 0, 0), color="blue")
o3 <- subdivision3d(o2)
shade3d(translate3d(o3, 2, 0, 0), color="green")
o4 <- subdivision3d(o3)
shade3d(translate3d(o4, 6, 0, 0), color="yellow")
shade3d(translate3d(dodecahedron3d(col = "cyan"), 6, 0, 0)) # 十二面體

play3d(spin3d(), duration=10)


# 153/208
library("rgl")
open3d()
plot3d(iris[,1:3], col=as.integer(iris[,5])+1, type ="p", size=10)

plot3d(iris[,1:3], col=as.integer(iris[,5])+1, type ="s", +        radius=0.15)
bbox3d(color=c("red", "black"), emission="gray", +        specular="yellow", shininess=5, alpha=0.8, nticks = 3) 
aspect3d(1,1,1)

lines3d(iris[c(1, 150), 1:3], col="purple", lwd=2)

shapes <- list(cube3d(), tetrahedron3d(), octahedron3d(), 
               icosahedron3d(), dodecahedron3d(), cuboctahedron3d())
shapelist3d(shapes, x=1, y=1:6, z=1, size=0.3, col=1:6)
aspect3d(1,1,1)

texts3d(x=2, y=6, z=6, texts="rgl Example", font=2, 
        color="blue", cex=2, family="serif")

fit <- lm(iris[,3] ~ iris[,1] + iris[,2])
coefs <- coef(fit)
planes3d(a=coefs[2], b=coefs[3], c=-1, d= coefs["(Intercept)"], 
         alpha = 0.5) 

play3d(spin3d(axis = c(0, 0, 1), rpm = 20), duration = 4)


# 154/208
terrain <- as.matrix(read.table("terrain_data.txt", header=F))
dim(terrain)

animal <- read.table("animal_data.txt", header=T)
dim(animal)

attach(animal)
head(animal, 3)

terrain.scale <- floor((terrain - min(terrain))/(max(terrain) - min(terrain))*99)+1
terrain.color <- terrain.colors(100)[terrain.scale] 
terrain.color[terrain==0] <- rgb(0, 0, 1) # set color for river
open3d()
clear3d("all") 
bg3d(col="gray") # setup background
light3d() # setup head-light
surface3d(1:100, seq(1,60,length=100), terrain, col=terrain.color, back="lines")
sex.col <- ifelse(sex==0, rgb(0, 0, 1), rgb(1, 0, 0)) # males: blue, females: red

z <- terrain[cbind(ceiling(loc.x), ceiling(loc.y*10/6))]
alpha.index <- (index-min(index))/(max(index)-min(index))
spheres3d(loc.x, loc.y, z + 0.5,
          radius=0.3*number, col=sex.col, 
          alpha=alpha.index)
detach(animal) 
play3d(spin3d(), duration=10)

# 155/208
open3d()
comet <- readOBJ("ESA_Rosetta_OSIRIS_67P_SHAP2P.obj")
class(comet)
str(comet)
shade3d(comet, col="gray")


# 156/208
my.data <- matrix(c(1:15), ncol=3, nrow=5)
my.data
image(my.data, col=grey(1:15/15))

image(t(my.data)[,nrow(my.data):1], col=grey(1:15/15))


# 157/208
image(t(as.matrix(iris[,1:4]))[,150:1], col=terrain.colors(100))
head(iris[,1:4])

tail(iris[,1:4])


# 158/208
data(volcano)
dim(volcano)

str(volcano)

par(mfrow = c(2, 2))
image(volcano, main = "heat.colors")
image(volcano, main = "rainbow", col = rainbow(15))
image(volcano, main = "topo", col = topo.colors(15))
image(volcano, main = "terrain.colors",col = terrain.colors(15))


# 159/208
x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column", 
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")


# 160/208
# par(mfrow=c(1, 2)) # not working
library(gridGraphics)
library(grid)

no.data <- 4
random.data <- function() { matrix(sample(1:100), nrow=10, ncol=10) }
simulated.data <- replicate(no.data, random.data(), simplify = FALSE)
class(simulated.data) # simplify = FALSE: "list"; ow. "array"


grid.newpage() # erases the current device or moves to a new page
library(gridExtra) # install.packages("gridExtra")
grid.arrange(grobs=myplots, ncol=no.data)

myplots <- lapply(1:no.data, function(i){
  heatmap(simulated.data[[i]], main=paste(i))
  grid.echo()
  grid.grab()
})



# 161/208
#hmap(x, distfun = dist, hclustfun = hclust, method = NULL, control = NULL, options = NULL, ...)


# 162/208
GBRcol <- color.Palette(low="green", mid="black", high="red")
image(matrix(1:400, ncol=20), col=GBRcol)
image(matrix(1:400, ncol=20), col=rainbow(400))

color.Palette <- function(low = "black", high = c("green", "red"), mid="black", k =50){
  
  low <- col2rgb(low)/255
  high <- col2rgb(high)/255
  
  if(is.null(mid)){
    r <- seq(low[1], high[1], len = k)
    g <- seq(low[2], high[2], len = k)
    b <- seq(low[3], high[3], len = k)
  }
  if(!is.null(mid)){
    k2 <- round(k/2)
    mid <- col2rgb(mid)/255
    r <- c(seq(low[1], mid[1], len = k2),
           seq(mid[1], high[1], len = k2))
    g <- c(seq(low[2], mid[2], len = k2),
           seq(mid[2], high[2], len = k2))
    b <- c(seq(low[3], mid[3], len = k2),
           seq(mid[3], high[3], len = k2))
  }
  rgb(r, g, b)
}


# 164/208
library(fields)
gbr <- two.colors(start="green", middle="black", end="red")
cell.raw <- read.table("trad_alpha103.txt", row.names=1, header=T)
cell.data <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))    
n <- nrow(cell.data)
p <- ncol(cell.data)
gene.phase <- cell.raw[,1]
range(cell.data)
cell.data[cell.data > 2.802712] <- 2.802712 
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
rc <- cellcycle.color[gene.phase+1]
cc <- rainbow(ncol(cell.data))

hv1 <- heatmap(cell.data[n:1,], col = gbr, Colv=NA, Rowv=NA,
               RowSideColors = rc, 
               ColSideColors = cc, margins = c(5,10),
               xlab = "Times", ylab =  "Genes",main = "Heatmap of Microarray Data")

hv2 <- heatmap(cell.data, col = gbr, Colv=NA, Rowv=NULL,
               RowSideColors = rc, 
               ColSideColors = cc, margins = c(5,10),
               xlab = "Times", ylab =  "Genes",main = "Heatmap of Microarray Data")

dd <- as.dendrogram(hclust(as.dist(1-cor(t(cell.data)))))
hv3 <- heatmap(cell.data, col = gbr, Colv=NA, Rowv=dd,
               RowSideColors = rc, 
               ColSideColors = cc, margins = c(5,10),
               scale = "row",
               xlab = "Times", ylab =  "Genes",main = "Heatmap of Microarray Data")


# 166/208
x <- -6:16; length(x)
my.data <- outer(x, x);dim(my.data)
my.data
contour(my.data, method = "edge")

image(x, x, z)
contour(x, x, z, col = "blue", add = TRUE, 
        method = "edge", lwd=1.5, lty=3)


# 167/208
filled.contour(volcano, color.palette = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map")
# asp: the y/x aspect ratio.
filled.contour(volcano, color.palette = colorRampPalette(c("red", "white", "blue")), asp = 1)


# 168/208
plot(iris[,1], iris[,2])
locations <- locator(6)
polygon(locations, col="lavender")


# 171/208
library(ggplot2)
qplot(Sepal.Length, Petal.Length, geom="point", 
      data=iris, colour = Species, main="scatterplot")
qplot(Species, Sepal.Length, geom="boxplot",
      fill=Species, data=iris)


# 174/208
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

mpg.df <- as.data.frame(mpg)
attach(mpg.df)
group <- as.factor(class)
plot(displ, hwy, col=group, pch=16)
legend("topright", legend=levels(group), 
       col=1:length(levels(group)), pch=16)
detach(mpg.df)


# 177/208
library(RgoogleMaps)
WorldMap <- GetMap(center=c(0,0), zoom =1, 
                   destfile = "World1.png")

# 178/208
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom =7, destfile = "Taiwan1.png")
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom = 10, destfile = "Taiwan2.png", maptype = "terrain")


# 179/208
my.lat <- c(25.175339, 25.082288, 25.042185, 25.046254)
my.lon <- c(121.450003, 121.565481, 121.614548, 121.517532)
bb = qbbox(my.lat, my.lon)
print(bb)
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")

My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], 
                        destfile = "my.png", cex=2.5, pch=20, col=1:4, add=F)


# 180/208
png("my2.png", 640, 640)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], 
                        cex=2.5, pch=20, col=1:4, add=F)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], 
                        col="blue", add=T, FUN = lines, lwd = 2)
dev.off()


# 181/208
library(RgoogleMaps)
my.lat <- c(25.175339, 25.14362, 24.942605)
my.lon <- c(121.450003, 121.501768, 121.368381)

bb = qbbox(my.lat, my.lon)
print(bb)

MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")

My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <- PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], 
                       lon = My.markers[,"lon"], 
                       destfile = "my.png", cex=2.5, pch=18:10, col=1:3, add=F)

TextOnStaticMap(MyMap, lat = My.markers[,"lat"]+0.01, 
                lon = My.markers[,"lon"], 
                labels=c("我家", "復興高中", "國立臺北大學三峽校區"),  add=T)

library(EBImage)
ntpu <- readImage("NTPUcolorlogo.jpg")
loc <- LatLon2XY.centered(MyMap, lat=My.markers[3, 1], lon=My.markers[3, 2])
rasterImage(ntpu, loc[[1]], loc[[2]]+30, loc[[1]]+50, loc[[2]]+80)
Fuxing <- readImage("Fuxinglogo.jpg")
loc <- LatLon2XY.centered(MyMap, lat=My.markers[2, 1], lon=My.markers[2, 2])
rasterImage(Fuxing, loc[[1]], loc[[2]]+30, loc[[1]]+50, loc[[2]]+80)


# 182/208
library(maps); library(maptools); library(mapdata); library(mapproj)
layout(matrix(c(1,1,1,0,2,0), ncol=2), widths=c(10, 1), heights=c(1, 10, 1))
map("world2Hires", xlim=c(118, 123), ylim=c(21, 26))
data <- read.table("20140714-weather.txt", sep="\t", header=TRUE, row.names=1)
x <- data$TEMP
tm <- floor((100-1)/(max(x)-min(x))*(x-min(x)) + 1)
used.col <- heat.colors(100)[tm]
points(data$lon, data$lat, pch=15, col=used.col)
text(data$lon, data$lat, labels=row.names(data))
title("20140714, 晚上8時各地溫度")
par(mar=c(1,1,1,1))
image(t(matrix(c(1:100), ncol=1)), 
      col=heat.colors(100), xaxt="n", yaxt="n")
axis(LEFT <- 2, at=tm/100, 
     labels=as.character(x), cex.axis=1)


# 183/208
library(ggplot2)
library(maps)
library(ggmap)
library(mapproj)
states.map <- map_data("state")
head(states.map, 3)

tail(states.map, 3)

ggplot(states.map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", colour="black")

ggplot(states.map, aes(x=long, y=lat, group=group)) +
  geom_path() + coord_map("mercator")


# 184/208
world.map <- map_data("world")
sort(unique(world.map$region)) 
east.asia <- map_data("world", region=c("Japan", "China", "North Korea", "South Korea"))
ggplot(east.asia, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black") +
  scale_fill_brewer(palette="Set2")


# 185/208
head(USArrests, 3)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(crimes, 3) 

library(maps)
library(ggmap)
states.map <- map_data("state")
head(states.map, 3)

crime.map <- merge(states.map, crimes, by.x="region", by.y="state")
head(crime.map, 3)


# 186/208
library(plyr) 
crime.map <- arrange(crime.map, group, order)
head(crime.map, 3)

ggplot(crime.map, aes(x=long, y=lat, group=group, fill=Assault)) +
  geom_polygon(colour="black") + 
  coord_map("polyconic")


# 187/208
ggplot(crime.map, aes(x=long, y=lat, group=group, fill=Assault)) +
  geom_polygon(colour="black") + coord_map("polyconic") +
  scale_fill_gradient2(low="blue", mid="grey", high="red",
                       midpoint=median(crimes$Assault)) 


# 188/208
library(googleVis)
demo(googleVis)


# 191/208
library(vcd)
