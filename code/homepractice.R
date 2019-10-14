#A05 ppt -> browseURL("http://www.hmwu.idv.tw/web/R/A05-hmwu_R-Graphics&Visualization.pdf")

#繪圖基礎
par(mfrow = c(2, 3))
n <- 24
pie(rep(1,n), col=rainbow(n), main = "rainbow")
pie(rep(1,n), col=heat.colors(n), main = "heat.col")
pie(rep(1,n), col=terrain.colors(n), main = "terrain.col")
pie(rep(1,n), col=topo.colors(n), main = "topo")
pie(rep(1,n), col=cm.colors(n), main = "cm")
pie(rep(1,n), col=grey(1:n/n), main = "grey")

n <- 10
(col.a <- colorRamp(c("red", "green"))((0:n)/n))

#56/208
par(mfrow = c(1, 1))
plot(iris[,3], iris[,4], type="n")
my.label <- c(rep("a", 50), rep("b", 50), rep("c", 50))
text(iris[,3], iris[,4], labels=my.label, cex=0.7)

#57/208
plot(iris[,1], iris[,2], xlim=c(0, 10), ylim=c(0, 10))
text(2,8, "This is a test")
arrows(x0=3, y0=7, x1=5, y1=5, length = 0.15, col="red")
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))

#61/208 axis
plot(1:7, rnorm(7), xaxt = "n", frame = FALSE)
axis(1, 1:7, LETTERS[1:7], col = "green")
axis(3, 1:7, paste("test", LETTERS[1:7]), col.axis = "blue", las=2)
axis(4, lty=2, lwd = 2, las=2)

plot(1:8, xaxt = "n", xlab = "")
axis(1, labels = FALSE)
my.labels <- paste("Label", 1:8, sep = "-")
text(1:8, par("usr")[3] - 0.25, srt = 45, adj = 1,
     labels = my.labels, xpd = TRUE) #xpd: all plotting is clipped to the figure region #srt: string rotation 
mtext(1, text = "X Axis Label", line = 3)

##A05 70/208 開始介紹基本統計圖

#75/208 Histogram
lab <- names(iris)[1]
title <- paste("Histogram of ", lab)        #breaks
hist(iris[,1], main=title, xlab=lab)        #pro = F 代表次數(預設) ; pro = T 為機率值
range(iris[,1])
hist(iris[,1], breaks=seq(3.5, 8.5, length=50),main=title, xlab=lab)
hist(iris[,1], breaks=seq(3.5, 8.5, length=50),main=title, xlab=lab, pro=T)

#76/208 Dotplot/DotChart
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")   #VADeaths

#77/208 Stripchart 帶形圖
attach(OrchardSprays)
OrchardSprays[1:5,]
stripchart(decrease~treatment, xlab="decrease", ylab="treatment")
detach()

#78/208 Density plots
plot(density(iris$Sepal.Length))
hist(iris$Sepal.Length, probability = T)
points(density(iris$Sepal.Length), type = "l")

hist(iris[,1], breaks=15, main=title, xlab=lab, col="green", pro=T)
lines(density(iris[,1], width=0.6, n=200))

#84/208 Normal Quantiles
x <- seq(-3, 3, 0.01)
plot(x, dnorm(x), main = "standard normal", type = "l", lwd = 2, xaxt = "n")
p <- c(0.05, seq(0.1, 0.9, 0.1), 0.95)
q <- round(qnorm(p), 2)
rbind(p, q)

abline(v = q, col = "blue")
abline(h = 0, col = "black")
text(q, -0.02, srt = 45, adj = 1, labels = q, xpd = T)
y <- dnorm(x)
polygon(c(x[x <= qnorm(0.05)], qnorm(0.05)), c(y[x <= qnorm(0.05)], y[x == -3]), col = "lightgreen")
text(-1.9, 0.03, "5%", col = "red")
text(-1.6, 0.05, "10%", col = "red")

#87/208 qqnorm, qqline, qqplot
par(mfrow = c(1, 2))
set.seed(12345)
n <- 100 ; mu <- 0.5 ; sigma <- 0.15
x <- rnorm(n, mu, sigma)
hist(x, freq = FALSE, ylim = c(0, 3), main = "")
y <- seq(0, 1, lenght = n)
lines(y, dnorm(y, mu, sigma), type = "l")
qqnorm(x, main = "rnorm(mu = 0.5, sigma = 0.15)")
qqline(x, col = "red")

qqplot(x, rnorm(300))
qqline(x, col = 2)
qqplot(scale(x), rnorm(300))
qqline(scale(x), col = 2)

#89/208 課堂練習
my.qqplot <- function(x){
  x.mean <- mean(x)
  x.var <- var(x)
  n <- length(x)
  
  z <- (x - x.mean) / sqrt(x.var)
  z.mean <- mean(z)
  z.var <- var(z)
  z.sort <- sort(z)
  
  k <- 1 : n
  p <- (k - 0.5) / n
  q <- qnorm(p)
  plot(q, z.sort, xlim = c(-3, 3), ylim = c(-3, 3))
  title("QQ plot")
  lines(q, q, col = 2)
}

qqnorm(iris[, 1])
qqline(iris[, 1])

qqnorm(scale(iris[, 1]))
qqline(scale(iris[, 1]))

my.qqplot(iris[, 1])

#95/208 Time Series
data(UKLungDeaths) # total, male, female death
ts.plot(ldeaths, mdeaths, fdeaths, xlab="year", ylab="deaths", lty=c(1:3))
data(sunspots)
plot(sunspots) # sunspots is ts class

#96/208
par(mfrow = c(1, 1))
cell.raw <- read.table("dataset/trad_alpha103.txt", row.names=1, header=T)
head(cell.raw)
cell.xdata <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))
y.C <- as.integer(cell.raw[,1])
table(y.C)
no.cluster <- length(unique(y.C))
p <- ncol(cell.raw) - 1
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
ycolors <- cellcycle.color[y.C+1]
my.pch <- c(1:no.cluster)[y.C+1]
phase <- c("G1", "S", "S/G2", "G2/M", "M/G1")
matplot(t(cell.xdata), lty=1, type = "l", ylab="gene expression",
        col=ycolors, xlab="time", main="Time series", xaxt="n")
time.label <- parse(text=paste("t[",0:p,"]",sep=""))
axis(1, 1:(p+1), time.label)
legend("bottom", legend=phase, col=cellcycle.color, lty=1, horiz = T, lwd=2)

#98/208 Pie Charts and Variants
slices <- summary(veteran$celltype)
p <- floor(100*slices/sum(slices))
pie3D(slices, labels=paste0(names(slices), " (",p, "%)"), explode=0.1)

#100/208 Scatter Plots
xlab <- names(iris)[1]
ylab <- names(iris)[2]
title <- paste(ylab, "against", xlab, " of Iris Data")
x <- iris[,1]
y <- iris[,2]
plot(x, y, col="red", xlab=xlab, ylab=ylab, main=title)

plot(y~x, xlab=xlab, ylab=ylab, xlim=c(1.5,9), ylim=c(1.5,9), type="n")
points(x[1:50], y[1:50], col="red")
points(x[51:100], y[51:100], col="blue")
points(x[101:150], y[101:150], col="green")
abline(lm(y~x))