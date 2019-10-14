#A05 ppt -> browseURL("http://www.hmwu.idv.tw/web/R/A05-hmwu_R-Graphics&Visualization.pdf")

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
<<<<<<< HEAD

#61/208 axis
plot(1:7, rnorm(7), xaxt = "n", frame = FALSE)
axis(1, 1:7, LETTERS[1:7], col = "green")
axis(3, 1:7, paste("test", LETTERS[1:7]), col.axis = "blue", las=2)
axis(4, lty=2, lwd = 2, las=2)
=======
>>>>>>> 54981e70c44194afdec8a874a31f499fc4587c28
