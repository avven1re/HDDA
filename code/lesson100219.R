#100219
#A05

lab <- names(iris)[1]
title <- paste("Histogram of", lab)
hist(iris[, 1], main = title, xlab = lab)
range(iris[, 1])
hist(iris[, 1], breaks = seq(3.5, 8.5, length = 50), main = title, xlab = lab)
hist(iris[, 1], breaks = seq(3.5, 8.5, length = 50), main = title, xlab = lab, pro = T)
#pro = F for default=次數 ； pro = T for機率值。

##84/208
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

##87/208
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

##89/208
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