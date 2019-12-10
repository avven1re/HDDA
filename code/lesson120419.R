#C01 Dimension Reduction 
#browseURL("http://www.hmwu.idv.tw/web/R/C01-hmwu_R-DimensionReduction.pdf")
#lesson112719

#9/144 Spectral Decomposition
X <- iris[,1:4]
(S <- cov(X))
(e <- eigen(S))

D <- diag(e$values)
C <- e$vectors
C%*%D%*%t(C)  #equal to S = cov(X)

#13/144 SVD: Making Approximations
iris.sub <- iris[sample(1:150, 8),1:4]
iris.sub

M.svd <- svd(iris.sub)
M.svd
M.svd$u %*% (diag(M.svd$d) %*% t(M.svd$v)) #equal to iris.sub

# use the first two values to approximate
d.sub <- diag(M.svd$d[1:2])
u.sub <- as.matrix(M.svd$u[, 1:2])
v.sub <- as.matrix(M.svd$v[, 1:2])
iris.sub.approx <- u.sub %*% d.sub %*% t(v.sub)
iris.sub.approx

# compute the sum of squared errors
sum((iris.sub - iris.sub.approx)^2)

#14/144
# require packages: locfit, tiff, fftwtools
library(EBImage) # (Repositories: BioC Software)
#use 'EBImage' or 'jpeg' packages to import image then draw it by 'graphics::image()'
lena <- EBImage::readImage("dataset/lena.jpg")
dims <- dim(lena)
dims

plot(c(0, dims[1]), c(0, dims[2]), type='n', xlab="", ylab="")
graphics::rasterImage(lena, 0, 0, dims[1], dims[2])

library(jpeg)
lena <- jpeg::readJPEG("dataset/lena.jpg")

#15/144
lena.flip <- EBImage::Image(flip(lena))
# convert RGB to grayscale
red.weight <- .2989
green.weight <- .587
blue.weight <- 0.114

{lena.gray <- red.weight * EBImage::imageData(lena.flip)[,,1] +
green.weight * EBImage::imageData(lena.flip)[,,2] +
 blue.weight * EBImage::imageData(lena.flip)[,,3]}
dim(lena.gray)

lena.gray[1:5, 1:5]
image(lena.gray, col = grey(seq(0, 1, length = 256)))

#16/144
lena.svd <- svd(lena.gray)
d <- diag(lena.svd$d)
dim(d)

u <- lena.svd$u
v <- lena.svd$v
plot(1:length(lena.svd$d), lena.svd$d, pch=19, xlab="i-th lena.svd$d", ylab="lena.svd$d")

used.no <- 20
u.sub <- as.matrix(u[, 1:used.no])
v.sub <- as.matrix(v[, 1:used.no])
d.sub <- as.matrix(d[1:used.no, 1:used.no])
lena.approx <- u.sub %*% d.sub %*% t(v.sub)
image(lena.approx, col = grey(seq(0, 1, length = 256)))

#21/144 PCA: General Methodology
x <- iris[, 1:4]
(covx <- cov(x))
e <- eigen(covx)
V <- e$vectors
V.inverse <- solve(e$vectors)
covx.hat <- V %*% diag(e$values) %*% V.inverse
covx.hat # same with covx