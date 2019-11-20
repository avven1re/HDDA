#111319 browseURL("http://www.hmwu.idv.tw/web/R/C01-hmwu_R-DimensionReduction.pdf")

# 12/95

iris.sub <- iris[sample(1:150, 8),1:4]
iris.sub
M.svd <- svd(iris.sub)
M.svd
M.svd$u %*% (diag(M.svd$d) %*% t(M.svd$v)) 

d.sub <- diag(M.svd$d[1:2])   
u.sub <- as.matrix(M.svd$u[, 1:2])
v.sub <- as.matrix(M.svd$v[, 1:2])
iris.sub.approx <- u.sub %*% d.sub %*% t(v.sub)
iris.sub.approx

sum((iris.sub - iris.sub.approx)^2)


# 13/95

# require packages: locfit, tiff, fftwtools
library(EBImage) #(Repositories: BioC Software)
lena <- readImage("pic/lena.jpg")
dims <- dim(lena)
dims

plot(c(0, dims[1]), c(0, dims[2]), type='n', xlab="", ylab="")
rasterImage(lena, 0, 0, dims[1], dims[2])


#  14/95

lena.flip <- Image(flip(lena))
# convert RGB to grayscale
red.weight   <- .2989
green.weight <- .587
blue.weight  <- 0.114

lena.gray <- red.weight   * imageData(lena.flip)[,,1] + 
  green.weight * imageData(lena.flip)[,,2] + 
  blue.weight  * imageData(lena.flip)[,,3]
dim(lena.gray)
lena.gray[1:5, 1:5]
image(lena.gray, col = grey(seq(0, 1, length = 256)))


# 15/95

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

#16/95
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

#21/144 PCA
x <- iris[, 1:4]
(covx <- cov(x))
e <- eigen(covx)
V <- e$vectors
V.inverse <- solve(e$vectors)
covx.hat <- V %*% diag(e$values) %*% V.inverse
covx.hat # same with covx

#23/144
# PCA for iris data
z <- as.matrix(x) %*% e$vectors[, 1:2]
plot(z[, 1], z[, 2], col=iris[, 5])

#31/144
m1 <- matrix(sample(1:16,16),4,4)
m1

m1.scale.svd <- svd(scale(m1))
m1.scale.svd

m1.pca <- prcomp(m1, scale=T)
m1.pca

pca2 <- princomp(m1, cor=T)
pca2$scores
pca2$loadings

#33/144
cell.matrix <- read.table("dataset/YeastCellCycle_alpha.txt", header=TRUE, row.names=1)
n <- dim(cell.matrix)[1]
p <- dim(cell.matrix)[2]-1
cell.data <- cell.matrix[,2:p+1]
gene.phase <- cell.matrix[,1]
phase <- unique(gene.phase)
phase.name <- c("G1", "S", "S/G2", "G2/M", "M/G1")
cell.sdata <- t(scale(t(cell.data)))
rc <- rainbow(5)[as.integer(gene.phase)]
cc <- rainbow(ncol(cell.sdata))
hv <- heatmap(cell.sdata, col = GBRcol, scale = "column", Colv=NA, Rowv=NA,
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "Times", ylab = "Genes", main = "Heatmap of Microarray Data")

#34/144
cell.pca <- princomp(cell.sdata, cor=TRUE, scores=TRUE)
# 2D plot for first two components
pca.dim1 <- cell.pca$scores[,1]
pca.dim2 <- cell.pca$scores[,2]
plot(pca.dim1, pca.dim2, main="PCA for Cell Cycle Data on Genes", xlab="1st PCA
Component", ylab="2nd PCA Component",col=c(phase), pch=c(phase))
legend(3, 4, phase.name, pch=c(phase), col=c(phase))
# shows a screeplot.
plot(cell.pca)
biplot(cell.pca)

#35/144 Loadings Plot
# loadings plot
plot(loadings(cell.pca)[,1], loadings(cell.pca)[,2], xlab="1st PCA",
     ylab="2nd PCA", main="Loadings Plot", type="n")
text(loadings(cell.pca)[,1], loadings(cell.pca)[,2], labels=paste(1:p))
abline(h=0)
abline(v=0)

# print loadings
loadings(cell.pca)
summary(cell.pca)

#36/144
library(MASS)
mu <- c(2, -1)
Sigma <- matrix(c(2.4, -0.5, -0.5, 1), 2)
n <- 250
X <- mvrnorm(n, mu, Sigma)
mycol <- terrain.colors(n)
sorted.x1 <- sort(X[,1])
order.x1 <- order(X[,1])
id <- 1:n
sorted.id <- id[order.x1]
x1.col <- mycol[order(sorted.id)]
par(mfrow=c(1, 2))
plot(X, col=x1.col, pch=16, main="simulated bivariate normal")
abline(h=0, v=0, col="gray")
X.pca <- princomp(X, cor = TRUE)
X.pca$sdev
X.pca$loadings
plot(X.pca$scores, col=x1.col, pch=16, main="PCA")
abline(h=0, v=0, col="gray")

#37/144
pca.pkg <- c("FactoMineR", "factoextra", "corrplot")
install.packages(pca.pkg)
lapply(pca.pkg, library, character.only=TRUE)
data(decathlon2) # 十項全能
head(decathlon2) # 100米, 跳遠, 鉛球, 跳高, 400米, 110米跨欄, 鐵餅, 撐竿跳高, 標槍, 1500米
dim(decathlon2)
x <- decathlon2[,1:10]

#38/144
x.pca <- PCA(x, graph = FALSE)

#39/144
eig.val <- get_eigenvalue(x.pca)
eig.val
fviz_eig(x.pca, addlabels = TRUE, ylim = c(0, 50))
class(x.pca)