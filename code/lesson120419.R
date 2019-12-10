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

#31/144 PCA and SVD
m1 <- matrix(sample(1:16,16),4,4)
m1

m1.scale.svd <- svd(scale(m1))
m1.scale.svd

m1.pca <- prcomp(m1, scale=T)
m1.pca

pca2 <- princomp(m1, cor=T)
pca2$scores
pca2$loadings

#32/144 PCA in R introduce 5 functions to do in R

#33-35/144
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
plot(X, col=x1.col, pch=16,
       main="simulated bivariate normal")
abline(h=0, v=0, col="gray")
X.pca <- princomp(X, cor = TRUE)
X.pca$sdev
X.pca$loadings
plot(X.pca$scores, col=x1.col, pch=16, main="PCA")
abline(h=0, v=0, col="gray")

#37-43/144
pca.pkg <- c("FactoMineR", "factoextra", "corrplot")
install.packages(pca.pkg)
lapply(pca.pkg, library, character.only=TRUE)
data(decathlon2) # 十項全能
head(decathlon2) # 100米, 跳遠, 鉛球, 跳高, 400米, 110米跨欄, 鐵餅, 撐竿跳高, 標槍, 1500米
dim(decathlon2)
# standardization # x <- scale(x)
x <- decathlon2[,1:10]
# (default, PCA {FactoMineR} standardizes the data automatically 
# PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)
x.pca <- PCA(x, graph = FALSE)
class(x.pca)
str(x.pca)
print(x.pca)

# get_eigenvalue(x.pca): Extract the eigenvalues/variances of principal components
# fviz_eig(x.pca): Visualize the eigenvalues
# get_pca_ind(x.pca), get_pca_var(x.pca): Extract the results for individuals and variables.
# fviz_pca_ind(x.pca), fviz_pca_var(x.pca): Visualize the results individuals and variables.
# fviz_pca_biplot(x.pca): Make a biplot of individuals and variables.

# Eigenvalues/Variances
eig.val <- get_eigenvalue(x.pca)
eig.val
# scree plot
fviz_eig(x.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(x.pca)
var
# Coordinates of variables
head(var$coord, 4)
# Correlation circle, variable correlation plots
fviz_pca_var(x.pca, col.var = "black")

head(var$cos2, 4)
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(x.pca, choice = "var", axes = 1:2)
# variables with low/mid/high cos2 values will be colored in blue/yellow/red
fviz_pca_var(x.pca, col.var = "cos2",
                 gradient.cols = c("blue", "yellow", "red"),
                 repel = TRUE) # Avoid text overlapping

head(var$contrib, 4)
corrplot(var$contrib, is.corr=FALSE)
# Contributions of variables to PC1
fviz_contrib(x.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(x.pca, choice = "var", axes = 2, top = 10)
# The total contribution to PC1 and PC2:
fviz_contrib(x.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(x.pca, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"))
set.seed(123)
var.kms <- kmeans(var$coord, centers = 3, nstart = 25)
kms.grp <- as.factor(var.kms$cluster)
# Color variables by kmeans' result
fviz_pca_var(x.pca, col.var = kms.grp, palette = c("blue", "green", "red"),
                 legend.title = "Cluster")

#44/144 Obtain PCA results for individuals
ind <- get_pca_ind(x.pca)
ind

# Coordinates of individuals
head(ind$coord, 3)

# Quality of individuals
head(ind$cos2, 3)

# Contributions of individuals
head(ind$contrib, 3)

#45-46/144 Graph of individuals
# color individuals by their cos2 values
fviz_pca_ind(x.pca, col.ind = "cos2", gradient.cols = c("blue", "black", "red"), repel = TRUE)
 # change the point size according the cos2 of the corresponding individuals
fviz_pca_ind(x.pca, pointsize = "cos2", pointshape = 21, fill = "lightblue", repel = TRUE)

fviz_pca_ind(x.pca, geom.ind = "point", col.ind = decathlon2[,13], palette = c("blue", "red"), legend.title = "Competition")
 # quality of representation (cos2) of individuals on the factor map
 fviz_cos2(x.pca, choice = "ind", top = 5)
 # Total contribution of individuals on PC1 and PC2
 fviz_contrib(x.pca, choice = "ind", axes = 1:2, top = 5)
 
#48-51/144 Biplot
 x <- c(-0.9, 0.6, 0.1)
 y <- c(-0.5, 0, 0.4)
 plot(x, y, xlim=c(-1, 1), ylim=c(-1, 1), main="Data Input Space")
 abline(h=0, v=0, col="blue", lwd=2)
 text(x+0.05, y+0.05, c("s1", "s2", "s3"), col="red")
 pca <- princomp(cbind(x, y)); ab <- pca$loadings
 arrows(-ab[1,1], -ab[2,1], ab[1,1], ab[2,1], col="green", angle = 10, lwd=2)
 text(-ab[1,1], -ab[2,1], "Comp.1")
 arrows(-ab[1,2], -ab[2,2], ab[1,2], ab[2,2], col="green", angle = 10, lwd=2)
 text(-ab[1,2], -ab[2,2], "Comp.2")
 
 iris.pca <- princomp(iris[,1:4])
 biplot(iris.pca, main="Biplot for iris data")

#54/144 ggbiplot for Iris Data
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
iris.prcomp <- prcomp(iris[,1:4], scale. = TRUE)
ggbiplot(iris.prcomp, groups = iris[,5])
ggbiplot(iris.prcomp, obs.scale = 1, var.scale = 1,
 groups = iris[,5], ellipse = TRUE, circle = TRUE) +
 scale_color_discrete(name = '') +
 theme(legend.direction = 'horizontal', legend.position = 'top')

#55-56/144 Creating A Biplot Using SVD
library(lattice)
state.spending <- read.table("StatePolicySpending.txt", header = T, row.names=1)
head(state.spending)
spend <- scale(state.spending)
spend.svd <- svd(spend, nu=2, nv=2)
D <- spend.svd$d[1:2]
V <- spend.svd$v[,1:2]
U <- spend.svd$u[,1:2]
# Create matrices for variables and observations by weighting singular vectors with
 # the square roots of the first two singular values. These will be used to construct
 # a symmetric biplot.

spend.var <- V * (rep(1, length(V[,1])) %*% t(D^.5))
spend.obs <- U * (rep(1, length(U[,1])) %*% t(D^.5))
row.names(spend.var) <- colnames(spend)
row.names(spend.obs) <- row.names(spend)
# Within the panel function, "panel.xyplot" draws the observation points and
# "panel.segments" draws the variable vectors. The first "panel.text" labels the vectors,
# and the second "panel.text" provides labels for relatively extreme observation points.
xyplot(spend.obs[,2] ~ spend.obs[,1],
 aspect = 1,
 panel = function(x, y) {
 panel.xyplot(x, y, col = "black")
 panel.segments(rep(0, length(spend.var[,1])),
 rep(0, length(spend.var[,1])),
 spend.var[,1], spend.var[,2], lty = 1, col = "blue")
 panel.text(spend.var[,1], spend.var[,2],
 row.names(spend.var), cex = .7, col = "green")
 panel.text(x[abs(x)>.7 | abs(y)>.7]+.04, y[abs(x)>.7 | abs(y)>.7],
 row.names(spend.obs)[abs(x)>.7 | abs(y)>.7], cex = .7,
 adj = 0, col= "red")
 },
 xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5),
 xlab = " ", ylab = " ")
# Calculate proportion of variance explained by
# first two pairs of singular vectors
var.explained <- sum(D^2)/sum(spend.svd$d^2)
var.explained

#64/144 Factor Analysis : factanal()
data(iris3)
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Species = rep(c("setosa","versicolor","virginica"), rep(50,3)))
## FA
data <- iris[,1:4]
class <- iris[,5]
iris.fa <- factanal(data, factors=1)
fa.dim1 <- as.matrix(data)%*%iris.fa$loadings[,1]