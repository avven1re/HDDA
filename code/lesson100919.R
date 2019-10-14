#100919

##148/208
install.packages("plot3D")
library("plot3D")
x <- iris$Sepal.Length
y <- iris$Sepal.Width
z <- iris$Petal.Length
w <- iris$Petal.Width
s <- iris$Species
par(mfrow = c(1,3), mai = c(0.3, 0.3, 0.3, 0.3))
scatter3D(x, y, z)
scatter3D(x, y, z, pch = 18, clab = c("Sepal", "Width (cm)"), main = "Iris data",
          xlab = "Sepal.Length", zlab = "Petal.Length", ylab = "Sepal.Width")
scatter3D(x, y, z, colvar = as.integer(s), col = "blue", pch = 19, cex = 1)

##149/208
par(mfrow = c(1,3), mai = c(0.3, 0.3, 0.2, 0.3))
# grey background with white grid lines
scatter3D(x, y, z, bty = "f", colkey = FALSE, ticktype = "detailed",
          theta=0, phi=60)
# theta: the azimuthal direction, phi: the co-latitude.
scatter3D(x, y, z, bty = "g", pch = 18,
          col = as.integer(s) + 1,
          pch = 18, ticktype = "detailed",
          colkey = list(at = c(2, 3, 4), side = 1,
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("setosa", "versicolor", "virginica")))
text3D(x, y, z, labels = as.integer(s), colvar = w, bty = "b2")
# "b2": back panels and grid lines are visible
# hist3D, text3D
# scatter2D(x, y, colvar = z, pch = 16)

##151/208
install.packages("rgl")
library(rgl)
swissroll <- function(n, sigma=0.05){
  angle <- (3*pi/2)*(1+2*runif(n));
  height <- runif(n);
  xdata <- cbind(angle*cos(angle), height, angle*sin(angle))
  xdata <- scale(xdata) + matrix(rnorm(n*3, 0, sigma), n, 3)
  order.angle <- order(angle)
  sort.angle <- sort(order.angle, index.return=TRUE)
  col.id <- rainbow(n)
  my.color <- col.id[sort.angle$ix]
  colnames(xdata) <- paste("x", 1:3, sep="")
  return(list(xdata=xdata, angle=angle, color=my.color))
}
swissdata <- swissroll(500)
xdata <- swissdata$xdata
x.color <- swissdata$color
open3d()
plot3d(xdata[,1], xdata[,2], xdata[,3], col=x.color,
       size=3, xlab="", ylab="", zlab="", axes = FALSE)

##164/208
install.packages("fields")
library(fields)
gbr <- two.colors(start="green", middle="black", end="red")
cell.raw <- read.table("data/trad_alpha103.txt", row.names=1, header=T)
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
               xlab = "Times", ylab = "Genes",main = "Heatmap of Microarray Data")
hv2 <- heatmap(cell.data, col = gbr, Colv=NA, Rowv=NULL,
               RowSideColors = rc,
               ColSideColors = cc, margins = c(5,10),
               xlab = "Times", ylab = "Genes",main = "Heatmap of Microarray Data")
dd <- as.dendrogram(hclust(as.dist(1-cor(t(cell.data)))))
hv3 <- heatmap(cell.data, col = gbr, Colv=NA, Rowv=dd,
               RowSideColors = rc,
               ColSideColors = cc, margins = c(5,10),
               scale = "row",
               xlab = "Times", ylab = "Genes",main = "Heatmap of Microarray Data")