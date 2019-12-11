#HDDA exam2
#2

install.packages("pheatmap")
library(pheatmap)

wine <- read.table("dataset/wine.data", sep = ",")

winen <- rep(NaN, length(wine[, 1]))
for (i in 1 : length(wine[, 1])) {
if(wine[i, 1] == 1){winen[i] <- c("type1")}
  else if(wine[i, 1] == 2){winen[i] <- c("type2")}
  else {winen[i] <- c("type3")}
}
typeNO <- wine[, 1]
wineclus <- as.matrix(cbind(winen, typeNO))

names(wineclus) <- c("type", "typeNO")
row.names(wineclus) <- paste0("row_", seq(nrow(wineclus)))
head(wineclus)
#windows()

pheatmap(scale(wine[, 2 : 14]), annotation_row = wineclus)

#2-2
wine_p <- princomp(scale(wine[, 2 : 14]))

windows()
plot(wine_p$scores[, 1], wine_p$scores[, 2], col = wine[, 1], main = "PCA")
legend("topright", c("type1", "type2", "type3"), col = c(1, 2, 3), pch = 1, text.col = c(1, 2, 3))

wine.sd <- t(scale(wine[2 : 14]))
wine.cor <- cor(wine.sd)
wine.dis <- sqrt(2 * (1 - wine.cor))

wine_mds <- cmdscale(wine.dis, k = 10)


windows()
plot(wine_mds[, 1], wine_mds[, 2], col = wine[, 1], main = "MDS")
legend("topright", c("type1", "type2", "type3"), col = c(1, 2, 3), pch = 1, text.col = c(1, 2, 3))


library(vegan)

wine_isom <- isomap(wine.dis, k = 3)

windows()
plot(wine_isom, col = wine[, 1], main = "ISOMAP")
legend("topright", c("type1", "type2", "type3"), col = c(1, 2, 3), pch = 1, text.col = c(1, 2, 3))


#2-3
library(e1071)
x <- wine[, 2 : 14]
y <- wine[, 1]
model <- svm(x, y)
pred <- predict(model, x)
accuracy <- sum(diag(table(pred, y))) / length(y)
accuracy

#2-4

#PCA
svmacc <- function(DR_data, num_dimesions, data_cate){
  acc_vec <- rep(NaN, num_dimesions)
  
  for (i in 1 : num_dimesions) {
  x <- DR_data[, i]
  y <- data_cate
  model <- svm(x, y)
  pred <- predict(model, x)
  acc_vec[i] <- sum(diag(table(pred, y))) / length(y)
  }
  acc_vec
}

wine_svm_pca <- svmacc(wine_p$scores, 10, wine[, 1])

wine_svm_mds <- svmacc(wine_mds, 10, wine[, 1])

wine_svm_isom <- svmacc(wine_isom$points, 10, wine[, 1])

plot(wine_svm_pca, type = "l", ylim = c(0, 0.05))
points(wine_svm_mds, type = "l", col = "green", lty = 2)
points(wine_svm_isom, type = "l", col = "red", lty = 3)
legend("topright", c("PCA", "MDS", "ISOMAP"), col = c("black", "green", "red"), lty = c(1, 2, 3))