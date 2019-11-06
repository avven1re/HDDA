#110619 browseURL("http://www.hmwu.idv.tw/web/R/B01-2-hmwu_R-MissingValuesOutliers.pdf")

#84/85
library(MASS)
cov(stackloss)

cov.mve(stackloss)$cov

par(mfrow=c(2,1))
library(MASS)
ccov <- cov(stackloss)
pca.scores <- as.matrix(stackloss) %*%
eigen(ccov)$vectors[,1:2]
plot(pca.scores, main="PCA", asp=1, type="n")
text(pca.scores, label=1:nrow(stackloss))
rcov <- cov.mve(stackloss)$cov
rpca.scores <- as.matrix(stackloss) %*%
  eigen(rcov)$vectors[,1:2]
plot(rpca.scores, main="Robust PCA", asp=1, type="n")
text(rpca.scores, label=1:nrow(stackloss))
