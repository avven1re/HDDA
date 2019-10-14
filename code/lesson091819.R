## 2019/09/118 test code 

#Anscombe's Quartet dataset
anscombe

?mapply
#mapply(自訂函數, ...)

?apply
#apply(DATASET(Matrix or Vector), (1 = rows ; 2 = columns; c(1, 2) = both), FUN)

apply(anscombe, 2, mean)
apply(anscombe, 2, sd)

mapply(cor, anscombe[, 1 : 4], anscombe[, 5 : 8])
mapply(function(x, y) lm(y~x)$coefficient, anscombe[, 1 : 4], anscombe[, 5 : 8])

#Plotting
par(mfrow = c(2, 2))#mfrow = multi frme row ; mfcol = multi frame column
regplot <- function(x, y){
  plot(x, y)
  abline(lm(y~x), col = "red")
}
mapply(regplot, anscombe[, 1 : 4], anscombe[, 5 : 8])

############################################

#The Datasaurus Dozen
install.packages("datasauRus")
library(datasauRus)
?datasauRus



