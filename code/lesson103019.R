#103019 browseURL("http://www.hmwu.idv.tw/web/R_AI_M/AI-M3-hmwu_R_DataManagement_v2.pdf")
library(mice)
library(VIM)

#17/70
mydata <- airquality
mydata[4:10, 3] <- rep(NA, 7)
mydata[1:5, 4] <- NA

summary(mydata)

md.pattern(mydata)
mydata.aggrplot <- aggr(mydata, col=c('lightblue','red'), numbers=TRUE,
                        prop = TRUE, sortVars=TRUE,
                        labels=names(mydata), cex.axis=.7, gap=3)