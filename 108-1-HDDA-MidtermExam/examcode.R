#HDDA Mid exam
#410578035-統計四-楊閔文

#1
#(a)
mamm <- read.table("108-1-HDDA-MidtermExam/mammographic_masses.data", sep = ",", na.strings = "?")
names(mamm) <- c("BI-RADS", "Age", "Shape", "Margin", "Density", "Severity")

summary(mamm)

library(mice)
library(VIM)

md.pattern(mamm)

mamm.aggrplot <- aggr(mamm, col = c("green", "red"), nubers = T, prop = T, sortVars = T, labels = names(mamm))















































