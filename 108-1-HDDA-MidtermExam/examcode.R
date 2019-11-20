#HDDA Mid exam
#410578035-統計四-楊閔文

#1
#(a)
mamm <- read.table("108-1-HDDA-MidtermExam/mammographic_masses.data", sep = ",", na.strings = "?")
<<<<<<< HEAD
names(mamm) <- c("BI-RADS", "Age", "Shape", "Margin", "Density", "Severity")
=======
>>>>>>> 31f6cecb4816dfb85af1d35ebacc71b8bf5f0de2

summary(mamm)

library(mice)
<<<<<<< HEAD
library(VIM)

md.pattern(mamm)

mamm.aggrplot <- aggr(mamm, col = c("green", "red"), nubers = T, prop = T, sortVars = T, labels = names(mamm))
=======
>>>>>>> 31f6cecb4816dfb85af1d35ebacc71b8bf5f0de2















































