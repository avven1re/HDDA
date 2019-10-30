#packages for preparing

#A05 ppt
browseURL("http://www.hmwu.idv.tw/web/R/A05-hmwu_R-Graphics&Visualization.pdf")

#Colors
install.packages("RColorBrewer")  #at 47/208 RColorBrewer
library(RColorBrewer)

install.packages("fields")        #at 50/208 fields
library(fields)

install.packages("corrplot")      #at 52/208 CorrPlot
library(corrplot)

#Charts
install.packages("plotrix")       #at 98/208 Plotrix
library(plotrix)

install.packages("survival")      #at 98/208 Survival
library(survival)

install.packages("plot3D")
library("plot3D")

#ggplot
install.packages("ggplot2")
library(ggplot2)

install.packages("gridExtra")     #for gird ggplot(one page include more than one ggplot)
library(gridExtra)

#GoogleMap
browseURL("http://www.hmwu.idv.tw/web/R/E04-hmwu_R-Map.pdf")

install.packages("RgoogleMaps")
library(RgoogleMaps)

install.packages("ggmap")
library(ggmap)

install.packages("mapproj")
library(mapproj)

install.packages("BiocManager") 
BiocManager::install("EBImage")
library(EBImage)

install.packages("plyr")
library(plyr) 

install.packages("maps")
library(maps)

install.packages("maptools")
library(maptools)

install.packages("mapdata")
library(mapdata)

install.packages("sf")
library(sf)

#Missing value
install.packages("mice")
library(mice)

install.packages("VIM")
library(VIM)