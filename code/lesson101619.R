##101619
#E04

#8/47 ggplot & 頁面分割
install.packages("gridExtra")
library(gridExtra)
h1 <- ggplot(data=iris, aes(x=Sepal.Length)) + geom_histogram()
h2 <- ggplot(data=iris, aes(x=Sepal.Length)) + geom_histogram(binwidth=1)
h3 <- ggplot(data=iris, aes(x=Sepal.Length)) +
  geom_histogram(color="black", fill="blue", bins = 10)
h4 <- ggplot(data=iris, aes(x=Sepal.Length, color=Species)) + geom_histogram(binwidth = 1)
grid.arrange(h1, h2, h3, h4, nrow=1, ncol=4)

#9/47 geom_histogram
p <- ggplot(data=iris, aes(x=Sepal.Length))
p <- p + geom_histogram()
p + facet_grid(Species~.)

# geom_density
ggplot(iris, aes(x=Sepal.Length)) + geom_density()
ggplot(iris, aes(x=Sepal.Length, color=Species)) + geom_density()
?geom_density

#12~13/47 geom_bar
p <- ggplot(mtcars, aes(x= cyl)) + geom_bar()
p
ggplot(mtcars, aes(x= cyl)) + geom_bar() + coord_flip()
p + labs(title = "Motor Trend Car Road Tests Data",
           x = "Number of cylinders", y = "Number of cars")

iris.mean <- aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean)
iris.mean     #aggregate 以Species 為根據做平均
mydata <- cbind(stack(iris.mean[,-1]), Species = iris.mean$Species)
mydata
ggplot(mydata, aes(x=ind, y=values, fill = Species)) + geom_bar(position="dodge", stat="identity")

#14/47
p <- ggplot(data=mtcars, aes(x=wt, y=mpg, label = rownames(mtcars)))
p + geom_point()
p + geom_text(size=3)
p + geom_label()

#15/47 geom_boxplot
mtcars$cyl <- factor(mtcars$cyl)
ggplot(data=mtcars, aes(x=cyl, y=disp)) + geom_boxplot()

#16/47
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width,
                      shape=Species, color=Species)) + geom_point()
p <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species, color=Species))
p <- p + geom_point()
p
p + geom_line(aes(y=Sepal.Width))
