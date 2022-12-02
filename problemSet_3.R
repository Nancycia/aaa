myName = 'Lintong Li'

library(dplyr)
library(ggplot2)
#1
iris.vers = subset(iris, Species =="versicolor")
ans_1 = iris.vers
#2
sepal.dif = iris.vers$Sepal.Length - iris.vers$Sepal.Width
ans_2 = sepal.dif
#3
iris.vers = data.frame(iris.vers,sepal.dif)
ans_3 = iris.vers
#4
vx = (apply(mtcars,2,class))
ans_4 = vx
#5
newmtc = mtcars
newmtc$am = as.integer(mtcars$am)
newmtc$cyl = as.integer(mtcars$cyl)
newmtc$vs = as.integer(mtcars$vs)
x = unlist(lapply(newmtc,class))
ans_5 = x
#6
newmtc = round(newmtc,1)
ans_6 = newmtc
#7
iris_7<- filter(iris, Species == "virginica" & Sepal.Width > 3.5)
ans_7 <- iris_7
#8
iris_8 = iris[iris$Species == "virginica" & iris$Sepal.Width > 3.5, 1:4]
ans_8 = iris_8
#9
r_id = row.names(iris[iris$Species == "virginica" & iris$Sepal.Width > 3.5, 1:4])
ans_9 = r_id
#10
data(diamonds)
diam_10 = sum(diamonds$cut == "Ideal" & diamonds$carat < 0.21)
ans_10 = diam_10
#11
diam_11 = sum ((diamonds$x + diamonds$y + diamonds$z) > 40)
ans_11 = diam_11
#12
diam_12 = sum(diamonds$price > 10000 | diamonds$depth >= 70)
ans_12 = diam_12
#13
diam_13 = diamonds[c(67,982), c('color','y')]
ans_13 = diam_13
#14
diam_14 = diamonds[c(453, 792, 10489), ]
ans_14 = diam_14
#15
diam_15 = head(as_tibble(diamonds[ , c('x','y','z')]),10)
ans_15 = diam_15
#16
newdiam = diamonds[1:1000,]
ans_16 = newdiam
#17
newdiam_17 = head(arrange(newdiam, price))
ans_17 = newdiam_17
#18
set.seed(56)
diam750 = sample_n(diamonds, 750)
ans_18 = diam750
#19
sum_diam750 = summary(diam750)
ans_19 = sum_diam750

