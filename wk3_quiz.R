setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
library(datasets)
data(mtcars)
head(mtcars)
round(t.test(mtcars$mpg)$conf.int)

round(qt(.975,df=8)*1/3,2)
round(qt(.025,df=8)*1/3,2)

a <- mtcars[which(mtcars$cyl %in% c(4,6)),]
round(t.test(a$mpg~a$cyl,var.equal=T)$conf.int,1)

m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
confint <- as.vector(t.test(m4, m6, var.equal = TRUE)$conf.int)

n1 <- n2 <- 9
x1 <- -3  ##treated
x2 <- 1  ##placebo
s1 <- 1.5  ##treated
s2 <- 1.8  ##placebo
spsq <- ( (n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)

# Q1
x<-1100
s <- 30
n <- 9
x+c(-1,1)*qt(.975,9)*sqrt((n*s^2)/n)*(1/n)^.5

# Q2
2*3/qt(.975, df = 8)

# Q4
n <- n.o <- 10
mean <- 3
var <- .6
mean.o <- 5
var.o <- .68
mean.a <- mean.o-mean
var.a <- var.o-var
new <- mean+c(-1,1)*sqrt(var/9)
old <- mean.o+c(-1,1)*sqrt(var.o/9)
set.seed(1234)
new.data <- rnorm(n,mean,sqrt(var))
old.data <- rnorm(n.o,mean.o,sqrt(var.o))
t.test(new.data,old.data, var.equal = T)

# Q6
n.new <- n.old <- 100
avg.new <- 4
sd.new <- 0.5
avg.old <- 6
sd.old <- 2

# Q7
treat.avg <- -3
placebo.avg <- 1
treat.sd <- 1.5
placebo.sd <- 1.8
n<-9
treat.data <- rnorm(n, treat.avg, treat.sd)
placebo.data <- rnorm(n, placebo.avg,placebo.sd)
t.test(treat.data-placebo.data,conf.level = .9)
-4.023737+1.822361

####################
sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5

sp <- sqrt((9*30^2)/9)
x + c(-1,1)*qt(.975, 9)*sp*(1/9)^.5
