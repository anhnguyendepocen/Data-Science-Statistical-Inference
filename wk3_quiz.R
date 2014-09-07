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
