setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
library(datasets)
data(mtcars)
mn<- mean(mtcars$mpg)
s <- sd(mtcars$mpg)
z <- qnorm(.05)
mu0 <- mn-z*s/sqrt(nrow(mtcars))
mu0


m4 <- mtcars$mpg[mtcars$cyl==4.]
m6 <- mtcars$mpg[mtcars$cyl==6.]
p <- t.test(m4,m6, paired=F, alternative='two.sided',var.equal = F)

n <- 100
avg <- 3
sd <- 1.1
z <- .05
avg + c(-1,1)*qnorm(.05)*sqrt((n*sd)/n)*(1/n)^.5


ans <- round(pbinom(54, prob = .5, size = 100, lower.tail = FALSE),4)

pv <- ppois(15800 - 1, lambda = 520 * 30, lower.tail = FALSE)
pnorm(15800 / 30, mean = 520, sd = sqrt(520 / 30), lower.tail = FALSE)

m1 <- 10; m2 <- 11
n1 <- n2 <- 100
s <- 4
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))


power <- pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)

n <- (qnorm(.95) + qnorm(.8)) ^ 2 * .04 ^ 2 / .01^2

mpg8 <- mtcars$mpg[mtcars$cyl==8]
mpg6 <- mtcars$mpg[mtcars$cyl==6]
p <- t.test(mpg8, mpg6, paired = FALSE, alternative="two.sided", var.equal=TRUE)$p.value
mixprob <- (n8 - 1) / (n8 + n6 - 2)
s <- sqrt(mixprob * s8 ^ 2  +  (1 - mixprob) * s6 ^ 2)
z <- (m8 - m6) / (s * sqrt(1 / n8 + 1 / n6))
pz <- 2 * pnorm(-abs(z))
## Hand calculating the T just to check
#2 * pt(-abs(z), df = n8 + n6 - 2)