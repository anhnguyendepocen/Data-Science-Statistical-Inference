setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
# Q1
a <- c(140,138,150,148,135)
b <- c(132, 135,151,146,130)
t.test(a,b,paired=T,var.equal = T)

# Q2
m <- 1100
s <- 30
n <- 9
z <- qt(.975,n)
m + c(-1,1)*z*sqrt((n*s^2)/n)*(1/n)^.5

# Q3
coke <- c(1,1,1,0)
pessi <- c(0,0,0,1)
t.test(coke,pessi,var.equal = TRUE, paired=T)

# Q4
1-pbinom(10, size = 1787, prob = 0.01, lower.tail = FALSE)


# Q5
m.t <- -3
m.p <- 1
s.t <- 1.5
s.p <- 1.8
n<-9
pt(0.95, 18, lower.tail = FALSE)

m4 <- rnorm(n, m.t,s.t)
m6 <- rnorm(n, m.p,s.p)
p <- t.test(m4, m6, paired = FALSE, alternative="two.sided", var.equal=FALSE)$p.value


# Q7
power.t.test(n = 100, delta = 0.01, sd = .04, type = "one.sample", alt = "one.sided")$power
?power.t.test

# Q8
power.t.test(power = 0.9, delta = .01, sd = .04, type = "one.sample", alt = "one.sided")$n

# Q10
s <- 12
ma <- 44
mb <- 42.04
n <- 288
z <- (ma-mb) / (s * sqrt(1 / n + 1 / n))
pz <- 2 * pnorm(-abs(z))

# Q11
p.adjust(0.05, method = "BH")
