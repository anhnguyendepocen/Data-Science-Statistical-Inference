setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
n <- 10000
means <- cumsum(rnorm(n))/(1:n)
library(ggplot2)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
png('asymptopia.png')
g
dev.off()

means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
png('asymptopia2.png')
g
dev.off()

library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12

round(1/sqrt(10^(1:6)), 3)
0.56 + c(-1, 1) * qnorm(0.975) * sqrt(0.56 * 0.44/100)
binom.test(56, 100)$conf.int

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
df <- data.frame(pvals=pvals,coverage=coverage)
g <- ggplot(df,aes(x=pvals,y=coverage))
png('simulation.png')
g + geom_line(size=2) + geom_hline(yintercept=.95)
dev.off()

n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage2 <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
df2 <- data.frame(pvals=pvals,coverage=coverage2)
g2 <- ggplot(df2,aes(x=pvals,y=coverage2))
png('simulation2.png')
g2 + geom_line(size=2) + geom_hline(yintercept=.95)
dev.off()

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
    phats <- (rbinom(nosim, prob = p, size = n) + 2)/(n + 4)
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
df3 <- data.frame(pvals=pvals,coverage=coverage)
g3 <- ggplot(df3,aes(x=pvals,y=coverage))
png('simulation3.png')
g3 + geom_line(size=2) + geom_hline(yintercept=.95)
dev.off()

x <- 5
t <- 94.32
lambda <- x/t
round(lambda + c(-1, 1) * qnorm(0.975) * sqrt(lambda/t), 3)
poisson.test(x, T = 94.32)$conf
