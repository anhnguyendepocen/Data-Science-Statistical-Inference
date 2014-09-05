# 2
mean<-80
sd <- 10
pnorm(70, mean=80, sd=10, lower.tail=T)

# 3
qnorm(.95, mean=1100, sd=75, lower.tail = T)

# 4
qnorm(.95, mean=1100, sd=75/sqrt(100), lower.tail = T)

# 5
pbinom(3, size = 5, prob = 0.5, lower.tail = FALSE)

# 8
round(pnorm(0.5, mean = 0.5, sd = sqrt(1 / 12 / 1000), lower.tail = FALSE), 3)

# 9
ppois(10, lambda = 5 * 3)

