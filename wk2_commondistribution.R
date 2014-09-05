setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
choose(8, 7) * 0.5^8 + choose(8, 8) * 0.5^8
pbinom(6, size = 8, prob = 0.5, lower.tail = FALSE)

qnorm(.95, mean = 0, sd = 1)

pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)
pnorm(2.8, lower.tail = FALSE)
qnorm(.75, mean = 1020, sd = 50, lower.tail = T)

ppois(3, lambda = 2.5 * 4)

pbinom(2, size = 500, prob = 0.01)
ppois(2, lambda = 500 * 0.01)
