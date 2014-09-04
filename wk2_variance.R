setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
library(ggplot2)
nosim <- 1000
n <- 10
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))
1 / sqrt(n)

nosim <- 1000
n <- 10
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
1 / sqrt(12 * n)

nosim <- 1000
n <- 10
sd(apply(matrix(sample(0 : 1, nosim * n, replace = TRUE),
                nosim), 1, mean))
1 / (2 * sqrt(n))

library(UsingR); data(father.son);
x <- father.son$sheight
n<-length(x)
qplot(x)
round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)),2)
