setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
# jackknife
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
theta <- median(x)
jk <- sapply(1:n, function(i) median(x[-i]))
thetaBar <- mean(jk)
biasEst <- (n - 1) * (thetaBar - theta)
seEst <- sqrt((n - 1) * mean((jk - thetaBar)^2))
c(biasEst, seEst)

library(bootstrap)
temp <- jackknife(x, median)
c(temp$jack.bias, temp$jack.se)

# bootstrap
B <- 1000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(0.025, 0.975))
par(mfcol=c(1,1))
png('bootstrap.png')
hist(medians)
dev.off()

data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)
# permutation tests
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"), ]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1:10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)
png('permutation.png')
hist(permutations)
dev.off()