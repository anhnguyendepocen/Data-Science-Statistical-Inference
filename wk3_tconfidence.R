setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
require(ggplot2)
require(manipulate)
# t distribution compare with normal distribution
k <- 1000
xvals <- seq(-5, 5, length = k)
myplot <- function(df){
    d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                    x = xvals,
                    dist = factor(rep(c("Normal", "T"), c(k,k))))
    g <- ggplot(d, aes(x = x, y = y))
    g <- g + geom_line(size = 2, aes(colour = dist))
    g
}
png('t_distribution.png')
manipulate(myplot(mu), mu = slider(1, 20, step = 1))
dev.off()

pvals <- seq(.5, .99, by = .01)
myplot2 <- function(df){
    d <- data.frame(n= qnorm(pvals),t=qt(pvals, df),
                    p = pvals)
    g <- ggplot(d, aes(x= n, y = t))
    g <- g + geom_abline(size = 2, col = "lightblue")
    g <- g + geom_line(size = 2, col = "black")
    g <- g + geom_vline(xintercept = qnorm(0.975))
    g <- g + geom_hline(yintercept = qt(0.975, df))
    g
}
png('t_distribution2.png')
manipulate(myplot2(df), df = slider(1, 20, step = 1))
dev.off()

# sleep data
data(sleep)
head(sleep)
x1 <- sleep$extra[sleep$group == 1]
x2 <- sleep$extra[sleep$group == 2]
n1 <- length(x1)
n2 <- length(x2)
sp <- sqrt( ((n1 - 1) * sd(x1)^2 + (n2-1) * sd(x2)^2) / (n1 + n2-2)) # Pooled variance estimate
md <- mean(x1) - mean(x2)
semd <- sp * sqrt(1 / n1 + 1/n2)
md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd
t.test(x1, x2, paired = FALSE, var.equal = TRUE)$conf
t.test(x1, x2, paired = TRUE)$conf

png('ttest.png')
plot(c(0.5, 2.5), range(x1, x2), type = "n", frame = FALSE, xlab = "group", ylab = "Extra", axes = FALSE)
axis(2)
axis(1, at = 1 : 2, labels = c("Group 1", "Group 2"))
for (i in 1 : n1) lines(c(1, 2), c(x1[i], x2[i]), lwd = 2, col = "red")
for (i in 1 : n1) points(c(1, 2), c(x1[i], x2[i]), lwd = 2, col = "black", bg = "salmon", pch = 21, cex = 3)
dev.off()

g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)

# Pooled variance estimate
sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5

n1 <- length(g1); n2 <- length(g2)
sp <- sqrt( ((n1 - 1) * sd(x1)^2 + (n2-1) * sd(x2)^2) / (n1 + n2-2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
    md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd,
    t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
    t.test(g2, g1, paired = TRUE)$conf
)

# Chick weight
library(datasets); data(ChickWeight); library(reshape2)
##define weight gain or loss
head(ChickWeight)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW, gain = time21 - time0)
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
    t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
    t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)
g1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
g1 + geom_point()
