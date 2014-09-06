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

