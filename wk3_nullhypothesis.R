setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Data-Science-Statistical-Inference")
# one group
library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)

# two groups
library(datasets); data(ChickWeight); library(reshape2)
##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)
