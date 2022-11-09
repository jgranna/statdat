miete_bar_true <- 600
miete <- miete_bar_true + rnorm(10000, mean = 0, sd = 100)
summary(miete)
hist(miete)
smpl <- sample(miete, 100)
mean(smpl)
left <- NA
right <- NA
m <- NA
n <- 100

for(i in 1:100) {
  smpl <- sample(miete, 100)
  s<- sd(smpl)
  m[i] <- mean(smpl)
  error <- qt(0.975,df=n-1)*s/sqrt(n)
  left [i] <- m[i]-error
  right[i] <- m[i]+error
}
library(plotrix)
plotCI(1:100, m, ui = right, li = left,
           xlab = "stichprobe", ylab = "miete")
abline(h = 600, col = 2)
sum(left <= 600 & right >= 600)
