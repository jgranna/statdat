### Frequentistische Interpretation KI ###

# Benoetigte Pakete laden
library(data.table)
library(plotrix)

# Grundgesamtheit simulieren
miete_bar_true <- 600
miete <- miete_bar_true + rnorm(10000, mean = 0, sd = 100)

# Simulation visualisieren
summary(miete)
hist(miete)

# Stichproben ziehen und KI berechnen
smpl <- sample(miete, 100)
mean(smpl)
left <- NA
right <- NA
m <- NA
cols <- NA
n <- 100
for(i in 1:100) {
  smpl <- sample(miete, 100)
  s<- sd(smpl)
  m[i]  <- mean(smpl)
  error <- qt(0.975,df=n-1)*s/sqrt(n)
  left [i] <- m[i]-error
  right[i] <- m[i]+error
  cols[i]  <- ifelse(between(600, left[i], right[i]), 1, 2)
}

# visualisieren
plotCI(1:100, m, ui = right, li = left,
           xlab = "stichprobe", ylab = "miete", col = cols)
abline(h = 600, col = 2)

# ueberpruefen, wie viele KI den wahren Wert umspannen
sum(left <= 600 & right >= 600) / 100
