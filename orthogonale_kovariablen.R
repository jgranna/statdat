## wenn covariablen unabh dann p Werte und anova() gleich
set.seed(111)
x1 <- seq(-4, 4, length.out = 101)
y <- - 3*x1 + 2*x1^2 + rnorm(101, sd = 10)
par(mfrow = c(1, 1))
plot(x1, y)

# lm anpassen
m <- lm(y ~ x1 + I(x1^2))
m0 <- lm(y ~ x1)
summary(m)
anova(m, m0)
coef(summary(m))
cor(x1, x1^2)
