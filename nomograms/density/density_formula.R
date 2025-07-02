rm(list=ls())
library(oce)
SAlim <- c(28, 36)
CTlim <- c(0, 25)
n <- 10
SAg <- seq(SAlim[1], SAlim[2], length.out = n)
CTg <- seq(CTlim[1], CTlim[2], length.out = n + 1)
g <- expand.grid(SA = SAg, CT = CTg)
g$sigma0 <- gsw_sigma0(g$SA, g$CT)
sigma0 <- matrix(g$sigma, nrow = n, byrow = FALSE)
if (!interactive()) png("density_formula_%02d.png", units = "in", width = 7, height = 7, res = 500)
contour(SAg, CTg, sigma0, xaxs = "i", yaxs = "i")
summary(m1 <- lm(sigma0 ~ CT + SA, data = g)) # RSE 0.2846
summary(m2 <- lm(sigma0 ~ CT + I(CT^2) + SA, data = g)) # RSE 0.0532
summary(m3 <- lm(sigma0 ~ CT + I(CT^2) + I(CT^3) + SA, data = g)) # RSE 0.0517
summary(m4 <- lm(sigma0 ~ CT + I(CT^2) + I(CT^3) + I(CT^4) + SA, data = g)) # RSE 0.05198
m <- m3

# m3 is best; m4 has higher RSE.
#names(summary(m))
RSE <- summary(m)$sigma # "Residual standard error" in summary(m3)


par(mfrow = c(1, 1), mar = c(3, 3, 1, 1), mgp = c(2.0, 0.7, 0))
# apar(mfrow = c(2, 1))
#plot(g$SA, g$sigma0)
#ylim <- par("usr")[3:4]
#scale <- diff(quantile(g$sigma, c(0.01, 0.99))) / 2
plot(g$SA, 100 * (g$sigma0 - predict(m))/mean(g$sigma0), ylab = "m1 % err")
if (!interactive()) dev.off()

cat("Next (m) is best i.t.o. misfit\n")
print(coef(m))

#  (Intercept)            CT       I(CT^2)      I(CT^3)            SA
# 9.679121e-01 -4.650992e-02 -6.342369e-03 3.588205e-05  7.687654e-01
