library(oce)
SAlim <- c(28, 36)
CTlim <- c(0, 25)
n <- 10
SAg <- seq(SAlim[1], SAlim[2], length.out = n)
CTg <- seq(CTlim[1], CTlim[2], length.out = n + 1)
g <- expand.grid(SA = SAg, CT = CTg)
g$spiciness0 <- gsw_spiciness0(g$SA, g$CT)
spiciness0 <- matrix(g$spiciness, nrow = n, byrow = FALSE)
if (!interactive()) png("spiciness_formula_%02d.png", units = "in", width = 7, height = 7, res = 500)
contour(SAg, CTg, spiciness0, xaxs = "i", yaxs = "i")
summary(m1 <- lm(spiciness0 ~ CT + SA, data = g)) # RSE 0.2673
summary(m2 <- lm(spiciness0 ~ CT + I(CT^2) + SA, data = g)) # RSE 0.05799
summary(m3 <- lm(spiciness0 ~ CT + I(CT^2) + I(CT^3) + SA, data = g)) # RSE 0.05427
par(mfrow = c(2, 2), mar = c(3, 3, 1, 1), mgp = c(2.0, 0.7, 0))
# apar(mfrow = c(2, 1))
plot(g$SA, g$spiciness0)
ylim <- par("usr")[3:4]
scale <- diff(quantile(g$spiciness, c(0.01, 0.99))) / 2
plot(g$SA, g$spiciness0 - predict(m1), ylab = "m1 % err")
plot(g$SA, g$spiciness0 - predict(m2), ylab = "m2 % err")
plot(g$SA, g$spiciness0 - predict(m3), ylab = "m3 % err")
cat("Next (m3) is best i.t.o. misfit\n")
print(coef(m3))
cat("Next (m2) has slightly higher misfit, but better t values on (fewer) parameters")
print(coef(m2))
if (!interactive()) dev.off()
