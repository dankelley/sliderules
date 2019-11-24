source("00.R") # sets S0, etc
if (!interactive()) pdf("03.pdf")
par(mfrow=c(2, 1), mar=c(3.3, 3.3, 1, 1), mgp=c(2, 0.7, 0))
contour(S, T, sigthe, xlab="S", ylab="T")
Sv <- rep(S, times=n) - S0
Tv <- rep(T, each=n) - T0
Sv2 <- Sv^2
Tv2 <- Tv^2
SvTv <- (Sv-S0) * (Tv-T0)
sigthev <- as.vector(sigthe)

## NOTE: the res. std. err is a tenth that of m02 (0.0257)
m03 <- lm(sigthev ~ Sv + Sv2 + Tv + Tv2 + SvTv)
summary(m03) # res std err 0.0029
C03 <- coef(m03)
res03 <- m03$residuals
label <- sprintf("%.3f+%.3f(S-%.0f)+%.2e(S-%.0f)^2-%.3f(T-%.0f)-%.2e(T-%.0f)^2-%.2e*(S-%.0f)*(T-%.0f) @ %.0fdbar (res %.4f)",
                 C03[1], C03["Sv"], S0, C03["Sv2"], S0, -C03["Tv"], T0, -C03["Tv2"], T0, -C03["SvTv"], S0, T0, p, RMS(res03))
mtext(label, line=0, cex=0.7)

## Trial inset for S*V correction
1/coef(m03)['SvTv'] # (S-30)*(T-10)/500
range(SvTv)
x <- seq(min(SvTv), max(SvTv), length.out=100)
y <- x * coef(m03)[['SvTv']]
plot(x,y, type="l", xlab=expression((S-30)*(T-5)), ylab=expression(Delta*sigma[theta]))
grid(lty=1, col="gray")


if (!interactive()) dev.off()
save(S0, T0, Smax, Tmax, p, n, m03, C03, res03, file="03.rda")


