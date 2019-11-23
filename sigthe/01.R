source("00.R") # sets S0, etc
if (!interactive()) pdf("01.pdf")
par(mfrow=c(1, 1), mar=c(3.3, 3.3, 1, 1), mgp=c(2, 0.7, 0))
contour(S, T, sigthe, xlab="S", ylab="T")
Sv <- rep(S, times=n) - S0
Tv <- rep(T, each=n) - T0
sigthev <- as.vector(sigthe)
m01 <- lm(sigthev ~ Sv + Tv)
summary(m01)
C01 <- coef(m01)
res01 <- m01$residuals
label <- sprintf("sigthe=%.3f*+%.4g(S-%.1f)-%.4g(T-%..1f) at %.0f dbar (res. %.3f)",
                 C01[1], C01[2], S0, -C01[3], T0, p, RMS(res01))
mtext(label, line=0, cex=0.8)
if (!interactive()) dev.off()
sigtheRange01 <- diff(range(sigthev))
save(m01, C01, res01, sigtheRange01, file="01.rda")

