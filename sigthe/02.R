source("00.R") # sets S0, etc
if (!interactive()) pdf("02.pdf")
par(mfrow=c(1, 1), mar=c(3.3, 3.3, 1, 1), mgp=c(2, 0.7, 0))
contour(S, T, sigthe, xlab="S", ylab="T")
Sv <- rep(S, times=n) - S0
Tv <- rep(T, each=n) - T0
Sv2 <- Sv^2
Tv2 <- Tv^2
sigthev <- as.vector(sigthe)
m02 <- lm(sigthev ~ Sv + Sv2 + Tv + Tv2)
summary(m02)
C02 <- coef(m02)
res02 <- m02$residuals
label <- sprintf("sigthe=%.3f+%.4g(S-%.1f)+%.4g(S-%.1f)^2-%.4g(T-%.1f)-%.4g(T-%.1f)^2 at %.0f dbar (res. %.3f)",
                 C02[1], C02["Sv"], S0, C02["Sv2"], S0, C02["Tv"], T0, C02["Tv2"], T0, p, RMS(res02))
mtext(label, line=0, cex=0.8)
if (!interactive()) dev.off()
sigtheRange02 <- diff(range(sigthev))
save(S0, T0, Smax, Tmax, p, n, m02, C02, res02, sigtheRange02, file="02.rda")

