library(oce)
RMS <- function(x) sqrt(mean(x^2))
n <- 20                                # grid size for S, T and p
S0 <- 28
T0 <- 0
p0 <- 0
Smax <- 36
Tmax <- 20
pmax <- 400
Smid <- 0.5 * (S0 + Smax)
Tmid <- 0.5 * (T0 + Tmax)
pmid <- 0.5 * (p0 + pmax)
S <- seq(S0, Smax, length.out=n)
T <- seq(T0, Tmax, length.out=n)
p <- seq(p0, pmax, length.out=n)
G <- expand.grid(S=S, T=T, p=p)
sigthe <- swSigmaTheta(S, T, p)

if (!interactive()) pdf("00.pdf")
par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
ST <- expand.grid(S=S, T=T)
contour(S, T, matrix(swSigmaTheta(ST$S, ST$T, 0), nrow=n), xlab="S", ylab="T", xaxs="i", yaxs="i")
mtext("At 0dbar pressure")
if (!interactive()) dev.off()

save(n, S0, T0, p0, Smax, Tmax, pmax, Smid, Tmid, pmid, G, RMS, file="00.rda")

