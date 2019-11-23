library(oce)
RMS <- function(x) sqrt(mean(x^2))
n <- 100
S0 <- 30
Smax <- 40
T0 <- 5
Tmax <- 20
S <- seq(S0, Smax, length.out=n)
T <- seq(T0, Tmax, length.out=n)
p <- 100
sigthe <- matrix(NA, nrow=n, ncol=n)
for (iS in 1:n) {
    for (iT in 1:n) {
        sigthe[iS, iT] <- swSigmaTheta(S[iS], T[iT], p)
    }
}
sigtheRange <- range(sigthe)
sigtheSpan <- diff(sigtheRange)
save(S0, T0, Smax, Tmax, p, n, sigtheRange, sigtheSpan, RMS, file="00.rda")

