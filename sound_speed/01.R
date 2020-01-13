library(oce)
RMS <- function(x) sqrt(mean(x^2))
n <- 30                                # grid size for S, T and p
S0 <- 25
T0 <- 0
p0 <- 0
Smax <- 40
Tmax <- 20
pmax <- 5000
Smid <- 0.5 * (S0 + Smax)
Tmid <- 0.5 * (T0 + Tmax)
pmid <- 0.5 * (p0 + pmax)
S <- seq(S0, Smax, length.out=n)
T <- seq(T0, Tmax, length.out=n)
p <- seq(p0, pmax, length.out=n)

G <- expand.grid(S=S, T=T, p=p)
ss <- swSoundSpeed(G$S, G$T, G$p, eos="unesco")


##par(mar=c(3,3,1,1), mgp=c(2,0.7,0), mfrow=c(2,2))
S2 <- G$S^2
T2 <- G$T^2
T3 <- G$T^3
T4 <- G$T^4
p2 <- G$p^2
p3 <- G$p^3
p4 <- G$p^4
pT <- (G$p - pmid) * (G$T - Tmid)
m0 <- lm(ss ~ S + T + p, data=G)
m1 <- lm(ss ~ S + T + p + S2, data=G)
m2 <- lm(ss ~ S + T + p + T2, data=G)
m3 <- lm(ss ~ S + T + p + T2 + T3, data=G)
m4 <- lm(ss ~ S + T + T2 + p + p2, data=G)
m4b <- lm(ss ~ S + T + T2 + p + p2 + p3, data=G)
m4c <- lm(ss ~ S + T + T2 + T3 + p + p2 + p3, data=G)
m4d <- lm(ss ~ S + T + T2 + T3 + T4 + p + p2 + p3 + p4, data=G)
m4e <- lm(ss ~ S + T + T2 + T3 + T4 + p + p2 + p3 + p4 + pT, data=G)
m5 <- lm(ss ~ S + T + T2 + p + p2 + pT, data=G)
m6 <- lm(ss ~ S + T + T2 + T3 + p + p2 + p3, data=G)
## summary(m0)
## summary(m1) # conclude: S^2 term not significant
## summary(m2)
## summary(m3)
## summary(m4)
## summary(m5)
## summary(m6)
for (m in list(m0, m2, m3, m4, m4b, m4c, m4e, m5, m6)) {
    message(sprintf("rms err. %.2f m/s (%.2f%% of range), worst %.2f m/s (%.2f%%) {%s}",
                    RMS(ss - predict(m)),
                    100*RMS(ss - predict(m))/diff(range(ss)),
                    max(abs(ss - predict(m))),
                    100*max(abs(ss - predict(m))) / diff(range(ss)),
                    as.character(m$call[2])))
}
cat(sprintf("overall S range: %.2f PSU\n", diff(range(G$S))))
cat(sprintf("overall T range: %.2f degC\n", diff(range(G$T))))
cat(sprintf("overall p range: %.2f dbar\n", diff(range(G$p))))
cat(sprintf("overall ss (sound speed) range: %.2f m/s\n", diff(range(ss))))

