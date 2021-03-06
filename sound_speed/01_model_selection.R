library(oce)
RMS <- function(x) sqrt(mean(x^2))
n <- 30                                # grid size for S, T and p
S0 <- 25
T0 <- -2
p0 <- 0
Smax <- 40
Tmax <- 20
pmax <- 4000
Smid <- 0.5 * (S0 + Smax)
Tmid <- 0.5 * (T0 + Tmax)
pmid <- 0.5 * (p0 + pmax)
S <- seq(S0, Smax, length.out=n)
T <- seq(T0, Tmax, length.out=n)
p <- seq(p0, pmax, length.out=n)

## Note that the T scale will go from T0 to Tmax, so we create a polynomial that starts
## at temperature T0.
G <- expand.grid(T=T, p=p, S=S-S0)
ss <- swSoundSpeed(G$S+S0, G$T, G$p, eos="unesco")
ss0 <- min(ss, na.rm=TRUE)
ssmax <- max(ss, na.rm=TRUE)

##par(mar=c(3,3,1,1), mgp=c(2,0.7,0), mfrow=c(2,2))
S2 <- G$S^2
T2 <- G$T^2
T3 <- G$T^3
T4 <- G$T^4
p2 <- G$p^2
p3 <- G$p^3
p4 <- G$p^4
pT <- (G$p - pmid) * (G$T - Tmid)
m0 <- lm(ss ~ T + p + S, data=G)
summary(m0)
m1 <- lm(ss ~ T + p + S + S2, data=G)
summary(m1) # shows that S2 is not significant (p=0.583 for that)
m2 <- lm(ss ~ T + T2 + p + S , data=G)
summary(m2) # adding T2 helps (rms error 0.4359)
m3 <- lm(ss ~ T + T2 + T3 + p + S, data=G)
summary(m3) # adding T3 helps (rms error 0.4294)
m4 <- lm(ss ~ T + T2 + T3 + p + p2 + S, data=G)
summary(m4) # adding p2 helps (rms error 0.4171)
m5 <- lm(ss ~ T + T2 + T3 + p + p2 + p3 + S, data=G)
summary(m5) # adding p3 does not help; its p is 0.0687
m6 <- lm(ss ~ T + T2 + T3 + T4 + p + p2 + S, data=G)
summary(m6) # adding T4 does not help much; its p is 0.0449
m7 <- lm(ss ~ T + T2 + T3 + T4 + p + p2 + pT + S, data=G)
summary(m7)
for (m in list(m0, m1, m2, m3, m4, m5, m6, m7)) {
    cat(sprintf("rms err. %.4f m/s (%.2f%% of range), worst %.4f m/s (%.2f%%) {%s}",
                RMS(ss - predict(m)),
                100*RMS(ss - predict(m)) / diff(range(ss)),
                max(abs(ss - predict(m))),
                100*max(abs(ss - predict(m))) / diff(range(ss)),
                as.character(m$call[2])), "\n")
}
cat(sprintf("overall S range: %.2f PSU\n", diff(range(G$S))))
cat(sprintf("overall T range: %.2f degC\n", diff(range(G$T))))
cat(sprintf("overall p range: %.2f dbar\n", diff(range(G$p))))
cat(sprintf("overall ss (sound speed) range: %.2f m/s\n", diff(range(ss))))

message("SAVING model m4 in 01_model_selection.rda; note that S is salinity - (", S0, " degC), T2 is T^2, p2 is p^2, etc")
model <- m4
save(S0, T0, p0, Smax, Tmax, pmax, ss0, ssmax, model, file="01_model_selection.rda")


