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
summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
message(sprintf("m4:  rms error %.3f%%, worst-case %.3f%% (2nd order in T and p)",
                100*RMS(ss - predict(m4))/diff(range(ss)),
                100*max(abs(ss - predict(m4))) / diff(range(ss))))
message(sprintf("m4b: rms error %.3f%%, worst-case %.3f%% (2nd order in T, 3rd order in p)",
                100*RMS(ss - predict(m4b))/diff(range(ss)),
                100*max(abs(ss - predict(m4b))) / diff(range(ss))))
message(sprintf("m4c: rms error %.3f%%, worst-case %.3f%% (3rd order in p and T)",
                100*RMS(ss - predict(m4c))/diff(range(ss)),
                100*max(abs(ss - predict(m4c))) / diff(range(ss))))
message(sprintf("m4d: rms error %.3f%%, worst-case %.3f%% (4th order in p and T)",
                100*RMS(ss - predict(m4d))/diff(range(ss)),
                100*max(abs(ss - predict(m4d))) / diff(range(ss))))
message(sprintf("m4e: rms error %.3f%%, worst-case %.3f%% (4th order in p and T; 1st order in pT)",
                100*RMS(ss - predict(m4e))/diff(range(ss)),
                100*max(abs(ss - predict(m4e))) / diff(range(ss))))
message(sprintf("m5:  rms error %.3f%%, worst-case %.3f%% (2nd order in p and T, 1st order in pT)",
                100*RMS(ss - predict(m5))/diff(range(ss)),
                100*max(abs(ss - predict(m5))) / diff(range(ss))))
message(sprintf("m6:  rms error %.3f%%, worst-case %.3f%% (3rd order in p and T)",
                100*RMS(ss - predict(m6))/diff(range(ss)),
                100*max(abs(ss - predict(m6))) / diff(range(ss))))
## coef(m4)
## coef(m4e)

# par(mar=c(3,3,1,1),mgp=c(2,0.7,0),mfrow=c(2,2))
# smoothScatter(ss, ss-predict(m4))
# hist(ss-predict(m4))
# coplot(ss~G$T|G$p)
#
# smoothScatter(ss, ss-predict(m4))
#
# smoothScatter(G$S, ss)
# smoothScatter(G$T, ss)
# smoothScatter(G$p, ss)

