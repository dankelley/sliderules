source("00.R") # sets S0, etc
S <- G$S
T <- G$T
p <- G$p
sigthe <- swSigmaTheta(S, T, p)
SS <- S - S0
TT <- T - T0
SS2 <- SS^2
## SS12 <- sqrt(SS) # p=0.788
TT2 <- TT^2
SSTT <- (S - Smid) * (T - Tmid)
## SSTT2 <- SSTT^2 # p=0.767
m <- lm(sigthe ~ SS + TT + SS2 + TT2 + SSTT + p)
summary(m)
save(m, file="04.rda")

