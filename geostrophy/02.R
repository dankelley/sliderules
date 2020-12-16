source("00.R") # sets S0, etc
look <- G$p == 0
S <- G$S[look]
T <- G$T[look]
sigthe <- swSigmaTheta(S, T, 0)
SS <- S - S0
TT <- T - T0
SS2 <- SS^2
TT2 <- TT^2
m <- lm(sigthe ~ SS + TT + SS2 + TT2)
summary(m)
save(m, file="02.rda")

