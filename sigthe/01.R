source("00.R") # sets S0, etc
look <- G$p == 0
S <- G$S[look]
T <- G$T[look]
sigthe <- swSigmaTheta(S, T, 0)
SS <- S - S0
TT <- T - T0
m <- lm(sigthe ~ SS + TT)
summary(m)
save(m, file="01.rda")

