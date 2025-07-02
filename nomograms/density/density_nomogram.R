#  (Intercept)            CT       I(CT^2)      I(CT^3)            SA
# 9.679121e-01 -4.650992e-02 -6.342369e-03 3.588205e-05  7.687654e-01

library(gsw)

A2a <- function(SA) { # SA -> term in sigma0 fit
    7.687654e-01 * SA
}
C2c <- function(CT) { # CT -> term in sigma0 fit (includes intercept)
    9.679121e-01 + CT * (-4.650992e-02 + CT * (-6.342369e-03 + CT * 3.588205e-05))
}

D <- 0.025 # axis name placement
eps <- 0.10 # tick length
gamma <- 0.6 # ratio of small tick length to large tick length
tickLong <- 0.02
tickShort <- 0.65 * tickLong
tickShorter <- 0.33 * tickLong
lwdTickLong <- 1.4
lwdTickShort <- 0.8 * lwdTickLong
lwdTickShorter <- 0.8 * lwdTickShort
dsigma <- 0.2
debug <- 1
error <- function(SA, CT) {
    gsw_sigma0(SA, CT) - (A2a(SA) + C2c(CT))
}
stopifnot(error(32, 10) < 0.002)

if (!interactive()) {
    pdf("density_nomogram.pdf", pointsize = 10)
}
par(mar = c(4, 3, 2, 3))
pagelim <- c(-0.025, 1.025)
plot(pagelim, pagelim, type = "n", axes = FALSE, xlab = "", ylab = "")
par(xpd = NA)

# Axis lines and names. The are conventionally named A, B and C.
lines(c(0, 0), c(0, 1))
text(0, 1 + 0.5 * D, "Absolute Salinity\n[g/kg]", pos = 3)
lines(c(0.5, 0.5), c(0, 1))
text(0.5, 1 + 0.5 * D, "Density Anomaly\n[g/kg]", pos = 3)
lines(c(1, 1), c(0, 1))
text(1, 1 + 0.5 * D, "Conservative Temperature\n[\u00B0C]", pos = 3)

SAlim <- c(28, 35)
CTlim <- c(0, 25)
Alim <- A2a(SAlim)
Clim <- C2c(CTlim)
Blim <- 0.5 * (Alim + Clim) # works for additive formulae

if (debug) {
    cat("SAlim: ", paste(SAlim, collapse = ", "), "\n")
    cat("CTlim: ", paste(CTlim, collapse = ", "), "\n")
}

# map SA and CT to respective positions, s, on axes
SA2s <- function(SA) (A2a(SA) - Alim[1]) / (Alim[2] - Alim[1])
CT2s <- function(CT) (C2c(CT) - Clim[1]) / (Clim[2] - Clim[1])
# Map position, s, on axis to SA, CT, and sigma0. This is done via
# piecewise linear interpolation, to avoid the need to invert functions
# symbolically.
Nv <- 100
vs <- seq(0, 1, length.out = Nv)
vSA <- seq(SAlim[1], SAlim[2], length.out = Nv)
vCT <- seq(CTlim[1], CTlim[2], length.out = Nv)
vsigma <- gsw_sigma0(vSA, vCT)
sigma2s <- approxfun(range(vsigma), c(0, 1), rule = 2)

n <- 10
SApretty <- pretty(SAlim, n = n)
SApretty <- SApretty[SAlim[1] <= SApretty & SApretty <= SAlim[2]]
CTpretty <- pretty(CTlim, n = n)
CTpretty <- CTpretty[CTlim[1] <= CTpretty & CTpretty <= CTlim[2]]
nSA <- length(SApretty)
for (i in seq_len(nSA)) {
    lines(c(-tickLong, 0), rep(SA2s(SApretty[i]), 2),
        lwd = lwdTickLong
    )
    text(-0.8 * D, SA2s(SApretty[i]), sprintf("%.1f", SApretty[i]), pos = 2, )
}
dSA <- diff(SApretty[1:2]) / 5
for (SA in seq(SApretty[1], tail(SApretty, 1), dSA)) {
    lines(c(-tickShort, 0), rep(SA2s(SA), 2), lwd = lwdTickShort)
}
nCT <- length(CTpretty)
for (i in seq_len(nCT)) {
    lines(c(1, 1 + tickLong), rep(CT2s(CTpretty[i]), 2), lwd = lwdTickLong)
    text(1 + 1.5 * tickLong, CT2s(CTpretty[i]), sprintf("%5.0f", CTpretty[i]))
}
dCT <- diff(CTpretty[1:2]) / 2
for (CT in seq(CTpretty[1], tail(CTpretty, 1), dCT)) {
    lines(c(1, 1 + tickShort), rep(CT2s(CT), 2), lwd = lwdTickShort)
}
dCT <- 0.5
for (CT in seq(CTpretty[1], tail(CTpretty, 1), dCT)) {
    lines(c(1, 1 + tickShorter), rep(CT2s(CT), 2), lwd = lwdTickShorter)
}
sigmalim <- gsw_sigma0(SAlim, CTlim)
if (debug) {
    cat("sigmalim=", paste(sigmalim, collapse = ","), "\n")
}

# build up piecewise linear representation of the middle axis,
# to we can interpolate to get locations for pretty values.
if (debug > 1) {
    par(mar = c(4, 4, 1, 1))
    sigmas <- seq(20, 28, 0.1)
    plot(sigma2s(sigmas), sigmas, type = "o", cex = 0.4, xlab = "f", ylab = "sigma0", xaxs = "i")
    abline(v = 0.5)
}
sigmas <- seq(20, 28, dsigma)
print(sigmas)
#sigmas <- sigmas[sigmalim[1] <= sigmas & sigmas <= sigmalim[2]]
print(sigmas)
if (debug > 1) {
    message(sprintf("sigma:%5.2f, s:%.2f (limit)", Blim[1], sigma2s(Blim[1])))
}

for (sigma in sigmas) {
    lines(c(0.5, 0.5 + tickLong), rep(sigma2s(sigma), 2), lwd = lwdTickLong)
    text(0.5 + tickLong, sigma2s(sigma), sprintf("%.1f", sigma), pos = 4)
    if (debug > 1) {
        message(sprintf("sigma:%5.2f, s:%.2f", sigma, sigma2s(sigma)))
    }
}
if (debug > 1) {
    message(sprintf("sigma:%5.2f, s:%.2f (limit)", Blim[2], sigma2s(Blim[2])))
}
for (sigma in seq(-10, 10, 0.5)) {
    lines(c(0.5, 0.5 + tickShort), rep(sigma2s(sigma), 2), lwd = lwdTickShort)
}
for (sigma in seq(10, 30, 0.1)) {
    lines(c(0.5, 0.5 + tickShorter), rep(sigma2s(sigma), 2), lwd = lwdTickShorter)
}
# Instructions
pm <- 0.6 # = max from plot made by density_formula.R
SA0 <- 34
# SA0 <- 30
CT0 <- 13 #
sigma0 <- gsw_sigma0(SA0, CT0)
msg <- sprintf(
    paste0(
        "INTRUCTIONS: Draw a line joining the Absolute Salinity with the\n",
        "Conservative Temperature, then read density anomaly on the middle axis.\n",
        "DEMONSTRATION:  SA=%.1f g/kg, CT=%.1f\u00B0C \u2192 \u03c3=%.2f g/kg.\n",
        "ACCURACY: \u00b1 %.1f g/kg in \u03c3 value."
    ),
    SA0, CT0, sigma0, pm
)
text(0.5, -1 * D, msg, pos = 1)

lines(c(0, 1), c(SA2s(SA0), CT2s(CT0)), col = "gray")

if (!interactive()) {
    dev.off()
}
message("BROKEN on middle scale (and limits)")

