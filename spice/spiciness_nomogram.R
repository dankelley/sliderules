library(gsw)

# from 01.R, the m3 resultrs:
#  (Intercept)            CT       I(CT^2)       I(CT^3)            SA
#-2.557009e+01  5.396252e-02  6.762300e-03 -5.595578e-05  7.303137e-01

A2a <- function(SA) { # SA -> term in spiciness formula
    0.730313712 * SA
}
C2c <- function(CT) { # CT -> term in spiciness formula (includes intercept)
    -2.557009e+01 + CT * (5.396252e-02 + CT * (6.762300e-03 - 5.595578e-05 * CT))
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
dspice <- 1
debug <- 1
error <- function(SA, CT) {
    gsw_spiciness0(SA, CT) - (A2a(SA) + C2c(CT))
}
stopifnot(abs(error(30, 10)) < 0.02) # value should be 0.010367

if (!interactive()) {
    pdf("spiciness_nomogram.pdf", pointsize = 10)
    # png("spiciness_nomogram.png", unit = "in", width = 7, height = 7, res = 500, pointsize = 10)
}
par(mar = c(4, 3, 2, 3))
pagelim <- c(-0.025, 1.025)
plot(pagelim, pagelim, type = "n", axes = FALSE, xlab = "", ylab = "")
par(xpd = NA)

# Axis lines and names. The are conventionally named A, B and C.
lines(c(0, 0), c(0, 1))
text(0, 1 + 0.5 * D, "Absolute Salinity\n[g/kg]", pos = 3)
lines(c(0.5, 0.5), c(0, 1))
text(0.5, 1 + 0.5 * D, "Spiciness (at surface pressure)\n[g/kg]", pos = 3)
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
# Map position, s, on axis to SA, CT, and spiciness. This is done via
# piecewise linear interpolation, to avoid the need to invert functions
# symbolically.
Nv <- 100
vs <- seq(0, 1, length.out = Nv)
vSA <- seq(SAlim[1], SAlim[2], length.out = Nv)
vCT <- seq(CTlim[1], CTlim[2], length.out = Nv)
vspiciness <- gsw_spiciness0(vSA, vCT)
# s2CT <- approxfun(vs, vCT, rule = 1)
# s2SA <- approxfun(vs, vSA, rule = 1)
# stopifnot(SAlim[1] == s2SA(0))
# stopifnot(SAlim[2] == s2SA(1))
# stopifnot(CTlim[1] == s2CT(0))
# stopifnot(CTlim[2] == s2CT(1))

# OLD spiciness2s <- approxfun(vspiciness, vs, rule = 2)
spiciness2s <- approxfun(range(vspiciness), c(0, 1), rule = 2)

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
spicelim <- gsw_spiciness0(SAlim, CTlim)
if (debug) {
    cat("spicelim=", paste(spicelim, collapse = ","), "\n")
}

# build up piecewise linear representation of the spiciness axis,
# to we can interpolate to get locations for pretty values.
if (debug > 1) {
    par(mar = c(4, 4, 1, 1))
    spices <- seq(-5, 5, 0.1)
    plot(spiciness2s(spices), spices, type = "o", cex = 0.4, xlab = "f", ylab = "spice", xaxs = "i")
    abline(v = 0.5)
}
spices <- seq(-10, 10, dspice)
spices <- spices[spicelim[1] <= spices & spices <= spicelim[2]]
if (debug > 1) {
    message(sprintf("spiciness:%5.2f, s:%.2f (limit)", Blim[1], spiciness2s(Blim[1])))
}

for (spice in spices) {
    lines(c(0.5, 0.5 + tickLong), rep(spiciness2s(spice), 2), lwd = lwdTickLong)
    text(0.5 + tickLong, spiciness2s(spice), sprintf("%.1f", spice), pos = 4)
    if (debug > 1) {
        message(sprintf("spiciness:%5.2f, s:%.2f", spice, spiciness2s(spice)))
    }
}
if (debug > 1) {
    message(sprintf("spiciness:%5.2f, s:%.2f (limit)", Blim[2], spiciness2s(Blim[2])))
}
for (spice in seq(-10, 10, 0.5)) {
    lines(c(0.5, 0.5 + tickShort), rep(spiciness2s(spice), 2), lwd = lwdTickShort)
}
for (spice in seq(-10, 10, 0.1)) {
    lines(c(0.5, 0.5 + tickShorter), rep(spiciness2s(spice), 2), lwd = lwdTickShorter)
}
# Instructions
pm <- 2 * 0.05 # =2*std.err. of regression error in 01.R
SA0 <- 34
# SA0 <- 30
CT0 <- 13 # spice=1.0
# CT0 <- 22
# SA0 <- 34
# CT0 <- 22.7
# SA0 <- 29.0
# CT0 <- 6.6
# CT0 <- 22.7 # spice=3.4
spice0 <- gsw_spiciness0(SA0, CT0)
msg <- sprintf(
    paste0(
        "INTRUCTIONS: Draw a line joining the Absolute Salinity with the\n",
        "Conservative Temperature, then read spiciness on the middle axis.\n",
        "DEMONSTRATION:  SA=%.1f g/kg, CT=%.1f\u00B0C \u2192 spiciness=%.2f g/kg.\n",
        "ACCURACY: \u00b1 %.1f g/kg in spiciness value."
    ),
    SA0, CT0, spice0, pm
)
text(0.5, -1 * D, msg, pos = 1)

lines(c(0, 1), c(SA2s(SA0), CT2s(CT0)), col = "gray")

if (!interactive()) {
    dev.off()
}
