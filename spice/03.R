rm(list = ls())
library(gsw)
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
debug <- FALSE
#> print(coef(m3))
#  (Intercept)            CT       I(CT^2)       I(CT^3)            SA
#-2.557009e+01  5.396252e-02  6.762300e-03 -5.595578e-05  7.303137e-01
fSA <- function(SA) {
    7.303137e-01 * SA
}
fCT <- function(CT) {
    -2.557009e+01 + CT * (5.396252e-02 + CT * (6.762300e-03 - 5.595578e-05 * CT))
}
fspice <- function(SA, CT) {
    fSA(SA) + fCT(CT)
}
error <- function(SA, CT) {
    gsw_spiciness0(SA, CT) - (fSA(SA) + fCT(CT))
}
stopifnot(abs(error(30, 10)) < 0.02) # value should be 0.010367

if (!interactive()) png("03.png", unit = "in", width = 7, height = 7, res = 500, pointsize = 10)
par(mar = c(4, 3, 2, 3))
pagelim <- c(-0.025, 1.025)
plot(pagelim, pagelim, type = "n", axes = FALSE, xlab = "", ylab = "")
par(xpd = NA)

# Axis lines and names
lines(c(0, 0), c(0, 1))
text(0, 1 + 0.5 * D, "Absolute Salinity\n[g/kg]", pos = 3)
lines(c(0.5, 0.5), c(0, 1))
text(0.5, 1 + 0.5 * D, "Spiciness (at surface pressure)\n[g/kg]", pos = 3)
lines(c(1, 1), c(0, 1))
text(1, 1 + 0.5 * D, "Conservative Temperature\n[\u00B0C]", pos = 3)

SAlim <- c(28, 35)
CTlim <- c(0, 25)
Alim <- fSA(SAlim)
Clim <- fCT(CTlim)
Blim <- Alim + Clim # works for additive formulae
# map SA and CT to respective positions, s, on axes
SA2s <- function(SA) (fSA(SA) - Alim[1]) / (Alim[2] - Alim[1])
CT2s <- function(CT) (fCT(CT) - Clim[1]) / (Clim[2] - Clim[1])
# Map position, s, on axis to SA, CT, and spiciness. This is done via
# piecewise linear interpolation, to avoid the need to invert functions
# symbolically.
Nv <- 100
vs <- seq(0, 1, length.out = Nv)
vSA <- seq(SAlim[1], SAlim[2], length.out = Nv)
vCT <- seq(CTlim[1], CTlim[2], length.out = Nv)
vspiciness <- gsw_spiciness0(vSA, vCT)
s2CT <- approxfun(vs, vCT, rule = 1)
s2SA <- approxfun(vs, vSA, rule = 1)
s2spiciness <- approxfun(vs, vspiciness, rule = 1)
stopifnot(SAlim[1] == s2SA(0))
stopifnot(SAlim[2] == s2SA(1))
stopifnot(CTlim[1] == s2CT(0))
stopifnot(CTlim[2] == s2CT(1))
spiciness2s <- approxfun(vspiciness, vs, rule = 1)

n <- 10
SApretty <- pretty(SAlim, n = n)
SApretty <- SApretty[SAlim[1] <= SApretty & SApretty <= SAlim[2]]
CTpretty <- pretty(CTlim, n = n)
CTpretty <- seq(CTlim[1], CTlim[2], 2)
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
# build up piecewise linear representation of the spiciness axis,
# to we can interpolate to get locations for pretty values.
if (debug) {
    par(mar = c(4, 4, 1, 1))
    spices <- seq(-5, 5, 0.1)
    plot(spiciness2s(spices), spices, type = "o", cex = 0.4, xlab = "f", ylab = "spice", xaxs = "i")
    abline(v = 0.5)
    abline(h = s2spiciness(0.5))
    stop("125")
}
spices <- seq(-10, 10, dspice)
spices <- spices[spicelim[1] <= spices & spices <= spicelim[2]]
for (spice in spices) {
    lines(c(0.5, 0.5 + tickLong), rep(spiciness2s(spice), 2), lwd = lwdTickLong)
    text(0.5 + tickLong, spiciness2s(spice), sprintf("%.1f", spice), pos = 4)
    if (debug) {
        message(sprintf("s:%5.2f, y:%.2f", spice, spiciness2s(spice)))
    }
}
for (spice in seq(-10, 10, 0.5)) {
    lines(c(0.5, 0.5 + tickShort), rep(spiciness2s(spice), 2), lwd = lwdTickShort)
}
for (spice in seq(-10, 10, 0.1)) {
    lines(c(0.5, 0.5 + tickShorter), rep(spiciness2s(spice), 2), lwd = lwdTickShorter)
}
# Instructions
msg <- "INTRUCTIONS: Draw a line joining Absolute Salinity with Conservative Temperature,\n then read spiciness on the middle axis.  (Standard error of spiciness misfit: 0.05.)"
text(0.5, -2 * D, msg, pos = 1)
SA0 <- 34
CT0 <- 13
spice0 <- gsw_spiciness0(SA0, CT0)
msg <- sprintf(
    "CHECK VALUE: spiciness=%.1f at %.0f g/kg and %.0f\u00B0C.",
    spice0, SA0, CT0
)
text(0.5, -4.5 * D, msg, pos = 1)
text(0.5, -6 * D, "BROKEN wrt 02.R on spice scale", pos = 1, col = 2, font = 2)

if (!interactive()) dev.off()
