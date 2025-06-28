library(gsw)
D <- 0.025 # axis placement
eps <- 0.12 # tick length
gamma <- 0.6 # ratio of small tick length to large tick length
#> print(coef(m3))
#  (Intercept)            CT       I(CT^2)       I(CT^3)            SA
#-2.557009e+01  5.396252e-02  6.762300e-03 -5.595578e-05  7.303137e-01
fSA <- function(SA) {
    7.303137e-01 * SA
}
fCT <- function(CT) {
    -2.557009e+01 + CT * (5.396252e-02 + CT * (6.762300e-03 - 5.595578e-05 * CT))
}
spiciness <- function(SA, CT) {
    fSA(SA) + fCT(CT)
}
error <- function(SA, CT) {
    gsw_spiciness0(SA, CT) - spiciness(SA, CT)
}
error(30, 10)

if (!interactive()) png("02.png", unit = "in", width = 7, height = 7, res = 500)
par(mar = c(0, 2, 0, 2))
lim <- c(-0.05, 1.05)
plot(lim, lim, type = "n", axes = FALSE, xlab = "", ylab = "")
points(0.5, 0.5)
lines(c(0, 0), c(0, 1))
lines(c(0.5, 0.5), c(0, 1))
lines(c(1, 1), c(0, 1))

SAlim <- c(28, 36)
CTlim <- c(0, 25)
Alim <- fSA(SAlim)
Clim <- fCT(CTlim)
SAfcn <- function(SA) (fSA(SA) - Alim[1]) / (Alim[2] - Alim[1])
CTfcn <- function(CT) (fCT(CT) - Clim[1]) / (Clim[2] - Clim[1])
n <- 10
SApretty <- pretty(SAlim, n = n)
SApretty <- SApretty[SAlim[1] <= SApretty & SApretty <= SAlim[2]]
CTpretty <- pretty(CTlim, n = n)
CTpretty <- CTpretty[CTlim[1] <= CTpretty & CTpretty <= CTlim[2]]
nSA <- length(SApretty)
par(xpd = NA)
for (i in seq_len(nSA)) {
    lines(c(-D, 0), rep(SAfcn(SApretty[i]), 2))
    text(-0.8 * D, SAfcn(SApretty[i]), sprintf("%.1f", SApretty[i]), pos = 2)
}
dSA <- diff(SApretty[1:2]) / 4
for (SA in seq(SApretty[1], tail(SApretty, 1), dSA)) {
    lines(c(-gamma * D, 0), rep(SAfcn(SA), 2))
}
text(-1.2 * eps, 0.5, "Absolute Salinity [g/kg]", srt = 90)
nCT <- length(CTpretty)
for (i in seq_len(nCT)) {
    lines(c(1, 1 + D), rep(CTfcn(CTpretty[i]), 2))
    text(1 + 0.3 * eps, CTfcn(CTpretty[i]), sprintf("%5.0f", CTpretty[i]))
}
dCT <- diff(CTpretty[1:2]) / 2
for (CT in seq(CTpretty[1], tail(CTpretty, 1), dCT)) {
    lines(c(1, 1 + gamma * D), rep(CTfcn(CT), 2))
}
text(1 + eps, 0.5, expression("Conservative Temperature [" * degree * "C]"), srt = 90)
text(0.5, 1 + eps / 4, "Spiciness [g/kg]")
if (!interactive()) dev.off()
