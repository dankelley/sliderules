library(gsw)
D <- 0.025 # axis placement
eps <- 0.12 # tick length
eps <- 0.10 # tick length
gamma <- 0.6 # ratio of small tick length to large tick length
tickLong <- 0.02
tickShort <- 0.60 * tickLong
lwdTickShort <- 1
lwdTickLong <- 1.4
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
message("Error in a test: ", error(30, 10))

if (!interactive()) png("02.png", unit = "in", width = 7, height = 7, res = 500)
par(mar = c(0, 2, 1, 2))
pagelim <- c(-0.025, 1.025)
plot(pagelim, pagelim, type = "n", axes = FALSE, xlab = "", ylab = "")
lines(c(0, 0), c(0, 1))
lines(c(0.5, 0.5), c(0, 1))
lines(c(1, 1), c(0, 1))

SAlim <- c(28, 35)
CTlim <- c(0, 25)
Alim <- fSA(SAlim)
Clim <- fCT(CTlim)
Blim <- Alim + Clim # works for additive formulae
SAfcn <- function(SA) (fSA(SA) - Alim[1]) / (Alim[2] - Alim[1])
SAfcnInverse <- function(eta) SAlim[1] + eta * (SAlim[2] - SAlim[1])
CTfcn <- function(CT) (fCT(CT) - Clim[1]) / (Clim[2] - Clim[1])
CTfcnInverse <- function(eta) CTlim[1] + eta * (CTlim[2] - CTlim[1])
spicefcn <- function(spice) (fspice(spice) - Blim[1]) / (Blim[2] - Blim[1])
n <- 10
SApretty <- pretty(SAlim, n = n)
SApretty <- SApretty[SAlim[1] <= SApretty & SApretty <= SAlim[2]]
CTpretty <- pretty(CTlim, n = n)
CTpretty <- CTpretty[CTlim[1] <= CTpretty & CTpretty <= CTlim[2]]
nSA <- length(SApretty)
par(xpd = NA)
for (i in seq_len(nSA)) {
    lines(c(-tickLong, 0), rep(SAfcn(SApretty[i]), 2),
        lwd = lwdTickLong
    )
    text(-0.8 * D, SAfcn(SApretty[i]), sprintf("%.1f", SApretty[i]), pos = 2, )
}
dSA <- diff(SApretty[1:2]) / 4
for (SA in seq(SApretty[1], tail(SApretty, 1), dSA)) {
    lines(c(-tickShort, 0), rep(SAfcn(SA), 2), lwd = lwdTickShort)
}
text(-1.2 * eps, 0.5, "Absolute Salinity [g/kg]", srt = 90)
nCT <- length(CTpretty)
for (i in seq_len(nCT)) {
    lines(c(1, 1 + tickLong), rep(CTfcn(CTpretty[i]), 2), lwd = lwdTickLong)
    text(1 + 0.3 * eps, CTfcn(CTpretty[i]), sprintf("%5.0f", CTpretty[i]))
}
dCT <- diff(CTpretty[1:2]) / 2
for (CT in seq(CTpretty[1], tail(CTpretty, 1), dCT)) {
    lines(c(1, 1 + gamma * D), rep(CTfcn(CT), 2), lwd = lwdTickShort)
}
text(1 + eps, 0.5, expression("Conservative Temperature [" * degree * "C]"), srt = 90)

# Middle ("b"): spiciness. How to join up? One way could
# be to use horizontal lines but that requires an inverse
# fcn from e.g. page-to-SA (and we only SA-to-page defined
# so far)
text(0.5, 1 + eps / 2, "Spiciness0 [g/kg]")
spicelim <- gsw_spiciness0(SAlim, CTlim)
message(oce::vectorShow(spicelim))
f <- 0
for (f in seq(0, 1, 0.1)) {
    SA <- SAfcnInverse(f)
    CT <- CTfcnInverse(f)
    spice <- gsw_spiciness0(SA, CT)
    message(sprintf("f=%.2f -> spice=%.2f", f, spice))
    lines(c(0.5, 0.5 + tickLong), rep(f, 2), lwd = lwdTickLong)
    text(0.5 + tickLong, f, sprintf("%.2f", spice), pos = 4)
}
for (f in seq(0, 1, 0.05)) {
    lines(c(0.5, 0.5 + tickShort), rep(f, 2), lwd = lwdTickShort)
}
text(0.1, 0.98, sprintf("check spice(35,25) = %.2f", gsw_spiciness0(35, 25)), pos = 4)
text(0.1, 0.02, sprintf("check spice(28,0)  = %.2f", gsw_spiciness0(28, 0)), pos = 4)

if (!interactive()) dev.off()
