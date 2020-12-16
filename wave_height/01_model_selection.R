rm(list=ls())
source("00_formula.R")
RMS <- function(x) sqrt(mean(x^2))
nu <- 100
nx <- 101
u0 <- 5
umax <- 40
x0 <- 1e3
xmax <- 1000e3
u <- seq(u0, umax, length.out=nu)
x <- seq(x0, xmax, length.out=nx)
G <- expand.grid(u=u, x=x)
G$lx <- log10(G$x)
G$lu <- log10(G$u)
wp <- waveProperties(G$u, G$x)
criterion <- matrix(wp$criterion, ncol=nx) # in seconds
height <- matrix(wp$height, ncol=nx) # in m
period <- matrix(wp$period, ncol=nx) # in seconds
G$criterion <- wp$criterion
G$height <- wp$height
G$period <- wp$period
G$lcriterion <- log10(wp$criterion)
G$lheight <- log10(wp$height)
G$lperiod <- log10(wp$period)

if (!interactive()) pdf("01_model_selection.pdf")
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(2, 0.7, 0))

levels <- pretty(criterion/3600, 15)
contour(u, x/1e3, criterion/3600, levels=levels,
        xaxs="i", yaxs="i", lwd=2,
        col=1,
        xlab="Wind Speed [m/s]", ylab="Fetch [km]")
mtext("log10(criterion) [seconds]", adj=1, cex=par('cex'))
mcriterion <- lm(log10(criterion) ~ log10(u) + log10(x), data=G)
G$criterionPrediction <- 10^predict(mcriterion)
criterionMisfit <- G$criterion - G$criterionPrediction
mtext(sprintf("RMS criterion error %.1e hours", RMS(criterionMisfit/3600)),
      adj=0, cex=par("cex"))
contour(u, x/1e3,
        matrix(G$criterionPrediction/3600, ncol=nx),
        levels=levels, col=2, add=TRUE, lwd=2, lty=3)


Height <- wp$height
Period <- wp$period
X <- G$x / 1e3
U <- G$u
X2 <- X^2
X3 <- X^3
X4 <- X^4
X5 <- X^5
X6 <- X^6
U2 <- U^2
U3 <- U^3
U4 <- U^4
U5 <- U^5
U6 <- U^6
lX <- log10(X)
lX2 <- lX^2
lX3 <- lX^3
lX4 <- lX^4
lX5 <- lX^5
lU <- log10(U)
lU2 <- lU^2
lU3 <- lU^3
lU4 <- lU^4
lU5 <- lU^5

mheight <- lm(Height ~ U2 + U3) # X not significant
#mheight <- lm(log10(Height) ~ U2 + U3) # X not significant
#mheight <- lm(log10(height) ~ lu + lx, data=G)
#G$heightPrediction <- 10^predict(mheight)
G$heightPrediction <- predict(mheight)
levels <- pretty(height, 15)
contour(u, x/1e3, height, levels=levels, lwd=2, col=4,
        xlab="Wind speed [m/s]", ylab="Fetch [km]", xaxs="i", yaxs="i")
contour(u, x/1e3, matrix(G$heightPrediction, ncol=nx),
        levels=levels, add=TRUE, col=2, lwd=2, lty=2)
mtext("Significant Wave Height [m]", adj=1, cex=par('cex'))
heightMisfit <- RMS(G$height - G$heightPrediction)
mtext(sprintf("RMS height error %.1e m", RMS(heightMisfit)),
      adj=0, cex=par("cex"))


## log10 model
lperiod <- log10(wp$period)
##contour(log10(u), log10(x/1e3), matrix(lperiod, ncol=nx), xlab="log10 wind speed [m/s]", ylab="log10 fetch [km]")
levels <- pretty(period, 15)
contour(u, x/1e3, period, levels=levels, lwd=2, col=4,
        xlab="Wind speed [m/s]", ylab="Fetch [km]", xaxs="i", yaxs="i")
mtext("Period [s]", adj=1, cex=par("cex"))
mperiod <- lm(log10(period) ~ log10(X) + log10(u) + I(log10(u)^2) + I(log10(u)^3) + I(log10(u)^4), data=G)
periodPrediction <- 10^matrix(predict(mperiod), ncol=nx)
mtext(sprintf("RMS period error %.4f s",
              RMS(wp$period - 10^predict(mperiod))),
      adj=0, cex=par("cex"))
contour(u, x/1e3, periodPrediction, levels=levels, lwd=2, col=2, add=TRUE, lty=2)

if (!interactive()) dev.off()

save(G, mcriterion, mheight, mperiod, file="01.rda")
summary(mcriterion)
summary(mheight)
summary(mperiod)
