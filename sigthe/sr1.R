rm(list=ls())
library(oce)
## based on 02.R, i.e. 2nd-order fit
draft <- !FALSE # set TRUE to get red guiding lines
load("00.rda")
load("02.rda")
S <- seq(S0, Smax, length.out=n)
T <- seq(T0, Tmax, length.out=n)
if (!interactive()) pdf("sr1.pdf")
par(mar=c(3, 1, 3, 1))
plot(c(floor(sigtheRange[1]+1), floor(sigtheRange[2])), c(0, 1), xlab="", ylab="", type="n", axes=draft, yaxs="i", xaxs="i")
myrug <- function(x, tcl, y)
{
    segments(x, y, x, y+tcl)
}
fancyAxis <- function(xSmall, xMiddle, xBig, func, tclSmall, tclMiddle, tclBig, side=1, y=0)
{
    if (missing(func)) func <- function(x) x
    if (missing(tclSmall)) tclSmall <- 0.01 * if (side==1) 1 else -1
    if (missing(tclMiddle)) tclMiddle <- 2 * tclSmall
    if (missing(tclBig)) tclBig <- sqrt(2) * tclMiddle
    labels <- seq(S0, Smax, by=1)
    par(xpd=NA)
    myrug(func(xSmall), tcl=tclSmall, y=y)
    myrug(func(xMiddle), tcl=tclMiddle, y=y)
    myrug(func(xBig), tcl=tclBig, y=y)
    text(func(xBig), 2*tclBig + rep(y, length.out=length(xBig)), xBig)
}


## S axis
SBig <- seq(S0, Smax, by=1)
SMiddle <- seq(S0, Smax, by=0.5)
SSmall <- seq(S0, Smax, by=0.1)
Sfunc <- function(S) {
    ## fit to sigma-theta at T=5degC
    C02[1] + C02[2]*(S-S0) + C02[3]*(S-S0)^2
}
fancyAxis(SSmall, SMiddle, SBig, func=Sfunc, side=3, y=1)

## sigma-theta axis
sigtheBig <- seq(floor(sigtheRange[1]), sigtheRange[2], by=1)
sigtheMiddle <- seq(floor(sigtheRange[1]), sigtheRange[2], by=0.5)
sigtheSmall <- seq(floor(sigtheRange[1]), sigtheRange[2], by=0.1)
fancyAxis(sigtheSmall, sigtheMiddle, sigtheBig, side=1, y=0)

## regression check
if (draft) {
    par(xpd=FALSE)
    abline(v=swSigmaTheta(31,T0,p), col=2)
    abline(v=24.58, col=2, lty=2)
    mtext("error 0.1 kg/m^3", side=3, at=24.58, col=2)
    par(xpd=NA)
}

if (!interactive()) dev.off()


