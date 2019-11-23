rm(list=ls())
## based on 02.R, i.e. 2nd-order fit
side <- 1 #1 # or 3
draft <- FALSE # set TRUE to get red guiding lines
load("00.rda")
load("02.rda")
S <- seq(S0, Smax, length.out=n)
T <- seq(T0, Tmax, length.out=n)
par(mar=c(3, 1, 3, 1))
plot(S, seq(0, 1, length.out=n), xlab="", ylab="", type="n", axes=draft, yaxs="i", xaxs="i")
myrug <- function(x, tcl, y)
{
    segments(x, y, x, y+tcl)
}
fancyAxis <- function(xSmall, xMiddle, xBig, tclSmall, tclMiddle, tclBig, side=1, y=0)
{
    if (missing(tclSmall)) tclSmall <- 0.01 * if (side==1) -1 else 1
    if (missing(tclMiddle)) tclMiddle <- 2 * tclSmall
    if (missing(tclBig)) tclBig <- sqrt(2) * tclMiddle
    labels <- seq(S0, Smax, by=1)
    par(xpd=NA)
    myrug(xSmall, tcl=tclSmall, y=y)
    myrug(xMiddle, tcl=tclMiddle, y=y)
    myrug(xBig, tcl=tclBig, y=y)
    text(xBig, 2*tclBig + rep(y, length.out=length(xBig)), xBig)
}
SBig <- seq(S0, Smax, by=1)
SMiddle <- seq(S0, Smax, by=0.5)
SSmall <- seq(S0, Smax, by=0.1)
fancyAxis(SSmall, SMiddle, SBig, side=1, y=0.5)
fancyAxis(SSmall, SMiddle, SBig, side=3, y=1)

