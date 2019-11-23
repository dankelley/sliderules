rm(list=ls())
## based on 02.R, i.e. 2nd-order fit
side <- 1 #1 # or 3
draft <- FALSE # set TRUE to get red guiding lines
load("00.rda")
load("02.rda")
S <- seq(S0, Smax, length.out=n)
T <- seq(T0, Tmax, length.out=n)
par(mar=c(3, 1, 1, 1))
plot(S, seq(0, 1, length.out=n), xlab="", ylab="", type="n", axes=draft, yaxs="i", xaxs="i")
if (draft)
    box(col=2)
Sbig <- seq(S0, Smax, by=1)
Smiddle <- seq(S0, Smax, by=0.5)
Ssmall <- seq(S0, Smax, by=0.1)
tclSmall <- 0.15 * if (side==1) -1 else 1
tclMiddle <- 2 * tclSmall
tclBig <- 2 * tclMiddle
labels <- seq(S0, Smax, by=1)
par(xpd=NA)
rug(Ssmall, tcl=tclSmall, side=side)
rug(Smiddle, tcl=tclMiddle, side=side)
rug(Sbig, tcl=tclBig, side=side)
if (side == 1) {
    text(Sbig, 0.3 * tclSmall + rep(0, length.out=length(Sbig)), Sbig)
} else {
    text(Sbig, -0.3 * tclSmall + rep(1, length.out=length(Sbig)), Sbig)
}
##lines(c(S0, Smax), rep(if (side==1) 0 else 1, 2), col="lightgray")
side

