library(oce)
debug <- 0
cexName <- 0.9
load("00.rda")
load("04.rda")
C <- coef(m)
Sfunc <- function(S)
{
    C["(Intercept)"] + C["SS"]*(S-S0) + C["SS2"]*(S-S0)^2
}
Tfunc <- function(T)
{
    Sfunc(Smax) + C["TT"]*(T-T0) + C["TT2"]*(T-T0)^2
}

if (!interactive()) pdf("sr1.pdf", width=7, height=2, pointsize=8)
par(mar=c(if (debug > 0) 2 else 1, if (debug > 0) 2 else 1, 2, 1))
sigthe0 <- floor(swSigmaTheta(min(G$S), max(G$T), 100))
sigthemax <- ceiling(swSigmaTheta(max(G$S), min(G$T), 100))
plot(c(sigthe0, sigthemax), c(0, 1), xlab="", ylab="", type="n", axes=debug>0, yaxs="i", xaxs="i")
myrug <- function(x, tcl, y)
{
    if (debug > 1)
        cat("  in myrug(),", vectorShow(x))
    oldxpd <- par("xpd")
    par(xpd=NA)
    segments(x, y, x, y+tcl)
    par(xpd=oldxpd)
}
fancyAxis <- function(xSmall, xMiddle, xBig, func, tclSmall, tclMiddle, tclBig, side=1, y=0, name="")
{
    if (missing(func)) func <- function(x) x
    if (missing(tclSmall)) tclSmall <- 0.01 * if (side==1) 1 else -1
    if (missing(tclMiddle)) tclMiddle <- 2 * tclSmall
    if (missing(tclBig)) tclBig <- sqrt(2) * tclMiddle
    labels <- xBig
    if (debug > 0) {
        cat("in fancyAxis()\n")
        cat(vectorShow(labels))
        cat(vectorShow(y))
        cat(vectorShow(xSmall))
        cat(vectorShow(xMiddle))
        cat(vectorShow(xBig))
    }
    oldxpd <- par("xpd")
    par(xpd=NA)
    myrug(func(xSmall), tcl=tclSmall, y=y)
    myrug(func(xMiddle), tcl=tclMiddle, y=y)
    myrug(func(xBig), tcl=tclBig, y=y)
    text(func(xBig), 2.5*tclBig + rep(y, length.out=length(xBig)), xBig)
    abline(h=y, lwd=0.5*par("lwd"))
    ##text(func(min(xSmall)), y[1], name, pos=if(side==1) 3 else 1)
    par(xpd=oldxpd)
}

## S axis
SBig <- seq(S0, Smax, by=1)
SMiddle <- seq(S0, Smax, by=0.5)
SSmall <- seq(S0, Smax, by=0.1)
fancyAxis(SSmall, SMiddle, SBig, func=Sfunc, side=3, y=0.7)
text(Sfunc(min(G$S)-1.0), 0.69, expression("Practical Salinity"), pos=1, cex=1)#cexName)
#text(Sfunc(min(SBig))+0.4, 0.69, expression("S"), pos=1, cex=cexName)

TBig <- seq(T0, Tmax, by=2)
TMiddle <- seq(T0, Tmax, by=1)
TSmall <- seq(T0, Tmax, by=0.5)
fancyAxis(TSmall, TMiddle, TBig, func=Tfunc, side=1, y=1, name=expression(T*degree*"C"))
oldxpd <- par("xpd")
par(xpd=NA)
text(Tfunc(max(TBig)+4), 1.125, expression("In-situ Temperature ["*degree*"C]"), pos=1, cex=1)#cexName)
#text(Tfunc(max(TBig)-1), 1.125, expression("T ["*degree*"C]"), pos=1, cex=cexName)
#text(Tfunc(max(TBig)-1), 1.125, expression("T"), pos=1, cex=cexName)
par(xpd=oldxpd)


## sigma-theta axis
sigtheBig <- seq(sigthe0, sigthemax, by=1)
sigtheMiddle <- seq(sigthe0, sigthemax, by=0.5)
sigtheSmall <- seq(sigthe0, sigthemax, by=0.1)
fancyAxis(sigtheSmall, sigtheMiddle, sigtheBig, side=1, y=0,
          name=expression(sigma[theta]*kg/m^3))
text(min(sigtheSmall)+0.49, 0.14, expression(sigma[theta]*" ["*kg/m^3*"]"), pos=1, cex=cexName)

D0 <- 0.20
DD <- 0.15
text(sigthe0, D0+2*DD, "Usage: place T=0 at S, slide pointer to T, read rough sigma-theta, add correction from graph.", pos=4)
text(sigthe0, D0+DD, sprintf("Error: %.3f kg/m^3 (rms), %.3f kg/m^3 (max) from 0 to 500dbar.",
                            RMS(residuals(m)), max(residuals(m))),
     pos=4)
text(sigthe0, D0, sprintf("E.G. %.2f kg/m^3 at S=%.0f, T=%.0fC",
                          swSigmaTheta(35, 0, 0), 35, 20, 0), pos=4)

omar <- par("mar")
par(new=TRUE)
par(mar=omar+c(3,38,5,1), mgp=c(0.7, 0.2, 0), cex=0.6, tcl=-0.25)
range <- range((G$S - Smid)*(G$T - Tmid))
x <- seq(range[1], range[2], length.out=n)
plot(x, C["SSTT"]*x, lwd=2,
     xlab=paste("(S-", Smid, ")*(T-", Tmid, ")", sep=""), ylab=expression(kg/m^3), type="l")
grid(lty=1, col="lightgray")
legend("topright", legend=expression("Add"), bg="white")
lines(x, C["SSTT"]*x, lwd=2)
## ## regression check
## if (debug > 0) {
##     par(xpd=FALSE)
##     abline(v=swSigmaTheta(31,T0,0), col=2)
##     abline(v=24.58, col=2, lty=2)
##     mtext("error 0.1 kg/m^3", side=3, at=24.58, col=2)
##     par(xpd=NA)
## }

if (!interactive()) dev.off()

