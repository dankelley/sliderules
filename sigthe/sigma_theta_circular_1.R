## Instructions: cut along the gray dotted circles.
library(oce)
debug <- 0
colSigmaTheta <- "royalblue"
colT <- "darkred"
colS <- "darkgreen"
load("00.rda")
load("04.rda")
C <- coef(m)
nseg <- 512                            # segments in circle (128 is enough, but more is okay)
xc <- 0                                # centre x
yc <- 0                                # centre y
R <- list(sigthe=0.65, S=0.8, T=0.81)  # radii of axis circles
scale <- 5 * 2 * pi                    # adjust this to fill most of circle
cexName <- 1.25                        # cex for axis names
## Get axis ranges from earlir analysis (see 00.R)
sigthe0 <- floor(swSigmaTheta(min(G$S), max(G$T), 100))
sigthemax <- ceiling(swSigmaTheta(max(G$S), min(G$T), 100))

Sfunc <- function(S)
{
    C["(Intercept)"] + C["SS"]*(S-S0) + C["SS2"]*(S-S0)^2
}

Tfunc <- function(T)
{
    Sfunc(Smax) + C["TT"]*(T-T0) + C["TT2"]*(T-T0)^2
}
circle <- function(R, ...)
{
    theta <- seq(0, 2*pi, length.out=nseg)
    lines(xc + R * cos(theta), yc + R * sin(theta), ...)
}

myrugCircular <- function(x, tcl, R, inside=TRUE, col=col, debug=debug)
{
    if (debug > 1)
        cat("  in myrugCircular(x=c(", x[1], ",", x[2], ",...,", tail(x,1), "), tcl=", tcl, ", R=", R, ")\n")
    oldxpd <- par("xpd")
    par(xpd=NA)
    tcl <- abs(tcl) * if (inside) -1 else 1
    x0 <- xc + R * cos(x*pi/180)
    y0 <- yc + R * sin(x*pi/180)
    x1 <- xc + (R + tcl) * cos(x*pi/180)
    y1 <- yc + (R + tcl) * sin(x*pi/180)
    segments(x0, y0, x1, y1, col=col)
    par(xpd=oldxpd)
}

fancyAxisCircular <- function(xSmall, xMiddle, xBig, func, tclSmall, tclMiddle, tclBig, inside=TRUE, R=1, col="black", debug=0)
{
    if (missing(func)) func <- function(x) x
    if (missing(tclSmall)) tclSmall <- 0.01 * if (inside) 1 else -1
    if (missing(tclMiddle)) tclMiddle <- 2 * tclSmall
    if (missing(tclBig)) tclBig <- sqrt(2) * tclMiddle
    labels <- xBig
    if (debug > 0) {
        cat("in fancyAxisCircular(inside=", inside, ", R=", R, ")\n")
        cat("  ", vectorShow(labels))
        cat("  ", vectorShow(xSmall))
        cat("  ", vectorShow(xMiddle))
        cat("  ", vectorShow(xBig))
    }
    oldxpd <- par("xpd")
    par(xpd=NA)
    myrugCircular(scale * func(xSmall), tcl=tclSmall, inside=inside, R=R, col=col, debug=debug)
    myrugCircular(scale * func(xMiddle), tcl=tclMiddle, inside=inside, R=R, col=col, debug=debug)
    myrugCircular(scale * func(xBig), tcl=tclBig, inside=inside, R=R, col=col, debug=debug)
    theta <- scale * pi / 180 * func(xBig) # location along circumferenace
    for (i in seq_along(theta)) {
        text(xc + (R-2.5*tclBig) * cos(theta[i]), yc + (R-2.5*tclBig) * sin(theta[i]), xBig[i],
             srt=-90+theta[i]*180/pi, col=col)
    }
    circle(R=R, col=col)
    par(xpd=oldxpd)
}

for (layer in c("lower", "upper")) {
    if (!interactive())
        pdf(paste("sigma_theta_circular_1_", layer, ".pdf", sep=""), width=7, height=7, pointsize=8)
    par(mar=rep(1, 4), lwd=1.4)
    plot(c(-1, 1), c(-1, 1), xlab="", ylab="", type="n", axes=debug>0)# , yaxs="i", xaxs="i")
    points(0, 0) # for rivet
    if (debug)
        grid(col="pink", lty=1)
    if (layer == "lower") {
        ## T axis
        TBig <- seq(T0, Tmax, by=2)
        TMiddle <- seq(T0, Tmax, by=1)
        TSmall <- seq(T0, Tmax, by=0.5)
        fancyAxisCircular(TSmall, TMiddle, TBig, func=Tfunc, inside=FALSE, R=R$T, col=colT, debug=debug)
        ## text(-0.312, R$T+0.030, expression("T"), pos=1, cex=cexName, srt=22, col=colT)
        ## text(-0.280, R$T+0.046, expression("["), pos=1, cex=cexName, srt=19, col=colT)
        ## text(-0.245, R$T+0.054, expression(degree*"C"), pos=1, cex=cexName, srt=17, col=colT)
        ## text(-0.210, R$T+0.065, expression("]"), pos=1, cex=cexName, srt=13, col=colT)
        text(-0.090, R$T+0.090, expression("T"), pos=1, cex=cexName, srt=5, col=colT)
        text(-0.065, R$T+0.095, expression("["), pos=1, cex=cexName, srt=3.5, col=colT)
        text(-0.035, R$T+0.095, expression(degree*"C"), pos=1, cex=cexName, srt=2, col=colT)
        text(-0.002, R$T+0.095, expression("]"), pos=1, cex=cexName, srt=0.5, col=colT)
        circle(R=R$T+0.15, col="gray", lty="dotted")
    }
    if (layer == "upper") {
        ## S axis
        SBig <- seq(S0, Smax, by=1)
        SMiddle <- seq(S0, Smax, by=0.5)
        SSmall <- seq(S0, Smax, by=0.1)
        fancyAxisCircular(SSmall, SMiddle, SBig, func=Sfunc, inside=TRUE, R=R$S, col=colS, debug=debug)
        text(0.19, 0.72, expression("S"), cex=cexName, srt=-15, col=colS)
        ## sigma-theta axis
        sigtheBig <- seq(sigthe0, sigthemax, by=1)
        sigtheMiddle <- seq(sigthe0, sigthemax, by=0.5)
        sigtheSmall <- seq(sigthe0, sigthemax, by=0.1)
        fancyAxisCircular(sigtheSmall, sigtheMiddle, sigtheBig, inside=TRUE, R=R$sigthe, col=colSigmaTheta, debug=debug)
        text(0.000, R$sigthe-0.070, expression(sigma[theta]), cex=1.2*cexName, srt=-4, col=colSigmaTheta)
        text(0.040, R$sigthe-0.068, expression(" ["), cex=cexName, srt=-5, col=colSigmaTheta)
        text(0.080, R$sigthe-0.073, "kg", cex=cexName, srt=-10, col=colSigmaTheta)
        text(0.118, R$sigthe-0.076, "/", cex=cexName, srt=-14, col=colSigmaTheta)
        text(0.153, R$sigthe-0.077, expression(" "*m^3), cex=cexName, srt=-17, col=colSigmaTheta)
        text(0.190, R$sigthe-0.100, "]", cex=cexName, srt=-20, col=colSigmaTheta)
        y0 <- 0.25
        dy <- 0.08
        x <- sigthe0 - 0.1
        cexText <- 0.95
        y <- 0.45
        dy <- 0.05
        text(-0.26, y, expression("Seawater "*sigma[theta]*" Calculator"), pos=4, cex=1.4*cexText)
        y <- y - dy
        text(-0.25, y, expression("(1) Place T=0 above observed S,"), pos=4, cex=cexText)
        y <- y - dy
        text(-0.24, y, expression("(2) move pointer to observed T,"), pos=4, cex=cexText)
        y <- y - dy
        text(-0.33, y, expression("(3) read approximate "*sigma[theta]*" from inner ring,"), pos=4, cex=cexText)
        y <- y - dy
        text(-0.35, y, expression("and then (4) add "*sigma[theta]*" correction from graph."), pos=4, cex=cexText)
        y <- y - dy
        EGS <- 32
        EGT <- 5
        EGp <- 0
        EGsigma <- sprintf("%.2f", round(swSigmaTheta(EGS, EGT, EGp), 3))
        text(-0.38, y, bquote("Example: "*sigma[theta]*"="*.(EGsigma)*kg/m^3*" at S="*.(EGS)*" and T="*.(EGT)*degree*"C."), pos=4, cex=cexText)
        y <- y - dy
        ERRrms <- round(RMS(residuals(m)), 2)
        ERRmax <- round(max(residuals(m)), 2)
        text(-0.5, y, bquote("Error: "*.(ERRrms)*kg/m^3*" (rms), "*.(ERRmax)*kg/m^3*" (max) from 0 to 500dbar."), pos=4, cex=cexText)
        y <- y - dy
        text(-0.6, y, "For CTT from DEK", pos=4, cex=cexText)
        text(+0.2, y, "(c) 2019 Dan Kelley", pos=4, cex=cexText)
        circle(R=R$S+0.01, col="gray", lty="dotted")
        omar <- par("mar")
        par(new=TRUE)
        par(mai=c(2.1,2.8,3.9,2.8), cex=0.9, tcl=-0.25, mgp=c(1.3, 0.3, 0))
        range <- range((G$S - Smid)*(G$T - Tmid))
        x <- seq(range[1], range[2], length.out=n)
        plot(x, C["SSTT"]*x, lwd=1.4, xaxs="i", ylim=c(-0.1, 0.1),
             xlab=paste("(S-", Smid, ")*(T-", Tmid, ")", sep=""), ylab=expression(kg/m^3), type="l")
        mtext("0.00", side=2, at=0, line=0.3, cex=0.9, col=colSigmaTheta)
        grid(lty=1, col="lightgray")
        rug(side=2, x=seq(-0.1,0.1,by=0.01), ticksize=-0.02, col=colSigmaTheta)
        legend("topright", legend=expression("Add to "*sigma[theta]), bg="white")
        lines(x, C["SSTT"]*x, lwd=1.4)
    }
    par(mar=rep(0.5, 4))
    box()
    if (!interactive())
        dev.off()
}

