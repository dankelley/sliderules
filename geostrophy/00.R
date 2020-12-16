library(oce)
## v = g/f*eta100 where eta100 is delta eta over 100km distance
## log(v) = log(g) + log(eta100) - log(2*Omega*sin(latitude))

## lower: ring1: latitude
## ring1: latitude
## ring2: eta100

## Instructions: cut along the gray dotted circles. Create a transparent radial
## pointer that reaches to the T axis. Pin these three things together, to
## swivel about the marked central point.

library(oce)
debug <- 0 # set to 1 if adjusting central text (to centre by eye)
C <- coef(m)
R <- list(sigthe=0.65, S=0.8, T=0.81)  # radii of axis circles
col <- list(sigmaTheta="royalblue", T="darkred", S="darkgreen")
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

circle <- function(R, nseg=512, ...)
{
    theta <- seq(0, 2*pi, length.out=nseg)
    lines(R * cos(theta), R * sin(theta), ...)
}

circularText <- function(R, theta0, dtheta, text, ...) # angles in deg
{
    letters <- strsplit(text, "")[[1]]
    theta <- theta0
    i <- 1
    while (i <= length(letters)) {
        if (letters[i] == "\\") {
            if (letters[i+1] == "!") {
                message("negative thinspace")
                theta <- theta - dtheta / 4
            } else if (letters[i+1] == ":") {
                message("positive thinspace")
                theta <- theta + dtheta / 4
            } else {
                warning("unrecognized \\ character\n")
            }
            i <- i + 1 # skip over next char, which is the code
        } else {
            thetaRadians <- pi / 180 * theta
            message("i=", i, ", letter='", letters[i], "'")
            ##text(R * cos(thetaRadians), R * sin(thetaRadians), letters[i], srt=theta-90, pos=1, ...)
            text(R * cos(thetaRadians), R * sin(thetaRadians), letters[i], srt=theta-90, pos=1, ...)
            theta <- theta + dtheta
        }
        i <- i + 1
    }
}

myrugCircular <- function(x, tcl, R, inside=TRUE, col=col, debug=debug)
{
    if (debug > 1)
        cat("  in myrugCircular(x=c(", x[1], ",", x[2], ",...,", tail(x,1), "), tcl=", tcl, ", R=", R, ")\n")
    oldxpd <- par("xpd")
    par(xpd=NA)
    tcl <- abs(tcl) * if (inside) -1 else 1
    x0 <- R * cos(x*pi/180)
    y0 <- R * sin(x*pi/180)
    x1 <- (R + tcl) * cos(x*pi/180)
    y1 <- (R + tcl) * sin(x*pi/180)
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
        text((R-2.5*tclBig) * cos(theta[i]), (R-2.5*tclBig) * sin(theta[i]), xBig[i],
             srt=-90+theta[i]*180/pi, col=col)
    }
    circle(R=R, col=col)
    par(xpd=oldxpd)
}

for (layer in c("lower", "upper", "pointer")) {
    if (!interactive())
        pdf(paste("sigma_theta_circular_1_", layer, ".pdf", sep=""), width=7, height=7, pointsize=8)
    par(mar=rep(1, 4), lwd=1.4)
    plot(c(-1, 1), c(-1, 1), xlab="", ylab="", type="n", axes=debug>0)# , yaxs="i", xaxs="i")
    if (debug)
        grid(nx=20, ny=20, col="pink")
    points(0, 0) # for rivet
    if (layer == "lower") {
        ## T axis
        TBig <- seq(T0, Tmax, by=2)
        TMiddle <- seq(T0, Tmax, by=1)
        TSmall <- seq(T0, Tmax, by=0.5)
        fancyAxisCircular(TSmall, TMiddle, TBig, func=Tfunc, inside=FALSE, R=R$T, col=col$T, debug=debug)
        circularText(R$T+0.08, 80, -2.1, "T\\!emp\\!e\\!r\\!a\\!t\\!u\\!r\\!e\\:\\:[ \\:]", cex=cexName, col=col$T)
        text(0.472, R$T-0.058, expression(degree*"C"), pos=1, cex=cexName, srt=-32, col=col$T)
        circle(R=R$T+0.15, col="gray", lty="dotted")
    } else if (layer == "upper") {
        ## S axis
        SBig <- seq(S0, Smax, by=1)
        SMiddle <- seq(S0, Smax, by=0.5)
        SSmall <- seq(S0, Smax, by=0.1)
        fancyAxisCircular(SSmall, SMiddle, SBig, func=Sfunc, inside=TRUE, R=R$S, col=col$S, debug=debug)
        circularText(R$S-0.02, 82, -2, "S\\:al\\!inity", cex=cexName, col=col$S)
        ##
        ## sigma-theta axis
        sigtheBig <- seq(sigthe0, sigthemax, by=1)
        sigtheMiddle <- seq(sigthe0, sigthemax, by=0.5)
        sigtheSmall <- seq(sigthe0, sigthemax, by=0.1)
        fancyAxisCircular(sigtheSmall, sigtheMiddle, sigtheBig, inside=TRUE, R=R$sigthe, col=col$sigmaTheta, debug=debug)
        text(0.000, R$sigthe-0.070, expression(sigma[theta]), cex=1.2*cexName, srt=-4, col=col$sigmaTheta)
        text(0.040, R$sigthe-0.068, expression(" ["), cex=cexName, srt=-5, col=col$sigmaTheta)
        text(0.080, R$sigthe-0.073, "kg", cex=cexName, srt=-10, col=col$sigmaTheta)
        text(0.118, R$sigthe-0.076, "/", cex=cexName, srt=-14, col=col$sigmaTheta)
        text(0.153, R$sigthe-0.077, expression(" "*m^3), cex=cexName, srt=-17, col=col$sigmaTheta)
        text(0.190, R$sigthe-0.100, "]", cex=cexName, srt=-20, col=col$sigmaTheta)
        y0 <- 0.25
        dy <- 0.08
        x <- sigthe0 - 0.1
        cexText <- 0.95
        y <- 0.45
        dy <- 0.05
        text(-0.27, y, expression("Seawater "*sigma[theta]*" Calculator"), pos=4, cex=1.4*cexText)
        y <- y - 1.2 * dy
        text(-0.32, y, expression("(1) Set 0"*degree*"C to be at observed salinity,"), pos=4, cex=cexText)
        y <- y - dy
        text(-0.414, y, expression("(2) move radial pointer to observed temperature,"), pos=4, cex=cexText)
        y <- y - dy
        text(-0.325, y, expression("(3) read approximate "*sigma[theta]*" from inner ring"), pos=4, cex=cexText)
        y <- y - dy
        text(-0.35, y, expression("and then (4) add "*sigma[theta]*" correction from graph."), pos=4, cex=cexText)
        y <- y - dy
        EGS <- 32
        EGT <- 5
        EGp <- 0
        EGsigma <- sprintf("%.2f", round(swSigmaTheta(EGS, EGT, EGp), 3))
        text(-0.375, y, bquote("Example: "*sigma[theta]*"="*.(EGsigma)*kg/m^3*" at S="*.(EGS)*" and T="*.(EGT)*degree*"C."), pos=4, cex=cexText)
        y <- y - dy
        ERRrms <- round(RMS(residuals(m)), 2)
        ERRmax <- round(max(residuals(m)), 2)
        text(-0.545, y, bquote("Accurate to "*.(ERRrms)*kg/m^3*" (rms) and "*.(ERRmax)*kg/m^3*" (max) up to 500 dbar."), pos=4, cex=cexText)
        y <- y - dy
        text(-0.48, y, "Ser. No. 1, for CTT", pos=4, cex=cexText, font=2)
        text(+0.15, y, "(c) 2019 Dan Kelley", pos=4, cex=cexText)
        circle(R=R$S+0.01, col="gray", lty="dotted")
        omar <- par("mar")
        par(new=TRUE)
        par(mai=c(2.2,2.8,3.8,2.8), cex=0.9, tcl=-0.25, mgp=c(1.3, 0.3, 0))
        range <- range((G$S - Smid)*(G$T - Tmid))
        x <- seq(range[1], range[2], length.out=n)
        plot(x, C["SSTT"]*x, lwd=1.4, xaxs="i", ylim=c(-0.1, 0.1),
             xlab=paste("(S-", Smid, ")*(T-", Tmid, ")", sep=""), ylab=expression(kg/m^3), type="l")
        mtext("0.00", side=2, at=0, line=0.3, cex=0.9, col=col$sigmaTheta)
        grid(lty=1, col="lightgray")
        rug(side=2, x=seq(-0.1,0.1,by=0.01), ticksize=-0.02, col=col$sigmaTheta)
        legend("topright", legend=expression("Add to "*sigma[theta]), bg="white")
        lines(x, C["SSTT"]*x, lwd=1.4)
    } else if (layer == "pointer") {
        circle(R=R$T+0.15, col="gray", lty="dotted")
        pointerWidth <- 0.1
        lines(rep(pointerWidth, 2), c(0, R$T+0.15-pointerWidth))
        lines(rep(-pointerWidth, 2), c(0, R$T+0.15-pointerWidth))
        theta <-seq(-pi, 0, length.out=128)
        pointerRadius <- pointerWidth
        lines(pointerRadius*cos(theta), pointerRadius*sin(theta))
        lines(pointerRadius*cos(-theta), R$T+0.15-pointerWidth+pointerRadius*sin(-theta))
    } else {
        stop("unrecognized 'layer' (programming error)")
    }
    par(mar=rep(0.5, 4))
    box()
    if (!interactive())
        dev.off()
}

