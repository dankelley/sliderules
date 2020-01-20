## Instructions for construction and use are printed on the output PDFs.
library(oce)
library(grid)
RMS <- function(x) sqrt(mean(x^2, na.rm=TRUE))
vectorLength <- 1000 # for axes
debug <- 0 # set to 1 if adjusting central text (to centre by eye)
load("01_model_selection.rda")
errorRMS <- RMS(residuals(model) / predict(model))
errorWorst <- max(residuals(model) / predict(model))
C <- coef(model)
R <- list(ss=0.65, T=0.8, p=0.9, S=0.91, pointer=0.91) # radii of axis circles
col <- list(ss="royalblue", T="darkred", p="cyan", S="darkgreen", cut="gray")
lty <- list(cut="31")
lwd <- list(cut=1.4)
scale <- 0.27 * 2 * pi                 # adjust this to fill most of circle
cexName <- 1.25                        # cex for axis names

Tfunc <- function(TT)
{
    C[["(Intercept)"]] + C[["T"]]*TT + C[["T2"]]*TT^2 + C[["T3"]]*TT^3
}

pfunc <- function(pp)
{
    Tfunc(Tmax) + C[["p"]]*pp + C[["p2"]]*pp^2
}

Sfunc <- function(SS)
{
    Tfunc(Tmax) + pfunc(pmax) + C["S"]*(SS-S0)
}

circle <- function(R, nseg=512, ...)
{
    theta <- seq(0, 2*pi, length.out=nseg)
    lines(R * cos(theta), R * sin(theta), ...)
}

cutCircle <- function(R)
{
    circle(R, col=col$cut, lty=lty$cut, lwd=lwd$cut)
}

#' draw text along a circular arc
#'
#' This has problems positioning letters, and so there are certain characters
#' that can be inserted in the string `text` to control positioning:
#' * `\\r` means to move right a small amount, e.g. `T\\re` draws `T` and `e` closer
#' * `\\l` means to move left a small amount
#' * `\\d` means to move down a small amount
#' * `\\u` means to move up a small amount
circularText <- function(R, theta0, dtheta=-2.5, text, ...) # angles in deg
{
    em <- grid::calcStringMetric("m")$width # width of 'm' in inches
    ex <- grid::calcStringMetric("x")$ascent # height of 'x' in inches
    if (is.numeric(text)) {
        ## Tighten space after a negative number
        text <- as.character(text)
        text <- gsub("-", "-\\\\l\\\\l", text)
    }
    letters <- strsplit(text, "")[[1]]
    theta <- theta0
    i <- 1
    dr <- 0
    while (i <= length(letters)) {
        if (letters[i] == "\\") {
            if (letters[i+1] == "l") {
                theta <- theta - dtheta / 4
            } else if (letters[i+1] == "r") {
                theta <- theta + dtheta / 4
            } else if (letters[i+1] == "u") {
                dr <- dr + 0.1 * ex * userPerInch 
            } else if (letters[i+1] == "d") {
                dr <- dr - 0.1 * ex * userPerInch 
            } else {
                warning("unrecognized \\ character, '", letters[i+1], "'\n")
            }
            i <- i + 1 # skip over next char, which is the code
        } else {
            thetaRadians <- pi / 180 * theta
            ## message("i=", i, ", letter='", letters[i], "'")
            ## points(R * cos(thetaRadians), R * sin(thetaRadians), col=2)
            descent <- calcStringMetric(letters[i])$descent # inches
            ## message(letters[i], ", dr=", dr)
            RR <- R - 0.75 * descent * userPerInch + dr
            text(RR * cos(thetaRadians), RR * sin(thetaRadians), letters[i],
                 srt=theta-90, ...)
            dtheta <- -(0.2 * em + 1.0 * grid::calcStringMetric(letters[i])$width) / (2 * pi * R / userPerInch) * 360
            theta <- theta + dtheta
        }
        i <- i + 1
    }
}

circularRug <- function(x, tcl, R, inside=TRUE, col=col, debug=debug)
{
    if (debug > 1)
        cat("  in circularRug(x=c(", x[1], ",", x[2], ",...,", tail(x,1), "), tcl=", tcl, ", R=", R, ")\n")
    oldxpd <- par("xpd")
    par(xpd=NA)
    tcl <- abs(tcl) * if (inside) -1 else 1
    if (debug)
        message("rug: ", paste(round(x,1), collapse=" "))
    theta <- -x * pi / 180
    x0 <- R * cos(theta)
    y0 <- R * sin(theta)
    x1 <- (R + tcl) * cos(theta)
    y1 <- (R + tcl) * sin(theta)
    segments(x0, y0, x1, y1, col=col)
    if (debug) {
        for (i in seq_along(x))
            cat(sprintf("rug[%d] at theta=%.2f x0=%5.2f y0=%5.2f and x1=%5.2f y1=%5.1f\n", i, theta[i], x0[i], y0[i], x1[i], y1[i]))
    }
    par(xpd=oldxpd)
}

#' @param theta0 locaiton of min(x), in degrees clockwise of the top of the circle
circularAxis2 <- function(x, sub, func, tclSmall, tclMiddle, tclBig,
                          inside=TRUE, R=1, col="black",
                          theta0=0, debug=0)
{
    if (missing(sub)) sub <- c(10, 5, 1)
    if (missing(func)) func <- function(x) x
    if (missing(tclSmall)) tclSmall <- 0.015 * if (inside) 1 else -1
    if (missing(tclMiddle)) tclMiddle <- 3/2 * tclSmall
    if (missing(tclBig)) tclBig <- 3/2 * tclMiddle
    xmin <- min(x, na.rm=TRUE)
    xmax <- max(x, na.rm=TRUE)
    cat(vectorShow(c(xmin, xmax)))
    cat(vectorShow(sub))
    start <- sub[1] * floor(xmin/sub[1])
    xSmall <- seq(sub[3]*floor(xmin/sub[3]), sub[3]*floor(xmax/sub[3]), sub[3])
    cat(vectorShow(xSmall))
    xMiddle <- seq(sub[2]*floor(xSmall[1]/sub[2]), sub[2]*floor(xmax/sub[2]), sub[2])
    cat(vectorShow(xMiddle))
    xBig <- seq(sub[3]*floor(xMiddle[1]/sub[3]), sub[3]*floor(xmax/sub[3]), sub[1])
    cat(vectorShow(xBig))
    labels <- xBig
    oldxpd <- par("xpd")
    par(xpd=NA)
    circularRug(scale * func(xSmall), tcl=tclSmall, inside=inside, R=R, col=col, debug=debug)
    circularRug(scale * func(xMiddle), tcl=tclMiddle, inside=inside, R=R, col=col, debug=debug)
    circularRug(scale * func(xBig), tcl=tclBig, inside=inside, R=R, col=col, debug=debug)
    theta <- -pi / 180 * (scale * func(xBig)) # location along circumferenace
    rr <- R - 1.75 * tclBig
    for (i in seq_along(theta)) {
        ##points(rr * cos(theta[i]), rr * sin(theta[i]), col=2,pch=20)
        text(rr * cos(theta[i]), rr * sin(theta[i]),
             xBig[i], srt=-90+theta[i]*180/pi, col=col)
        if (debug) {
            cat(sprintf("text[%d]='%s' at %.2f deg x=%5.2f y=%5.2f\n", i, xBig[i], theta[i], rr*cos(theta[i]), rr*sin(theta[i])))
        }
    }
    message("circularAxis2(): func(xBig) = ", paste(round(func(xBig)), collapse=" "))
    ##points((R-2.5*tclBig) * cos(theta[1]), (R-2.5*tclBig) * sin(theta[1]),pch=20)
    circle(R=R, col=col)
    par(xpd=oldxpd)
}

circularAxis <- function(xSmall, xMiddle, xBig, func, tclSmall, tclMiddle, tclBig, inside=TRUE, R=1, col="black", theta0=pi/20, direction="cw", debug=0)
{
    if (missing(func)) func <- function(x) x
    if (missing(tclSmall)) tclSmall <- 0.01 * if (inside) 1 else -1
    if (missing(tclMiddle)) tclMiddle <- 2 * tclSmall
    if (missing(tclBig)) tclBig <- sqrt(2) * tclMiddle
    labels <- xBig
    oldxpd <- par("xpd")
    par(xpd=NA)
    circularRug(scale * func(xSmall), tcl=tclSmall, inside=inside, R=R, col=col, debug=debug)
    circularRug(scale * func(xMiddle), tcl=tclMiddle, inside=inside, R=R, col=col, debug=debug)
    circularRug(scale * func(xBig), tcl=tclBig, inside=inside, R=R, col=col, debug=debug)
    theta <- scale * pi / 180 * func(xBig) # location along circumferenace
    angle <- theta0 + theta * if (direction=="cw") -1 else 1
    rr <- R - 2.5 * tclBig
    for (i in seq_along(angle)) {
        text(rr * cos(angle[i]), rr * sin(angle[i]),
             xBig[i], srt=-90+angle[i]*180/pi, col=col)
    }
    circle(R=R, col=col)
    par(xpd=oldxpd)
}

startPage <- function(layer)
{
    if (!interactive())
        pdf(paste("sound_speed_circular_1_", layer, ".pdf", sep=""), width=7, height=7, pointsize=8)
    par(mar=rep(0.5, 4))
    par(mar=rep(1, 4), lwd=1.4)
    plot(c(-1, 1), c(-1, 1), asp=1, xlab="", ylab="", type="n", axes=debug>0)# , yaxs="i", xaxs="i")
    box()
    if (debug)
        grid(nx=20, ny=20, col="pink")
    mtext(paste0(" Seawater sound-speed slide rule (", layer, " layer)"), line=0, adj=0, font=2)
    line <- -1
    mtext(" Assembly instructions", line=line, adj=0, font=3)
    if (layer == "pointer") {
        line <- line - 1
        mtext(" Cut along the dotted gray oblong shape,", line=line, adj=0)
        line <- line - 1
        mtext("   trace the resultant perimeter onto transparent plastic,", line=line, adj=0)
        line <- line - 1
        mtext("   cut plastic along the traced outline,", line=line, adj=0)
        line <- line - 1
        mtext("   and draw the mid-line with waterproof ink.", line=line, adj=0)
    } else {
        line <- line - 1
        mtext(" Cut just inside the dotted gray circle.", line=line, adj=0)
    }
    line <- line - 1
    mtext(" Pierce at the central dot.", line=line, adj=0)
    line <- line - 1
    mtext(" Assemble layers by size, with pointer layer at top.", line=line, adj=0)
    line <- line - 1
    mtext(" Insert a pivot at the piercing.", line=line, adj=0)
}

endPage <- function()
{
    if (!interactive())
        dev.off()
}

for (layer in c("top", "middle", "bottom", "pointer")[c(1,4)]) {
    startPage(layer)
    userPerInch <- diff(par("usr")[1:2]) / par("pin")[1]
    points(0, 0)                       # pivet point
    if (layer == "top") {
        ## Temperature axis
        T <- seq(T0, Tmax, length.out=vectorLength)
        RR <- R$T - par("cex")/20
        ## Position axis name, which is fiddly work
        angle <- 138
        ##circularText(R=RR, theta0=angle, text="T\\!emp\\!e\\!r\\!a\\!t\\!u\\!r\\!e\\:\\:[ ]", cex=cexName, col=col$T)
        circularText(R=RR, theta0=angle, text="Te\\rmper\\ra\\ut\\d\\r\\rur\\re \\u\\u\\u\\u[\\d\\d\\d\\d    \\u\\u\\u\\u]\\d\\d\\d", cex=cexName, col=col$T)
        tmp <- 26
        text(RR*cos((angle-tmp)*pi/180), RR*sin((angle-tmp)*pi/180), expression(degree*"C"), cex=cexName, srt=26, col=col$T)
        circularAxis2(T, c(2,1,0.5), fun=Tfunc, inside=TRUE, R=R$T, col=col$T, debug=debug)
        ## sound-speed axis
        ss <- seq(ss0, ssmax, length.out=vectorLength)
        circularAxis2(ss, c(10,5,1), inside=TRUE, R=R$ss, col=col$ss, debug=debug)
        RR <- R$ss - par("cex")/20
        circularText(R=RR, theta0=155, text="Soun\\u\\ud\\d\\d Spee\\u\\ud\\d\\d \\u\\u\\u[\\d\\d\\d m/\\rs\\r\\u\\u\\u]", cex=cexName, col=col$ss)
        y0 <- 0.25
        dy <- 0.08
        x <- ss - 0.1
        cexText <- 0.95
        y <- 0.5
        dy <- 0.05
        text(0, y, "Seawater", cex=1.4*cexText)
        y <- y - 1.2 * dy
        text(0, y, "Sound Speed Calculator", cex=1.4*cexText)
        y <- y - 1.2 * dy
        text(0, y, "Usage: Align p=0dbar with observed T,", cex=cexText)
        y <- y - dy
        text(0, y, paste0("align S=", S0, " with observed p,"), cex=cexText)
        y <- y - dy
        text(0, y, "align pointer with observed S,", cex=cexText)
        y <- y - dy
        text(0, y, "and then read sound speed at pointer.", cex=cexText)
        y <- y - dy
        text(0, y, sprintf("Example: %.1fm/s at T=%.0f p=%.0f S=%.0f", swSoundSpeed(S0,0,p0),0,p0,S0), cex=cexText)
        y <- -0.15
        y <- y - dy
        text(0, y, sprintf("RMS error: %.1f m/s (%.2f%%)",
                           RMS(residuals(model)),
                           100*RMS(residuals(model)/predict(model))),
             cex=cexText)
        y <- y - dy
        text(0, y, sprintf("Maximum error: %.1f m/s (%.1f%%)",
                           max(residuals(model)),
                           100*max(residuals(model)/predict(model))),
             cex=cexText)
        y <- -0.4
        text(0, y, "Model 1, S/N 1 (for EC)", cex=cexText)
        y <- y - dy
        text(0, y, "(c) 2020 Dan Kelley", cex=cexText)


        cutCircle(R$T+0.01)

        ##Cross-terms omar <- par("mar")
        ##Cross-terms par(new=TRUE)
        ##Cross-terms par(mai=c(2.2,2.8,3.8,2.8), cex=0.9, tcl=-0.25, mgp=c(1.3, 0.3, 0))
        ##Cross-terms range <- range((G$S - Smid)*(G$T - Tmid))
        ##Cross-terms x <- seq(range[1], range[2], length.out=n)
        ##Cross-terms plot(x, C["SSTT"]*x, lwd=1.4, xaxs="i", ylim=c(-0.1, 0.1),
        ##Cross-terms      xlab=paste("(S-", Smid, ")*(T-", Tmid, ")", sep=""), ylab=expression(kg/m^3), type="l")
        ##Cross-terms mtext("0.00", side=2, at=0, line=0.3, cex=0.9, col=col$sigmaTheta)
        ##Cross-terms grid(lty=1, col="lightgray")
        ##Cross-terms rug(side=2, x=seq(-0.1,0.1,by=0.01), ticksize=-0.02, col=col$sigmaTheta)
        ##Cross-terms legend("topright", legend=expression("Add to "*sigma[theta]), bg="white")
        ##Cross-terms lines(x, C["SSTT"]*x, lwd=1.4)
        ##Cross-terms stop() 
    } else if (layer == "middle") {
        pBig <- seq(p0, pmax, by=1)
        pMiddle <- seq(p0, pmax, by=0.5)
        ppmall <- seq(p0, pmax, by=0.1)
        circularAxis(ppmall, pMiddle, pBig, func=pfunc, inside=TRUE, R=R$p, col=col$p, debug=debug)
        circularText(R=R$p-0.02, theta0=82, text="Pressure", cex=cexName, col=col$p)
    } else if (layer == "bottom") {
        ##
        ## sound-speed a axis
        ssBig <- seq(ss0, ssmax, by=1)
        ssMiddle <- seq(sigthe0, sigthemax, by=5)
        ssSmall <- seq(sigthe0, sigthemax, by=1)
        circularAxis(ssSmall, ssMiddle, ssBig, inside=TRUE, R=R$ss, col=col$sigmaTheta, debug=debug)
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
        cutcircle(R=R$S+0.01)
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
        pointerWidth <- 0.1
        lines(rep(pointerWidth, 2), c(0, R$pointer+0.15-pointerWidth), col=col$cut, lty=lty$cut, lwd=lwd$cut)
        lines(rep(-pointerWidth, 2), c(0, R$pointer+0.15-pointerWidth), col=col$cut, lty=lty$cut, lwd=lwd$cut)
        theta <-seq(-pi, 0, length.out=128)
        pointerRadius <- pointerWidth
        lines(pointerRadius*cos(theta), pointerRadius*sin(theta), col=col$cut, lty=lty$cut, lwd=lwd$cut)
        lines(pointerRadius*cos(-theta), R$pointer+0.15-pointerWidth+pointerRadius*sin(-theta), col=col$cut, lty=lty$cut, lwd=lwd$cut)
        lines(rep(0, 2), c(0, R$pointer+0.15))
    } else {
        stop("unrecognized 'layer' (programming error)")
    }
    endPage()
}

