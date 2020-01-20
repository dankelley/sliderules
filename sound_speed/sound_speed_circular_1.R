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
R4col <- c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#F5C710", "gray62")
col <- list(ss=R4col[1], T=R4col[2], p=R4col[3], S=R4col[4], cut="gray")
lty <- list(cut="31")
lwd <- list(cut=1.4, axis=1)
scale <- 0.27 * 2 * pi                 # adjust this to fill most of circle
cexName <- 1.25                        # cex for axis names
cutSpace <- c(0.01, 0.05, 0.03)        # inches between axis and cut lines for speed (and T) axes, p axis, S axis, and pointer

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

circularRug <- function(x, tcl, R, inside=TRUE, col=col, lwd=par("lwd"), debug=debug)
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
    segments(x0, y0, x1, y1, lwd=lwd, col=col)
    if (debug) {
        for (i in seq_along(x))
            cat(sprintf("rug[%d] at theta=%.2f x0=%5.2f y0=%5.2f and x1=%5.2f y1=%5.1f\n", i, theta[i], x0[i], y0[i], x1[i], y1[i]))
    }
    par(xpd=oldxpd)
}

#' @param theta0 locaiton of min(x), in degrees clockwise of the top of the circle
circularAxis2 <- function(x, sub, func, tclSmall, tclMiddle, tclBig,
                          inside=TRUE, R=1, lwd=par("lwd"), col="black",
                          theta0=0, debug=0)
{
    if (missing(sub)) sub <- c(10, 5, 1)
    if (missing(func)) func <- function(x) x
    if (missing(tclSmall)) tclSmall <- 0.015 * if (inside) 1 else -1
    if (missing(tclMiddle)) tclMiddle <- 1.8 * tclSmall
    if (missing(tclBig)) tclBig <- 1.41 * tclMiddle
    xmin <- min(x, na.rm=TRUE)
    xmax <- max(x, na.rm=TRUE)
    if (debug) {
        cat(vectorShow(c(xmin, xmax)))
        cat(vectorShow(sub))
    }
    start <- sub[1] * floor(xmin/sub[1])
    xSmall <- seq(sub[3]*floor(xmin/sub[3]), sub[3]*floor(xmax/sub[3]), sub[3])
    xMiddle <- seq(sub[2]*floor(xSmall[1]/sub[2]), sub[2]*floor(xmax/sub[2]), sub[2])
    xBig <- seq(sub[3]*floor(xMiddle[1]/sub[3]), sub[3]*floor(xmax/sub[3]), sub[1])
    if (debug) {
        cat(vectorShow(xSmall))
        cat(vectorShow(xMiddle))
        cat(vectorShow(xBig))
    }
    labels <- xBig
    oldxpd <- par("xpd")
    par(xpd=NA)
    circularRug(scale * func(xSmall), tcl=tclSmall, inside=inside, R=R, col=col, lwd=lwd, debug=debug)
    circularRug(scale * func(xMiddle), tcl=tclMiddle, inside=inside, R=R, col=col, lwd=lwd, debug=debug)
    circularRug(scale * func(xBig), tcl=tclBig, inside=inside, R=R, col=col, lwd=lwd, debug=debug)
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
    circle(R=R, col=col, lwd=lwd, )
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
    mtext(" Stack layers by size, with pointer at top.", line=line, adj=0)
    line <- line - 1
    mtext(" Insert a pivot at the piercing.", line=line, adj=0)
}

endPage <- function()
{
    if (!interactive())
        dev.off()
}

## for (layer in c("top", "middle", "bottom", "pointer")[c(3)]) {
for (layer in c("top", "middle", "bottom", "pointer")) {
    startPage(layer)
    userPerInch <- diff(par("usr")[1:2]) / par("pin")[1]
    points(0, 0)                       # pivet point
    if (layer == "top") {
        ## Temperature axis
        T <- seq(T0, Tmax, length.out=vectorLength)
        RR <- R$T - par("cex")/20
        ## Position axis name, which is fiddly work
        angle <- 138
        circularText(R=RR, theta0=angle, text="Te\\rmper\\ra\\ut\\d\\r\\rur\\re \\u\\u\\u\\u[\\d\\d\\d\\d    \\u\\u\\u\\u]\\d\\d\\d", cex=cexName, col=col$T)
        tmp <- 26
        text(RR*cos((angle-tmp)*pi/180), RR*sin((angle-tmp)*pi/180), expression(degree*"C"), cex=cexName, srt=26, col=col$T)
        circularAxis2(T, c(2,1,0.2), func=Tfunc, inside=TRUE, R=R$T, col=col$T, lwd=lwd$axis, debug=debug)
        ## sound-speed axis
        ss <- seq(ss0, ssmax, length.out=vectorLength)
        circularAxis2(ss, c(10,5,1), inside=TRUE, R=R$ss, col=col$ss, lwd=lwd$axis, debug=debug)
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
        text(0, y, sprintf("Example: %.1fm/s at T=%.0f p=%.0f S=%.0f", swSoundSpeed(30,10,200),10,200,30), cex=cexText)
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
        cutCircle(R$T + cutSpace[1])
    } else if (layer == "middle") {
        ## pressure axis
        p <- seq(p0, pmax, length.out=vectorLength)
        circularAxis2(p, c(500, 100, 50), func=pfunc, inside=TRUE, R=R$p, col=col$p, lwd=lwd$axis, debug=debug)
        RR <- R$p - par("cex")/20
        circularText(RR, theta0=-5, text="P\\lressur\\re \\u\\u\\u\\u[\\d\\d\\d\\d \\!\\!\\!\\!\\udb\\dar\\:\\:\\u\\u\\u\\u]", cex=cexName, col=col$p)
        cutCircle(R$p + cutSpace[1])
    } else if (layer == "bottom") {
        ## salinity axis
        S <- seq(S0, Smax, length.out=vectorLength)
        RR <- R$S - par("cex")/20
        circularAxis2(S, c(5, 1, 0.5), func=Sfunc, inside=TRUE, R=R$S, col=col$S, lwd=lwd$axis, debug=debug)
        circularText(RR, theta0=190, text="\\uS\\da\\ul\\l\\d\\r\\ri\\r\\dn\\ui\\rt\\ry", cex=cexName, col=col$S)
        cutCircle(R$S + cutSpace[2])
    } else if (layer == "pointer") {
        pointerWidth <- 0.1
        lines(rep( pointerWidth, 2), c(0, R$S + cutSpace[2] - pointerWidth), col=col$cut, lty=lty$cut, lwd=lwd$cut)
        lines(rep(-pointerWidth, 2), c(0, R$S + cutSpace[2] - pointerWidth), col=col$cut, lty=lty$cut, lwd=lwd$cut)
        theta <-seq(-pi, 0, length.out=128)
        pointerRadius <- pointerWidth
        lines(pointerRadius*cos(theta), pointerRadius*sin(theta),
              col=col$cut, lty=lty$cut, lwd=lwd$cut)
        lines(pointerRadius*cos(-theta), R$S - cutSpace[3] - 0.25*pointerRadius + pointerRadius*sin(-theta),
              col=col$cut, lty=lty$cut, lwd=lwd$cut)
        lines(rep(0, 2), c(0, R$S))
    } else {
        stop("unrecognized 'layer' (programming error)")
    }
    endPage()
}

