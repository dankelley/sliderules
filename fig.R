palette("R4")
orange <- palette.colors()[2]
skyblue <- palette.colors()[3]
if (!interactive())
    png("fig.png", unit="in", width=5, height=1, pointsize=9, res=100)

ruler <- function(y, shift, side=1, height=0.5, eps=0.05, ...)
{
    n <- 500
    x <- seq(0, 8, length.out=n)
    message("y=", y, ", shift=", shift)
    lines(x + shift, rep(y, n), ...)
    if (side == 1) {
        lines(x + shift, height + rep(y, n), ...)
        lines(rep(x[1]+shift, 2), c(y, y+height), ...)
        lines(rep(x[n]+shift, 2), c(y, y+height), ...)
        segments(seq(0, 8, 1)+shift, rep(y, 9),
                 seq(0, 8, 1)+shift, rep(y+eps, 9), ...)
        segments(seq(0, 8, .1)+shift, rep(y, 90),
                 seq(0, 8, .1)+shift, rep(y+eps/2, 90), ...)
        text(seq(0, 8, 1)+shift, rep(y+4*eps, 9)-eps, 0:8, pos=1, ...)
    } else {
        lines(x + shift, -height + rep(y, n), ...)
        lines(rep(x[1]+shift, 2), c(y, y-height), ...)
        lines(rep(x[n]+shift, 2), c(y, y-height), ...)
        segments(seq(0, 8, 1)+shift, rep(y, 9),
                 seq(0, 8, 1)+shift, rep(y-eps, 9), ...)
        segments(seq(0, 8, .1)+shift, rep(y, 90),
                 seq(0, 8, .1)+shift, rep(y-eps/2, 90), ...)
        text(seq(0, 8, 1)+shift, rep(y, 9)-eps, 0:8, pos=1, ...)
     }
}

par(mar=c(0.25, 1, 0.25, 1), mgp=c(1.8,0.7,0))
plot(seq(0, 10), rep(0:1, length.out=11), xlab="", ylab="", axes=FALSE, type="n")

ruler(0.495, 0, side=3, col=orange)
ruler(0.505, 2, side=1, col=skyblue)

if (!interactive()) dev.off()
