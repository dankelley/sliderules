## omega^2 = g*k*tanh(k*h)

rm(list=ls())

n <- 100

RMS <- function(x) sqrt(mean(x^2))

Period <- function(k, h, g=9.8)
{
    2 * pi / sqrt(g*k*tanh(k*h))
}

k <- function(period, h, g=9.8)
{
    r <- try(uniroot(function(kk) Period(kk, h=h) - period, c(1e-5, 1e5)), silent=TRUE)
    if (inherits(r, "try-error")) NA else r$root
}

Ck <- function(k, h, g=9.8)
{
    sqrt(g/k)*sqrt(tanh(k*h))
}

Cperiod <- function(period, h)
{
    ## cat("length(period):", length(period), "\n")
    res <- rep(NA, length(period))
    for (i in seq_along(period)) {
        ## cat(sprintf("%d: %.3fs %.3fm\n", i, period[i], h[i]))
        res[i] <- Ck(k(period[i], h[i]), h[i])
    }
    res
}

period <- seq(2, 10, length.out=n)
h <- seq(1, 50, length.out=n)
Cperiod(period[1], h[1])
z <- outer(period, h, Cperiod)
#print(range(z, na.rm=TRUE))
par(mar=c(3.5,3.5,1,1), mgp=c(2,0.7,0))
contour(period, h, z, labcex=1,
        xlab="Period [s]", ylab="Depth [m]", main=expression(C[P]))
contour(log10(period), log10(h), z, labcex=1,
        xlab="log Period [s]", ylab="log Depth [m]", main=expression(log[10]*C[P]))
##oce::imagep(period, h, z)
G <- expand.grid(period=period, h=h)
G$lperiod <- log10(G$period)
G$lperiod2 <- G$lperiod^2
G$lperiod3 <- G$lperiod^3
G$lh <- log10(G$h)
G$lh2 <- G$lh^2
G$lh3 <- G$lh^3
G$C <- Cperiod(G$period, G$h)
G$lC <- log10(G$C)
smoothScatter(G$lperiod+G$lh, G$lC)
summary(m1 <- lm(lC ~ lperiod + lh, data=G))
summary(m2 <- lm(lC ~ lperiod + lperiod2 + lh + lh2, data=G))
summary(m3 <- lm(lC ~ lperiod + lperiod2 + lperiod3 + lh + lh2 + lh3, data=G))
summary(m4 <- lm(lC ~ lperiod + lperiod2 + lperiod3 + lh + lh2 + lh3, data=G))
par(mfrow=c(2,2))
hist(G$lC - predict(m4), breaks=100)
hist(100*(G$lC - predict(m4))/G$lC, main="percent diff", breaks=100)
hist(10^G$lC - 10^predict(m4), breaks=100)
hist(100*(10^G$lC - 10^predict(m4))/10^G$lC, main="percent diff", breaks=100)
par(mfrow=c(2,2))
plot(m4)
