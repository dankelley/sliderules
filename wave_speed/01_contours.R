# Dispersion Relationship
#   Start with omega^2 = g*k*tanh(k*h)
# Phase speed, C_P
#   CP = omega / k
#   CP = sqrt(g/k*tanh(k*h))

rm(list = ls())

n <- 200

RMS <- function(x) sqrt(mean(x^2))

Period <- function(k, h, g = 9.8) {
    # Use omega^2 = g*k*tanh(k*h)
    res <- rep(NA, length(k))
    for (i in seq_along(k)) {
        res[i] <- 2 * pi / sqrt(g * k[i] * tanh(k[i] * h[i]))
    }
    res
}

k <- function(period, h, g = 9.8) {
    cat(
        "k(period=", paste(period, collapse = " "),
        ", h=", paste(h, collapse = " "), ")\n",
        sep = ""
    )
    res <- rep(NA, length.out = length(period))
    for (i in seq_along(period)) {
        cat("  computation for i=", i, "\n", sep = "")
        r <- try(uniroot(
            function(kk) {
                cat("    uniroot trying kk=", kk, " and h=", h[i], "\n", sep = "")
                A <- Period(kk, h = h[i])
                cat("      guess=", A, "; seeking ", period[i], ")\n", sep = "")
                Period(kk, h = h[i]) - period[i]
            },
            c(1e-4, 1e3)
        ), silent = TRUE)
        res[i] <- if (inherits(r, "try-error")) NA else r$root
    }
    res
} # k()

CPk <- function(k, h, g = 9.8) {
    sqrt(g / k) * sqrt(tanh(k * h))
}

CPperiod <- function(period, h) {
    # cat("length(period):", length(period), "\n")
    res <- rep(NA, length(period))
    for (i in seq_along(period)) {
        ## cat(sprintf("%d: %.3fs %.3fm\n", i, period[i], h[i]))
        res[i] <- CPk(k(period[i], h[i]), h[i])
    }
    res
}

CGperiod <- function(period, h, g = 9.8) { # FIXME
    cat("Cgperiod {\n")
    cat("    period=", paste(period, collapse = " "), "\n")
    cat("    h=", paste(h, collapse = " "), "\n")
    kk <- k(period, h)
    cat("    -> kk=", paste(kk, collapse = " "), "\n")
    CP <- CPperiod(period, h)
    (CP / 2) * (1 + 2 * kk * h / sinh(2 * kk * h))
}


period <- seq(2, 20, length.out = n)
h <- seq(1, 200, length.out = n + 1) # make lengths diff. to check code
if (!interactive()) {
    png("01_contours.png",
        unit = "in",
        width = 7, height = 7, pointsize = 10, res = 250
    )
}
par(mfrow = c(2, 1), mar = c(3, 3, 1, 1), mgp = c(1.5, 0.45, 0))
contour(period, h, outer(period, h, CPperiod),
    labcex = 1,
    xaxs = "i", yaxs = "i",
    xlab = "Period [s]", ylab = "Depth [m]"
)
grid()
mtext(expression("Phase Speed, " * C[P] * " [m/s]"))
# CGperiod(period[1], h[1])
contour(period, h, outer(period, h, CGperiod),
    labcex = 1,
    xaxs = "i", yaxs = "i",
    xlab = "Period [s]", ylab = "Depth [m]"
)
grid()
mtext(expression("Group Speed, " * C[G] * " [m/s]"))
if (!interactive()) dev.off()
