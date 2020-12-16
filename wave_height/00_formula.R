#' Compute wave height and period from wind speed and fetch
#'
#' @param wind numeric value giving wind speed at 10m above the surface in m/s.
#' @param fetch numeric value giving fetch in m.
#' @param g numeric value giving acceleration due to gravity.  (This is a slight
#' function of latitude, so a worrier might use `oce::gravity()` to calculate this,
#' but it seems unlikely that either fetch or wind speed will be known to 1 part
#' in 1000, so the mid-latitude default ought to suffice.
#'
#' @return A list containing `criterion`, the time (in seconds) that the wind must have
#' blown steadily for the results to be valid, `height` (in metres), the significant wave height
#' and `period` (in seconds) the period at the spectral peak.
#'
#' @examples
#' wp <- waveProperties(30, 50e3) # 16092.52 s, 4.105m, 6.739s
#' cat("Assuming wind is steady for", round(wp$criterion/3600, 2), 'hours (check: 4.47h?), we predict\n')
#' cat("significant wave height:", round(wp$height, 1), 'm (check: 4.1m?)\n')
#' cat("spectral peak at:", round(wp$period, 1), 's (check: 6.7s?)\n')
#'
#' @references
#' U.S. Army Corps of Engineers. “Coastal Engineering Manual,” April 30, 2002.
#' https://www.publications.usace.army.mil/USACE-Publications/Engineer-Manuals/u43544q/636F617374616C20656E67696E656572696E67206D616E75616C/.
waveProperties <- function(wind, fetch, g=9.81)
{
    rval <- list()
    u10 <- wind
    x <- fetch
    t <- 77.23 * x^0.67 / u10^0.34 / g^0.33 # s
    rval$criterion <- t
    CD <- 0.001 * (1.1 + 0.035 * u10)
    ustar <- sqrt(CD) * u10
    xhat <- g * x / ustar^2            # nondimensional fetch
    ## wave height
    lambda1 <- 0.0413
    m1 <- 1/2
    Hhatm0 <- lambda1 * xhat^m1
    Hhatm0 <- 0.0413*(2.54e5)^(1/2)
    Hm0 <- Hhatm0 * ustar^2 / g
    rval$height <- Hm0
    ## Period at spectral peak (note that the CEM seems to have typesetting errors
    ## here, using upper-case Xhat, whereas the preceding text has lower-case xhat,
    ## and putting a hat on lambda2 in one equation but not in he next.
    lambda2 <- 0.751
    m2 <- 1/3
    Thatp <- lambda2 * xhat^m2
    Tp <- Thatp * ustar / g
    rval$period <- Tp
    rval
}
