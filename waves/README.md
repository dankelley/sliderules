A test case is given as \`\`Example II-2-9’’ in Chapter 2 of the Coastal
Engineering Manual (2002): with wind speed 30m/s and fetch 50km,
significant wave height is 4.1m, while the period at the spectral peak
is 6.7s.

    cat('Example problem II-2-9 of Coastal Engineering Manual\n')

    ## Example problem II-2-9 of Coastal Engineering Manual

    library(testthat)
    library(oce)                           # for gravity

    ## Loading required package: gsw

    u10 <- 30                              # m/s
    x <- 50e3                              # m
    g <- gravity(45)                       # m/s^2 (CEM uss 9.82, appropriate at 60N)
    t <- 77.23 * x^0.67 / u10^0.34 / g^0.33 # s
    cat('Wind must blow steadily for >', round(t/3600, 2), 'h for result to hold.\n')

    ## Wind must blow steadily for > 4.47 h for result to hold.

    expect_equal(t/3600, 4.47, tol=0.005)

    CD <- 0.001 * (1.1 + 0.035 * u10)
    ustar <- sqrt(CD) * u10
    expect_equal(ustar, 1.39, tol=0.005)
    cat('ustar=', round(ustar, 2), 'm/s\n')

    ## ustar= 1.39 m/s

    # Nondimensional fetch
    xhat <- g * x / ustar^2
    expect_equal(xhat, 2.54e5, tol=0.0055)

    # wave height
    lambda1 <- 0.0413
    m1 <- 1/2
    Hhatm0 <- lambda1 * xhat^m1
    Hhatm0 <- 0.0413*(2.54e5)^(1/2)
    expect_equal(Hhatm0, 20.8, tol=0.05)
    Hm0 <- Hhatm0 * ustar^2 / g
    expect_equal(Hm0, 4.1, tol=0.05)
    cat('Significant wave height:', round(Hm0, 1), 'm\n')

    ## Significant wave height: 4.1 m

    # Period at spectral peak (note that the CEM seems to have typesetting errors
    # here, using upper-case Xhat, whereas the preceding text has lower-case xhat,
    # and putting a hat on lambda2 in one equation but not in he next.
    lambda2 <- 0.751
    m2 <- 1/3
    Thatp <- lambda2 * xhat^m2
    expect_equal(Thatp, 47.5, tol=0.05)
    Tp <- Thatp * ustar / g
    expect_equal(Tp, 6.7, tol=0.05)
    cat('Spectral peak at:', round(Tp, 1), 's\n')

    ## Spectral peak at: 6.7 s

**References** 1.
<a href="https://github.com/dankelley/sliderules/issues/2" class="uri">https://github.com/dankelley/sliderules/issues/2</a>
2. U.S. Army Corps of Engineers. “Coastal Engineering Manual,” April 30,
2002.
<a href="https://www.publications.usace.army.mil/USACE-Publications/Engineer-Manuals/u43544q/636F617374616C20656E67696E656572696E67206D616E75616C/" class="uri">https://www.publications.usace.army.mil/USACE-Publications/Engineer-Manuals/u43544q/636F617374616C20656E67696E656572696E67206D616E75616C/</a>.
