**NOTE:** looking only at equilibrium fetch-limitd case, so far.

**Test case**

\`\`Example II-2-9’’ in Chapter 2 of the Coastal Engineering Manual
(2002): with wind speed 30m/s and fetch 50km, significant wave height is
4.1m, while the period at the spectral peak is 6.7s.

**Files in this directory**

-   `00_formula.R` code patterned on example II-2-9 of CEM. For the test
    case, this yields as below

<!-- -->

    waveProperties(30, 50e3)

    ## $criterion
    ## [1] 16092.52
    ## 
    ## $height
    ## [1] 4.105621
    ## 
    ## $period
    ## [1] 6.739548

-   `01_model_selection.R` code to fit models to the critrion, the
    height, and the period. The (immeasurably small) misfits are shown
    in the top-left margins of the three test panels. This also creates
    `01.rda`, for use later.

**References** 1.
<a href="https://github.com/dankelley/sliderules/issues/2" class="uri">https://github.com/dankelley/sliderules/issues/2</a>
2. U.S. Army Corps of Engineers. “Coastal Engineering Manual,” April 30,
2002.
<a href="https://www.publications.usace.army.mil/USACE-Publications/Engineer-Manuals/u43544q/636F617374616C20656E67696E656572696E67206D616E75616C/" class="uri">https://www.publications.usace.army.mil/USACE-Publications/Engineer-Manuals/u43544q/636F617374616C20656E67696E656572696E67206D616E75616C/</a>.
