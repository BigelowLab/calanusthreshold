calanusthreshold
================

Provides R tools for producing threshold prey models for Calanus.

## Requirements

-   [R v4.1+](https://www.r-project.org/)

-   [biomod2](https://CRAN.R-project.org/package=biomod2)

-   [rlang](https://CRAN.R-project.org/package=rlang)

-   [dplyr](https://CRAN.R-project.org/package=dplyr)

-   [readr](https://CRAN.R-project.org/package=readr)

-   [sf](https://CRAN.R-project.org/package=sf)

## Installation

    remotes::install_github("BigelowLab/calanusthreshold")

## Read Input Data

We provide an anonymized example dataset.

``` r
library(calanusthreshold)
x <- read_input()
x
```

    ## Simple feature collection with 500 features and 31 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -70.7319 ymin: 40.57081 xmax: -56.14393 ymax: 51.63103
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 500 × 32
    ##    station  year  month longitude latitude `Calanus finmarchi… `Calanus finmarc…
    ##  * <chr>   <dbl>  <dbl>     <dbl>    <dbl>               <dbl>             <dbl>
    ##  1 A-01    2004.  6.98      -67.0     45.3               9398.            -6205.
    ##  2 B-01    2009.  6.49      -63.2     49.1              40012.            18310.
    ##  3 C-01    2006. 12.7       -65.4     50.2              -1817.            17998.
    ##  4 C-01    2005.  3.12      -66.2     50.6               9261.            13804.
    ##  5 A-01    2005.  0.802     -67.7     46.3               4051.            -8919.
    ##  6 D-01    1997.  5.63      -64.6     44.3              18878.             2877.
    ##  7 E-01    2011.  6.32      -70.6     48.1               5755.             7948.
    ##  8 F-01    2018.  3.59      -59.4     47.3              10069.            -7545.
    ##  9 E-01    2013.  5.98      -70.6     49.7                916.            -5300.
    ## 10 G-01    2001.  9.49      -59.8     47.6              -2535.             7528.
    ## # … with 490 more rows, and 25 more variables: Calanus finmarchicus VI <dbl>,
    ## #   Calanus hyperboreus IV <dbl>, Calanus hyperboreus V <dbl>,
    ## #   Calanus hyperboreus VI <dbl>, Calanus glacialis IV <dbl>,
    ## #   Calanus glacialis V <dbl>, Calanus glacialis VI <dbl>, bathymetry <dbl>,
    ## #   slope <dbl>, aspect <dbl>, roughness <dbl>, proximity <dbl>, mlotst <dbl>,
    ## #   siconc <dbl>, thetao <dbl>, usi <dbl>, sithick <dbl>, bottomT <dbl>,
    ## #   vsi <dbl>, vo <dbl>, uo <dbl>, so <dbl>, zos <dbl>, chlor_a <dbl>, …

But if you have your own dataset, just provide the filename and path.

    x <- read_input("/path/to/input/data.csv")
