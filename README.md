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
x <- read_dataset()
x
```

    ## Simple feature collection with 500 features and 31 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -70.85388 ymin: 40.54264 xmax: -55.5305 ymax: 51.67751
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 500 × 32
    ##    station  year month longitude latitude `Calanus finmarchi… `Calanus finmarch…
    ##  * <chr>   <dbl> <dbl>     <dbl>    <dbl>               <dbl>              <dbl>
    ##  1 A-01    2019.  4.01     -60.2     45.2                  0              33823.
    ##  2 B-01    2014.  6.44     -67.6     47.8              13684.              6461.
    ##  3 C-01    2018.  9.85     -68.1     41.2               6675.              5116.
    ##  4 D-01    2020  10.2      -70.1     50.0                  0                  0 
    ##  5 E-01    2020  10.6      -59.2     45.8              31330.                 0 
    ##  6 F-01    2008.  2.51     -64.7     47.0               6878.              7678.
    ##  7 A-01    2010.  1.89     -62.4     44.9              19216.              9855.
    ##  8 A-01    2014.  4.89     -64.0     44.0               2474.                 0 
    ##  9 G-01    2006.  4.37     -58.7     50.5               4054.             12186.
    ## 10 H-01    2020   5.59     -68.1     44.7              12990.              3908.
    ## # … with 490 more rows, and 25 more variables: Calanus finmarchicus VI <dbl>,
    ## #   Calanus hyperboreus IV <dbl>, Calanus hyperboreus V <dbl>,
    ## #   Calanus hyperboreus VI <dbl>, Calanus glacialis IV <dbl>,
    ## #   Calanus glacialis V <dbl>, Calanus glacialis VI <dbl>, bathymetry <dbl>,
    ## #   slope <dbl>, aspect <dbl>, roughness <dbl>, proximity <dbl>, mlotst <dbl>,
    ## #   siconc <dbl>, thetao <dbl>, usi <dbl>, sithick <dbl>, bottomT <dbl>,
    ## #   vsi <dbl>, vo <dbl>, uo <dbl>, so <dbl>, zos <dbl>, chlor_a <dbl>, …

But if you have your own dataset, just provide the filename and path.

    x <- read_dataset("/path/to/input/data.csv")
