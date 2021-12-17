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
    ## Bounding box:  xmin: -70.99562 ymin: 40.9032 xmax: -55.91088 ymax: 51.44948
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 500 × 32
    ##    station  year month longitude latitude `Calanus finmarchi… `Calanus finmarch…
    ##  * <chr>   <dbl> <dbl>     <dbl>    <dbl>               <dbl>              <dbl>
    ##  1 A-01     2007     8     -58.0     49.8                  0               5294.
    ##  2 B-01     2010     8     -61.9     46.8              14858.              4134.
    ##  3 C-01     2018     8     -58.7     45.4                429.               909.
    ##  4 D-01     2004     5     -58.4     48.5                  0                  0 
    ##  5 E-01     2015     9     -65.4     43.6                  0                  0 
    ##  6 F-01     2002     6     -59.8     47.7                  0               4927.
    ##  7 G-01     2007     2     -63.1     49.0              15624.              4028.
    ##  8 H-01     2015     4     -65.1     46.8              14283.                 0 
    ##  9 I-01     2009    11     -62.1     45.3                  0               4125.
    ## 10 C-01     2013    10     -59.8     45.3                  0                  0 
    ## # … with 490 more rows, and 25 more variables: Calanus finmarchicus VI <dbl>,
    ## #   Calanus hyperboreus IV <dbl>, Calanus hyperboreus V <dbl>,
    ## #   Calanus hyperboreus VI <dbl>, Calanus glacialis IV <dbl>,
    ## #   Calanus glacialis V <dbl>, Calanus glacialis VI <dbl>, bathymetry <dbl>,
    ## #   slope <dbl>, aspect <dbl>, roughness <dbl>, proximity <dbl>, mlotst <dbl>,
    ## #   siconc <dbl>, thetao <dbl>, usi <dbl>, sithick <dbl>, bottomT <dbl>,
    ## #   vsi <dbl>, vo <dbl>, uo <dbl>, so <dbl>, zos <dbl>, chlor_a <dbl>, …

But if you have your own dataset, just provide the filename and path.

    x <- read_dataset("/path/to/input/data.csv")
