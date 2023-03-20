
<!-- README.md is generated from README.Rmd. Please edit that file -->

# discAUC

The goal of discAUC is to provide a solution to easily calculate AUC for
delay discounting data. It includes logAUC and ordAUC as published in
Borges et al.  (2016). It also includes a solution for 0 delays for
logAUC.

## Installation

You can install the released version of discAUC from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("discAUC")
```

This is a basic example which shows you how to solve a common problem:

``` r
library(discAUC)

#Calculate AUC for proportional indiference points for each outcome per subject.
AUC(dat = examp_DD,
    x_axis = "delay_months",
    indiff = "prop_indiff",
    amount = 1,
    groupings = c("subject","outcome"))
#> # A tibble: 60 × 3
#> # Groups:   subject [15]
#>    subject outcome            AUC
#>      <dbl> <chr>            <dbl>
#>  1   -988. $100 Gain     0.359   
#>  2   -988. alcohol       0.0953  
#>  3   -988. entertainment 0.405   
#>  4   -988. food          0.158   
#>  5     -2  $100 Gain     0.000278
#>  6     -2  alcohol       0.000278
#>  7     -2  entertainment 0.000278
#>  8     -2  food          0.000278
#>  9     -1  $100 Gain     1       
#> 10     -1  alcohol       1       
#> # … with 50 more rows
```
