
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vabl

<!-- badges: start -->
<!-- badges: end -->

This repo implements the record linkage techniques proposed in
Variational Beta Linkage (citation TBD).

## Installation

You can install the development version of vabl like so:

``` r
devtools::install_github("briankundinger/vabl")
#> Downloading GitHub repo briankundinger/vabl@HEAD
#> parallelly   (1.36.0  -> 1.37.1  ) [CRAN]
#> listenv      (0.9.0   -> 0.9.1   ) [CRAN]
#> digest       (0.6.33  -> 0.6.35  ) [CRAN]
#> globals      (0.16.2  -> 0.16.3  ) [CRAN]
#> future.apply (1.11.0  -> 1.11.1  ) [CRAN]
#> shape        (1.4.6   -> 1.4.6.1 ) [CRAN]
#> lava         (1.7.2.1 -> 1.8.0   ) [CRAN]
#> data.table   (1.14.8  -> 1.15.2  ) [CRAN]
#> Rcpp         (1.0.11  -> 1.0.12  ) [CRAN]
#> glue         (1.6.2   -> 1.7.0   ) [CRAN]
#> rlang        (1.1.2   -> 1.1.3   ) [CRAN]
#> DBI          (1.2.1   -> 1.2.2   ) [CRAN]
#> MatrixModels (0.5-2   -> 0.5-3   ) [CRAN]
#> withr        (2.5.2   -> 3.0.0   ) [CRAN]
#> e1071        (1.7-13  -> 1.7-14  ) [CRAN]
#> ff           (4.0.9   -> 4.0.12  ) [CRAN]
#> RSQLite      (2.3.1   -> 2.3.5   ) [CRAN]
#> stringdist   (0.9.10  -> 0.9.12  ) [CRAN]
#> mcmc         (0.9-7   -> 0.9-8   ) [CRAN]
#> coda         (0.19-4  -> 0.19-4.1) [CRAN]
#> tidyselect   (1.2.0   -> 1.2.1   ) [CRAN]
#> MCMCpack     (1.6-3   -> 1.7-0   ) [CRAN]
#> Installing 22 packages: parallelly, listenv, digest, globals, future.apply, shape, lava, data.table, Rcpp, glue, rlang, DBI, MatrixModels, withr, e1071, ff, RSQLite, stringdist, mcmc, coda, tidyselect, MCMCpack
#> Installing packages into 'C:/Users/brian/AppData/Local/Temp/RtmpktMGV7/temp_libpath3fcc537e2df8'
#> (as 'lib' is unspecified)
#> package 'parallelly' successfully unpacked and MD5 sums checked
#> package 'listenv' successfully unpacked and MD5 sums checked
#> package 'digest' successfully unpacked and MD5 sums checked
#> package 'globals' successfully unpacked and MD5 sums checked
#> package 'future.apply' successfully unpacked and MD5 sums checked
#> package 'shape' successfully unpacked and MD5 sums checked
#> package 'lava' successfully unpacked and MD5 sums checked
#> package 'data.table' successfully unpacked and MD5 sums checked
#> package 'Rcpp' successfully unpacked and MD5 sums checked
#> package 'glue' successfully unpacked and MD5 sums checked
#> package 'rlang' successfully unpacked and MD5 sums checked
#> package 'DBI' successfully unpacked and MD5 sums checked
#> package 'MatrixModels' successfully unpacked and MD5 sums checked
#> package 'withr' successfully unpacked and MD5 sums checked
#> package 'e1071' successfully unpacked and MD5 sums checked
#> package 'ff' successfully unpacked and MD5 sums checked
#> package 'RSQLite' successfully unpacked and MD5 sums checked
#> package 'stringdist' successfully unpacked and MD5 sums checked
#> package 'mcmc' successfully unpacked and MD5 sums checked
#> package 'coda' successfully unpacked and MD5 sums checked
#> package 'tidyselect' successfully unpacked and MD5 sums checked
#> package 'MCMCpack' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\brian\AppData\Local\Temp\RtmpaOHwca\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\brian\AppData\Local\Temp\RtmpaOHwca\remotes40a44d28dd9\briankundinger-vabl-94f8005/DESCRIPTION' ...  ✔  checking for file 'C:\Users\brian\AppData\Local\Temp\RtmpaOHwca\remotes40a44d28dd9\briankundinger-vabl-94f8005/DESCRIPTION'
#>       ─  preparing 'vabl':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>       ─  building 'vabl_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/brian/AppData/Local/Temp/RtmpktMGV7/temp_libpath3fcc537e2df8'
#> (as 'lib' is unspecified)
```

## Example

Add vignette?

``` r
library(vabl)
#> Loading required package: dplyr
#> Warning: package 'dplyr' was built under R version 4.3.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

<!-- devtools::build_readme() -->
