
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
#> rlang        (1.1.2  -> 1.1.3   ) [CRAN]
#> glue         (1.6.2  -> 1.7.0   ) [CRAN]
#> MatrixModels (0.5-2  -> 0.5-3   ) [CRAN]
#> withr        (2.5.2  -> 3.0.0   ) [CRAN]
#> mcmc         (0.9-7  -> 0.9-8   ) [CRAN]
#> coda         (0.19-4 -> 0.19-4.1) [CRAN]
#> tidyselect   (1.2.0  -> 1.2.1   ) [CRAN]
#> MCMCpack     (1.6-3  -> 1.7-0   ) [CRAN]
#> Installing 8 packages: rlang, glue, MatrixModels, withr, mcmc, coda, tidyselect, MCMCpack
#> Installing packages into 'C:/Users/brian/AppData/Local/Temp/RtmpktMGV7/temp_libpath3fcc36e33f4b'
#> (as 'lib' is unspecified)
#> package 'rlang' successfully unpacked and MD5 sums checked
#> package 'glue' successfully unpacked and MD5 sums checked
#> package 'MatrixModels' successfully unpacked and MD5 sums checked
#> package 'withr' successfully unpacked and MD5 sums checked
#> package 'mcmc' successfully unpacked and MD5 sums checked
#> package 'coda' successfully unpacked and MD5 sums checked
#> package 'tidyselect' successfully unpacked and MD5 sums checked
#> package 'MCMCpack' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\brian\AppData\Local\Temp\RtmpkbiDgk\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\brian\AppData\Local\Temp\RtmpkbiDgk\remotes4a044ba2e1c\briankundinger-vabl-38da6e5/DESCRIPTION' ...  ✔  checking for file 'C:\Users\brian\AppData\Local\Temp\RtmpkbiDgk\remotes4a044ba2e1c\briankundinger-vabl-38da6e5/DESCRIPTION'
#>       ─  preparing 'vabl':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>       ─  building 'vabl_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/brian/AppData/Local/Temp/RtmpktMGV7/temp_libpath3fcc36e33f4b'
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
