
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mzRAPP

<!-- badges: start -->

<!-- badges: end -->

The goal of mzRAPP is to allow reliability assessment of non-targeted
data pre-processing (NPP) in the realm of liquid chromatography high
resolution mass spectrometry (LC-HRMS). This is achieved by utilizing
user provided information on a set of molecules (at best \> 50) with
known retention behavior. mzRAPP extracts and validates chromatographic
peaks for all (envipat predicted) isotopologues of those target
molecules directly from mzML files. The resulting benchmark dataset is
used to extract different performance measures for NPP performed on the
same mzML files.

## Installation

You can install mzRAPP from [GitHub](https://github.com/) with:

``` r
require("devtools")
devtools::install_github("YasinEl/mzRAPP")
```

## Usage

Open mzRAPP as shiny app using:

``` r
library(mzRAPP)
callmzRAPP()
```

Following that you will be in the ‘Generate Benchmark’ panel of mzRAPP:

<img src="D:/Yasin/mzRAPP_shiny frontpage_first.png" width="100%" style="background-color: #9ecff7; padding:10px; display: inline-block;" />
<br> In order to generate a benchmark you need to provide your
centroided mzML files as well as two additional csv files. The
<u><b>sample-group file</b></u> should contain two columns:

<b>sample\_name:</b> names of all mzML files <br> <b>sample\_group:</b>
group labels of the respective samples (e.g. treated, untreated,..) <br>

The <u><b>target file</b></u> should contain information on the target
molecules:

<b>molecule:</b> names of target molecules (should be unique
identifiers) <br> <b>adduct\_c:</b> adducts that should be evaluated
(e.g. M+H or M-Cl). If more than one adduct is to be investigated
another line with the same molecule name should be added. All adducts
enabled in the enviPat package are allowed:

``` r
library(enviPat)
#> 
#>  
#>  Welcome to enviPat version 2.4 
#>  Check www.envipat.eawag.ch for an interactive online version
data(adducts)
adducts$Name
#>  [1] "M+H"            "M+NH4"          "M+Na"           "M+K"           
#>  [5] "M+"             "M-H"            "M-2H"           "M-3H"          
#>  [9] "M+FA-H"         "M+Hac-H"        "M-"             "M+3H"          
#> [13] "M+2H+Na"        "M+H+2Na"        "M+3Na"          "M+2H"          
#> [17] "M+H+NH4"        "M+H+Na"         "M+H+K"          "M+ACN+2H"      
#> [21] "M+2Na"          "M+2ACN+2H"      "M+3ACN+2H"      "M+CH3OH+H"     
#> [25] "M+ACN+H"        "M+2Na-H"        "M+IsoProp+H"    "M+ACN+Na"      
#> [29] "M+2K-H"         "M+DMSO+H"       "M+2ACN+H"       "M+IsoProp+Na+H"
#> [33] "2M+H"           "2M+NH4"         "2M+Na"          "2M+3H2O+2H"    
#> [37] "2M+K"           "2M+ACN+H"       "2M+ACN+Na"      "M-H2O-H"       
#> [41] "M+Na-2H"        "M+Cl"           "M+K-2H"         "M+Br"          
#> [45] "M+TFA-H"        "2M-H"           "2M+FA-H"        "2M+Hac-H"      
#> [49] "3M-H"
```

<b>main\_adduct:</b> One main adduct has to be defined for each
molecule. <br> <b>SumForm\_c:</b> molecular composition of the neutral
molecule (e.g. C10H15N5O10P2). <br> <b>StartTime.EIC:</b> Starting time
for chromatograms extracted for this molecule (seconds). <br>
<b>EndTime.EIC:</b> End time for chromatograms extracted for this
molecule (seconds). <br> <b>user.rtmin:</b> (optional) Lower end of time
window in which points should be considered for chromatographic peak
detection (seconds). Defaults to StartTime.EIC. <br> <b>user.rtmin:</b>
(optional) Higher end of time window in which points should be
considered for chromatographic peak detection (seconds). Defaults to
EndTime.EIC. <br> <b>user.rt:</b> Retention time expected for this
molecule. If multiple peaks are detected the peak closest to this time
is chosen (seconds). <br>

Afterwards the used instrument and resolution has to be selected. All
instruments enabled via the enviPat package are enabled.

In a next step a view paramters have to be set:

<img src="D:/Yasin/mzRAPP_shiny frontpage_second.png" width="100%" style="background-color: #9ecff7; padding:10px; display: inline-block;" />

<b>Lowest iso. to be considered \[%\]:</b> Lowest relative isotopologue
abundance to be considered for each molecule. <br> <b>Min. \# of scans
per peak:</b> Minimum number of points for a chromatographic peak to be
considered as such. <br> <b>mz precision \[ppm\]:</b> Maximum spread of
mass peaks in the mz dimension to be still considered part of the same
chromatogram. <br> <b>mz accuracy \[ppm\]:</b> Maximum difference
between the accurate mz of two ion traces to be considered to be
originating from the same ion. <br> <b>Processing plan:</b> How should
the benchmark generation be done? <u>sequential</u> (only using one
core; often slow but does not use much RAM), <u>multiprocess</u> (using
multiple cores; faster but needs more RAM; works on Windows machines) or
<u>multicore</u> (using multiple cores; faster but needs more RAM; does
not work on Windows machines) <br>

Afterwards benchmark generation can be started using the blue Start
button. The necessary time for the generation depends on the number of
mzML files, the number of target compounds and of course computational
recources.

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
