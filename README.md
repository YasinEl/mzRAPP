
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
if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
devtools::install_github("YasinEl/mzRAPP")
```

## Usage

mzRAPP can be used via a shiny interface or via a set of functions. <br>

<h3>

Benchmark dataset generation

</h3>

<b>Via user interface:</b> <br>

Open mzRAPP as shiny app using:

``` r
library(mzRAPP)
callmzRAPP()
```

Following that you can go to the ‘Generate Benchmark’ panel of mzRAPP:

<h4>

First:

</h4>

In order to generate a benchmark you need to provide your centroided
mzML files as well as two additional csv files. The <u><b>sample-group
file</b></u> should contain two columns:

<b>sample\_name:</b> names of all mzML files <br> <b>sample\_group:</b>
group labels of the respective samples (e.g. treated, untreated,..) <br>

The <u><b>target file</b></u> should contain information on the target
molecules:

<b>molecule:</b> names of target molecules (should be unique
identifiers) <br> <b>adduct\_c:</b> adducts that should be evaluated
(e.g. M+H or M+Cl). If more than one adduct is to be investigated
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

<b>main\_adduct:</b> One main adduct has to be defined for each molecule
(e.g. M+H). If the main\_adduct is not detected also other adducts wont
be accepted. <br> <b>SumForm\_c:</b> Molecular composition of the
neutral molecule (e.g. C10H15N5O10P2). <br> <b>StartTime.EIC:</b>
Starting time for chromatograms extracted for this molecule (seconds).
<br> <b>EndTime.EIC:</b> End time for chromatograms extracted for this
molecule (seconds). <br> <b>user.rtmin:</b> (optional) Lower end of time
window in which points should be considered for chromatographic peak
detection (seconds). Defaults to StartTime.EIC. <br> <b>user.rtmin:</b>
(optional) Higher end of time window in which points should be
considered for chromatographic peak detection (seconds). Defaults to
EndTime.EIC. <br> <b>user.rt:</b> Retention time expected for this
molecule. If multiple peaks are detected the peak closest to this time
is chosen (seconds). <br>

Afterwards the used <u><b>instrument and resolution</u></b> has to be
selected. This is necessary in order to apply the correct mass
resolution for any given m/z value. All instruments enabled via the
enviPat package can be selected from the envipat resolution list. For
other instruments a custom resolution list has to be uploaded as .csv
file. This .csv file has to have two columns: <br> <b>R: </b> Resolution
value at half height of a mass peak <br> <b>m/z: </b> m/z value for the
correspondig resolution <br> Resolution values for at least 10 equally
distributed m/z value sis recommended.

<h4>

Second:

</h4>

In a next step a view paramters have to be set: <br> <b>Lowest iso. to
be considered \[%\]:</b> Lowest relative isotopologue abundance to be
considered for each molecule. <br> <b>Min. \# of scans per peak:</b>
Minimum number of points for a chromatographic peak to be considered as
such. <br> <b>mz precision \[ppm\]:</b> Maximum spread of mass peaks in
the mz dimension to be still considered part of the same chromatogram.
<br> <b>mz accuracy \[ppm\]:</b> Maximum difference between the accurate
mz of two ion traces to be considered to be originating from the same
ion. <br> <b>Processing plan:</b> How should the benchmark generation be
done? <u>sequential</u> (only using one core; often slow but does not
use much RAM) or <u>multiprocess</u> (using multiple cores; faster but
needs more RAM; works on Windows machines) <br>

<h4>

Third:

</h4>

Benchmark generation can be started using the blue Start button. The
necessary time for the generation depends on the number of mzML files,
the number of target compounds and of course computational recources.

<h4>

Fourth:

</h4>

Afterwards the generated benchmark dataset can be inspected in the
“Benchmark overview” panel. The plots in the first row can be used to
inspect different qualities of the dataset. The heatmap gives an
overview on which molecules were added to the benchmark (validated via
at least 1 additional isotopologue). A molecule not being added does no
mean that it was not there, but that mzRAPP was not able to validate it.
This could be because no additional isotopologue (fullfilling strict
criteria in abundance and peak shape correlation) coule be detected. The
plot in the lower right corner can be used to plot peaks corresponding
to individual target molecules. For additional information on the
individual plot click the blue question mark-icon above the plots.<br>
<br> <b>Via R-functions:</b><br>

``` r
library(mzRAPP)

#load necessary files into environment
targets <- fread("PATH_TO_TARGET_MOLECULE_FILE/TARGETS.csv")
grps <- fread("PATH_TO_SAMPLE_INFORMATION_FILE/SAMPLE_INFORMATION.csv")
files <- list.files("PATH_TO_FOLDER_WITH_MZML_FILES", recursive = TRUE, full.names = TRUE, pattern=".mzML")

#load resolution list from envipat package
data("resolution_list")
mz_res_dependence_df <- resolution_list[["Q-Exactive,ExactivePlus_R70000@200"]]

#generate table with mass traces
MassTraces <- getMZtable(
  targets,
  instrumentRes = mz_res_dependence_df,
)

#genereate table with regions of interest
rois <- getROIsForEICs(
  files = files,
  Target.table = MassTraces,
  PrecisionMZtol = 8,
  AccurateMZtol = 5
)

#generate table with peaks
PCbp <- findBenchPeaks(
  files = files,
  Grps = grps,
  CompCol = rois,
  Min.PointsperPeak = 7
)

#reducing number of peaks to a maximum of 1 per mass trace and generating final benchmark dataset
PCal <- align_PC(PCbp)
```

<h3>

Reliability assessment of non-targeted data pre-processing

</h3>

The generated benchmark can now be used to assess the reliability of
non-targeted data pre-processing. Such an assessment can be set up in
the panel “Assess NT data pre-processing”. First the tool to be
evaluated has to be set. Afterwards the unaligned files (One for XCMS
(csv) and Compound Discoverer (txt), multiple for mzMine (csv) and
MS-DIAL (txt)) and one aligned file have to be selected. <br> <br>
<b>How to export unaligned and aligned files from the different
tools:</b><br> <br> <u>XCMS (R-version):</u>

``` r
#unaligned file:
data.table::fwrite(xcms::peaks(xcmsSet_object), "blabla_unaligned_file.csv")

#aligned file:
data.table::fwrite(xcms::peakTable(xcmsSet_object), "blabla_aligned_file.csv")
```

<br> <u>XCMS online:</u> <br> Download all results from XCMS online via
the button “Download Results”. Afterwards extract all Results from the
zipped folder. <br> unaligned file: select the xcms3xset.Rda file <br>
aligned file: select the same xcms3xset.Rda file <br> <br>
<u>MS-DIAL:</u> <br> unaligned files: Export -\> Peak list result -\>
\[Add all files\] -\> \[set Export format to txt\] <br> aligned file:
When performing the alignment make sure to activate the isotope tracking
option in the alignment step (for most cases selecting 13C and 15N as
labeling elements will be adequat). Afterwards export via:<br>

Export -\> Alignment result -\> \[check Raw data matrix Area\] -\> \[set
Export format to txt\] <br> <br> <u>mzMine:</u> <br> unaligned files:
\[select all files generated in the chromatogram deconvolution step\]
-\> Feature list methods -\> Export/Import -\> Export to CSV file -\>
\[set Filename including pattern/curly brackets
(e.g. blabla\_{}\_blabla.csv)\] -\> \[check “Peak height”, “Peak area”,
“Peak RT start”, “Peak RT end”, “Peak RT”, “Peak m/z”, “Peak m/z min”
and “Peak m/z max”\] -\> \[set Filter rows to ALL\] <br> aligned file:
\[select file after alignment step\] -\> Feature list methods -\>
Export/Import -\> Export to CSV file -\> \[additional to checks set for
unaligned files check “Export row retention time” and “Export row
m/z”\]<br> <br> <u>Compound Discoverer:</u> <br> unaligned file: \[go
to panel “Features”\] -\> \[click in any row\] -\> \[click “Show related
Tables” on the bottom of the screen\] -\> \[go to panel “Chromatogram
Peaks”\] -\> \[make sure the columns “Apex Intensity”, “Area”, “Study
File ID”, “Left RT \[min\]”, “Right RT \[min\]” “Apex RT \[min\]” and
“Apex m/z” are visible\] -\> \[right click any row\] -\> Export -\> As
plain text… <br> <br> <b>Selecting a benchmark dataset and starting
assessment:</b><br> <br> Next the benchmark file has to be selected. If
a benchmark has been created during this shiny session (the benchmark is
still visible in the panel benchmark overview) the switch button “Use
generated benchmark” can be clicked as an alternative. If you want to
adapt mzRAPP to a tool/non-targeted output format other than those
mentioned above this can be done by clicking the “Use generated options”
switch button. The way those option files can be generated manually is
described at the bottom of this readme. <br> <br> After performing those
steps the assessment can be started via the blue “Start assessment
button”.

## Interpretation of assessment results

Reliability assessment results are distributed over the panels
“Assessment results (peaks)” and “Assessment results (alignment)”. In
the first panel a number of performance metrics is given on the top:
<br> <br> <u>Found peaks:</u> <br> The number of benchmark peaks for
which a match was found among the NT peaks in the peak picking step.
<br> <br> <u>Peak fragments:</u> <br> The number of peak fragments which
have been found that have been found over the whole dataset. For a
graphical explanation of a split peak please look at the mzRAPP
publication. <br> <br> <u>Missing Value (S|R):</u> <br> Number of
systematic (s) and random (R) missing values after the peak picking
step. This classification is depending on the peak with the lowest
benchmark area detected via NPP. If a missing peak has a benchmark area
\> 20% higher than the lowest NPP-detected peak it is classified as R if
it is \< than 20% as S. <br> <br> <u>Pred. error increase \>20%p:</u>
<br> Isotopologue areas are predicted from the most abundant
isotopologue of each compound. If the relative error of this prediction
is more than 20 %p higher for NT-peak areas (compared to
benchmark-areas) it is refelcted in this variable. Here only peak areas
from the peak picking step are considered. <br> <br> <u>Min. \# of
alignment errors:</u> <br> Gives the minimum number of alignment errors
done by the non-targeted algorithm. For the exact way of how this count
is esatblished please check the original mzRAPP publication. <br> <br>
<u>Found peaks (features):</u> <br> The number of benchmark peaks for
which a match was found among the NT peaks after the alignment step.
Here only one feature per benchmark feature is allowed. If multiple
features are found for one benchmark feature the one which fits the
predicted isotopic pattern best is chosen. <br> <br> <u>Found features
(features):</u> <br> The number of benchmark features for which at least
one benchmark peak was detected via the non-targeted algorithm. <br>
<br> <u>Pred. error increase \>20%p (features):</u> <br> Isotopologue
areas are predicted from the most abundant isotopologue of each
compound. If the relative error of this prediction is more than 20 %p
higher for NT-peak areas (compared to benchmark-areas) it is refelcted
in this variable. Here only peak areas after the alignment step are
considered. <br> <br> <b>Overview plots:</b><br> <br> For explanations
of the overview plots please click the blue question marks above the
individual plots.
