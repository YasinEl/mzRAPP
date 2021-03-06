% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Skyline_functions.R
\name{SkylinePeakBoundaries}
\alias{SkylinePeakBoundaries}
\title{SkylinePeakBoundaries}
\usage{
SkylinePeakBoundaries(BM, export_to_csv = TRUE)
}
\arguments{
\item{BM}{output of \code{\link{find_bench_peaks}}}

\item{export_to_csv}{export output automatically to working directory}
}
\value{
Skyline peak boundaries
}
\description{
Takes the output of \code{\link{find_bench_peaks}} and generates a Skyline peak-boundaries file (automatically exported to working directory) which can then be imported to Skyline via
Skyline -> File -> Import -> Peak Boundaries... (after the required mzML files have been imported into Skyline using Skyline -> Import -> Results...)
}
