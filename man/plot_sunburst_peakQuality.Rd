% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sunburst_plots.R
\name{plot_sunburst_peakQuality}
\alias{plot_sunburst_peakQuality}
\title{plot_sunburst_peakQuality}
\usage{
plot_sunburst_peakQuality(result_txt, comparison_object)
}
\arguments{
\item{result_txt}{output from \code{\link{derive_performance_metrics}}}

\item{comparison_object}{output from \code{\link{compare_peaks}}}
}
\value{
plotly object
}
\description{
Generates a sunburst plot visualizing the proportions of well recovered isotopologue ratios in order to assess non-targeted data pre-processing.
From inside to outside the donuts correspond to peaks found during peak detection and peaks found after alignment/feature processing. For
more information please check the mzRAPP readme.
}
