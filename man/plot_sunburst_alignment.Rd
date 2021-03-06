% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sunburst_plots.R
\name{plot_sunburst_alignment}
\alias{plot_sunburst_alignment}
\title{plot_sunburst_alignment}
\usage{
plot_sunburst_alignment(result_txt)
}
\arguments{
\item{result_txt}{output from \code{\link{derive_performance_metrics}}}
}
\value{
plotly object
}
\description{
Generates a sunburst plot visualizing non-targeted data pre-processing alignment errors.
From inside to outside the donuts correspond to peaks found during peak detection, aligned/lost peaks, correct/incorrect alignments and error type. For
more information please check the mzRAPP readme.
}
