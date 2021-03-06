% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetFWXM.R
\name{GetFWXM}
\alias{GetFWXM}
\title{GetFWXM}
\usage{
GetFWXM(RT_vect, Int_vect, baseL, X, peak_borders = FALSE, return_diff = FALSE)
}
\arguments{
\item{RT_vect}{vector with retention times}

\item{Int_vect}{vector with intensities}

\item{baseL}{height of baseline}

\item{X}{peak height at which width should be measured (e.g. 0.5 means width at halfe maximum)}

\item{return_diff}{TRUE/FALSE; should the output be a difference of end point and start point, or both points as a vector}
}
\value{
Width of the peak (numeric)
}
\description{
Returns the width of a peak at Full Width at X Maximum, with X being the factor by which the maximum of the peak is multiplied.
}
\keyword{internal}
