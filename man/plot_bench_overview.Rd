% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bench_overview.R
\name{plot_bench_overview}
\alias{plot_bench_overview}
\title{plot_bench_overview}
\usage{
plot_bench_overview(benchmark_data, x, y, colb, choice_vector_bench)
}
\arguments{
\item{benchmark_data}{output from \code{\link{find_bench_peaks}}}

\item{x}{variable (column name) to be plotted on x axis}

\item{y}{variable (column name) to be plotted on y axis}

\item{colb}{variable (column name) to color by}

\item{choice_vector_bench}{named vector including all variables used as elements}
}
\value{
plotly object
}
\description{
plot_bench_overview
}
