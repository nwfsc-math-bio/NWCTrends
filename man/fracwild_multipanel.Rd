% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fracwild_plot_multipanel.R
\name{fracwild_multipanel}
\alias{fracwild_multipanel}
\title{Figure of the raw and smoothed fracwild estimates.}
\usage{
fracwild_multipanel(
  esu,
  pops,
  fracwild.fit,
  min.year = NULL,
  max.year = NULL,
  show.all = TRUE
)
}
\arguments{
\item{esu}{The name of the ESU}

\item{pops}{The population names that will be plotted (populations with too few data are eliminated)}

\item{fracwild.fit}{fracwild fit returned by \code{trend_fits()}}

\item{min.year}{The x axis minimum.}

\item{max.year}{The x axis maximum.}

\item{show.all}{If there is no fracwild data for a population, should that population still have a fracwild plot, which will be blank.}

\item{total.fit}{total fit returned by \code{trend_fits()}}
}
\value{
A plot
}
\description{
This is the fracwild figure function. Not exported. It is used by \code{\link{NWCTrends_report}} and \code{inst/doc/report_files/esu_report.Rmd}.

The dots are the raw fracwild values, the black
line is the smoothed fracwild estimate.
For the smoothed fracwild estimates, there is no data sharing across populations.
}
\author{
Eli Holmes, NOAA, Seattle, USA.  eli(dot)holmes(at)noaa(dot)gov
}
\keyword{report}