% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers_func.R
\name{outliers_func}
\alias{outliers_func}
\title{Label outliers}
\usage{
outliers_func(mod_plot_agg, OD_adjust, Poisson_limits, limit, multiplier)
}
\arguments{
\item{mod_plot_agg}{Aggregated data set for plotting}

\item{OD_adjust}{Logical for drawing OD limits, takes precedence over Poisson for outliers}

\item{Poisson_limits}{Logical for drawing Poisson limits}

\item{limit}{which limit to use.  Currently 95 or 99.}

\item{multiplier}{the amount to scale the RR / limits by. Default is 1 \(no scaling\).}
}
\description{
Label outliers
}
\keyword{internal}
