% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{estimate}
\alias{estimate}
\title{Perform bootstrap estimation of confidence intervals of ranked HR scores}
\usage{
estimate(df, ..., control = "A", nits = 100, low = 0.025, high = 0.975)
}
\arguments{
\item{df}{data frame of score and group data. Contains minimally a score and
group column}

\item{...}{bare names of columns to use, minimally the score column and the
group column in that order. Optionally a third technical replicate column can
be provided}

\item{control}{the value of the grouping column taken to be the control group}

\item{nits}{the number of bootstap iterations to be done}

\item{low}{the low probability value of the quantile}

\item{high}{the high probability value of the quantile}
}
\value{
a list object of class "hrest"
}
\description{
\code{estimate} carries out estimation of bootstrap confidence intervals on
ranked score data. Returns a \code{hrest} object of the result
Proceeeds by calculating score ranks, then bootstrapping ranks in non-control
groups retaining the mean for each bootstrap iteration. Calculates
low and high quantiles of bootstrap mean distributions for each group.
If technical replicates are provided in a second grouping column these will
be averaged before proceeding.
}
\examples{

 d1 <- make_data()
 estimate(d1, score, group)

 d2 <- make_data2()
 estimate(d2, score_column_name, sample_column_name, rep_column_name )

\donttest{
 d3 <- make_data3()
 estimate(d3, score, sample, rep, nits = 1000)
}
}
