% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_hypothesis.r
\name{interval_hypothesis}
\alias{interval_hypothesis}
\title{Hypothesis tests for identification regions}
\usage{
interval_hypothesis(fit, term, interval, type = c("subset", "equal"))
}
\arguments{
\item{fit}{Fitted model of class \code{"coefbounds"}.  Must have been run
with \code{boot > 0} and contain a non-null \code{dist} element.}

\item{term}{Name of the coefficient whose identification region is being
tested.}

\item{interval}{Interval specified in the null hypothesis.  May be a
singleton; e.g., \code{interval = c(0, 0)} to test that the
identification region contains 0.}

\item{type}{Whether the null hypothesis should be that \code{interval} is a
\code{"subset"} of the population identification region (the default) or
is \code{"equal"} to it.}
}
\value{
A list of class \code{"interval_hypothesis"} containing:
\describe{
\item{\code{test_stat}}{Test statistic.}
\item{\code{p}}{Bootstrap p-value.}
\item{\code{n_boot}}{Number of bootstrap iterations.}
\item{\code{clus}}{Whether a cluster bootstrap was used.}
\item{\code{term}}{Name of coefficient tested.}
\item{\code{interval}}{Interval specified.}
\item{\code{estimate}}{Sample estimate of coefficient bounds.}
\item{\code{type}}{Type of test performed.}
}
}
\description{
Given an interval \eqn{B = [b_L, b_U]}, tests against the null hypothesis that
the identification region for the specified coefficient is a superset of
\eqn{B} (\code{type = "subset"}, the default) or equals \eqn{B} (\code{type
= "equal"}).
}
\details{
The test with \code{type = "subset"} corresponds to
\code{\link{confint.coefbounds}} with \code{type = "DU"}.  The test with
\code{type = "equal"} corresponds to \code{\link{confint.coefbounds}} with
\code{type = "CC"}.

Implements largely the same functionality as \code{CI1D} in Beresteanu et
    al.'s (2010) Stata program.
}
\examples{
## Simulate data
set.seed(18)
x1 <- rnorm(50)
x2 <- rnorm(50)
y <- 1 - x1 + x2 + rnorm(50)
yl <- floor(y)
yu <- ceiling(y)

## Fit model
fit <- coefbounds(yl + yu ~ x1 + x2, boot = 100)

## Test hypothesis that the identification region for the coefficient on x1
## contains -0.25
interval_hypothesis(fit,
                    term = "x1",
                    interval = c(-0.25, -0.25),
                    type = "subset")

## Test hypothesis that the identification region for the coefficient on x2
## equals [0, 1.5]
interval_hypothesis(fit,
                    term = "x2",
                    interval = c(0, 1.5),
                    type = "equal")
}
\author{
Brenton Kenkel
}
\references{
Arie Beresteanu, Francesca Molinari and Darcy Steeg Morris.  2010.
    "Asymptotics for Partially Identified Models in Stata."
    \url{https://molinari.economics.cornell.edu/programs.html}
}

