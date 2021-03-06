% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coefbounds.r
\name{coefbounds_fit}
\alias{coefbounds_fit}
\title{Direct to appropriate fitting function}
\usage{
coefbounds_fit(YL, YU, X, Z, model, maxit)
}
\arguments{
\item{YL}{response lower bound}

\item{YU}{response upper bound}

\item{X}{design matrix}

\item{Z}{instrument matrix}

\item{model}{"linear" or "logit"}

\item{maxit}{max iterations for logit approximation}
}
\value{
matrix containing the estimated bounds
}
\description{
Direct to appropriate fitting function
}
\author{
Brenton Kenkel
}
\keyword{internal}

