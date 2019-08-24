\name{sdPercentiles}
\alias{sdPercentiles}

\title{
Test the significance of signal from rolling correlation analysis
}
\description{
Implements the approach of Gershunov et al. (2001) to test the significance of signal from rolling correlation analysis. 
}
\usage{
sdPercentiles(n = 150, cor = 0.5, width = 5, N = 500, 
              percentiles = c(.05, .95))
}
\arguments{
  \item{n}{
  The length of the series in the rolling correlation analysis.
  }
  \item{cor}{
  The magnitude of raw correaltion betweeen two time series in the rolling correlation analysis.
  } 
  \item{width}{
  Window length of the rolling correlation analysis.
  } 
 \item{N}{
  Number of Monte Carlo replications for simulations.
  } 
 \item{percentiles}{
  Percentiles to be reported for the Monte Carlo distribution of standard deviations of rolling correlations for the given window width.
  } 
}
\details{
\code{N} samples of correlated white noise series are generated with a magnitude of \code{cor}; rolling correlations analysis is applied with the window length of \code{width}; Monte Carlo distribution of standard deviations of rolling correlations are generated; and desired \code{percentiles} of the MC distribution of standard deviations are reported (Gershunov et al. 2001). 
}

\value{

\item{rollCorSd.limits}{
Percentiles of MC distribution of standard deviations of rolling correlations as the test limits.
}

}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\references{
Gershunov, A., Scheider, N., Barnett, T. (2001). Low-Frequency Modulation of the ENSO-Indian Monsoon Rainfall Relationship: Signal or Noise? \emph{Journal of Climate}, 14, 2486 - 2492.
}


\examples{
sdPercentiles(n = 50, cor = 0.5, width = 5, N = 100, 
              percentiles = c(.025, .975))
}
