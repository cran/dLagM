\name{rolCorPlot}
\alias{rolCorPlot}

\title{
PLot the rolling correlations
}
\description{
Plots the rolling correlations along with other required statistics to visualise the approach of Gershunov et al. (2001) to test the significance of signal from rolling correlation analysis. 
}
\usage{
rolCorPlot(x , y , width, level = 0.95, main = NULL, 
           SDtest = TRUE, N = 500)
}
\arguments{
  \item{x}{
   A ts object.
  }
  \item{y}{
   A ts object.
  } 
  \item{width}{
  A numeric vector of window lengths of the rolling correlation analysis.
  } 
 \item{level}{
  Confidence level for intervals.
  } 
  \item{main}{
  The main title of the plot.
  } 
  \item{SDtest}{
  Set to \code{TRUE} to run test the significance of signal from rolling correlation analysis along with plotting.
  } 
  \item{N}{
  An integer showing the number of series to be generated in Monte Carlo simulation.
  } 
}


\value{

\item{rolCor}{
  A matrix showing rolling correlations for each \code{width} on its columns.
}
\item{rolcCor.avr.filtered}{
 A vector showing average rolling correlations filtered by running median nonlinear filter against outliers.
}
\item{rolcCor.avr.raw}{
  A vector showing unfiltered average rolling correlations.
}
\item{rolCor.sd}{
  A vector showing standard deviations of rolling correlations for each \code{width}.
}
\item{rawCor}{
  Pearson correlation between two series.
}
\item{sdPercentiles}{
  Percentiles of MC distribution of standard deviations of rolling correlations as the test limits.
}
\item{test}{
  A data frame showing the standard deviations of rolling correlations for each \code{width} along with \code{level} and (1-\code{level}) limits.
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
\dontrun{
data(wheat)
prod.ts <-ts(wheat[,5], start = 1960)
CO2.ts <- ts(wheat[,2], start = 1960)
rolCorPlot(x = prod.ts, y = CO2.ts , width = c(7, 11, 15), level = 0.95, N = 50)
}
}
