\name{ardlDlmForecast}
\alias{ardlDlmForecast}

\title{
Compute forecasts for autoregressive distributed lag models
}
\description{
A function that computes forecasts for autoregressive distributed lag model with one predictor.
}
\usage{
ardlDlmForecast(model , x , h = 1)
}
\arguments{
  \item{model}{
  An object of class \code{lm} including the fitted model with \code{ardl.dlm()} function.
  }
  \item{x}{
  A vector including the new observations of independent time series. This is not restricted to \code{ts} objects.
  } 
  \item{h}{
  The number of ahead forecasts.
  }  

}
\details{
This function directly uses the model formula and estimates of model coefficients to find forecast one-by-one starting from the one-step ahead forecast.
}

\value{

\item{forecasts}{A vector including forecasts.}

}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\examples{
data(warming)
model.ardl = ardlDlm(x = warming$NoMotorVehicles, 
y = warming$Warming, p = 1 , q = 1 , show.summary = TRUE)
ardlDlmForecast(model = model.ardl , x = c(95, 98) , h = 2)
}
