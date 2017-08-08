\name{koyckDlmForecast}
\alias{koyckDlmForecast}

\title{
Compute forecasts for Koyck transformation of distributed lag models
}
\description{
A function that computes forecasts for Koyck transformation of distributed lag model with one predictor.
}
\usage{
koyckDlmForecast(model , x , h = 1)
}
\arguments{
  \item{model}{
  An object of class \code{lm} including the fitted model with \code{koyck.dlm()} function.
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
model.koyck = koyckDlm(x = warming$NoMotorVehicles , 
y = warming$Warming , show.summary = TRUE)
koyckDlmForecast(model = model.koyck  , x = c(95, 98, 101), h = 3)
}
