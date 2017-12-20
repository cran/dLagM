\name{polyDlmForecast}
\alias{polyDlmForecast}

\title{
Compute forecasts for polynomial distributed lag model
}
\description{
A function that computes forecasts for polynomial distributed lag model with one predictor.
}
\usage{
polyDlmForecast(model , x , h = 1 , interval = FALSE , alpha = 0.05 , nSim = 500)
}
\arguments{
  \item{model}{
  An object of class \code{lm} including the fitted model with \code{poly.dlm()} function.
  }
  \item{x}{
  A vector including the new observations of independent time series. This is not restricted to \code{ts} objects.
  } 
  \item{h}{
  The number of ahead forecasts.
  }  
  \item{interval}{
  If \code{TRUE}, \eqn{(1-\alpha)\%} confidence intervals for forecasts are displayed along with forecasts.
  }
  \item{alpha}{
  Significance level of confidence interval. 
  }
  \item{nSim}{
  An integer showing the number of Monte Carlo simulations used to compute confidence intervals for forecasts.
  }
}
\details{
This function directly uses the model formula and estimates of model coefficients to find forecast one-by-one starting from the one-step ahead forecast.

Confidence intervals are found by the Monte Carlo approach using a Gaussian error distribution with zero mean and empirical variance of the dependent series.
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
model.poly = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                q = 2 , k = 2 , show.beta = TRUE ,  show.summary = TRUE)
polyDlmForecast(model = model.poly , x = c(95, 98) , h = 1 , 
                interval = FALSE)
polyDlmForecast(model = model.poly , x = c(95, 98) , h = 1 , 
                interval = TRUE, nSim = 100)
}
