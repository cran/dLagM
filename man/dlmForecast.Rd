\name{dlmForecast}
\alias{dlmForecast}

\title{
Compute forecasts for finite distributed lag model
}
\description{
A function that computes forecasts for finite distributed lag model with one or multiple predictor(s).
}
\usage{
dlmForecast(model , x , h = 1 , interval = TRUE , alpha = 0.05 , nSim = 500)
}
\arguments{
  \item{model}{
  An object of class \code{lm} including the fitted model with \code{dlm()} function.
  }
  \item{x}{
  A vector or matrix including the new observations of independent time series. This is not restricted to \code{ts} objects. See the details for specification of the values of \code{x}.
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
This function directly uses the model formula and estimates of model coefficients to find forecasts one-by-one starting from the one-step ahead forecast. 

Confidence intervals are found by the Monte Carlo approach using a Gaussian error distribution with zero mean and empirical variance of the dependent series.

When the \code{model} argument includes multiple independent series, \code{x} must be entered as a matrix including the new observations of each independent series in its rows. The number of columns of \code{x} must be equal to the forecast horizon \code{h} and the rows of \code{x} are matched with the independent series in the order they appear in the \code{data}.

This function can still be used when some of the lags of independent series are removed from the model.
}

\value{

\item{forecasts}{A vector including forecasts.}

}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\examples{
# Only one independent series
data(warming)
model.dlm = dlm(x = warming$NoMotorVehicles , 
y = warming$Warming , q = 2 ,  show.summary = TRUE)
dlmForecast(model = model.dlm , x = c(95 , 98, 101) , h = 3)


# Multiple independent series
data(M1Germany)
data = M1Germany[1:144,]
model.dlm  = dlm(formula = logprice ~ interest + logm1, 
          data = data.frame(data) , q = 4 , show.summary = TRUE)

x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09), 
                ncol = 2, nrow = 2)
dlmForecast(model = model.dlm , x = x.new , h = 2 , 
            interval = FALSE)
dlmForecast(model = model.dlm , x = x.new , h = 2 , 
            interval = TRUE, nSim = 100)

# Some lags are removed:
# Remove lag 2 from "interest" and 
# lags 1 and 3 from "logm1"
removed = matrix(c(2 , NA , 1 , 3), nrow = 2, ncol = 2 , 
                byrow = TRUE)
model.dlm  = dlm(formula = logprice ~ interest + logm1 , 
              data = data.frame(data) , q = 4 , remove = removed ,
              show.summary = TRUE)

x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09), 
                ncol = 2, nrow = 2)
dlmForecast(model = model.dlm , x = x.new , h = 2 , 
            interval = FALSE)
dlmForecast(model = model.dlm , x = x.new , h = 2 , 
            interval = TRUE, nSim = 100)
  
x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09, 0.08 , 9.12), ncol = 3, 
                nrow = 2)
dlmForecast(model = model.dlm , x = x.new , h = 3,  interval = FALSE)
dlmForecast(model = model.dlm , x = x.new , h = 3,  interval = TRUE, 
            nSim = 100)

}
