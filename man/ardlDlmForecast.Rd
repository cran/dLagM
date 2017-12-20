\name{ardlDlmForecast}
\alias{ardlDlmForecast}

\title{
Compute forecasts for autoregressive distributed lag models
}
\description{
A function that computes forecasts for autoregressive distributed lag model with one predictor.
}
\usage{
ardlDlmForecast(model , x , h = 1 , interval = FALSE, alpha = 0.05 , nSim = 500)
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
# Only one independent series
data(warming)
model.ardl = ardlDlm(x = warming$NoMotorVehicles, 
y = warming$Warming, p = 1 , q = 1 , show.summary = TRUE)
ardlDlmForecast(model = model.ardl , x = c(95, 98) , 
                h = 2 , interval = FALSE)
ardlDlmForecast(model = model.ardl , x = c(95, 98, 87) , 
                h = 3 , interval = TRUE, nSim = 100)
                
# Multiple independent series
data(M1Germany)
data = M1Germany[1:144,]
model.ardlDlm1  = ardlDlm(formula = logprice ~ interest + logm1, 
       data = data.frame(data) , p = 2 , q = 1 , show.summary = TRUE)
x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09), ncol = 2, 
                nrow = 2)
ardlDlmForecast(model = model.ardlDlm1 , x = x.new , h = 2 , 
                interval = TRUE, nSim = 100)       
       
rem.p = matrix(c(1,2, NA), 3 , 1)
rem.q = c(1)
model.ardlDlm2  = ardlDlm(formula = logprice ~ interest + logm1, 
       data = data.frame(data) , p = 2 , q = 2 , remove.p = rem.p, 
       remove.q = rem.q , show.summary = TRUE)

ardlDlmForecast(model = model.ardlDlm2 , x = x.new , h = 2 , 
                interval = TRUE, nSim = 100)        
}
