\name{forecast}
\alias{forecast}

\title{
Compute forecasts for distributed lag models
}
\description{
A function that computes forecasts for the finite distributed lag models, autoregressive distributed lag models, Koyck transformation of distributed lag models, and polynomial distributed lag models.
}
\usage{
forecast(model , x , h = 1 , interval = FALSE, level = 0.95 , nSim = 500)
}
\arguments{
  \item{model}{
  An object of class \code{lm} including the fitted model with \code{ardl.dlm()} function.
  }
  \item{x}{
  A vector or matrix including the new observations of independent time series. This is not restricted to \code{ts} objects. Please see the details for construction of this argument.
  } 
  \item{h}{
  The number of ahead forecasts.
  }  
  \item{interval}{
  If \code{TRUE}, \eqn{(1-\alpha)\%} prediction intervals for forecasts are displayed along with forecasts.
  }
  \item{level}{
  Confidence level of prediction interval. 
  }
  \item{nSim}{
  An integer showing the number of Monte Carlo simulations used to compute prediction intervals for forecasts.
  }
}
\details{
This function directly uses the model formula and estimates of model coefficients to find forecast one-by-one starting from the one-step ahead forecast. 

Prediction intervals are found by the Monte Carlo approach using a Gaussian error distribution with zero mean and empirical variance of the dependent series.

When the \code{model} argument includes multiple independent series, \code{x} must be entered as a matrix including the new observations of each independent series in its rows. The number of columns of \code{x} must be equal to the forecast horizon \code{h} and the rows of \code{x} must match with the independent series in the order they appear in the \code{data}.

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
#--- ARDL dlm ---
model.ardl = ardlDlm(x = warming$NoMotorVehicles, 
              y = warming$Warming, p = 1 , q = 1 )
forecast(model = model.ardl , x = c(95, 98) , 
                h = 2 , interval = FALSE)
forecast(model = model.ardl , x = c(95, 98, 87) , 
                h = 3 , interval = FALSE)
             
# Multiple independent series
data(M1Germany)
data = M1Germany[1:144,]
model.ardlDlm1  = ardlDlm(formula = logprice ~ interest + logm1, 
       data = data.frame(data) , p = 2 , q = 1 )
x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09), ncol = 2, 
                nrow = 2)
forecast(model = model.ardlDlm1 , x = x.new , h = 2 , 
                interval = TRUE, nSim = 100)       
       
rem.p = list(interest = c(1,2))
rem.q = c(1)
remove = list(p = rem.p , q = rem.q)
model.ardlDlm2  = ardlDlm(formula = logprice ~ interest + logm1, 
       data = data.frame(data) , p = 2 , q = 2 , 
       remove = remove)

forecast(model = model.ardlDlm2 , x = x.new , h = 2 , 
                interval = FALSE)  
                
#--- Finite dlm ---                
model.dlm = dlm(x = warming$NoMotorVehicles , 
                y = warming$Warming , q = 2)
forecast(model = model.dlm , x = c(95 , 98, 101) , h = 3)


# Multiple independent series
model.dlm  = dlm(formula = logprice ~ interest + logm1, 
          data = data.frame(data) , q = 4)

x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09), 
                ncol = 2, nrow = 2)
forecast(model = model.dlm , x = x.new , h = 2 , 
            interval = FALSE)

# Some lags are removed:
# Remove lags 0 and 2 from "interest" and 
# lags 1 and 3 from "logm1"
removed = list(interest = c(0,2), logm1 = c(1,3))
removed
model.dlm  = dlm(formula = logprice ~ interest + logm1 , 
              data = data.frame(data) , q = 4 , remove = removed)

x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09 , 0.079 , 9.19 , 
                  0.069 , 9.21) , ncol = 4, nrow = 2)
forecast(model = model.dlm , x = x.new , h = 4 , 
         interval = FALSE)
forecast(model = model.dlm , x = x.new , h = 4 , 
            interval = FALSE)
  
x.new =  matrix(c(0.07 , 9.06 , 0.071 , 9.09, 0.08 , 9.12), ncol = 3, 
                nrow = 2)
forecast(model = model.dlm , x = x.new , h = 3,  interval = FALSE)

#--- Koyck dlm --- 
model.koyck = koyckDlm(x = warming$NoMotorVehicles , 
                       y = warming$Warming)
forecast(model = model.koyck  , x = c(95, 98, 101), h = 3 , 
                interval = FALSE)

#--- Polynomial dlm ---         
model.poly = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                q = 2 , k = 2 , show.beta = TRUE)
forecast(model = model.poly , x = c(95, 98) , h = 1 , 
                interval = FALSE)

}