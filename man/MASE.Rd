\name{GoF}
\alias{GoF}
\alias{MASE}
\alias{sMAPE}
\alias{MAPE}
\alias{MRAE}
\alias{GMRAE}
\alias{MBRAE}

\title{
Compute goodness-of-fit measures for DLMs
}
\description{
Computes mean absolute error (MAE), mean squared error (MSE), mean percentage error (MPE), symmetric mean absolute percentage error (sMAPE), mean absolute percentage error (MAPE), mean absolute scaled error (MASE), mean relative absolute error (MRAE), geometric mean relative absolute error (GMRAE), mean bounded relative absolute error (MBRAE), unscaled MBRAE (UMBRAE) for distributed lag models.
}
\usage{
GoF(model, ...)
MASE(model, ...)
sMAPE(model, ...)
MAPE(model, ...)
MRAE(model, ...)
GMRAE(model, ...)
MBRAE(model, ...)
}
\arguments{
  \item{model}{
  Model object fitted for time series data. 
  }
  \item{...}{
  Optionally, more fitted models.
  } 

}
\details{

See Chen et al. (2017) for the definitions of MSE, MAE, MAPE, sMAPE, MRAE, GMRAE, MBRAE, UMBMRAE.

Let \eqn{e_{t} = Y_{t}-\hat{Y}_{t}} be the one-step-ahead forecast error. Then, a scaled error is defined as

\deqn{
q_{t}=\frac{e_{t}}{\frac{1}{n-1}\sum_{i=2}^{n}|Y_{i}-Y_{i-1}|},
}
which is independent of the scale of the data. Mean absolute scaled error is defined as 

\deqn{
MASE = mean(|q_{t}|)
}
(Hyndman and Koehler, 2006).

Fitted models would be finite, polynomial, Koyck, ARDL DLMs, or linear model fitted with \code{lm()} function. This function also computes MASE values of multiple models when fed at the same time.
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\references{
Chen, C.,  Twycross, J., Garibaldi, J.M. (2017). A new accuracy measure based on bounded relative error for time series forecasting. \emph{PLoS ONE},
12(3), e0174202.


Hyndman, R.J. and Koehler, A.B. (2006). Another look at measures of forecast accuracy. \emph{International Journal of Forecasting}, 22, 679-688.
}


\examples{
data(warming)
# Fit a bunch of polynomial DLMs
model.poly1 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                     q = 2 , k = 2 , show.beta = TRUE)
model.poly2 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 3 , k = 2 , show.beta = TRUE)
model.poly3 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 4 , k = 2 , show.beta = TRUE)
MASE(model.poly1, model.poly2, model.poly3)

# Fit a bunch of finite DLMs
model.dlm1 = dlm(x = warming$NoMotorVehicles , y = warming$Warming, q =2)
model.dlm2 = dlm(x = warming$NoMotorVehicles , y = warming$Warming, q =3)
model.dlm3 = dlm(x = warming$NoMotorVehicles , y = warming$Warming, q =4)
MASE(model.dlm1, model.dlm2, model.dlm3)
MBRAE(model.dlm1, model.dlm2, model.dlm3)
GoF(model.dlm1, model.dlm2, model.dlm3)

# Fit a linear model
model.lm = lm(Warming ~ NoMotorVehicles , data = warming)
MASE(model.lm)
GoF(model.lm)

# Fit a Koyck model
model.koyck = koyckDlm(x = warming$NoMotorVehicles , y = warming$Warming )
MASE(model.koyck)
GoF(model.koyck)

# Fit a bunch of ARDLs
model.ardl1 = ardlDlm(x = warming$NoMotorVehicles , y = warming$Warming, p=1, q=2)
model.ardl2 = ardlDlm(x = warming$NoMotorVehicles , y = warming$Warming, p=2, q=2)
model.ardl3 = ardlDlm(x = warming$NoMotorVehicles , y = warming$Warming, p=3, q=2)
MASE(model.ardl1 , model.ardl2 , model.ardl3)
GoF(model.ardl1 , model.ardl2 , model.ardl3)

# Find MASEs of different model objects
MASE(model.ardl1 , model.dlm1 , model.poly1, model.lm)
GoF(model.ardl1 , model.dlm1 , model.poly1, model.lm)
}
