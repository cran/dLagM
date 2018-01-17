\name{MASE}
\alias{MASE}

\title{
Compute mean absolute scaled error (MASE) for distributed lag models
}
\description{
A function that computes mean absolute scaled error for fitted DLMs.
}
\usage{
MASE(model, ...)
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

\value{

\item{MASE}{Mean absolute scaled error (MASE) for the observed and fitted series sent into the function.}

}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\references{
Hyndman, R.J. and Koehler, A.B. (2006). Another look at measures of forecast accuracy. \emph{International Journal of Forecasting}, 22, 679-688.
}


\examples{
data(warming)
# Fit a bunch of polynomial DLMs
model.poly1 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                     q = 2 , k = 2 , show.beta = TRUE ,  show.summary = FALSE)
model.poly2 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 3 , k = 2 , show.beta = TRUE ,  show.summary = FALSE)
model.poly3 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 4 , k = 2 , show.beta = TRUE ,  show.summary = FALSE)
MASE(model.poly1, model.poly2, model.poly3)

# Fit a bunch of finite DLMs
model.dlm1 = dlm(x = warming$NoMotorVehicles , y = warming$Warming, q =2)
model.dlm2 = dlm(x = warming$NoMotorVehicles , y = warming$Warming, q =3)
model.dlm3 = dlm(x = warming$NoMotorVehicles , y = warming$Warming, q =4)
MASE(model.dlm1, model.dlm2, model.dlm3)

# Fit a linear model
model.lm = lm(Warming ~ NoMotorVehicles , data = warming)
MASE(model.lm)

# Fit a Koyck model
model.koyck = koyckDlm(x = warming$NoMotorVehicles , y = warming$Warming )
MASE(model.koyck)

# Fit a bunch of ARDLs
model.ardl1 = ardlDlm(x = warming$NoMotorVehicles , y = warming$Warming, p=1, q=2)
model.ardl2 = ardlDlm(x = warming$NoMotorVehicles , y = warming$Warming, p=2, q=2)
model.ardl3 = ardlDlm(x = warming$NoMotorVehicles , y = warming$Warming, p=3, q=2)
MASE(model.ardl1 , model.ardl2 , model.ardl3)

# Find MASEs of different model objects
MASE(model.ardl1 , model.dlm1 , model.poly1, model.lm)
}
