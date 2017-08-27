\name{MASE}
\alias{MASE}

\title{
Compute mean absolute scaled error (MASE)
}
\description{
A function that computes mean absolute scaled error.
}
\usage{
MASE(observed, fitted)
}
\arguments{
  \item{observed}{
  Observed time series data. 
  }
  \item{fitted}{
  Fitted values of the model for which the MASE value will be calculated.
  } 

}
\details{
Let \eqn{e_{t} = Y_{t}-\hat{Y}_{t}} be the one-step-ahead forecast error. Then, a scaled error is defined as

\deqn{
q_{t}=\frac{e_{t}}{\frac{1}{n-1}\sum_{i=2}^{n}|Y_{i}-Y_{i-1}|},
}
which is independent of the scale of the data. Mean absolute scaled error is defined as 

\deqn{
MASE = mean(|q_{t}|).
}
(Hyndman and Koehler, 2006)
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
model.poly = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
q = 2 , k = 2 , show.beta = TRUE ,  show.summary = TRUE)
MASE(observed = warming$Warming, fitted = model.poly$model$fitted.values)
}
