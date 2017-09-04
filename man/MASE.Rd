\name{MASE}
\alias{MASE}

\title{
Compute mean absolute scaled error (MASE)
}
\description{
A function that computes mean absolute scaled error for fitted models.
}
\usage{
MASE(model, ...)
}
\arguments{
  \item{model}{
  Model object fitted for time series data. 
  }
  \item{...}{
  Optionally more fitted models.
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
model.poly1 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                     q = 2 , k = 2 , show.beta = TRUE ,  show.summary = FALSE)
model.poly2 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 3 , k = 2 , show.beta = TRUE ,  show.summary = FALSE)
model.poly3 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 4 , k = 2 , show.beta = TRUE ,  show.summary = FALSE)
                      
mase = MASE(model.poly1$model, model.poly2$model, model.poly3$model)
}
