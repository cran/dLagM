\name{dLagM-package}
\alias{dLagM-package}

\docType{package}
\title{
Implementation of Time Series Regression Models with Distributed Lag Models
}
\description{
Provides time series regression models with one predictor using finite distributed lag models, polynomial (Almon) distributed lag models, geometric distributed lag models with Koyck transformation, and autoregressive distributed lag models. It also consists of functions for computation of h-step ahead forecasts from these models. See \href{dx.doi.org/10.1007/978-3-642-20059-5}{Baltagi (2011)} for more information.
}
\details{
\tabular{ll}{
Package: \tab dLagM\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2017-09-11\cr
License: \tab GPL-3\cr
}

To implement time series regression with finite distributed lag models use \code{dlm} function.

To implement time series regression with polynomial distributed lag models use \code{polyDlm} function. 

To implement time series regression with geometric distributed lag models with Koyck transformation use \code{koyckDlm} function. 

To implement time series regression with autoregressive distributed lag models use \code{ardlDlm} function. 
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}
\references{
B.H. Baltagi. \emph{Econometrics}, Fifth Ed. Springer, 2011.

R.C. Hill, W.E. Griffiths, G.G. Judge. \emph{Undergraduate Econometrics}. Wiley, 2000. 
}

\keyword{distributed lag model, time series, regression model, polynomial lag, predictor}

\seealso{
\code{\link[dLagM]{dlm}}, \code{\link[dLagM]{polyDlm}}, \code{\link[dLagM]{koyckDlm}}, \code{\link[dLagM]{ardlDlm}}
}
\examples{

# --- For examples, please refer to specific functions ---
}
