\name{dLagM-package}
\alias{dLagM-package}

\docType{package}
\title{
Implementation of Time Series Regression Models with Distributed Lag Models
}
\description{
Provides time series regression models with one predictor using finite distributed lag models, polynomial (Almon) distributed lag models, geometric distributed lag models with Koyck transformation, and autoregressive distributed lag models. It also consists of functions for computation of h-step ahead forecasts from these models. See Demirhan (2020)(<doi:10.1371/journal.pone.0228812>) and Baltagi (2011)(<doi:10.1007/978-3-642-20059-5>) for more information.
}
\details{
\tabular{ll}{
Package: \tab dLagM\cr
Type: \tab Package\cr
Version: \tab 1.1.3\cr
Date: \tab 2020-05-10\cr
License: \tab GPL-3\cr
}

To implement time series regression with finite distributed lag models, use \code{dlm} function.

To implement time series regression with polynomial distributed lag models, use \code{polyDlm} function. 

To implement time series regression with geometric distributed lag models with Koyck transformation, use \code{koyckDlm} function. 

To implement time series regression with autoregressive distributed lag models, use \code{ardlDlm} function. 

To implement ARDL Bounds test, use \code{ardlBound} function. 

To produce forecasts for any of the models, use \code{forecast} function.

To summarise the results of a model fitting, use \code{summary} function.
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}
\references{
B.H. Baltagi. \emph{Econometrics}, Fifth Ed. Springer, 2011.

H. Demirhan. dLagM: An R package for distributed lag models and ARDL bounds testing. \emph{PLoS ONE}, 15(2): e0228812, 2020. DOI: 10.1371/journal.pone.0228812.

R.C. Hill, W.E. Griffiths, G.G. Judge. \emph{Undergraduate Econometrics}. Wiley, 2000. 

J. Soren, A.Q. Philips. "pss: Perform bounds test for cointegration and perform dynamic simulations."

P.K. Narayan. The Saving and Investment Nexus for China: Evidence from cointegration tests. \emph{Applied Economics} 37(17):1979-1990, 2005.

M.H. Pesaran, S. Yongcheol, R.J. Smith. Bounds testing approaches to the analysis of level relationships. \emph{Journal of Applied Econometrics} 16(3):289-326, 2001.
}

\concept{distributed lag model}
\concept{time series}
\concept{regression model}
\concept{polynomial lag}
\concept{ARDL bounds test}
\concept{predictor}
\concept{MASE}
\concept{MRAE}
\concept{GMRAE}
\concept{MBMRAE}

\seealso{
\code{\link[dLagM]{dlm}}, \code{\link[dLagM]{polyDlm}}, \code{\link[dLagM]{koyckDlm}}, \code{\link[dLagM]{ardlDlm}}
}
\examples{

# --- For examples, please refer to specific functions ---
}
