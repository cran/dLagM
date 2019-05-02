\name{ardlBound}
\alias{ardlBound}

\title{
Implement ARDL bounds test
}
\description{
Applies ARDL bounds test with the approach of Pesaran et al. (2001).
}
\usage{
ardlBound(data = NULL, formula = NULL, case = 3, p = NULL, k = NULL, 
          autoOrder = FALSE, ic = c("AIC", "BIC"), max.p = 15, 
          max.q = 15, ECM = TRUE, stability = TRUE)         
}
\arguments{
  \item{data}{
  A \code{data.frame} including all dependent and independent series. 
  }
  \item{formula}{A \code{formula} object showing the dependent and independent series. 
  }
  \item{case}{
  An integer up to 5 showing the case number. See details.
  }
  \item{p}{
  An integer representing the order of short-run response or a \code{data.frame} to specify a different order of short-run response for each variable.  
  }
  \item{k}{
  An integer representing the number of independent series.
  }  
  \item{autoOrder}{
  If \code{TRUE}, the order of ARDL will be found by the \code{ardlBounOrders} function.
  }
  \item{ic}{
  Information criterion to be used in the search for optimal orders.
  }
  \item{max.p}{
  Maximum order for the short-run coefficients.
  }
  \item{max.q}{
  Maximum auto-regressive order.
  }
  \item{ECM}{
  If \code{TRUE}, the error correction model corresponding to the \code{case} is also fitted and included in the outputs.
  }
  \item{stability}{
  If both \code{ECM} and \code{stability} are \code{TRUE}, the CUSUM and MOCUM charts are generated over recursive residuals using the package \code{strucchange}.
  }
}
\details{

The argument \code{case} takes the values 1 for "no intercept, no trend", 2 for "restricted intercept, no trend", 3 for "unrestricted intercept, no trend", 
4 for "unrestricted intercept, restricted trend", and 5 for "unrestricted intercept, unrestricted trend." 

If the argument \code{p} is entered as an integer, the same value is used to specify order of short-run response for all variables.

We follow the formulation of Pesaran et al. (2001). So, the upper limit of summations in the model formulation go up to (p-1). User should consider this while setting the value(s) of \code{p}.

Both of DW and BP tests are applied to detect autocorrelation and heteroskedasticity in residuals as a part of bounds testing procedure. If either of the tests fails, the procedure is stopped. 
}

\value{

\item{model}{An object including the fitted model under the null and alternative hypotheses.}

\item{F.stat}{The value of F-statistic coming out of the Wald test.}

\item{p}{p-orders in the lag structure.}

\item{q}{Autoregressive order in the lag structure.}

\item{k}{The number of independent series.} 

\item{bg}{Breusch-Godfrey test results. Returns \code{NULL} if skipped.}

\item{lb}{Ljung-Box test results. Returns \code{NULL} if skipped.}

\item{bp}{Breusch-Pagan test results. Returns \code{NULL} if skipped.}

\item{ECM}{A list including the error correction series in the element \code{EC.t}, the fitted error correction model in the element \code{EC.model}, and the coefficient of error correction part in the element \code{EC.beta}.}

}

\references{

C-S. J. Chu, K. Hornik, C-M. Kuan. MOSUM tests for parameter constancy. Biometrika, 82, 603-617, 1995.

P.K. Narayan. The Saving and Investment Nexus for China: Evidence from Cointegration Tests. \emph{Applied Economics} 37(17):1979-1990, 2005.

M.H. Pesaran, S. Yongcheol, R.J. Smith. Bounds testing approaches to the analysis of level relationships. \emph{Journal of Applied Econometrics} 16(3):289-326, 2001.

J. Soren, A.Q. Philips. "pss: Perform bounds test for cointegration and perform dynamic simulations."

A. Zeileis, F. Leisch, K. Hornik, C. Kleiber. strucchange: An R Package for Testing for Structural Change in Linear Regression
  Models. Journal of Statistical Software, 7, 1-38, 2002.
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\examples{
data(M1Germany)
data = M1Germany[1:144,]
model <- ardlBound(data = data , formula = logprice ~ interest + logm1 , case = 2 , p = 2)

# Let ardlBoundOrders() function find the orders
model <- ardlBound(data = data , formula = logprice ~ interest + logm1 , case = 2 , 
                    max.p = 2, max.q = 2)

}
