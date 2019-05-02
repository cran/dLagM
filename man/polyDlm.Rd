\name{polyDlm}
\alias{polyDlm}

\title{
Implement finite polynomial distributed lag model
}
\description{
Applies polynomial distributed lag models with one predictor.
}
\usage{
polyDlm(x , y , q , k , show.beta = TRUE)
}
\arguments{
  \item{x}{
  A vector including the observations of predictor time series. This is not restricted to \code{ts} objects.
  }
  \item{y}{
  A vector including the observations of dependent time series. This is not restricted to \code{ts} objects.
  } 
  \item{q}{
  An integer representing finite lag length.
  }  
  \item{k}{
  An integer representing order of polynomial distributed lags.
  }
  \item{show.beta}{
  If \code{TRUE}, generates original beta parameters and associated t-tests and prints the results.
  }
}
\details{
Finite distributed lag models, in general, suffer from the multicollinearity due to inclusion of the lags of the same variable in the model. To reduce the impact of this multicollinearity, a polynomial shape is imposed on the lag distribution (Judge and Griffiths, 2000). The resulting model is called Polynomial Distributed Lag model or Almond Distributed Lag Model. 

Imposing a polynomial pattern on the lag distribution is equivalent to representing \eqn{\beta} parameters with another $k$th order polynomial model of time. So, the effect of change in \eqn{X_{t-s}} on the expected value of \eqn{Y_{t}} is represented as follows:

\deqn{
  \frac{\partial E(Y_{t})}{\partial X_{t-s}}=\beta_{s}=\gamma_{0}+\gamma_{1}s+\gamma_{2}s^{2}+\cdots+\gamma_{k}s^{k}
}

where \eqn{s=0,\dots,q} (Judge and Griffiths, 2000). Then the model becomes:

\deqn{
  Y_{t} = \alpha +\gamma_{0}Z_{t0}+\gamma_{1}Z_{t1}+\gamma_{2}Z_{t2}+\cdots +\gamma_{k}Z_{tk}  + \epsilon_{t}.
}

The standard function \code{summary()} prints model summary for the model of interest.

}

\value{

\item{model}{An object of class \code{lm}.}

\item{designMatrix}{The design matrix composed of transformed z-variables.}

\item{designMatrix.x}{The design matrix composed of original x-variables.}

\item{beta.coefficients}{Estimates and t-tests of original beta coefficients. This will be generated if \code{show.beta} is set to \code{TRUE}.}
}

\references{
B.H. Baltagi. \emph{Econometrics}, Fifth Ed. Springer, 2011.

R.C. Hill, W.E. Griffiths, G.G. Judge. \emph{Undergraduate Econometrics}. Wiley, 2000. 
}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

\examples{
data(warming)
model.poly = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
q = 2 , k = 2 , show.beta = TRUE)
summary(model.poly)
}
