\name{dlm}
\alias{dlm}

\title{
Implement finite distributed lag model
}
\description{
A function that applies distributed lag models with one predictor.
}
\usage{
dlm(x , y , q , show.summary = TRUE)
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
\item{show.summary}{
  If \code{TRUE}, prints standard model summary for the model of interest.
  }  
 
}
\details{
When a decision made on a variable, some of the related variables would be effected through time. For example, when income tax rate is increased, this would reduce expenditures of consumers on goods and services, which reduces profits of suppliers, which reduces the demand for productive inputs, which reduces the profits of the input suppliers, and so on (Judge and Griffiths, 2000). These effects occur over the future time periods; hence, they are distributed across the time.    

In a distributed-lag model, the effect of an independent variable \eqn{X} on a dependent variable \eqn{Y} occurs over the time. Therefore, DLMs are dynamic models. A linear finite DLM with one independent variable is written as follows:


\deqn{
      Y_{t} = \alpha +\sum_{s = 0}^{q}\beta_{s}X_{t-s}+\epsilon_{t},
      }

where \eqn{\epsilon_{t}} is a stationary error term with \eqn{E(\epsilon_{t})=0, Var(\epsilon_{t})=\sigma^{2},Cov(\epsilon_{t},\epsilon_{s})=0}.
}

\value{

\item{model}{An object of class \code{lm}. See the details of \code{href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html}{lm}} function.}

\item{designMatrix}{The design matrix composed of transformed z-variables.}

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
model.dlm = dlm(x = warming$NoMotorVehicles , 
y = warming$Warming , q = 2 ,  show.summary = TRUE)
}
