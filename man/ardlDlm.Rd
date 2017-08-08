\name{ardlDlm}
\alias{ardlDlm}

\title{
Implement finite autoregressive distributed lag model
}
\description{
A function that applies autoregressive distributed lag models of order (p , q) with one predictor.
}
\usage{
ardlDlm(x , y , p = 1 , q = 1 , show.summary = TRUE)
}
\arguments{
  \item{x}{
  A vector including the observations of predictor time series. This is not restricted to \code{ts} objects.
  }
  \item{y}{
  A vector including the observations of dependent time series. This is not restricted to \code{ts} objects.
  }
  \item{p}{
  An integer representing the order of autoregressive process.
  }
  \item{q}{
  An integer representing finite lag length.
  }  
  \item{show.summary}{
  If \code{TRUE}, prints standard model summary for the model of interest.
  }  
 
}
\details{
The autoregressive DLM is a flexible and parsimonious infinite DLMs. We write the model ARDL\eqn{(p, q)} as

\deqn{      Y_{t} = \mu+ \beta_{0}X_{t}+\beta_{1}X_{t-1}+\cdots +\beta_{p}X_{t-p}+\gamma_{1}Y_{t-1}+\cdots+\gamma_{q}Y_{t-q}+e_{t}.     }

}

\value{

\item{model}{An object of class \code{lm}. See the details of \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html}{\code{lm}} function.}

\item{order}{A vector composed of \eqn{p} and \eqn{q} orders.}
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
model.ardl = ardlDlm(x = warming$NoMotorVehicles, 
y = warming$Warming, p = 1 , q = 1 , show.summary = TRUE)
}
