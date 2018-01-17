\name{ardlDlm}
\alias{ardlDlm}

\title{
Implement finite autoregressive distributed lag model
}
\description{
A function that applies autoregressive distributed lag models of order (p , q) with one predictor.
}
\usage{
ardlDlm(formula = NULL , data = NULL , x = NULL , y = NULL , p = 1 , q = 1 , 
         remove.p = NULL , remove.q = NULL , show.summary = TRUE)
}
\arguments{
  \item{formula}{A \code{formula} object for the model to be fitted. In the case of multiple predictor series, the model should be entered via a formula object. 
  }
  \item{data}{
  A \code{data.frame} including all dependent and independent series. In the case of multiple predictor series, the data should be entered via the \code{data} argument.
  }
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
  \item{remove.p}{
  A matrix or vector showing the lags of independent series to be removed from the full model for each independent series. Please see the details for the construction of this argument.
  }
  \item{remove.q}{
  A vector showing the autoregressive lags to be removed from the full model. Please see the details for the construction of this argument.
  }
  \item{show.summary}{
  If \code{TRUE}, prints standard model summary for the model of interest.
  }  
 
}
\details{
The autoregressive DLM is a flexible and parsimonious infinite distributed lag model. The model ARDL\eqn{(p, q)} is written as

\deqn{      Y_{t} = \mu+ \beta_{0}X_{t}+\beta_{1}X_{t-1}+\cdots +\beta_{p}X_{t-p}+\gamma_{1}Y_{t-1}+\cdots+\gamma_{q}Y_{t-q}+e_{t}.     }

When there is only one predictor series, both of \code{model} and \code{formula} objects can be used. But when they are supplied, both \code{x} and \code{y} arguments should be \code{NULL}.

The variable names in \code{formula} must match with the names of variables in \code{data} argument and it must be in the form of a generic formula for R functions. 

The argument \code{data} contains dependent series and independent series. 

The argument \code{remove.p} is used to specify which lags of each independent series will be removed from the full model. Each row of \code{remove.p} includes the numbers of lags to be removed in its columns and the rest of the elements should be set to \code{NA}. 

The argument \code{remove.q} is used to specify the autoregressive lags of dependent series to be removed from the full model. 

}

\value{

\item{model}{An object of class \code{lm}. See the details of \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html}{\code{lm}} function.}

\item{order}{A vector composed of \eqn{p} and \eqn{q} orders.}

\item{removed.p}{A matrix or vector showing the lags of independent series to be removed from the full model.}

\item{removed.q}{A vector showing the autoregressive lags to be removed from the full model.}

\item{formula}{Model formula of the fitted model. This is returned if multiple independent series are entered.}

\item{data}{A \code{data.frame} including all dependent and independent series. This is returned if multiple independent series are entered.}
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
# Only one independent series
data(warming)
model.ardl = ardlDlm(x = warming$NoMotorVehicles, 
y = warming$Warming, p = 1 , q = 1 , show.summary = TRUE)

# Remove some lags
rem.p = c(1)
rem.q = c(1,3)
model.ardl = ardlDlm(x = warming$NoMotorVehicles, 
y = warming$Warming, p = 2 , q = 3 , remove.p = rem.p, 
remove.q = rem.q, show.summary = TRUE)

# Multiple independent series
data(M1Germany)
data = M1Germany[1:144,]
model.ardlDlm  = ardlDlm(formula = logprice ~ interest + logm1, 
       data = data.frame(data) , p = 2 , q = 1 , show.summary = TRUE)
       
rem.p = matrix(c(1,2, NA), 3 , 1)
rem.q = c(1)
model.ardlDlm  = ardlDlm(formula = logprice ~ interest + logm1, 
       data = data.frame(data) , p = 2 , q = 2 , remove.p = rem.p, 
       remove.q = rem.q , show.summary = TRUE)
}
