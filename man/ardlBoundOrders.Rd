\name{ardlBoundOrders}
\alias{ardlBoundOrders}

\title{
Find optimal orders (lag structure) for ARDL bounds test
}
\description{
Computes optimal orders (lag structure) for the short-run relationships and autoregressive part of the ARDL model prior to ARDL bounds test with the approach of Pesaran et al. (2001).
}
\usage{
ardlBoundOrders(data = NULL , formula = NULL, ic = c("AIC", "BIC"), 
                max.p = 15,  max.q = 15  )
}
\arguments{
  \item{data}{
  A \code{data.frame} including all dependent and independent series. 
  }
  \item{formula}{A \code{formula} object showing the dependent and independent series. 
  }
  \item{ic}{
  Information criterion to be used in the serach for optimal orders.
  }
  \item{max.p}{
  Maximum order for the short-run coefficients.
  }
  \item{max.q}{
  Maximum auto-regressive order.
  }
}
\details{
 This function first assumes that all p-orders are equal for the short-run relationships and finds the optimal p-order and autoregressive orders. Then, it finds the best subset of p-orders allowing them to change for each series in the short-run relationship part of the ARDL model under alternative hypothesis of ARDL bounds test. 
}

\value{

\item{p}{An \code{integer} or \code{data.frame} object including p-orders for the short-run relationship part.}

\item{q}{The autoregressive order.}

\item{IC.table}{The selected IC of all the considered models where all short-run relationship orders (p-orders) are equal.}

\item{IC.p}{The selected IC of all possible combinations of short-run relationship orders (p-orders). The reported lag structure is the one that gives the minimum IC among these combinations.}

}

\author{
Haydar Demirhan

Maintainer: Haydar Demirhan <haydar.demirhan@rmit.edu.au>
}

