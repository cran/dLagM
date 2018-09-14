\name{sortScore}
\alias{sortScore}

\title{
Sort ACI, BIC and MASE scores
}
\description{
A function that displays sorted AIC, BIC, and MASE scores.
}
\usage{
sortScore(x, score = c("bic", "aic", "mase"))
}
\arguments{
  \item{x}{
  A vector of AIC, BIC, or MASE values.
  }
  \item{score}{
  The type of scores to be sorted.
  } 

}
\details{
This function sorts the AIC, BIC, or MASE scores to display the smallest one at the top of a bunch of AIC, BIC, or MASE scores.
}

\author{
Cameron Doyle

Maintainer: Cameron Doyle <cdoyle305@gmail.com>
}

\examples{
data(warming)
model.poly1 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                     q = 2 , k = 2 , show.beta = TRUE)
model.poly2 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 3 , k = 2 , show.beta = TRUE)
model.poly3 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 4 , k = 2 , show.beta = TRUE)

aic = AIC(model.poly1$model, model.poly2$model, model.poly3$model)
bic = BIC(model.poly1$model, model.poly2$model, model.poly3$model)
mase = MASE(model.poly1$model, model.poly2$model, model.poly3$model)

sortScore(aic , score = "aic")
sortScore(bic , score = "bic")
sortScore(mase , score = "mase")

}
