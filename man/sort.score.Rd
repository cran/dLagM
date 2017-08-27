\name{sortScore}
\alias{sortScore}

\title{
Sort ACI and BIC scores
}
\description{
A function that displays sorted AIC and BIC scores.
}
\usage{
sortScore(x, score = c("bic", "aic"))
}
\arguments{
  \item{x}{
  A vector of AIC or BIC values.
  }
  \item{score}{
  The type of scores to be sorted.
  } 

}
\details{
This function sorts the AIC or BIC scores to display the smallest one at the top of a bunch of AIC or BIC scores.
}

\author{
Cameron Doyle

Maintainer: Cameron Doyle <cdoyle305@gmail.com>
}

\examples{
data(warming)
model.poly1 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                     q = 2 , k = 2 , show.beta = TRUE ,  show.summary = TRUE)
model.poly2 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 3 , k = 2 , show.beta = TRUE ,  show.summary = TRUE)
model.poly3 = polyDlm(x = warming$NoMotorVehicles , y = warming$Warming , 
                      q = 4 , k = 2 , show.beta = TRUE ,  show.summary = TRUE)

aic = AIC(model.poly1$model, model.poly2$model, model.poly3$model)
bic = BIC(model.poly1$model, model.poly2$model, model.poly3$model)
sortScore(aic,score = "aic")
sortScore(bic,score = "bic")

}
