\name{sortScore}
\alias{sortScore}

\title{
Sort AIC, BIC, MASE, MAPE, sMAPE, MRAE, GMRAE, or MBRAE scores
}
\description{
Displays sorted AIC, BIC, and MASE scores.
}
\usage{
sortScore(x, score = c("bic", "aic", "mase", "smape", "mape", "mrae", "gmrae", "mbrae"))
}
\arguments{
  \item{x}{
  A vector of AIC, BIC, MASE, MAPE, sMAPE, MRAE, GMRAE, or MBRAE values.
  }
  \item{score}{
  The type of scores to be sorted.
  } 

}
\details{
This function sorts the AIC, BIC, MASE, MAPE, sMAPE, MRAE, GMRAE, or MBRAE scores to display the smallest one at the top of a bunch of AIC, BIC, MASE, MAPE, sMAPE, MRAE, GMRAE, or MBRAE scores.
}

\author{
Cameron Doyle

Maintainer: Cameron Doyle <cdoyle305@gmail.com>
}

\examples{
\dontrun{
data(seaLevelTempSOI)
model.poly1 = polyDlm(x = seaLevelTempSOI$LandOcean,  y = seaLevelTempSOI$GMSL , 
                     q = 2 , k = 2 , show.beta = TRUE)
model.poly2 = polyDlm(x = seaLevelTempSOI$LandOcean,  y = seaLevelTempSOI$GMSL , 
                      q = 3 , k = 2 , show.beta = TRUE)
model.poly3 = polyDlm(x = seaLevelTempSOI$LandOcean,  y = seaLevelTempSOI$GMSL , 
                      q = 4 , k = 2 , show.beta = TRUE)

aic = AIC(model.poly1$model, model.poly2$model, model.poly3$model)
bic = BIC(model.poly1$model, model.poly2$model, model.poly3$model)
mase = MASE(model.poly1$model, model.poly2$model, model.poly3$model)
mbrae = MBRAE(model.poly1$model, model.poly2$model, model.poly3$model)

sortScore(aic , score = "aic")
sortScore(bic , score = "bic")
sortScore(mase , score = "mase")
sortScore(mbrae , score = "mbrae")
}
}
