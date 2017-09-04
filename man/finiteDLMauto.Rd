\name{finiteDLMauto}
\alias{finiteDLMauto}

\title{
Find the optimal lag lenght for finite DLMs
}
\description{
A function that fits finite DLMs for a range of lag lenths and orders the fitted models according to a desired measure.
}
\usage{
finiteDLMauto(x, y, q.min = NULL, q.max = NULL, k.order = NULL, 
model.type = c("dlm","poly"), error.type = c("MASE","AIC","BIC","radj"), trace = FALSE)
}
\arguments{
  \item{x}{
  A vector including the observations of predictor time series. This is not restricted to \code{ts} objects.
  }
  \item{y}{
  A vector including the observations of dependent time series. This is not restricted to \code{ts} objects.
  }
  \item{q.min}{
  An integer representing the lower limit of the range of lag lengths to be considered. If missing, it will be set to 1.
  }
  \item{q.max}{
  An integer representing the upper limit of the range of lag lengths to be considered. If missing, it will be set to 10.
  }
  \item{k.order}{
  An integer representing order of polynomial distributed lags.
  }
  \item{model.type}{
  The type of model to be fitted. If set to \code{dlm}, finite distributed lag models are fitted. If set to \code{poly}, polynomial distributed lag models are fitted. 
  }
  \item{error.type}{
  The type of goodness-of-fit measure to be used for the selection of optimal lag length. If set to \code{MASE}, the optimal lag lenthg is determined according to MASE. If set to \code{AIC}, the optimal lag lenthg is determined according to AIC. If set to \code{BIC}, the optimal lag lenthg is determined according to BIC. If set to \code{radj}, the optimal lag lenthg is determined according to Adjusted R-square.
  }
  \item{trace}{
  If \code{TRUE}, prints all of the goodness-of-fit measures for all fitted models.
  }  
 
}
\details{
If \code{q.max} is entered too large, its value will be adjusted to have a sample size of 10 for fitting the regression model for finite DLMs.
}

\value{
Retunrs a \code{data.frame} including the values of goodness-of-fit measures and corresponding lag lenghts.
}


\author{
Agung Andiojaya

Maintainer: Agung Andiojaya <a@gmail.com>
}

\examples{
data(warming)
# Run the search over polynomial DLMs according to MASE values
finiteDLMauto(x = warming$NoMotorVehicles , y = warming$Warming ,
q.max = 12, k.order = 3, model.type = "poly",
error.type = "MASE", trace = TRUE)

# Run the search over finite DLMs according to AIC values
finiteDLMauto(x = warming$NoMotorVehicles , y = warming$Warming , 
q.min = 2, q.max = 12, model.type = "dlm" ,error.type = "AIC", 
trace = TRUE)


}
