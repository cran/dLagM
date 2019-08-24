\name{koyckDlm}
\alias{koyckDlm}

\title{
Implement distributed lag models with Koyck transformation
}
\description{
Applies distributed lag models with Koyck transformation with one predictor.
}
\usage{
koyckDlm(x , y)
}
\arguments{
  \item{x}{
  A vector including the observations of predictor time series. This is not restricted to \code{ts} objects.
  }
  \item{y}{
  A vector including the observations of dependent time series. This is not restricted to \code{ts} objects.
  }
}
\details{

To deal with infinite DLMs, we can use the Koyck transformation. When we apply Koyck transformation, we get the following:

\deqn{
      Y_{t} - \phi Y_{t-1}  = \alpha (1-\phi)+\beta X_{t} + (\epsilon_{t}-\phi \epsilon_{t-1}).
      }

When we solve this equation for \eqn{Y_{t}}, we obtain Koyck DLM as follows:
\deqn{
 Y_{t} = \delta_{1} + \delta_{2} Y_{t-1} + \delta_{3} X_{t} + \nu_{t},
}
where \eqn{\delta_{1} = \alpha (1-\phi),\delta_{2}=\phi,\delta_{3}=\beta} and the random error after the transformation is \eqn{\nu_{t}=(\epsilon_{t}-\phi \epsilon_{t-1})} (Judge and Griffiths, 2000).

Then, instrumental variables estimation is employed to fit the model.

The standard function \code{summary()} prints model summary for the model of interest.

AIC/BIC of a fitted KOyck model is displayed by setting the \code{class} attribute of model to \code{lm}. See the example.

}



\value{

\item{model}{An object of class \code{ivreg}. See the details of \href{https://www.rdocumentation.org/packages/AER/versions/1.2-5/topics/ivreg}{\code{ivreg}} function.}

\item{geometric.coefficients}{A vector composed of corresponding geometric distributed lag model coefficients.}
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
model.koyck = koyckDlm(x = warming$NoMotorVehicles , 
                       y = warming$Warming)
summary(model.koyck)     

attr(model.koyck$model,"class") = "lm"
AIC(model.koyck$model)
BIC(model.koyck$model)
}
