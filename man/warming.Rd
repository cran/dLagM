\name{warming}
\alias{warming}
\docType{data}
\title{Global warming and vehicle prediction data}
\description{This data set is composed of annual mean global warming series between 1997 and 2016 showing the change in global surface temperature relative to 1951-1980 average temperatures and the number of vehicles produced within the same time span over the globe. }

\usage{data(warming)}
\format{Multiple time series}
\source{
  Global Climate Center, NASA
  Organisation Internationale des Constructeurs d'Automobiles (OICA)
}
\references{
 \url{https://climate.nasa.gov/vital-signs/global-temperature/}
 
 \url{http://www.oica.net/category/production-statistics/}
}
\examples{
data(warming)
vehicleWarming.ts = ts(warming[,2:3], start = 1997)
plot(vehicleWarming.ts, main="Time series plots 
of global warming and the nuber of produced motor 
vehciles series.")
}
\keyword{datasets}


