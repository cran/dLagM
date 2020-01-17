\name{sunspotTemp}
\alias{sunspotTemp}
\docType{data}
\title{Sunspot numbers and mean temperature anomalies data}
\description{This data set is composed of monthly mean global surface temperature series by GISS NASA and sunspot numbers recorded by SWPC Space Weather Operations (SWO) between January 1991 and November 2019. }

\usage{data(sunspotTemp)}
\format{Multiple time series}
\source{
  Goddard Institute for Space Studies, NASA, US.
  Space Weather Prediction Center National Oceanic and Atmospheric Administration, US.
  
}
\references{
 \url{https://data.giss.nasa.gov/gistemp/graphs_v4/}
 
 \url{ftp://ftp.swpc.noaa.gov/pub/weekly/RecentIndices.txt}
}
\examples{
data(sunspotTemp)
sunspots.ts <- ts(sunspotTemp[,3], start = c(1991,1), freq = 12)
temp.ts <- ts(sunspotTemp[,4], start = c(1991,1), freq = 12)
plot(sunspots.ts, main="Time series plots 
of sunspot numbers series.")
}
\keyword{datasets}


