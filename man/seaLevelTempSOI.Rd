\name{seaLevelTempSOI}
\alias{seaLevelTempSOI}
\docType{data}
\title{Global mean sea level (GMSL), mean land and ocean temperature anomalies, and Southern Oscillation Index (SOI) data}
\description{This data set is composed of monthly global mean sea level (compared to 1993-2008 average) series by CSIRO, land ocean temperature anomalies (1951-1980 as a baseline period) by GISS, NASA, and monthly Southern Oscillation Index (SOI) by Australian Government Bureau of Meteorology (BOM) between July 1885 and June 2013. GMSL and temperature anomalies series are smoothed and seasonally adjusted.}

\usage{data(seaLevelTempSOI)}
\format{Multiple time series}
\source{
  Goddard Institute for Space Studies, NASA, US.
  Marine and Atmospheric Research, CSIRO, Australia.
  Australian Government Bureau of Meteorology (BOM).
  
}
\references{
Church, J. A. and N.J. White (2011), Sea-level rise from the late 19th to the early 21st Century. Surveys in Geophysics, doi:10.1007/s10712-011-9119-1.

 https://www.cmar.csiro.au/sealevel/sl_data_cmar.html
 
 https://data.giss.nasa.gov/gistemp/graphs_v4/
 
 https://www.bom.gov.au/climate/current/soihtm1.shtml
}
\examples{
data(seaLevelTempSOI)
level.ts <- ts(seaLevelTempSOI[,1], start = c(1880,7), freq = 12)
temp.ts <- ts(seaLevelTempSOI[,2], start = c(1880,7), freq = 12)
plot(level.ts, main="Time series plot of GMSL series.")
}
\keyword{datasets}


