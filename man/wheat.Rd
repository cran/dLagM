\name{wheat}
\alias{wheat}
\docType{data}
\title{World wheat production, CO2 emissions, and temperature anomalies data}
\description{This data set is composed of annual world total CO2 emissions, wheat production, harvested area, wheat production per hectare, and annual average temperature anomalies series between 1960 and 2017.}

\usage{data(wheat)}
\format{Multiple time series

\code{CO2..ppm.} column shows the global mean annual concentration of carbon dioxide (CO2) measured in parts per million (ppm).

\code{HarvestedArea..million.ha.} column shows the annual harvested area in the world scale in million hectare.

\code{Production..Mt.} column shows the annual world wheat production in million tons.

\code{ProducationPerArea} column shows the annual wheat production per hectare in tons.

\code{TempAnomaly..C.degrees.} column shows the land-ocean temperature index without smoothing.


}
\source{
  Australian Government Department of Agriculture and Water Services, ABARES
  CO2 and other Greenhouse Gas Emissions by Hannah Ritchie and Max Roser
}
\references{
 \url{http://www.agriculture.gov.au/abares/research-topics/agricultural-commodities/agricultural-commodities-trade-data#australian-crop-report-data}
 
 \url{https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions#annual-co2-emissions}
 
 \url{https://climate.nasa.gov/vital-signs/global-temperature/}
}
\examples{
data(wheat)
wheatProduction.ts = ts(wheat[,4], start = 1960)
plot(wheatProduction.ts, main="Time series plot 
of world wheat production series.")
}
\keyword{datasets}


