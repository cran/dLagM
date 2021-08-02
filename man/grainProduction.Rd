\name{grainProduction}
\alias{grainProduction}
\docType{data}
\title{World oats, corn, rice, wheat production, CO2 emissions, temperature anomalies, cropland data}

\description{This data set is composed of annual world total CO2 emissions, oats, corn, rice, and wheat production, cropland area, and annual average temperature anomalies series between 1961 and 2018.}

\usage{data(grainProduction)}
\format{Multiple time series

\code{CO2(Tons)} column shows the global mean annual carbon dioxide (CO2) emissions measured in tons.

\code{Land-OceanTempIndex(C)} column shows the land-ocean temperature index without smoothing.

\code{Cropland(1Mha)} column shows the annual cropland area in the world scale in million hectares.

\code{Oats(1Mt)} column shows the annual world oats production in million tons.

\code{Corn(1Mt)} column shows the annual world corn production in million tons.

\code{Rice(1Mt)} column shows the annual world rice production in million tons.

\code{Wheat(1Mt)} column shows the annual world wheat production in million tons.

}

\source{
  CO2 emissions data: Hannah Ritchie and Max Roser (2020) - "CO2 and Greenhouse Gas Emissions". Published online at OurWorldInData.org.
  
  Temperature anomalies data: NASA
  
  Grain production data: United States Department of Agriculture, Foreign Agricultural Service
  
  Cropland data: Food and Agriculture Organization of United Nations
}
\references{
 CO2 emissions data: https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions
 
 Temperature anomalies: https://climate.nasa.gov/vital-signs/global-temperature/
 
 Grain production data: psd_grains_pulses.csv from https://apps.fas.usda.gov/psdonline/app/index.html#/app/downloads
  
 Cropland data: https://www.fao.org/faostat/en/#data/RL/visualize
}

\examples{
data(grainProduction)
oatsProduction.ts = ts(grainProduction[,5], start = 1961)
plot(oatsProduction.ts, main="Time series plot 
of world oats production series.")
}
\keyword{datasets}


