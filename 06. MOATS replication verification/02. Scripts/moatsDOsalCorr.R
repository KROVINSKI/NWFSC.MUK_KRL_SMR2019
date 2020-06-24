
#library with function that does DO saturation calculation
library(wql)

# The Library wql
# # Functions to assist in the processing and
# exploration of data from environmental monitoring programs.
# The package name stands for ``water quality'' and reflects the
# original focus on time series data for physical and chemical
# properties of water, as well as the biota. Intended for
# programs that sample approximately monthly, quarterly or
# annually at discrete stations, a feature of many legacy data
# sets. Most of the functions should be useful for analysis of
# similar-frequency time series regardless of the subject
# matter.

#observed temperature of the MOATS
tMoats <- 12.01
# assumed salinity of MOATS for labiew calculation
assumedS <- 28.8
# saturated mg/L DO at obseved temperature and assumed salinity
# the oxySol() function is form the wql package
assumedSatDOmg <- oxySol(tMoats,assumedS)
# reported DO from labview output
reportedDOmg <- 3.56
# Back calculated fraction DO as reported by the oxygen sensor
percentDO <- reportedDOmg / assumedSatDOmg
# observed salinity
observedS <- 30.1
#satured mg/L at observed temperature and observed (not assumed) salinity
obseveredSatDOmg <- oxySol(tMoats, observedS)
# actual DO mg at observed temperature and salinity
actualDOmg <- percentDO * obseveredSatDOmg


# make a fun function!
moatsDOsalinityCorrection <- function(tMoats, assumedS, reportedDOmg, observedS){
  #I'll leave this an exersice to write a funcion for this correction...
  
  #this is what you want to return...
  return(actualDOmg)
}
