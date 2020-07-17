##Hello World

#*********************************
## Version Check
#********************************* 
R.version


## Author: OA Lab, NWFSC
## Title: Aquarium Water Chemistry Investigation: Dissolved Oxygen and Salinity Corrections
## Date: May-July 2020

#*********************************
##Libraries
#********************************* 
library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(readr)
library(tidyr)
library(data.table)
library(lubridate)
library(violinmplot)
library(vioplot)
library(yarrr)
library(datapasta)
library(reprex)
library(miniUI)
library(gridExtra)
library(wql)

#*********************************
## Outline Current 2020.06.01
#*********************************

# 1. Setting the Working Directory
# 2. Calling & Reading in " dml "
# 3. Reformatting variables/vectors (Factors)
# 4. Creating dateTime objects
# 5. Creating Treatment Variables
# 6. Creating Night & Day Variables 
# 7. Salinity (4 New Vectors)
# 8. Creating Corrected Salinity Value 
# 9. DO
# 10. Filtering Water Chemistry Dataframe

# 0. placeholder
# 5. placeHODOR
# 6. placeHODOR
# 7. placeHODOR
# 8. placeHODOR
# 9. placeHODOR
# 10. placeHODOR


#*********************************
## 1. Setting the Working Directory
#*********************************

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/01a. WaterChemistry Data")


#*********************************
## 2. Calling & Reading in " dml " 
#*********************************
#### START OF SIMPLE DML DATAFRAME ####

dml <- read.csv(file = "M01thruM13moatslog_n17.csv", stringsAsFactors = FALSE)
dim(dml)

# # Super Speedy subsampled data
# dml <- read.csv(file = "M01thruM13moatslog_n333.csv", stringsAsFactors = FALSE)
# dim(dml)


#*********************************
## 3. Reformatting variables/vectors (Factors) 
#*********************************

## 3.3 Changing variables | 
## Changing MOATs to Factors for the 13 different MOATs- these will be the discrete units for follow analysis
dml$moats <- factor(dml$moats)
# Checking the names of the different levels
levels(dml$moats)
##checking the dataset, dimensions
dim(dml)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

#*********************************
## 4.) Creating dateTime objects  
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
# 4.0 establish the date time object of the CSV |
dml$dateTime <- as.POSIXct(dml$dateTime, format="%Y-%m-%d %H:%M:%OS")
ReferenceTime <- as.POSIXct("2019-09-20 23:59:00")
class(ReferenceTime)

# QA check
dim(dml)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 5.) Creating Treatment Variables  
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 5.1 Identifying treatments by moats 
## establishing treatments
dml$treatment <- ""
dml$treatment[dml$moats == "M07" | dml$moats== "M10" | dml$moats== "M12"] <- "current"
dml$treatment[dml$moats == "M01"| dml$moats== "M06"] <- "hightemperature"
dml$treatment[dml$moats == "M02"| dml$moats== "M08" | dml$moats== "M13"] <- "allchange"
dml$treatment[dml$moats == "M03"| dml$moats == "M04" | dml$moats == "M05" | dml$moats== "M11"] <- "broken_and_ambientbroken"
#verify that this new column has been created
names(dml)
#results should include:
#[1] "moats"        "dateTime"     "aTemperature" "sTemperature" "pH"          
#[6] "DO"           "salinity"     "treatment"  

# QA check
dim(dml)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#*********************************
## 6.) Creating Night and Day Periods  
#*********************************


## 6.1 Narrative (Overall)
# Creating a day and night variables 
# Day and night periods will only refer to time under treatment as a way to 
#   exclude the acclimation period.
# day and night changed at about ~ 1230 on 05OCT19 
# Treatment start date considered to begin Monday 23SEP19 at 1200pm



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



# Krill Night Starts 1200 (~1230*) and ends 2100
# Krill Days Starts 2101 and ends 1159 (~1229*) 



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
# Interval 1 start 1200 23SEP19, end 1229 05OCT19
# Interval 2 start 1230 05OCT19, end 2100 30OCT19



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


# Concept Diagram graphic saved at /Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/Day_Night Period Loop.pdf/



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 6.2 New Column, New Variable in dml
#creating a new column, new variable "period"
dml$period <- ""


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



## 6.3 Disassembling dateTime to create 2 new variables
# Create new split date and time columns
dml$ObservationDate <- as.Date(dml$dateTime)
dml$ObservationTime <- format(as.POSIXct(dml$dateTime) ,format = "%H:%M:%S")


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 6.4 Narrative about Intervals 
# Interval 1
## Interval Date Start  "2019-09-23"
## Interval Date End    "2019-10-05"
## Day Start Time       "21:01:00"
## Day End Time         "11:29:00"
## Night Start Time     "12:00:00"
## Night End Time       "21:00:00"
## DaytoNight Ramp(D2N) "11:30:00"
# start
## DaytoNight Ramp(D2N) "11:59:00"
# stop

## Other Time
# Interval 2
## Interval Date Start  "2019-10-05"
## Interval Date End    "2019-10-30"
## Day Start Time       "21:01:00"
## Day End Time         "12:29:00"
## Night Start Time     "12:30:00"
## Night End Time       "21:00:00"
## Other Time

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 6.5 Day / Night Assignments 
# Using the "case_when" function in the tidyverse in the place of a loop
 
dml <- dml %>% mutate(period=case_when(
  (ObservationDate >= "2019-09-23") 
  & (ObservationDate <="2019-10-05") 
  & (ObservationTime >= "12:00:00") 
  & (ObservationTime <="21:00:00") ~"night",
  
  (ObservationDate >= "2019-10-05")
  & (ObservationDate <= "2019-10-30")
  & (ObservationTime >= "12:31:00") 
  & (ObservationTime <="21:00:00") ~"night",
  
  (ObservationDate >= "2019-09-23") 
  & (ObservationDate <="2019-10-05")
  & ((ObservationTime >= "21:01:00") 
     | (ObservationTime <="11:29:00")) ~"day",
  
  (ObservationDate >= "2019-10-05")
  & (ObservationDate <= "2019-10-30")
  & ((ObservationTime >= "21:01:00")
     | (ObservationTime <= "12:01:00")) ~"day",
  
  (ObservationDate >= "2019-09-23") 
  & (ObservationDate <="2019-10-05")
  & ((ObservationTime >= "11:30:00") 
     | (ObservationTime <="12:00:00")) ~"D2Nramp",
  
  (ObservationDate >= "2019-10-05")
  & (ObservationDate <= "2019-10-30")
  & ((ObservationTime >= "12:01:00")
     | (ObservationTime <= "12:30:00")) ~"D2Nramp",

  
  TRUE ~"other"
)

) 


period.intervals <- ggplot(dml, aes(x=dateTime, y=aTemperature)) +
  geom_point(aes(colour=period, point=))   +
  ggtitle("aTemperature Time Series Plots to Invesitgate Different Intervals") +
  ylim (5.0, 15.00)

period.intervals


period.interval.allchg <- ggplot(subset(dml[dml$treatment == "allchange", ])) + 
  aes(x=dateTime, y=aTemperature) + 
  geom_point(aes(colour=period, point=))   +
  ggtitle("aTemperature Time Series Plots to Invesitgate Different Intervals, Example All Change Treatment") +
  ylim (5.0, 15.00)

period.interval.allchg


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
dml
# QA check
dim(dml)








# Removing "other" period from day and night 
# not including acclimation period in this investigation

H2Ochemdf <- dml %>% filter(period != "other")



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#### END OF SIMPLE DML DATA FRAME ####



#*********************************
## 7.) Bringing in Salinity   
#*********************************

all_salinity <- read.csv(file = "KRL19_salinityreadings_all.csv", stringsAsFactors = FALSE)
all_salinity$PSU_ObsDate <- ""

all_salinity$Date <- as.POSIXct(all_salinity$Date, format= "%m/%d/%y")
all_salinity$PSU_ObsDate <- as.Date(all_salinity$Date)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

#*********************************
## 8.) Creating Corrected Salinity Value   
#*********************************

##  Notes from the 2020.05.29 office hours 
# creating 4 new variables in Water Chemistry Dataframe
# create a vector 181845  for the large n17 file
# create a vector 9283 observations long for the sub-sub sample n333 file
# logical vectors- boolean answers to those three below conditions
# 1 - the measurement per moats per day - MOATs and Data are both available
# 2 - the measurement (averaged across moats) - group by / summarize tools
# 3- (data gapped situation- no value matches) take the previous daily average based on observation  - lag? 
# previous line on a dataframe ... dplyr tool ... 
# https://dplyr.tidyverse.org/reference/lead-lag.html
# 4 - "the winning value" case_when #corrected conductivity value


# Salinity Values 
H2Ochemdf$PSUperMOATs <- ""
H2Ochemdf$PSUavgDaily <- ""
H2Ochemdf$PSUprevObs <- ""
H2Ochemdf$assumed_PSU <- ""
H2Ochemdf$Final_PSU <- ""




#*********************************
## 8.1 Plots (timeseries verification)
#*********************************
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

# Simple Time series plot 
# p <- ggplot(H2Ochemdf, aes(x=ObservationDate, y=salinity))+
#             geom_line() + 
#             ylim (30.05, 27.00) 
# p
# 
# p1 <- ggplot(subset(H2Ochemdf[H2Ochemdf$period == "night", ], 
#        aes(x=ObservationDate, y=Salinity))) + 
#   geom_line(aes(colour=moats, point=)) +
#   ggtitle("Salinity Values all Treatments, All MOATs, During Night Period")
# p1
# 
# ggplot(subset(H2Ochemdf, 
#               period %in% ("night")), 
#        aes(x=ObservationDate, y=salinity)) + 
#   geom_point(aes(colour=moats, point=)) +
#   ggtitle("Salinity Values all Treatments, All MOATs, During Night Period")
# 
# 
# ggplot(subset(H2Ochemdf[H2Ochemdf$moats == "M01", ], 
#               period %in% ("day")), 
#        aes(x=dateTime, y=salinity)) + 
#   geom_point(aes(colour=moats, point=)) +
#   ggtitle("Salinity Values, MOATs 01, During Night Period")




#*********************************
## 8.2 Practical Salinity Units - Measurements Per Moats per Day 
#*********************************


# Manual Readings
# Per MOAT measurement
H2Ochemdf$PSUperMOATs <- all_salinity$salinity[match
                                               (paste
                                                 (H2Ochemdf$ObservationDate,
                                                   H2Ochemdf$moats), 
                                                 paste
                                                 (all_salinity$PSU_ObsDate, 
                                                   all_salinity$moats))]

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#*********************************
## 8.3 Practical Salinity Units - Daily Avg Measurement 
#*********************************

# All MOATs daily
H2Ochemdf$PSUavgDaily <- all_salinity$salinity[match
                                               (paste
                                                 (H2Ochemdf$ObservationDate,'All'), 
                                                 paste
                                                 (all_salinity$PSU_ObsDate, 
                                                   all_salinity$moats), 
                                                 nomatch = NA_character_)]

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

#*********************************
## 8.4 Practical Salinity Units - Previous Daily Avg Measurement 
#*********************************

# Previous All MOATs daily
H2Ochemdf$PSUprevObs <- na.locf(H2Ochemdf$PSUavgDaily, na.rm = FALSE)


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 8.5 Practical Salinity Units - Assumed Salinity Value 
#*********************************
H2Ochemdf$assumed_PSU <- 28.8
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 8.6 Practical Salinity Units - Final Salinity Value 
#*********************************

# 8.6a Review of Vectors
# Review of the three different salinity values
# H2Ochemdf$PSUperMOATs       measurement
#           PSUperMOATs       per MOATS per Day
PSUperMOATs

# H2Ochemdf$PSUprevObs        Daily Average
#           PSUavgDaily       measurment across MOATs
PSUavgDaily 

# H2Ochemdf$PSUprevObs        Previous Daily 
#           PSUprevObs        Average to cover those gaps
PSUprevObs 

# Sailinity Assumption        28.8
#           Dates w/o         Dates without measurment 23~30SEP20
28.8


# Final Salinity
H2Ochemdf$Final_PSU <- 0.0

# H2Ochemdf$PSUperMOATs <- ""
# H2Ochemdf$PSUavgDaily <- ""
# H2Ochemdf$PSUprevObs <- ""
# H2Ochemdf$assumed_PSU <- ""

H2Ochemdf$Final_PSU <- as.numeric(case_when(
  H2Ochemdf$PSUperMOATs != 'NA' ~ H2Ochemdf$PSUperMOATs,
  H2Ochemdf$PSUavgDaily != 'NA' ~ H2Ochemdf$PSUavgDaily,
  H2Ochemdf$PSUprevObs != 'NA' ~ H2Ochemdf$PSUprevObs,
  TRUE ~ as.character(H2Ochemdf$assumed_PSU),
))


# Simple Time series plot for final salintiy 
salinityplot <- ggplot(H2Ochemdf, aes(x=ObservationDate, y=Final_PSU))+
  geom_point() + 
  ylim (27.00, 33.00) +
  ggtitle("Final Salinity Values, simple timeseries")
salinityplot
# 


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 9.) Creating Percent Dissoved Oxy Value   
#*********************************

## Review of the wql library and the functions in that package
# Review the Oxygen Solubility 
#
#library with function that does DO saturation calculation
#library(wql)
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
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 9.1) Creating Percent Dissoved Oxy Value - creating variables  
#*********************************

#assumption with the 28.8 standard salinity reading
H2Ochemdf$percentDOassumpt <- ""
H2Ochemdf$percentDOassumpt <- as.numeric(H2Ochemdf$percentDOassumpt)

H2Ochemdf$assumedSatDOmg <- ""
H2Ochemdf$assumedSatDOmg <- as.numeric(H2Ochemdf$assumedSatDOmg)

# the percent DO
H2Ochemdf$percentDO <- "" 
H2Ochemdf$percentDO <- as.numeric(H2Ochemdf$percentDO)


# Observed / Measured Salinity readings informed answers
H2Ochemdf$obseveredSatDOmg <- "" 
H2Ochemdf$obseveredSatDOmg <- as.numeric(H2Ochemdf$obseveredSatDOmg)


H2Ochemdf$actualDOmg <- ""
H2Ochemdf$actualDOmg <- as.numeric(H2Ochemdf$actualDOmg)

# Review of Values
H2Ochemdf$percentDOassumpt
H2Ochemdf$assumedSatDOmg
H2Ochemdf$percentDO
H2Ochemdf$obseveredSatDOmg
H2Ochemdf$actualDOmg


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 9.2) Creating Percent Dissoved Oxy Value - assumed DO saturation variable creation
#*********************************


# saturated mg/L DO at obseved temperature and assumed salinity
# the oxySol() function is form the wql package
H2Ochemdf$assumedSatDOmg <- oxySol(H2Ochemdf$sTemperature, 
                                   H2Ochemdf$assumed_PSU)


SatDOplot <- ggplot(H2Ochemdf, aes(x=ObservationDate, 
                                   y=assumedSatDOmg))+
  geom_point() +
  ggtitle("Assumed Saturation of Dissolved Oxygen Across all MOATS")

SatDOplot


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 9.3) Creating Percent Dissoved Oxy Value - Percent DO (with assumptions)
#*********************************
# Back calculated fraction DO as reported by the oxygen sensor

H2Ochemdf$percentDOassumpt <- ""
H2Ochemdf$percentDOassumpt <- as.numeric(H2Ochemdf$percentDOassumpt)
H2Ochemdf$percentDOassumpt <- H2Ochemdf$DO / H2Ochemdf$assumedSatDOmg


p5 <- ggplot(H2Ochemdf, aes(x=dateTime, y=percentDOassumpt)) +
  geom_point(aes(colour=period, point=))   +
  ggtitle("Assumed Percent DO (All Treatments & All MOATs) Colored by Period")

p5





#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



#*********************************
## 9.5) Creating Percent Dissoved Oxy Value - Observed Saturated DOmg (measured salinity)
#*********************************

# #satured mg/L at observed temperature and observed (not assumed) salinity
H2Ochemdf$sTemperature <- as.numeric(H2Ochemdf$sTemperature)

H2Ochemdf$obseveredSatDOmg <- oxySol(H2Ochemdf$sTemperature, H2Ochemdf$Final_PSU)





#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 9.6) Creating Percent Dissoved Oxy Value - Observed/Actual DO (in mg) Saturated DOmg (measured salinity)
#*********************************
# actual DO mg at observed temperature and salinity
H2Ochemdf$percentDO <- H2Ochemdf$DO / H2Ochemdf$assumedSatDOmg

H2Ochemdf$actualDOmg <- H2Ochemdf$percentDO * H2Ochemdf$obseveredSatDOmg


actualDOmgPLOT <- ggplot(H2Ochemdf, aes(x=dateTime, y=H2Ochemdf$actualDOmg)) +
  geom_point(aes(colour=period, point=))   +
  ggtitle("Actual DO (mg/L) (All Treatments & All MOATs) Colored by Period")

actualDOmgPLOT


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#### CONSTRUCTION ZONE - FUNCTION no FUNCTION####
# A Function to rule them all 
# Assumed DO values to corrected DO values with measured Salinity

# bombadil_DO <- function(H2Ochemdf$DO,
#                         H2Ochemdf$assumedSatDOmg
#                         H2Ochemdf$percentDOassumpt,
#                         H2Ochemdf$sTemperature
#                         H2Ochemdf$percentDO, 
#                         H2Ochemdf$obseveredSatDOmg,
#                         H2Ochemdf$Final_PSU) {
#               bombadil_DOmgL <- H2Ochemdf$assumedSatDOmg <- oxySol(H2Ochemdf$sTemperature, 
#                                                                    H2Ochemdf$assumed_PSU)
#               return(H2Ochemdf$actualDOmg)
# }

# ReasonableMOATdata <- function(#data 1st arg I want it to set a dataframe
#                                 moatsfilter, 
#                                Tempfilter, 
#                                Treatmentfilter, 
#                                periodfitler){
#                       moatsfilter<- #data #reference that dataframe
#                         filter(!moats %in% c("M03", "M04", "M05", "M11")),
#                       
#                       Tempfilter <- filter(aTemperature>= 5 & aTemperature<=30),
#                       Treatmentfilter <- filter(treatment %in% c("current", "allchange", "hightemperature")), 
#                       periodfitler <- filter(period != "other")
#                       return(#updated dataframe)
# }
# 
# #take two
# 
# # think about default values for items of the arglist




#*********************************
## 10.0) Filtering Data
#*********************************


H2Ochemdf <- H2Ochemdf  %>% filter(!moats %in% c("M03", "M04", "M05", "M11")) %>%
  filter(sTemperature>= 5 & sTemperature<=30) %>%
  filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
  filter(period != "other")


# 
# ReasonableMOATdata <- function(data,
#                                 moatsfilter, 
#                                 mintemp,  
#                                 maxtemp, 
#                                 Treatmentfilter, 
#                                 periodfitler) {
#                                 data_moatsfilter<- data %>% filter(!moats %in% moatsfilter)
#                                 data_Tempfilter <- data_moatsfilter %>% filter(aTemperature>= mintemp & aTemperature<=maxtemp)
#                                 data_Treatmentfilter <- data_Tempfilter %>% filter(treatment %in% Treatmentfilter)
#                                 data_periodfitler <- data_Treatmentfilter %>% filter(period != "other")
#                                 return(data_periodfitler)
# }
#   
# 
# ReasonableMOATdata(data=H2Ochemdf, 
#                    moatsfilter=c("M03", "M11"), 
#                           mintemp=5,  maxtemp=30, 
#                                 Treatmentfilter=c("current", "allchange", "hightemperature"), 
#                                     periodfitler = c("day", "night")) 

#*********************************
## 10.1) Filtering Data- dropping lvls to clean graphs
#*********************************


filteredFrame = filter(H2Ochemdf,
                       !moats %in% c('M03', "M04", "M05", "M11") & 
                         (aTemperature>= 5 & aTemperature<=30) &
                         treatment %in% c("current", "allchange", "hightemperature") &
                         period != "other")

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 10.1a Dropping levels and factors
filteredFrame$moats <- droplevels(filteredFrame$moats)
filteredFrame$treatment <- factor(filteredFrame$treatment)



#*********************************
## 11) DO averages- days and nights
#*********************************

# Creating Values
# Day

allchgDayDO <- subset(H2Ochemdf, 
                      period == "day" & treatment == "allchange",
                      select = c(dateTime, H2Ochemdf$actualDOmg ))


curDayDO <- subset(H2Ochemdf, 
                   period == "day" & treatment == "current",
                   select = c(dateTime, H2Ochemdf$actualDOmg ))


hitempDayDO <- subset(H2Ochemdf, 
                      period == "day" & treatment == "hightemperature",
                      select = c(dateTime, H2Ochemdf$actualDOmg ))


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

# Night

allchgNightDO <- subset(H2Ochemdf, 
                        period == "night" & treatment == "allchange",
                        select = c(dateTime, H2Ochemdf$actualDOmg ))


curNightDO <- subset(H2Ochemdf, 
                     period == "night" & treatment == "current",
                     select = c(dateTime, H2Ochemdf$actualDOmg ))


hitempNightDO <- subset(H2Ochemdf, 
                        period == "night" & treatment == "hightemperature",
                        select = c(dateTime, H2Ochemdf$actualDOmg ))



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


# Review of values
allchgDayDO
allchgNightDO
curDayDO
curNightDO
hitempDayDO
hitempNightDO


#*********************************
## 12) DO Day/Night Summary
#*********************************


H2Ochemdf.daynight.summary <- H2Ochemdf %>% group_by(treatment, period) %>%
  summarize(sd = sd(H2Ochemdf$actualDOmg, na.rm = TRUE), 
            mean = mean(H2Ochemdf$actualDOmg, na.rm = TRUE), 
            median = median(H2Ochemdf$actualDOmg, na.rm = TRUE),
            IQR = IQR(H2Ochemdf$actualDOmg, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ci = se*1.96)
















#**************E*N*D*************# 
#*********************************
## END OF SCRIPT | END OF DOCUMENT 
#*********************************


# ___________________8888,
# ____________________Y8888b,
# ___________________,oA8888888b,
# _____________,aaad8888888888888888bo,
# __________,d888888888888888888888888888b,
# ________,888888888888888888888888888888888b,
# _______d8888888888888888888888888888888888888,
# ______d888888888888888888888888888888888888888b
# _____d888888P'                    `Y888888888888,
# _____88888P'                    Ybaaaa8888888888l
# ___a8888'                      `Y8888P' `V888888
# __d8888888a                                `Y8888
# AY/'' `\Y8b                                 ``Y8b
# Y'      `YP                                    ~~

