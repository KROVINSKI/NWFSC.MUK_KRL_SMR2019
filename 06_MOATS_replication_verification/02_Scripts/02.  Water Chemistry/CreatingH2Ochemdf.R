##Hello World

#*********************************
## Version Check
#********************************* 
R.version  

## Author: OA Lab, NWFSC
## Title: Aquarium Water Chemistry Investigation: Creating the Dataframe "CreatingH2Ochemdf"
## Date: May- June 2020

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

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_MOATs_SMR2019/WaterChemistryData/01a. WaterChemistry Data")


#*********************************
## 2. Calling & Reading in " dml " 
#*********************************

#dml <- read.csv(file = "M01thruM13moatslog_n17.csv", stringsAsFactors = FALSE)
#dim(dml)

# Super Speedy subsampled data
dml <- read.csv(file = "M01thruM13moatslog_n333.csv", stringsAsFactors = FALSE)
dim(dml)

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

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
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
## Day End Time         "12:01:00"
## Night Start Time     "12:00:00"
## Night End Time       "21:00:00"
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
  & (ObservationTime >= "12:30:00") 
  & (ObservationTime <="21:00:00") ~"night",
  
  (ObservationDate >= "2019-09-23") 
  & (ObservationDate <="2019-10-05")
  & ((ObservationTime >= "21:01:00") 
     | (ObservationTime <="11:59:00")) ~"day",
  
  (ObservationDate >= "2019-10-05")
  & (ObservationDate <= "2019-10-30")
  & ((ObservationTime >= "21:01:00")
     | (ObservationTime <= "12:29:00")) ~"day",
  TRUE ~"other"
)

) 

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
dml
# QA check
dim(dml)


# Removing "other" period from day and night 
# not including acclimation period in this investigation

H2Ochemdf <- dml %>% filter(period != "other")



#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


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
# creating 4 new variables in dml

# create a vector 181845  for the large n17 file
# create a vector 9283 observations long for the sub-sub sample n333 file
# logical vectors- boolean answers to those three below conditions

# 1 - the measurement per moats per day - MOATs and Data are both available
# 2 - the measurement (averaged across moats) - group by / summarize tools
# 3- (data gapped situation- no value matches) take the previous daily average based on observation  - lag? 
# previous line on a dataframe ... dplyr tool ... 

# https://dplyr.tidyverse.org/reference/lead-lag.html
# 4 - "the winning value" case_when #corrected conductivity value



#*********************************
## 8.1 Plots (timeseries verification)
#*********************************
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

# Simple Time series plot 

p <- ggplot(H2Ochemdf, aes(x=ObservationDate, y=salinity))+
            geom_line() + 
            ylim (30.05, 27.00) 

p

p1 <- ggplot(subset(H2Ochemdf[H2Ochemdf$period == "night", ], 
       aes(x=ObservationDate, y=Salinity))) + 
  geom_line(aes(colour=moats, point=)) +
  ggtitle("Salinity Values all Treatments, All MOATs, During Night Period")

p1

ggplot(subset(H2Ochemdf, 
              period %in% ("night")), 
       aes(x=ObservationDate, y=salinity)) + 
  geom_point(aes(colour=moats, point=)) +
  ggtitle("Salinity Values all Treatments, All MOATs, During Night Period")


ggplot(subset(H2Ochemdf[H2Ochemdf$moats == "M01", ], 
              period %in% ("day")), 
       aes(x=dateTime, y=salinity)) + 
  geom_point(aes(colour=moats, point=)) +
  ggtitle("Salinity Values, MOATs 01, During Night Period")




#*********************************
## 8.2 Practical Salinity Units - Salinity Determination (conditional statements)
#*********************************
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

# Manual Readings = Many Readings

# Per MOAT measurement
H2Ochemdf$PSUperMOATs <- all_salinity$salinity[match
                                    (paste
                                      (H2Ochemdf$ObservationDate,
                                          H2Ochemdf$moats), 
                                     paste
                                      (all_salinity$PSU_ObsDate, 
                                          all_salinity$moats))]

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


# All MOATs daily
H2Ochemdf$PSUavgDaily <- all_salinity$salinity[match
                                    (paste
                                      (H2Ochemdf$ObservationDate,'All'), 
                                     paste
                                      (all_salinity$PSU_ObsDate, 
                                        all_salinity$moats), 
                                          nomatch = NA_character_)]

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


# Previous All MOATs daily
H2Ochemdf$PSUprevObs <- na.locf(H2Ochemdf$PSUavgDaily, na.rm = FALSE)


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

# Final conductivity
H2Ochemdf$Final_PSU <- ""

H2Ochemdf$Final_PSU <- 28.8

# 
# 
# H2Ochemdf$Conductivity <- H2Ochemdf %>% mutate
#                               (H2Ochemdf$Conductivity=case_when(
#                                 # conditional statements - what's the best function to make value determinations? 
#                               ))


# could set to 28.8 and then use the replace function, writing replacements three times

write.csv(H2Ochemdf, file = "WaterChemistryDataframe.csv", row.names = FALSE)


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |



# Final Method used, winning method - maybe we'll delete let's not argue in front of flipper
# H2Ochemdf$CONDmsrMethod <- ""






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

