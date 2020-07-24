##Hello World

#*********************************
## Version Check
#********************************* 
R.version


## Author: OA Lab, NWFSC
## Title: Aquarium Temperature Investigation: Temperature SubSelectScript.R 
## Date: April 2020

# R script below will subselect and plot temperature data for MOATs
# Overall goal is to determine if MOATs (per treatment) are true replicates
# Steps 1 thru 9+



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
library (sparklyr)
library(xts)
library(TSstudio)
library(lubridate)
library(violinmplot)
library(vioplot)
library(yarrr)
library(datapasta)
library(reprex)
library(miniUI)


#*********************************
## Outline Current 2020.05.08
#*********************************

# 4.) Creating dateTime objects 
# 5.) Creating Treatment Variables
# 6.) Creating Night and Day Periods


#*********************************
## 1.) Set working directory
#*********************************

# Inside the working directory are the CSV files generated after using the moats graph app
# https://github.com/pmcelhany/moatsGraphs.git
# Moving average window (nObs) set to 4
# this results in the observations being averaged into a 24second timeframe
# CSVs were created for each moats: M01, M02, M03, M04, M05, M06, M07, M08, M09, M10, M11, M12, M13

# files are also available on the OA Google drive:
# https://drive.google.com/open?id=13a2hk1a9I9gRIgf2Xyl1dSYDf5vnyYvs



setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_MOATs_SMR2019/LabViewLogs.AllMOATS")


# * * * * * * * * * * * * * * * *
## 3.2 Checking variables 
# * * * * * * * * * * * * * * * *

#starting dimensions for QA check
# > dim(dml)
# [1] 60615     7


dml <- read.csv(file = "M01thruM13moatslog_n17.csv")

dim(dml)

## Looking to ensure the different variables are treated as the correct variable type
## Checking the names in the dataframe
names(dml)
## Checking variable type/class 
class(dml$moats)
factor(moats)

## 3.3 Changing variables | 
## Changing MOATs to Factors for the 13 different MOATs- these will be the discrete units for follow analysis
dml$moats <- factor(dml$moats)
# Checking the names of the different levels
levels(dml$moats)
##checking the dataset, dimensions
dim(dml)



#*********************************
## 4.) Creating dateTime objects  
#*********************************


# 4.0 establish the date time object of the CSV |
dml$dateTime <- as.POSIXct(dml$dateTime, format="%Y-%m-%d %H:%M:%OS")
ReferenceTime <- as.POSIXct("2019-09-20 23:59:00")
class(ReferenceTime)

# QA check
dim(dml)


#*********************************
## 5.) Creating Treatment Variables  
#*********************************

## 5.1 Identifying moats per treatment |
## creating a new dataframe
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




#*********************************
## 6.) Creating Night and Day Periods  
#*********************************
## 6.1 Narrative (Overall)
# Creating a day and night variables 
# Day and night periods will only refer to time under treatment as a way to exclude the acclimation period.
# day and night changed at about ~ 1230 on 05OCT19 
# Treatment start date considered to begin Monday 23SEP19 at 1200pm
# Krill Night Starts 1200 (~1230*) and ends 2100
# Krill Days Starts 2101 and ends 1159 (~1229*) 
# Interval 1 start 1200 23SEP19, end 1229 05OCT19
# Interval 2 start 1230 05OCT19, end 2100 30OCT19
# graphic for the loop saved at /Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/Day_Night Period Loop.pdf/

# Create new split date and time columns
dml$ObservationDate <- as.Date(dml$dateTime)
dml$ObservationTime <- format(as.POSIXct(dml$dateTime) ,format = "%H:%M:%S")




## 6.2 Narrative (Intervals) 
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

##6.3 Applying the tidyverse 
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

dml

write.csv(dml, file = "M01thruM13_n17_dml.csv", row.names = FALSE)


#**********E*N*D*****************# 
#*********************************
## End of Document End of Script  
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
# `'
