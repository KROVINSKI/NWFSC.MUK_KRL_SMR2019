##Hello World

## Author: OA Lab, NWFSC
## Title: Aquarium Temperature Investigation - Steps 1 through 
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


#*********************************
## 1.) Set working directory
#*********************************

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_MOATs_SMR2019/LabViewLogs.AllMOATS")


#*********************************
## 2.) Spolling Data into one CSV 
#*********************************
#Combining multiple CSV files into 1 document. Original input files from individual LVM (logical volumne management) files off each MOATs.
#LVM files were then inputed to the Moats Graphy (Shiny) app
#Output from app are the CSV files manipulated below. 
#Critical user inputs at the U/I on app is the observation window- will default to 4 and average those four windows. LVM files were generated at a rate of 1:6secs. The default of 4 observation windows will generate observations 24seconds apart.

## 2.1 Create a list of files |
# All files to be joined have ext. "csv" can use that pattern to join 
files <- list.files(pattern = ".csv")
print(files)

## 2.2 Create a temporary place for files |
temp <- lapply(files, fread, sep= ",")
print(temp)

## 2.3 Create a new vector for Moats data logs |
# "M01thruM13Moatslog_data" via rbind
M01thruM13moatslog_data <- rbindlist(temp)
print(M01thruM13moatslog_data)

## 2.4 Write the new csv document | 
# "M01thruM13moatslog"
write.csv(M01thruM13moatslog_data, file = "M01thruM13moatslog.csv", row.names = FALSE)

#*********************************
## 3.) Creating the Dataframe "dml" 
#*********************************

## 3.1 Reading the CSV |  
## ensuring column names and types
## Data Moats Log = dml
dml <- read.csv(file = "M01thruM13moatslog.csv", stringsAsFactors = FALSE)
dim(dml)
#current results should read 
#[1] 14792440        7


# * * * * * * * * * * * * * * * *
## 3.1a Sub sampling dataframe "dml"  
# * * * * * * * * * * * * * * * *
## creating a sub sample of the data moats log dml dataframe to allow for quick graphs 
#subsample every 17th row (because prime numbers are indeed cool)
dml <- dml %>% arrange(moats, dateTime) %>% filter(row_number() %% 17 == 0)

write.csv(dml, file = "M01thruM13moatslog_n17.csv", row.names = FALSE)



dim(dml)
# after subsampling results should read
# [1] 870143      7

## 3.2 Checking variables | 
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
# [1] 870143      7

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
#[1] 1673352       8
# increase due to "treatment" addition




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

# creating a new column, new variable "period"
dml$period <- ""

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
# QA check
dim(dml)
# before
# #[1] 1673352       8  
# after
# [1] 1673352      11

# Three new variables ObservationTime, ObservationDate, and period
# all 11 variables contained inside this new CSV document
write.csv(dml, file = "allmoats_observations.csv", row.names = FALSE)




#*********************************
## 7.) Cleaning up dml  - Cdml
#*********************************



# Creating a new dataframe cleaned with the various filters below
# checking on the variables inside dml

## Cleaned-Up dml = Cdml

names(dml)

# Changes to be made the dataframe by variable
#[1] "moats" "M03", "M04", "M05", "M09", "M11" to be filtered out- all these MOATs were dropped from the study
#[2] "dateTime" - no changes
#[3] "aTemperature" - no changes (till I determine the neighborhood check)
#[4] "sTemperature" - no changes    
#[5] "pH" - no changes
#[6] "DO"- no changes
#[7] "salinity" - no changes 
#[8] "treatment" - dropping the listed MOATs will eliminate the "broken_and_ambientbroken" treatment
#[9] "period" - filtering out "other"
#[10] "ObservationDate" - no changes
#[11] "ObservationTime" - no changes, note that each observation could be spaced 6.8minutes apart


Cdml <- dml %>% filter(!moats %in% c("M03", "M04", "M05", "M11")) %>%
  filter(aTemperature>= 9 & aTemperature<=17) %>%
  filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
  filter(period != "other")

write.csv(dml, file = "Cdml.csv", row.names = FALSE)



#*********************************
## 8.) Determining outliers
#*********************************



#ggplot(subset(Cdml[Cdml$treatment == "allchange", ], period %in% ("night")), 
#       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=))

ggplot(subset(Cdml[Cdml$treatment == "current", ], period %in% ("night")), 
       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=))

#ggplot(subset(Cdml[Cdml$treatment == "hightemperature", ], period %in% ("night")), 
#       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=))


#Moats 12 "M12" appears to have suspected disconnected time periods with the server



#*********************************
## 9.) Creating flag logic
#*********************************



# Writing flag logic to determine if an observation is valid or not
# creating a new object to be a validation flag
# will create a numerical value that will represent those observations to be filtered out

#Cdml$ValidationFlag <- ""

#Cdml$ValidationFlag<- abs(diff(Cdml$aTemperature, lag = 1))

#diff(Cdml$aTemperature, lag = 3)
#diff(Cdml$aTemperature- lag(Cdml$aTemperature))


#ValidationFlag used to filter





#*********************************
## X.) Boxplots with Cdml prep- cleaning up MOATs characters that would display
#*********************************

#want to hide the names of MOATs "cleaned out" of Cdml 
#Can't use filter to truely hide/remove the elements you want gone
#filterFrame will truely removed- use & and include the drop levels portion  

filteredFrame = filter(Cdml,
                       !moats %in% c('M03', "M04", "M05", "M11") & 
                         (aTemperature>= 9 & aTemperature<=17) &
                         treatment %in% c("current", "allchange", "hightemperature") &
                         period != "other")

#this is from when I just used filter
#filter(aTemperature>= 9 & aTemperature<=17) %>%
#filter(ValidationFlag<= .250) %>%
#filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
#filter(period != "other")
# Error: Result must have length 1105183, not 456364

filteredFrame$moats <- droplevels(filteredFrame$moats)


write.csv(filteredFrame, file = "Cdml_filteredFrame.csv", row.names = FALSE)
