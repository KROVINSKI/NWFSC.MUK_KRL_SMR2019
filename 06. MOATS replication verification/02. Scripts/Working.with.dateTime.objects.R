##Hello World



## Author: OA Lab, NWFSC, Kate Rovinski
## Title: Bits of Code / Ways of making and altering dateTime objects
## Date: April 2020


#*********************************
##Libraries - 
#********************************* 
#you might be able to get away with less

library(tidyverse)
library(stringr)
library(readxl)
library(readr)
library(tidyr)
#You will need lubridate
library(lubridate)


#*********************************
## Example 1-  Creating dateTime objects from the Aquarium Temperature Investigation
#*********************************

# 4.0 establishing the date time object of the CSV |
dml$dateTime <- as.POSIXct(dml$dateTime, format="%Y-%m-%d %H:%M:%OS")
ReferenceTime <- as.POSIXct("2019-09-20 23:59:00")
class(ReferenceTime)

#*********************************
## Example 2- Creating Night and Day Periods breaking apart the dateTime object
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



#*********************************
## Example 3- Paul's guidance on creating Night and Day Periods breaking apart the dateTime object
#*********************************

#Now make a new column called dayNight and assign a category based on time in time1809
#this uses tidy functions. Mutate() makes a new column and case_when() assigns the categories based on conditions
# I had not used case_when() before - it is a bit cleaner than how I have done it the past
# old way is dml$dayNight[dml$time1809 >= dayStart & dml$time1809 < dayStop] <- "Day", etc.
#either way works
#have to split night into two different conditional statements at midnight 

dml <- dml %>% mutate(dayNight = case_when(time1809 >= dayStart & time1809 < dayStop ~ "Day",
                                           time1809 >= dayStop & time1809 < nightStart ~ "TransDayToNight",
                                           time1809 >= nightStart & time1809 <= as.POSIXct("1809-02-12 23:59:59") ~ "Night",
                                           time1809 >= as.POSIXct("1809-02-12 00:00:00") & time1809 <= nightStop ~ "Night",
                                           time1809 >= nightStop & time1809 < dayStart ~ "TransNightToDay"))




#*********************************
## End of Script | End of Document
#*********************************