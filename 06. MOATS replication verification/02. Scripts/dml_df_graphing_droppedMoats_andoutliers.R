##Hello World

## Author: OA Lab, NWFSC
## Title: Aquarium Temperature Investigation
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

#Saved on the OA Google Drive
# https://drive.google.com/open?id=15iBXct9b4EjKDq75vKnm5NobowBwK3G- 



# 2.5 In-situ data
 
# read_csv()
# 
# d.insitu <- read.csv(file = "Krill2019_insitu_sample_Day.csv", stringsAsFactors = FALSE)
# n.insitu <- read.csv(file = "Krill2019_insitu_sample_Night.csv", stringsAsFactors = FALSE)


#*********************************
## 3.) Creating the Dataframe "dml" 
#*********************************

## 3.1 Reading the CSV |  
## ensuring column names and types
## Data Moats Log = dml
dml <- read.csv(file = "M01thruM13moatslog.csv", stringsAsFactors = FALSE)
dim(dml)


# * * * * * * * * * * * * * * * *
## 3.1a Sub sampling dataframe "dml"  
# * * * * * * * * * * * * * * * *
## creating a sub sample of the data moats log dml dataframe to allow for quick graphs 
#subsample every 17th row (because prime numbers are indeed cool)
dml <- dml %>% arrange(moats, dateTime) %>% filter(row_number() %% 17 == 0)

write.csv(dml, file = "M01thruM13moatslog_n17.csv", row.names = FALSE)



dim(dml)


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


#*********************************
## 5.) Creating Treatment Variables  
#*********************************

## 5.1 Identifying moats per treatment |
## creating a new dataframe
## establishing treatments
dml$treatment <- ""
dml$treatment[dml$moats == "M03"| dml$moats == "M07" | dml$moats== "M10" | dml$moats== "M12"] <- "current"
dml$treatment[dml$moats == "M01"| dml$moats== "M06"| dml$moats== "M11"] <- "hightemperature"
dml$treatment[dml$moats == "M02"| dml$moats== "M08" | dml$moats== "M13"] <- "allchange"
dml$treatment[dml$moats == "M04"] <- "ambient_M04"
dml$treatment[dml$moats == "M05"] <- "ambient_M05"
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

# Three new variables ObservationTime, ObservationDate, and period
# all 11 variables contained inside this new CSV document
write.csv(dml, file = "allmoats_observations.csv", row.names = FALSE)




#*********************************
## 7.) Scrubbing up dml  
#*********************************



# Creating a new dataframe cleaned with the various filters below
# checking on the variables inside dml

## scrubbing dml = scrubdml

names(dml)

# Changes to be made the dataframe by variable
#[1] "moats" "M03", "M04", "M05", "M09", "M11" all MOATs included
#[2] "dateTime" - no changes
#[3] "aTemperature" - removing negative numbers, 30C the upper limit
#[4] "sTemperature" - no changes    
#[5] "pH" - no changes
#[6] "DO"- no changes
#[7] "salinity" - no changes 
#[8] "treatment" - dropping the listed MOATs will eliminate the "broken_and_ambientbroken" treatment
#[9] "period" - filtering out "other" to remove acclimation period
#[10] "ObservationDate" - no changes
#[11] "ObservationTime" - no changes, note that each observation could be spaced 6.8minutes apart


scrubdml <- dml %>% 
  filter(aTemperature>= 1 & aTemperature<=30) %>%
  filter(treatment %in% c("current", 
                          "allchange", 
                          "hightemperature", 
                          "ambient_M04", 
                          "ambient_M05")) %>%
  filter(period != "other")

write.csv(dml, file = "scrubdml.csv", row.names = FALSE)


#*********************************
## 8.) Framing the filters - scrubdml
#*********************************



# #want to hide the names of the "other period" "scrubbed out" of dml 
# #filterFrame will truely removed- use & and include the drop levels portion  
# 
# filteredFrame = filter(scrubdml, (period != "other"))
# 
# #this is from when I just used filter
# #filter(aTemperature>= 9 & aTemperature<=17) %>%
# #filter(ValidationFlag<= .250) %>%
# #filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
# #filter(period != "other")
# # Error: Result must have length 1105183, not 456364
# 
# filteredFrame$period <- droplevels(filteredFrame$period)
# 
# 
# write.csv(filteredFrame, file = "Cdml_filteredFrame.csv", row.names = FALSE)
# 
# 
# #this make a new column with the different between 2 adjacent times
# #first sort the data so you are comparing adjacent times
# #then add new column
# # the diff() function computes x[i+lag] - x[i], the default value is lag = 1
# 
# #the result of the diff() function has a length that is the length of the original vector - lag
# #therefore need to fill the new variable, deltaTempLag1,
# 
# # with c(0,abs(diff(aTemperature))) it is right length and first value (with no valid diff) is zero
# 


#*********************************
## 10.) Temperature Jump flag logic
#*********************************



# Writing flag logic to determine if an observation is valid or not
# creating a new object to be a validation flag under name deltaTempLag1

# will create a numerical value that will represent those observations to be filtered out


scrubdml <- scrubdml %>% arrange(moats, dateTime)%>% 
  mutate(deltaTempLag1 = c(0,abs(diff(aTemperature))))

dim(scrubdml)
# 12th variable is created

#the diff value comparing the last time in M01 to the first time in M02 (etc.) is not valid, 
#so set those to zero
scrubdml$deltaTempLag1[lag(scrubdml$moats) != scrubdml$moats] <- 0
#this shows all the rows that jump more that 1 degree from previous row (ingoring rows that are transition from one moats to another)
scrubdml %>% filter(deltaTempLag1 >1)


#Creating another column of variables to be able to graph the temperature jumps 
scrubdml <- scrubdml %>% mutate(tDeltaThreshold = if_else(deltaTempLag1 > 0.5, TRUE, FALSE))

dim(scrubdml)
#Expected Results
#[1] 456365     13

write.csv(filteredFrame, file = "scrubdml_FlaggedforTempjumps", row.names = FALSE)




#*********************************
## 11.) Calculating averages by treatment & day/night (scrubdml)
#*********************************

# These averages will be the yintercepts in plots
# example of what to put into with ggplot " + geom_hline(yintercept = dtemperatur$`mean(aTemperature)`)   " 


# Night Period
scrubsubsetNightaTemp <- subset(scrubdml, period == "night" & aTemperature >0,
                           select = c( moats, dateTime, treatment, aTemperature ))

# Day Period
scrubsubsetDayaTemp <- subset(scrubdml, period == "day" & aTemperature >0,
                         select = c( moats, dateTime, treatment, aTemperature ))



#Current Treatment Averages--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--
# Current Night
scrubcurNightaTemp <- subset(scrubdml, period == "night" & treatment == "current",
                        select = c(dateTime, aTemperature ))
scrubavg_curNightaTemp <- mean(scrubcurNightaTemp$aTemperature)
print(scrubavg_curNightaTemp)
# print(scrubavg_curNightaTemp)
# [1] 12.00645 previous average
# [1] 11.95122 new average


# Current Day
scrubcurDayaTemp <- subset(scrubdml, period == "day" & treatment == "current",
                             select = c(dateTime, aTemperature ))
scrubavg_curDayaTemp <- mean(scrubcurDayaTemp$aTemperature)
print(scrubavg_curNightaTemp)
# print(scrubavg_curNightaTemp)
# [1] 11.09983 previous average
# [1] 12.00645 new average


#All Change Treatment Averages --All_Chg--All_Chg--All_Chg--All_Chg--All_Chg--All_Chg--All_Chg--
# All Change Night

scruballchgNightaTemp <- subset(scrubdml, period == "night" & treatment == "allchange",
                             select = c(dateTime, aTemperature ))
scrubavg_allchgNightaTemp <- mean(scruballchgNightaTemp$aTemperature)
print(scrubavg_allchgNightaTemp)


# All Change Day

scruballchgDayaTemp <- subset(scrubdml, period == "day" & treatment == "allchange",
                                select = c(dateTime, aTemperature ))
scrubavg_allchgDayaTemp <- mean(scruballchgDayaTemp$aTemperature)
print(scrubavg_allchgDayaTemp)



#High Temperature Treatment Averages -- HiTemp -- -- HiTemp -- HiTemp -- HiTemp -- HiTemp -- HiTemp 
# High Temperature Night
scrubhitempNightaTemp <- subset(scrubdml, period == "night" & treatment == "hightemperature",
                           select = c(dateTime, aTemperature ))
scrubavg_hitempNightaTemp <- mean(scrubhitempNightaTemp$aTemperature)
print(scrubavg_hitempNightaTemp)



# High Temperature Day
scrubhitempDayaTemp <- subset(scrubdml, period == "day" & treatment == "hightemperature",
                         select = c(dateTime, aTemperature ))
scrubavg_hitempDayaTemp <- mean(scrubhitempDayaTemp$aTemperature)
print(scrubavg_hitempDayaTemp)



# Ambient M04 Treatment Averages -- ambM04 -- ambM04 -- ambM04 -- ambM04 -- ambM04 -- ambM04
# Ambient M04 Night
scrubamb04NightaTemp <- subset(scrubdml, period == "night" & treatment == "ambient_M04",
                                select = c(dateTime, aTemperature ))
scrubavg_amb04NightaTemp <- mean(scrubamb04NightaTemp$aTemperature)
print(scrubavg_amb04NightaTemp)


# Ambient M04 Day
scrubamb04DayaTemp <- subset(scrubdml, period == "day" & treatment == "ambient_M04",
                               select = c(dateTime, aTemperature ))
scrubavg_amb04DayaTemp <- mean(scrubamb04DayaTemp$aTemperature)
print(scrubavg_amb04DayaTemp)



# Ambient M05 Treatment Averages -- ambM05 -- ambM05 -- ambM05 -- ambM05 -- ambM05 -- ambM05
# Ambient M05 Night
scrubamb05NightaTemp <- subset(scrubdml, period == "night" & treatment == "ambient_M05",
                               select = c(dateTime, aTemperature ))
scrubavg_amb05NightaTemp <- mean(scrubamb05NightaTemp$aTemperature)
print(scrubavg_amb05NightaTemp)


# Ambient M05 Day
scrubamb05DayaTemp <- subset(scrubdml, period == "day" & treatment == "ambient_M05",
                             select = c(dateTime, aTemperature ))
scrubavg_amb05DayaTemp <- mean(scrubamb05DayaTemp$aTemperature)
print(scrubavg_amb05DayaTemp)




# Review of 6 new variables - 6 New Variables - - 6 New Variables - 6 New Variables - 6 New Variables 

scrubavg_allchgDayaTemp
scrubavg_allchgNightaTemp
scrubavg_curDayaTemp
scrubavg_curNightaTemp
scrubavg_hitempDayaTemp
scrubavg_hitempNightaTemp

scrubavg_amb04NightaTemp
scrubavg_amb04DayaTemp

scrubavg_amb05NightaTemp
scrubavg_amb05DayaTemp

#Table of Averages -- Table of Averages -- Table of Averages -- Table of Averages -- Table of Averages --


meanscrubdmlatemp <- scrubdml %>% group_by(treatment, period) %>% summarise(mean(aTemperature))
print(meanscrubdmlatemp)
# # Groups:   treatment [5]
# treatment           period          `mean(aTemperature)`
# <chr>               <chr>           <dbl>
# 1 allchange         day             13.0
# 2 allchange         night           13.9
# 3 ambient_M04       day             11.9
# 4 ambient_M04       night           12.4
# 5 ambient_M05       day             12.3
# 6 ambient_M05       night           11.9
# 7 current           day             11.2
# 8 current           night           12.0
# 9 hightemperature   day             12.9
# 10 hightemperature  night           13.9


#*********************************
## 12.) Summary & Group by scrubdml
#*********************************


#  * * * * * * * * * * * * * * * * 
# Group By Treatment
#  * * * * * * * * * * * * * * * * 

scrubdml.summary <- scrubdml %>% group_by(treatment) %>% 
  summarize(sd = sd(aTemperature, na.rm = TRUE), mean(aTemperature))
scrubdml.summary


#  * * * * * * * * * * * * * * * * 
# scrubdml Summaries, group by, mutate
#  * * * * * * * * * * * * * * * * 
scrubdml.daynight.summary <- scrubdml %>% group_by(treatment, period) %>%
  summarize(sd = sd(aTemperature, na.rm = TRUE), 
            mean = mean(aTemperature, na.rm = TRUE), 
            median = median(aTemperature, na.rm = TRUE),
            IQR = IQR(aTemperature, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ci = se*1.96)


write.csv(scrubdml.daynight.summary, "scrub_daynight_summary.csv")


scrubdml.day.summary <- scrubdml %>% group_by (treatment) %>%
  filter(period == "day") %>%
  summarize(sd = sd(aTemperature, na.rm = TRUE), 
            mean = mean(aTemperature, na.rm = TRUE), 
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ci = se*1.96)


write.csv(scrubdml.day.summary, file = "scrubdml.day.summary")



scrubdml.night.summary <- scrubdml %>% group_by (treatment) %>%
  filter(period == "night") %>%
  summarize(sd = sd(aTemperature, na.rm = TRUE), 
            mean = mean(aTemperature, na.rm = TRUE), 
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ci = se*1.96)


write.csv(scrubdml.night.summary, file = "scrubdml.night.summary")


#*********************************
## XX.) Plots
#*********************************


n.insitu_aTemp <- n.insitu %>% filter (TempSensor %in% c("orionCOND", "fluke"))
d.insitu_aTemp <- d.insitu %>% filter (TempSensor %in% c("orionCOND", "fluke"))


#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
## 1. Time Series 
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

# 1. Time series - geo point
#_____________________________
##Cycling Through Treatments  |
# allchange         day/night |  
# ambient_M04       day/night |
# ambient_M05       day/night |
# current           day/night |
# hightemperature   day/night |
# ____________________________|

# All Change # # All Change # # All Change # # All Change # # All Change # # All Change # 

# All MOATs included for the "All Change" treatment
# Rplot.scrubdml.timeseries.allchange.day
# saved as KRL19.Rplot.scrubdml.timeseries.allchange.day
ggplot(subset(scrubdml[scrubdml$treatment == "allchange", ], 
              period %in% ("day")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=)) +
              ylim (10, 16) 


# All MOATs included for the "All Change" treatment
# Rplot.scrubdml.timeseries.allchange.night
# saved as KRL19.Rplot.scrubdml.timeseries.allchange.night
ggplot(subset(scrubdml[scrubdml$treatment == "allchange", ], 
              period %in% ("night")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=)) +
              ylim (10, 16) 

#Ambient M04 ##Ambient M04 ##Ambient M04 ##Ambient M04 ##Ambient M04 ##Ambient M04 # 

# Rplot.scrubdml.timeseries.ambient_M04.day
# saved as KRL19.Rplot.scrubdml.timeseries.ambient_M04.day
ggplot(subset(scrubdml[scrubdml$treatment == "ambient_M04", ], 
              period %in% ("day")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=)) +
              ylim (5, 20) 


# Rplot.scrubdml.timeseries.ambient_M04.night
# saved as KRL19.Rplot.scrubdml.timeseries.ambient_M04.night
ggplot(subset(scrubdml[scrubdml$treatment == "ambient_M04", ], 
              period %in% ("night")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=)) +
              ylim (5, 20) 

#Ambient M05 ##Ambient M05 ##Ambient M05 ##Ambient M05 ##Ambient M05 ##Ambient M05 #

# Rplot.scrubdml.timeseries.ambient_M05.day
# saved as KRL19.Rplot.scrubdml.timeseries.ambient_M05.day
ggplot(subset(scrubdml[scrubdml$treatment == "ambient_M05", ], 
              period %in% ("day")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=)) +
              ylim (1, 20) 

# Rplot.scrubdml.timeseries.ambient_M05.night
# saved as KRL19.Rplot.scrubdml.timeseries.ambient_M05.night
ggplot(subset(scrubdml[scrubdml$treatment == "ambient_M05", ], 
              period %in% ("night")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=)) +
              ylim (1, 20) 


# Current ## Current ## Current ## Current ## Current ## Current ## Current # 


# Rplot.scrubdml.timeseries.current.day
# saved as KRL19.Rplot.scrubdml.timeseries.current.day
ggplot(subset(scrubdml[scrubdml$treatment == "current", ], 
              period %in% ("day")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=)) +
              ylim (8, 16)

# All MOATs included for the "current" treatment
# Rplot.scrubdml.timeseries.current.night
# saved as KRL19.Rplot.scrubdml.timeseries.current.night
ggplot(subset(scrubdml[scrubdml$treatment == "current", ], 
              period %in% ("night")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats, point=))+
              ylim (8, 16)

# High Temperature ## High Temperature ## High Temperature### High Temperature## High Temperature## High Temperature#

# Rplot.scrubdml.timeseries.hightemperature.day
# saved as KRL19.Rplot.scrubdml.timeseries.highttemperature.day
ggplot(subset(scrubdml[scrubdml$treatment == "hightemperature", ], 
              period %in% ("day")), 
       aes(x=dateTime, y=aTemperature)) + 
  geom_point(aes(colour=moats, point=)) +
  ylim (8, 16)

# All MOATs included for the "high temperature" treatment
# Rplot.scrubdml.timeseries.hightemperature.night
# saved as KRL19.Rplot.scrubdml.timeseries.hightemperature.night
ggplot(subset(scrubdml[scrubdml$treatment == "hightemperature", ], 
              period %in% ("night")), 
       aes(x=dateTime, y=aTemperature)) + 
  geom_point(aes(colour=moats, point=))+
  ylim (8, 16)


# 1. Time series - geo smooth
#_____________________________
##Cycling Through Treatments  |
# allchange         day/night |  
# ambient_M04       day/night |
# ambient_M05       day/night |
# current           day/night |
# hightemperature   day/night |
# ____________________________|
# # 1a. Time series - geo smooth
# 
# # allchange         day/night |
# 
# # saved as KRL19.Rplot.scrubdml.geomsmooth.allchange.day
# ggplot(scrubdml, aes(x= dateTime, y= aTemperature)) +
#                   geom_smooth(aes(colour=treatment)) + 
#                   stat_smooth(level = .95) +
#                   facet_wrap(treatment~period
# 
# ggplot(subset(scrubdml[scrubdml$treatment == "allchange", ]), 
# 
# 
# # saved as KRL19.Rplot.scrubdml.geomsmooth.allchange.day
# ggplot(subset(scrubdml[scrubdml$treatment == "allchange", ], period %in% ("day")), 
#        aes(x=dateTime, y=aTemperature)) + 
#   geom_smooth(aes(colour=moats)) + 
#   stat_smooth(level = .95)


#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
## 2. Boxplots
#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

# 2. Boxplot - Series
#_____________________________
##Cycling Through Treatments  |
# allchange         day/night |  
# ambient_M04       day/night |
# ambient_M05       day/night |
# current           day/night |
# hightemperature   day/night |
# ____________________________|

#2. Simple Boxplot aTemp by moats
#boxplot(aTemperature~moats, scrubdml)

# Saved as KRL19.Rplot.scrubdml.ggplot.boxplot.alltreatments.insitujitter
ggplot(scrubdml, aes(treatment, aTemperature)) +
              geom_jitter(color = "grey") +
              geom_jitter(data = d.insitu_aTemp, aes(treatment, aTemperature), color = "yellow") +
              geom_jitter(data = n.insitu_aTemp, aes(treatment, aTemperature), color = "orange") +
              geom_boxplot(notch = TRUE, outlier.shape = NA, colour = "green") +
              geom_point(data = scrubdml.daynight.summary, aes(x=treatment, y=mean), size=5, color = "purple") + 
              geom_errorbar(data = scrubdml.daynight.summary, 
                            aes(x=treatment, y=mean, ymin = mean-sd, ymax = mean+sd), 
                            color = "blue") +
              geom_errorbar(data = scrubdml.daynight.summary,
                            aes(x=treatment, y=mean, ymin = mean-ci, ymax = mean+ci),
                            colour = "red") +
              facet_wrap(~period) +
              ggtitle("All Treatments") +
              theme_bw() 


# Saved as KRL19.Rplot.scrubdml.ggplot.boxplot.allchange.day
# Simple plot boxplot (treatment ~ aTemp ) with intercepts
p_ambM04_day <- ggplot(subset(scrubdml[scrubdml$treatment == "allchange", ], 
                                   period %in% ("day")), 
                                   aes(x=moats, y=aTemperature, colour=treatment)) + 
                      stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                      geom_jitter(data = d.insitu_aTemp, aes(treatment="allchange", aTemperature), color = "yellow") +
                      geom_hline(yintercept = scrubavg_amb04DayaTemp) +
                      geom_boxplot() +
                      ggtitle("Ambient Treatment, M04, Day Period")


p_ambM04_day

scrubavg_amb04NightaTemp


#*********************************
## XX.) aTemp bands investig.
#*********************************
# Selecting for the high aTemperature Band
investig.HighaTemp <- scrubdml %>% 
  filter(aTemperature>= 14.50 & aTemperature<=15.50) %>%
  filter(treatment %in% c("current", 
                          "allchange", 
                          "hightemperature", 
                          "ambient_M04", 
                          "ambient_M05")) %>%
  filter(period != "other")

# Time series plot 
ggplot(subset(investig.HighaTemp[investig.HighaTemp$treatment == "current", ], 
                              period %in% ("night")), 
                              aes(x=dateTime, y=aTemperature)) + 
                              geom_point(aes(colour=moats, point=)) +
                              ylim (10, 20)


# Selecting for the high aTemperature Band
investig.LowaTemp <- scrubdml %>% 
  filter(aTemperature>= 10.50 & aTemperature<=11.50) %>%
  filter(treatment %in% c("current", 
                          "allchange", 
                          "hightemperature", 
                          "ambient_M04", 
                          "ambient_M05")) %>%
  filter(period != "other")



p_cur_night_investig <- ggplot(subset(investig.HighaTemp[investig.HighaTemp$treatment == "current", ], 
                                            period %in% ("night")), 
                                            aes(x=moats, y=aTemperature, colour=treatment)) + 
                                            geom_boxplot() +
                                            geom_boxplot(data= scrubdml, x=moats, y=aTemperature)
  
                                            ggtitle("CurrentTreatment, Night Period")


p_cur_night_investig 




# Time series plot 
ggplot(subset(investig.LowaTemp[investig.LowaTemp$treatment == "hightemperature", ], 
              period %in% ("night")), 
       aes(x=dateTime, y=aTemperature)) + 
  geom_point(aes(colour=moats, point=)) +
  ylim (9, 12.5)
