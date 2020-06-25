##Hello World

#*********************************
## Version Check
#********************************* 
R.version



## Author: OA Lab, NWFSC
## Title: Aquarium Temperature Investigation: Temperature SubSelectScript.R 
## Date: April-May 2020

# R script below will subselect and plot temperature data for MOATs
# Overall goal is to determine if MOATs (per treatment) are true replicates


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
## Outline Current 2020.05.13
#*********************************

# 1.) Working Directory
# 2.) Spolling Data into one CSV 
# 3.) Creating the Dataframe "dml"

# X.) Setting a new working directory

# 4.) Creating dateTime objects 
# 5.) Creating Treatment Variables
# 6.) Creating Night and Day Periods
# 7.) Insitu data
# 8.) Cleaning up dml "Cdml"
# 9.) Framing filters for Cdml
# 10.) Temperature "Jumps"
# 11.) Calculating averages by treatment & day/night
# 12.) Summary & Group by Cdml
# 13.) Plots Boxplots & timeseries
# 14.) Duration between conditions- Histogram & Cummulative Distribtuion Plot  section
# XX.) aTemp bands investig.
# 15.0 Share-able tibbles & reprex readiness  



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
#setwd("/Users/paul.mcelhany/Downloads")


#*********************************
## 2.) Spolling Data into one CSV 
#*********************************
#Combining multiple CSV files into 1 document. Original input files from individual 
# LVM (logical volumne management) files off each MOATs.


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 2.1 Create a list of files 
# All files to be joined have ext. "csv" can use that pattern to join 
files <- list.files(pattern = ".csv")
print(files)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 2.2 Create a temporary place for files 
temp <- lapply(files, fread, sep= ",")
print(temp)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 2.3 Create a new vector for Moats data logs 
# "M01thruM13Moatslog_data" via rbind
M01thruM13moatslog_data <- rbindlist(temp)
print(M01thruM13moatslog_data)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 2.4 Setting new working directory for output file organization
#May find duplicates appear if not careful about clone documents

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification")
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 2.5 Write the new csv document | 
# "M01thruM13moatslog"
write.csv(M01thruM13moatslog_data, file = "M01thruM13moatslog.csv", row.names = FALSE)

#Saved on the OA Google Drive
# https://drive.google.com/open?id=15iBXct9b4EjKDq75vKnm5NobowBwK3G- 

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 2.6 Duplication Check
## duplicates observed in "dml" on 2020.05.07  
# 2020.05.08 Paul McElhany Patch
#there are no duplicates
#if no dups, then dup2 has 0 rows
dup2 <- dml[duplicated(dml),]
#if no dups, Cdml2 has same number of rows as Cdml
dml2 <- dml %>% distinct() 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 3.) Creating the Dataframe "dml" 
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 3.1 Reading the CSV |  
## ensuring column names and types
## Data Moats Log = dml
dml <- read.csv(file = "M01thruM13moatslog.csv", stringsAsFactors = FALSE)
dim(dml)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 3.1a Sub sampling dataframe "dml"  
## creating a sub sample of the data moats log dml dataframe to allow for quick graphs 
#subsample every 17th row (because prime numbers are indeed cool)
dml <- dml %>% arrange(moats, dateTime) %>% filter(row_number() %% 17 == 0)

write.csv(dml, file = "M01thruM13moatslog_n17.csv", row.names = FALSE)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 3.2 Checking variables  
## Looking to ensure the different variables are treated as the correct variable type
## Checking the names in the dataframe
names(dml)
## Checking variable type/class 
class(dml$moats)
factor(moats)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

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

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

#*********************************
## 7.) Insitu data  
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 7.0 In-situ data files
# two files represent in the person manual checks of aquarium salinity with 
# (at times) a Fluke temperature probe & conductivity probe

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 7.1 Read in files
# carefule about navigating to the second working directory
read_csv()

d.insitu <- read.csv(file = "KRL19_insitu_sample_Day.csv", stringsAsFactors = FALSE)
n.insitu <- read.csv(file = "KRL19_insitu_sample_Night.csv", stringsAsFactors = FALSE)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 8.) Cleaning up dml  
#*********************************


# Creating a new dataframe cleaned with the various filters below
# checking on the variables inside dml
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 8.0 Cleaned-Up dml = Cdml

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 8.1 Noting names of the different variables in dml
names(dml)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 8.2 Narrative on desired changes to create Cdml

# Changes to be made the dataframe by variable
#[1] "moats" "M03", "M04", "M05", "M09", "M11" to be filtered out- all these 
#                                           MOATs were dropped from the study
#[2] "dateTime" - no changes
#[3] "aTemperature" - no changes 
#[4] "sTemperature" - no changes    
#[5] "pH" - no changes
#[6] "DO"- no changes
#[7] "salinity" - no changes 
#[8] "treatment" - dropping the listed MOATs will eliminate the 
#                           "broken_and_ambientbroken" treatment
#[9] "period" - filtering out "other"
#[10] "ObservationDate" - no changes
#[11] "ObservationTime" - no changes, note that each observation could be 
#                                                 spaced 6.8minutes apart
          
Cdml <- dml %>% filter(!moats %in% c("M03", "M04", "M05", "M11")) %>%
  filter(aTemperature>= 5 & aTemperature<=30) %>%
  filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
  filter(period != "other")
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 8.3 Option to write a CSV of Cdml parameters
# write.csv(dml, file = "2020.05.08_Cdml.csv", row.names = FALSE)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 8.4 Removing moats not ultimately included
levels(Cdml$moats)
Cdml$moats <- droplevels(Cdml$moats)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 9.) Framing filters for Cdml
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 9.0 Removing the names of moats & treatments removed from Cdml 
# Removal should allow for cleaner graphs 
# Determined moats lab never got under 5C and was never over 30C 
 
filteredFrame = filter(Cdml,
  !moats %in% c('M03', "M04", "M05", "M11") & 
  (aTemperature>= 5 & aTemperature<=30) &
  treatment %in% c("current", "allchange", "hightemperature") &
  period != "other")

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 9.1 Dropping levels and factors
filteredFrame$moats <- droplevels(filteredFrame$moats)
filteredFrame$treatment <- factor(filteredFrame$treatment)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 9.2 Option to write the Cdml after filtering
#write.csv(filteredFrame, file = "2020.05.08_Cdml_filteredFrame.csv", row.names = FALSE)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 10.) Temperature Jumps
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 10.0 Creating new column - showing the different between 2 adjacent times
# first sort the data so you are comparing adjacent times
# then add new column
# the diff() function computes x[i+lag] - x[i], the default value is lag = 1

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 10.1 diff()function  & deltaTempLag1 Narrative
#the result of the diff() function has a length that is the length of 
#                                           the original vector - lag

#therefore need to fill the new variable, deltaTempLag1,
# with c(0,abs(diff(aTemperature))) it is right length and first value 
#                                         (with no valid diff) is zero

# creating a new object to be a validation flag under name deltaTempLag1

# will create a numerical value that will represent those observations 
#                                                   to be filtered out
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 10.2 Creating deltaTempLag1
Cdml <- filteredFrame %>% arrange(moats, dateTime)%>% 
  mutate(deltaTempLag1 = c(0,abs(diff(aTemperature))))
#the diff value comparing the last time in M01 to the first time in M02 (etc.) 
#                                                                 is not valid, 
# Set those to zero
Cdml$deltaTempLag1[lag(Cdml$moats) != cdml$moats] <- 0

#this shows all the rows that jump more that 1 degree from previous row 
#                     (ingoring rows that are transition from one moats to another)
Cdml %>% filter(deltaTempLag1 >1)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 10.2 Creating tDeltaThreshold
#Creating another column of variables to be able to graph the temperature jumps 

Cdml <- Cdml %>% mutate(tDeltaThreshold = if_else(deltaTempLag1 > 0.5, TRUE, FALSE))
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 11.) Calculating Averages by Treatment & Day/Night
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 11.0 Intercept Narrative
# These averages will be the yintercepts in plots
# example of what to put into with ggplot "  
#    geom_hline(yintercept = dtemperatur$`mean(aTemperature)`)   " 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 11.1 All Treatments (Day & Night)
# Night Period
subsetNightaTemp <- subset(filteredFrame, period == "night" & aTemperature >0,
                                  select = c( moats, dateTime, treatment, aTemperature ))

# Day Period
subsetDayaTemp <- subset(filteredFrame, period == "day" & aTemperature >0,
                           select = c( moats, dateTime, treatment, aTemperature ))
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 11.2 Current Treatment
# Current Treatment Averages--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR
# Current Night
curNightaTemp <- subset(Cdml, period == "night" & treatment == "current",
                           select = c(dateTime, aTemperature ))
avg_curNightaTemp <- mean(curNightaTemp$aTemperature)
print(avg_curNightaTemp)
# [1] 11.95122


# Current Day
curDayaTemp <- subset(Cdml, period == "day" & treatment == "current",
                        select = c(dateTime, aTemperature ))
avg_curDayaTemp <- mean(curDayaTemp$aTemperature)
print(avg_curDayaTemp)
# [1] 11.09983

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 11.3 All Change Treatment
# All Change Treatment Averages --All_Chg--All_Chg--All_Chg--All_Chg--All_Chg--All_Chg
# All Change Night
allchgNightaTemp <- subset(Cdml, period == "night" & treatment == "allchange",
                        select = c(dateTime, aTemperature ))
avg_allchgNightaTemp <- mean(allchgNightaTemp$aTemperature)
print(avg_allchgNightaTemp)
# [1] 13.89847

# All Change Day
allchgDayaTemp <- subset(Cdml, period == "day" & treatment == "allchange",
                      select = c(dateTime, aTemperature ))
avg_allchgDayaTemp <- mean(allchgDayaTemp$aTemperature)
print(avg_allchgDayaTemp)
# [1] 12.97466

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 11.4 High Temperature Treatment
# High Temperature Treatment Averages -- HiTemp -- -- HiTemp -- HiTemp -- HiTemp -- HiTemp 
# High Temperature Night
hitempNightaTemp <- subset(Cdml, period == "night" & treatment == "hightemperature",
                           select = c(dateTime, aTemperature ))
avg_hitempNightaTemp <- mean(hitempNightaTemp$aTemperature)
print(avg_hitempNightaTemp)
#[1] 13.80397

# High Temperature Day
hitempDayaTemp <- subset(Cdml, period == "day" & treatment == "hightemperature",
                           select = c(dateTime, aTemperature ))
avg_hitempDayaTemp <- mean(hitempDayaTemp$aTemperature)
print(avg_hitempDayaTemp)
#[1] 12.92148

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 11.5 Review of all Treatments
# Review of 6 new variables 

avg_allchgDayaTemp
avg_allchgNightaTemp
avg_curDayaTemp
avg_curNightaTemp
avg_hitempDayaTemp
avg_hitempNightaTemp

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


## 11.6 Table of Averages
meanCdmldatemp <- filteredFrame %>% group_by(treatment, period) %>% 
                                                      summarise(mean(aTemperature))
# Groups:   treatment [3]
#treatment       period `mean(aTemperature)`
#<chr>           <chr>                 <dbl>
# 1 allchange       day                    13.0
# 2 allchange       night                  13.9
# 3 current         day                    11.1
# 4 current         night                  12.0
# 5 hightemperature day                    12.9
# 6 hightemperature night                  13.8
print(meanCdmldatemp)

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

#*********************************
## 12.) Summary & Group by Cdml
#*********************************

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
# 12.0 Cdml Day Summary, group by, mutate

Cdml.daynight.summary <- Cdml %>% group_by(treatment, period) %>%
  summarize(sd = sd(aTemperature, na.rm = TRUE), 
            mean = mean(aTemperature, na.rm = TRUE), 
            median = median(aTemperature, na.rm = TRUE),
            IQR = IQR(aTemperature, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(ci = se*1.96)


write.csv(Cdml.daynight.summary, "2020.05.07_Cdml_daynight_summary.csv")
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 13.) Plots
#*********************************


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 13.1 Boxplot aTemp by moats
#boxplot(aTemperature~moats, Cdml)

ggplot(Cdml, aes(treatment, aTemperature)) +
            geom_jitter(color = "grey") +
            geom_jitter(data = d.insitu, aes(treatment, aTemperature)) +
            geom_jitter(data = n.insitu, aes(treatment, aTemperature)) +
            geom_jitter(aes(colour = tDeltaThreshold)) +
            geom_boxplot(notch = TRUE, outlier.shape = NA, colour = "green") +
            geom_point(data = Cdml.daynight.summary, aes(x=treatment, y=mean), size=5, color = "purple") + 
            geom_errorbar(data = Cdml.daynight.summary, 
                          aes(x=treatment, y=mean, ymin = mean-sd, ymax = mean+sd), 
                          color = "blue") +
            geom_errorbar(data = Cdml.daynight.summary,
                          aes(x=treatment, y=mean, ymin = mean-ci, ymax = mean+ci),
                          colour = "red") +
            facet_wrap(~period) +
            ggtitle("All Treatments, Temperature Jumps at .5C") +
            theme_bw() 

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 13.1a Boxplots by Treatment
# plot boxplot (treatment ~ aTemp ) with intercepts
p_allchg_d_yinter <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ], 
                                          period %in% ("day")), 
                                          aes(x=moats, y=aTemperature, 
                                                        colour=treatment)) + 
                                          stat_summary(fun=mean, geom="point", 
                                                        size=2, color="red")  + 
                                          geom_hline(yintercept = avg_allchgDayaTemp) +
                                          geom_boxplot() +
                                          ggtitle("All Change Day Period")


p_allchg_d_yinter
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 13.1b Boxplot by Treatment with Facets
# Boxplot (treatment ~ aTemp ) with intercepts & Facets

p_allchg_d_facet <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ]),
                                                aes(x=moats, y=aTemperature, 
                                                        colour=moats)) +
                                          geom_hline(yintercept = avg_allchgDayaTemp) +
                                          geom_hline(yintercept = avg_allchgNightaTemp) +
                                          geom_boxplot() +
                                          stat_summary(fun=mean, 
                                                        geom="point", size=2, 
                                                        color="red")  +
                                          facet_grid(period~moats) +
                                          ggtitle("All Change Day Period")

 p_allchg_d_facet

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

 
## 13.2 Histogram plot- all moats, all treatments, all periods 

 Cdml %>% ggplot(data=.)+geom_histogram(aes(x=aTemperature), binwidth = .05) + 
                          xlim(5,30) +
                          ylim(0, 10000)
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 13.2a Histrogram Scaled by Time Narrative 
# Section below walks how to quantify each "bin" by the observation period = 6.8minutes  

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 13.2b Histogram scaled by time
#find duration of each observations in hours

durObsHours <- 6.8/60

# the count (number of  observations in each bin) is scale by time duration of each obs
# option to also include polygon
# could fix the vline to only show the value relevant for each plot
#vline could show acutal mean temperature, not just target
# each bin shows 0.2 degrees
p <- ggplot(Cdml, aes(aTemperature)) +
                  geom_histogram(aes(y=..count..*durObsHours), 
                                              binwidth = 0.2) +
                   #   geom_freqpoly(aes(y=..count..*durObs)) +
      geom_vline(xintercept = c(11,12,13,14), colour = "red") +
      facet_wrap(vars(treatment, period), ncol = 2, scales = "fixed") +
      theme_bw()
p

#just show bins < 12 hours
p + ylim(c(0,12))
 
## Conclusion- problem remains of how to show what percent of time it spent around
##             the average- not plain "stacking time". 

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 13.3 Cumulative Frequency Distribution

p <- ggplot(Cdml, aes(aTemperature)) +
            stat_ecdf(aes(colour = moats)) + 
            geom_vline(xintercept = c(11,12,13,14), colour = "red") +
            facet_wrap(vars(treatment, period), ncol = 2, scales = "fixed")  +
            theme_bw()
p

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 12.3a  Using a ECDF to better visualize distribution
#Compute empirical cumulative distribution : The empirical cumulative distribution 
#                                            function (ECDF) provides an alternative 
#                                            visualisation of distribution. 
#                                         Compared to other visualisations that rely 
#                                         on density (like geom_histogram()), the 
#                                         ECDF doesn't require any tuning parameters 
#                                         and handles both continuous and categorical 
#                                         variables. 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 13.3b ECDF Plot

p <-      ggplot(Cdml, aes(aTemperature)) +
                stat_ecdf(aes(colour = moats)) + 
                geom_vline(xintercept = Cdml.daynight.summary$mean, colour = "red") +
                facet_wrap(vars(treatment, period), ncol = 2, scales = "fixed")  +
                theme_bw()
p
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 13.3c Creating a function to incorporate our statisitics- show our averages

 coolECDF <- function(d, dsum, treat, per){
   dsum <- dsum %>% filter(treatment == treat & period == per )
   p <- d %>% filter(treatment == treat & period == per ) %>% ggplot(aes(aTemperature)) +
     stat_ecdf(aes(colour = moats)) + 
     geom_vline(xintercept = dsum$mean, colour = "red") +
     ggtitle(paste(treat, per)) +
     theme_bw()
   return(p)
 }
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

  levels(Cdml$treatment)
  levels(factor(Cdml$period))

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 
## 13.3d Using the coolECDF function per treatment & period

        cfd.a.d <- coolECDF(Cdml, Cdml.daynight.summary, "allchange", "day")
        cfd.a.n <- coolECDF(Cdml, Cdml.daynight.summary, "allchange", "night")
        cfd.c.d <- coolECDF(Cdml, Cdml.daynight.summary, "current", "day")
        cfd.c.n <- coolECDF(Cdml, Cdml.daynight.summary, "current", "night")
        cfd.h.d <- coolECDF(Cdml, Cdml.daynight.summary, "hightemperature", "day")
        cfd.h.n <- coolECDF(Cdml, Cdml.daynight.summary, "hightemperature", "night")
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 13.4 Incorporating Grid Arrange to display the ECDF plots        
# Basic Grid Arrange

      grid.arrange(cfd.a.d, cfd.c.d, cfd.h.d, nrow = 1)
 
      grid.arrange(cfd.a.n, cfd.c.n, cfd.h.n, nrow = 1) 
      
      #make a list of all the plots then pass the list to grid.arrange()
      ecdfList <- list(cfd.a.d, cfd.c.d, cfd.h.d, cfd.a.n, cfd.c.n, cfd.h.n)
      grid.arrange(grobs = ecdfList, ncol=3)
      
      
 

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 13.5 Investigating tails
#to look a little more closely at the tails
#lower 1% 
      
lowerTail <- function(p){
  return(p + ylim(c(0,0.01)))
}

upperTail <- function(p){
  return(p + ylim(c(0.99,1.0)))
}

lowerP <- lapply(ecdfList, lowerTail)
grid.arrange(grobs = lowerP, ncol=3)

upperP <- lapply(ecdfList, upperTail)
grid.arrange(grobs = upperP, ncol=3) 

 # p + ylim(c(0,0.01))
 # #upper 1%
 # p + ylim(c(0.99,1))
 # 
 # #to look a little more closely at the tails
 # #lower 1% 
 # cfd.a.d + ylim(c(0,0.01))
 # #upper 1%
 # p + ylim(c(0.99,1))

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 
## 13.6 Plain time series
# example plot

 ggplot(subset(Cdml[Cdml$treatment == "allchange", ], 
               period %in% ("night")), 
        aes(x=dateTime, y=aTemperature)) + 
        geom_point(aes(colour=moats, point=)) + 
        ylim (10, 16) 
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |  
## 13.7 Violin Plot 
# example plot
 p_allchg_d_facet_violin <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ]), 
                                   aes(x=moats, y=aTemperature, colour=moats)) + 
                            geom_hline(yintercept = avg_allchgDayaTemp) +
                            geom_jitter(shape=16, position=position_jitter(0.2), 
                                  color="light grey") + 
                            geom_hline(yintercept = avg_allchgNightaTemp) +
                            geom_violin(aes(colour = period)) +
                            stat_summary(fun=mean, geom="point", size=1, color="purple")  + 
                            facet_grid(period~moats) 
 
 p_allchg_d_facet_violin
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |  
# 13.7a faceted violin plot

 p_allchg_d_facet_violin <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ]), 
                                   aes(x=moats, y=aTemperature, colour=moats)) + 
                            geom_hline(yintercept = avg_allchgDayaTemp) +
                            geom_jitter(shape=16, position=position_jitter(0.2), 
                                  color="light grey") + 
                            geom_hline(yintercept = avg_allchgNightaTemp) +
                            geom_violin(aes(colour = period)) +
                            geom_boxplot() +
                            stat_summary(fun=mean, geom="point", size=1, color="purple")  + 
                            facet_grid(period~moats) 
 
 
 p_allchg_d_facet_violin
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 
## 13.7b Simple violin plot
# note that day and night are not filtered
vp <-    ggplot(filteredFrame, 
                aes(x=filteredFrame$aTemperature, 
                y=filteredFrame$treatment)) + 
        geom_violin() 

vp

#display the plot
vp

# Rotate the violin plot
vp + coord_flip()
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |  
## 13.7c Simple violin plot with ggplot, filtered day and night

p_allchg_n <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ], 
                            period %in% ("night")), 
                            aes(x=moats, y=aTemperature, colour=treatment)) + 
              stat_summary(fun=mean, geom="point", size=2, color="red")  + 
              geom_jitter(shape=16, position=position_jitter(0.2), 
                            color="blue") + 
              geom_hline(yintercept = 13.9) +
              geom_violin() +
              geom_boxplot(width= 0.1) +
  
              ggtitle("Day Period - All Change Treatment")


#display the plot
p_allchg_n

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 

## 13.7c Simple violin plot, filtered day and night, jitter points for Temp Jumps
p_allchg_d_jitter <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ], 
                                   period %in% ("day")), 
                            aes(x=moats, y=aTemperature, colour=treatment)) + 
                     stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                    geom_jitter(aes(colour = tDeltaThreshold)) +
                    geom_hline(yintercept = avg_allchgDayaTemp) +
                    geom_boxplot() +
                    ggtitle("Day Period - All Change Treatment- Temp Jumps (.5C)")


#display the plot
p_allchg_d_jitter

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 




 
#*********************************
 ## 14.) aTemp bands investig.
#*********************************

 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 
# Selecting for the high aTemperature Band
 investig.HighaTemp <- Cdml %>% 
   filter(aTemperature>= 14.50 & aTemperature<=15.50) %>%
   filter(treatment %in% c("current", 
                           "allchange", 
                           "hightemperature")) %>%
   filter(period != "other")

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |  
 # Time series plot 
 ggplot(subset(investig.HighaTemp[investig.HighaTemp$treatment == "current", ], 
               period %in% ("night")), 
        aes(x=dateTime, y=aTemperature)) + 
   geom_point(aes(colour=moats, point=)) +
   ylim (10, 20)
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 
 # Selecting for the high aTemperature Band
 investig.LowaTemp <- scrubdml %>% 
   filter(aTemperature>= 10.50 & aTemperature<=11.50) %>%
   filter(treatment %in% c("current", 
                           "allchange", 
                           "hightemperature", 
                           "ambient_M04", 
                           "ambient_M05")) %>%
   filter(period != "other")
 
 #|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 

ggplot(subset(Cdml[Cdml$treatment == "hightemperature", ],
              period %in% ("day")), 
              aes(x=dateTime, y=aTemperature)) + 
              geom_point(aes(colour=moats)) + 
              ylim (10, 16)
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 

p_cur_night_investig <- ggplot(subset(investig.HighaTemp[investig.HighaTemp$treatment == "current", ], 
                                       period %in% ("night")), 
                                aes(x=moats, y=aTemperature, colour=treatment)) + 
   geom_boxplot() +
   geom_boxplot(data= scrubdml, x=moats, y=aTemperature) +
   ggtitle("CurrentTreatment, Night Period")

p_cur_night_investig 

 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 
 
 # Time series plot 
 ggplot(subset(investig.LowaTemp[investig.LowaTemp$treatment == "hightemperature", ], 
               period %in% ("night")), 
        aes(x=dateTime, y=aTemperature)) + 
   geom_point(aes(colour=moats, point=)) +
   ylim (9, 12.5)
 
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 
 
 

#*********************************
## 15.0 Share-able tibbles & reprex readiness 
#*********************************

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |  
library(datapasta)
library(lubridate)
library(tidyr)
library(reprex)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 

Cdml_tibble <- tibble::tribble(
  ~moats,       ~dateTime, ~aTemperature, ~sTemperature,        ~pH,        ~DO, ~salinity,        ~treatment, ~period, ~ObservationDate, ~ObservationTime,
  "M01", "9/22/19 17:00",   13.85183275,    12.3611915, 7.72147075,  8.7852685,      28.8, "hightemperature", "night",        "9/23/19",       "17:00:11",
  "M01", "9/22/19 17:00",   13.85183275,    12.3611915, 7.72147075,  8.7852685,      28.8, "hightemperature", "night",        "9/23/19",       "17:00:11",
  "M01", "9/22/19 17:00",   13.84228175,   12.37342325,  7.7217335, 8.78630075,      28.8, "hightemperature", "night",        "9/23/19",       "17:00:35",
  "M01", "9/22/19 17:00",   13.83609575,   12.38346925, 7.72306525,   8.787511,      28.8, "hightemperature", "night",        "9/23/19",       "17:00:59",
  "M01", "9/22/19 17:01",   13.82840625,    12.3968505, 7.72276925,  8.7883735,      28.8, "hightemperature", "night",        "9/23/19",       "17:01:23",
  "M01", "9/22/19 17:01",   13.82840625,    12.3968505, 7.72276925,  8.7883735,      28.8, "hightemperature", "night",        "9/23/19",       "17:01:23",
  "M01", "9/22/19 17:01",     13.820917,    12.4082725, 7.72247825,  8.7879725,      28.8, "hightemperature", "night",        "9/23/19",       "17:01:47",
  "M01", "9/22/19 17:02",     13.821128,   12.41897925,  7.7213995, 8.78786875,      28.8, "hightemperature", "night",        "9/23/19",       "17:02:11",
  "M01", "9/22/19 17:02",     13.821128,   12.41897925,  7.7213995, 8.78786875,      28.8, "hightemperature", "night",        "9/23/19",       "17:02:11",
  "M01", "9/22/19 17:02",    13.8290415,   12.43227075,   7.720327,  8.7873385,      28.8, "hightemperature", "night",        "9/23/19",       "17:02:35",
  "M01", "9/22/19 17:02",     13.830335,   12.44425375, 7.72050525,   8.786096,      28.8, "hightemperature", "night",        "9/23/19",       "17:02:59",
  "M01", "9/22/19 17:02",     13.830335,   12.44425375, 7.72050525,   8.786096,      28.8, "hightemperature", "night",        "9/23/19",       "17:02:59",
  "M01", "9/22/19 17:03",    13.8286205,     12.454543,   7.721349,    8.78333,      28.8, "hightemperature", "night",        "9/23/19",       "17:03:23",
  "M01", "9/22/19 17:03",   13.83192275,   12.46454925, 7.72017975, 8.78026575,      28.8, "hightemperature", "night",        "9/23/19",       "17:03:47",
  "M01", "9/22/19 17:03",   13.83192275,   12.46454925, 7.72017975, 8.78026575,      28.8, "hightemperature", "night",        "9/23/19",       "17:03:47",
  "M01", "9/22/19 17:04",   13.83587875,   12.47555575, 7.71890175, 8.77756875,      28.8, "hightemperature", "night",        "9/23/19",       "17:04:11",
  "M01", "9/22/19 17:04",     13.826523,   12.48512475, 7.71868825,  8.7742965,      28.8, "hightemperature", "night",        "9/23/19",       "17:04:35",
  "M01", "9/22/19 17:04",     13.826523,   12.48512475, 7.71868825,  8.7742965,      28.8, "hightemperature", "night",        "9/23/19",       "17:04:35",
  "M01", "9/22/19 17:04",   13.82531925,    12.4925115,  7.7178825, 8.77148775,      28.8, "hightemperature", "night",        "9/23/19",       "17:04:59",
  "M01", "9/22/19 17:05",   13.82626475,   12.49812425,   7.716637, 8.76831025,      28.8, "hightemperature", "night",        "9/23/19",       "17:05:23",
  "M01", "9/22/19 17:05",   13.82626475,   12.49812425,   7.716637, 8.76831025,      28.8, "hightemperature", "night",        "9/23/19",       "17:05:23",
  "M01", "9/22/19 17:05",     13.826818,    12.5047895,   7.716765,    8.76564,      28.8, "hightemperature", "night",        "9/23/19",       "17:05:47",
  "M01", "9/22/19 17:06",   13.82612375,     12.511182,   7.716661, 8.76295525,      28.8, "hightemperature", "night",        "9/23/19",       "17:06:11",
  "M01", "9/22/19 17:06",   13.82612375,     12.511182,   7.716661, 8.76295525,      28.8, "hightemperature", "night",        "9/23/19",       "17:06:11",
  "M01", "9/22/19 17:06",   13.82453375,   12.51655025,   7.717783, 8.76115575,      28.8, "hightemperature", "night",        "9/23/19",       "17:06:35",
  "M01", "9/22/19 17:06",   13.83250375,   12.52548325, 7.71752775, 8.76097725,      28.8, "hightemperature", "night",        "9/23/19",       "17:06:59",
  "M01", "9/22/19 17:06",   13.83250375,   12.52548325, 7.71752775, 8.76097725,      28.8, "hightemperature", "night",        "9/23/19",       "17:06:59",
  "M01", "9/22/19 17:07",     13.834792,   12.53531075, 7.71708175, 8.76185275,      28.8, "hightemperature", "night",        "9/23/19",       "17:07:23",
  "M01", "9/22/19 17:07",    13.8318725,   12.54257025,   7.717908, 8.76319425,      28.8, "hightemperature", "night",        "9/23/19",       "17:07:47",
  "M01", "9/22/19 17:08",    13.8361665,   12.55218375,   7.718755, 8.76480275,      28.8, "hightemperature", "night",        "9/23/19",       "17:08:11",
  "M01", "9/22/19 17:08",    13.8361665,   12.55218375,   7.718755, 8.76480275,      28.8, "hightemperature", "night",        "9/23/19",       "17:08:11",
  "M01", "9/22/19 17:08",    13.8457875,      12.56068,   7.717928, 8.76695025,      28.8, "hightemperature", "night",        "9/23/19",       "17:08:35",
  "M01", "9/22/19 17:08",    13.8500435,    12.5711655,   7.717523, 8.76921975,      28.8, "hightemperature", "night",        "9/23/19",       "17:08:59",
  "M01", "9/22/19 17:08",    13.8500435,    12.5711655,   7.717523, 8.76921975,      28.8, "hightemperature", "night",        "9/23/19",       "17:08:59",
  "M01", "9/22/19 17:09",     13.850412,   12.57595675, 7.71843875,   8.772005,      28.8, "hightemperature", "night",        "9/23/19",       "17:09:23",
  "M01", "9/22/19 17:09",   13.84312675,   12.57948975,    7.71705, 8.77434125,      28.8, "hightemperature", "night",        "9/23/19",       "17:09:47",
  "M01", "9/22/19 17:09",   13.84312675,   12.57948975,    7.71705, 8.77434125,      28.8, "hightemperature", "night",        "9/23/19",       "17:09:47",
  "M01", "9/22/19 17:10",    13.8401945,     12.588541,  7.7151665, 8.77656075,      28.8, "hightemperature", "night",        "9/23/19",       "17:10:11",
  "M01", "9/22/19 17:10",    13.8327885,   12.59370075,  7.7155165, 8.77884025,      28.8, "hightemperature", "night",        "9/23/19",       "17:10:35",
  "M01", "9/22/19 17:10",    13.8327885,   12.59370075,  7.7155165, 8.77884025,      28.8, "hightemperature", "night",        "9/23/19",       "17:10:35",
  "M01", "9/22/19 17:10",   13.82068175,   12.59439575,   7.716084,  8.7808175,      28.8, "hightemperature", "night",        "9/23/19",       "17:10:59",
  "M01", "9/22/19 17:11",     13.809785,   12.59439175,  7.7162305,  8.7821775,      28.8, "hightemperature", "night",        "9/23/19",       "17:11:23",
  "M01", "9/22/19 17:11",     13.809785,   12.59439175,  7.7162305,  8.7821775,      28.8, "hightemperature", "night",        "9/23/19",       "17:11:23",
  "M01", "9/22/19 17:11",   13.79610625,    12.5977915,   7.715906, 8.78290475,      28.8, "hightemperature", "night",        "9/23/19",       "17:11:47",
  "M01", "9/22/19 17:12",     13.788232,     12.601455, 7.71547575,   8.782347,      28.8, "hightemperature", "night",        "9/23/19",       "17:12:11"
)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - | 


#**************E*N*D*************# 
#*********************************
## END OF SCRIPT | END OF DOCUMENT 
#*********************************

#________$$$$..
#______$$$$$$$$$
#  ______$$$$$$$_$
#  _____$$$$$$$$$$
#  ______$$$$$$$$$$
#  _____$$$$$$_$$$$$
#  ____$$$$$$$_____$$$
#  ____$$$$$$$$_____$
#  ____$$$$$$$$$$
#  _____$$$$$$$$$$
#  _____$$$$$$$$$$$
#  ______$$$$$$$$$$$
#  _$$$$___$$$$$$$$$
#  __$$$$$$$$$$$$$$$
#  _$$$$$$$$$$$$$$$
#  __$$$$$$$$$$$$$
#  $$$$$$$$$$$$$
#  __$__$$$$$$
#  ____$$$$$$
#  ____$$$$$
#  ___$$$$$$_____$
#  ___$$$$$$___$$_$$
#  ____$$$$$___$__$$
#  ____$$$$$______$$
#  _____$$$$$____$$$
#  _______$$$$$$$$$
#  __________$$$$
  

