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

# 1.) Working Directory
# 2.) Spolling Data into one CSV 
#    *this includes insitu samples
# 3.) Creating the Dataframe "dml"


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


# * * * * * * * * * * * * * * * *
## 3.1b duplicates observed in "dml" - checkpoint  
# * * * * * * * * * * * * * * * *

# 2020.05.08 Paul McElhany Patch

#there are no duplicates
#if no dups, then dup2 has 0 rows
dup2 <- dml[duplicated(dml),]
#if no dups, Cdml2 has same number of rows as Cdml
dml2 <- dml %>% distinct() 

# 
# # 2020.05.07 Patch'ski
# # Duplicates observed in observations

# dim(dml) 
# uniqueDml <- unique(dml)
# dim(uniqueDml) 

# dml <- uniqueDml
# 
# 

dim(dml)


#**********E*N*D*****************# 
#*********************************
## End of Document End of Script  
#*********************************

# 
# ┊┊┊┊┊╭╭╭╮╮╮┊┊┊┊ 
# ┊┊┊┊┊╰╰╲╱╯╯┊┊┊┊ 
# ┊┏╮╭┓╭━━━━━━╮┊┊ 
# ┊╰╮╭╯┃┈┈┈┈┈┈┃┊┊ 
# ┊┊┃╰━╯┈┈╰╯┈┈┃┊┊ 
# ┊┊┃┈┈┈┈┈┈┈╰━┫┊┊ 
# ╲╱╲╱╲╱╲╱╲╱╲╱╲╱╲
