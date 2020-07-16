##Hello World

#*********************************
## Version Check
#********************************* 
R.version



## Author: OA Lab, NWFSC
## Title: Creating Water Chemistry File M01thruM13moatslog_n333.csv
## Date: July 2020

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
## Outline Current 2020.07.16
#*********************************

# 1.) Working Directory
# 2.) Spolling Data into one CSV 
# 3.) Creating the Dataframe for later water chemistry investigations


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
# LVM (logical volumn management) files off each MOATs.


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

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/01a. WaterChemistry Data")
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 2.5 Write the new csv document | 
# # "M01thruM13moatslog"
write.csv(M01thruM13moatslog_data, file = "M01thruM13moatslog_n333.csv", row.names = FALSE)


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


#*********************************
## 3.) Creating the Dataframe "water chemisty dml" 
#*********************************

#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 3.1 Reading the CSV |  
## ensuring column names and types
wcdml <- read.csv( file = "M01thruM13moatslog_n333.csv", stringsAsFactors = FALSE)
dim(wcdml)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 3.1a Sub sampling dataframe "waterchem.df"  
## creating a sub sample of the data moats log waterchem.df dataframe to allow for quick graphs 
#subsample every 17th row (because prime numbers are indeed cool)
wcdml <- wcdml %>% arrange(moats, dateTime) %>% filter(row_number() %% 333 == 0)

write.csv(wcdml, file = "M01thruM13moatslog_n333.csv", row.names = FALSE)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
