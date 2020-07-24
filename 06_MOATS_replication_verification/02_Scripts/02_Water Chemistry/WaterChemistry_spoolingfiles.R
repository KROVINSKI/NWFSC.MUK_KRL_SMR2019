##Hello World

#*********************************
## Version Check
#********************************* 
R.version

## Author: OA Lab, NWFSC
## Title: Aquarium Temperature Investigation: Water Chemistry 
## Date: May 2020

# R script below will spoll together all labview Logical Volumne Management files 
# This is to create one document to be loaded into the "moats graph" shiny app

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
library(datapasta)
library(reprex)
library(miniUI)


#*********************************
## Outline Current 2020.05.18
#*********************************

# 1.) 1st Working Directory
# 2.) Spolling Data into one TSV
# 3.) Changing the extension to CSV

#*********************************
## 1.) Set working directory
#*********************************

# The folder contains all the LVM "raw" files as TSV
#setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/02. Scripts/02.  Water Chemistry/TSVlog")
#setwd("/Users/paul.mcelhany/Downloads")

# The folder containing M01thruM13moatslog.csv
#setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/02. Scripts/02.  Water Chemistry/TSVlog")

#*********************************
## 2.) Spolling Data into one CSV 
#*********************************
#Combining multiple CSV files into 1 document. Original input files from individual 
# LVM (logical volumne management) files off each MOATs.


#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 2.1 Create a list of files 
# All files to be joined have ext. "csv" can use that pattern to join 
files <- list.files(pattern = ".tsv")
print(files)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 2.2 Create a temporary place for files 
temp <- lapply(files, fread, sep= ",")
print(temp)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
## 2.3 Create a new vector for Moats data logs 
# "M01thruM13Moatslog_data" via rbind
M01thruM13moatsTSVlogs <- rbindlist(temp)
print(M01thruM13moatsTSVlogs)
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 2.4 Setting new working directory for output file organization
#May find duplicates appear if not careful about clone documents

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/02. Scripts/02.  Water Chemistry")
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

## 2.5 Write the new csv document | 
# "M01thruM13moatsTSVlogs"
write.csv(M01thruM13moatsTSVlogs, file = "M01thruM13moatslabviewlogs.csv", row.names = FALSE)

#Saved on the OA Google Drive
# https://drive.google.com/open?id=1eCj2sJYCz6OL-SQa9p7HAJmGisR2y0Qk
#|- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |




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


