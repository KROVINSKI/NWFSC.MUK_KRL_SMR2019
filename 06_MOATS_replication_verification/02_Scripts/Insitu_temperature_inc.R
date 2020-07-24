#Hello World

## Author: OA Lab, NWFSC 
## Title: Incorporating In-Situ Temperature Measurements
## Date: April 2020

 



# R script below designed to bring in the in-situ data sampling points
# Overall goal is to incorporate this data points into the moats log data
# Steps 1 thru X.X 

#*********************************
##Libraries
#*********************************
library(shiny)
library(tidyverse)
library(ggplot)
library(stringr)
library(readxl)
library(readr)
library(tidyr)
library(data.table)
library (sparklyr)
library(xts)
library(TSstudio)
library(lubridate)


#*********************************
## 1.) Setting the working directory 
#*********************************

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/06. MOATS replication verification/InSituSampling")

#*********************************
## 2.) Reading in Day/Night Period Measurements (.csv)
#*********************************

# Get a list of the files needed
files <- list.files(pattern = ".csv")

# QA Check
print(files)

# creating a temporary file, from the list
temp <- lapply(files, fread, sep= ",")
# QA Check 
print(temp)



#*********************************
## 3.) Creating the new dataframe to inc. sampling data
#*********************************
## Create a new dataframe for the in-situ temperature measurements 
## In situ temperautre measurements were taken during observations recorded during DO sampling
InsituTemperatureMeasurements <- rbindlist(temp)

#QA Check

print(InsituTemperatureMeasurements)
names(InsituTemperatureMeasurements)
#Results
#> names(InsituTemperatureMeasurements)
#[1] "Date"             "MOATS"            "Treatment"        "Time"             "Temp"            
#[6] "Sensor"           "Dissolved Oxygen" "Period"

#[1] "Date" given in MM/DD/YYYY format as of step 3
#[2] "MOATs" MOATs doesn't match "moats", MOATs written as "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"
#[3] "Treatment" listed as "HighTemp", "Ambient", "Current", "AllChange" 
#[4] "Time" given in "HH:MM" minute format
#[5] "Temp" is a four digit, 2 decimal places numeric
#[6] "Sensor" either character "MOATs" or "PreSens"
#[7] "Dissolved Oxygen" numeric value
#[8] "Period" either character, "day" or "night"


##3.1 reading the dataframe to create a dataframe
# insitu temperatures were recorded as dissolved oxygen was spot checked
# name will be shortened to:
#spotchecktemps = spchktemp
write.csv(InsituTemperatureMeasurements, file = "InsituTemperatureMeasurements.csv", row.names = FALSE)

spchktemp <- read.csv(file = "InsituTemperatureMeasurements.csv", stringsAsFactors = FALSE)

#QA Check
dim(spchktemp)
#Results
# [1] 268   8


#**********E*N*D*****************# 
#*********************************
## End of R Script End of Document 
#*********************************

#__________________ ##
#_________________###*
#______________.*#####
#_____________*#######
#___________*######### sneaky as always Dalls 
#__________*##########.
#_________*###########.
#_________*#######*###*
#________*#########*###
#_______*##########*_###
#_____*###########_____*
#____#############
#___*##*##########
#___*_____########
#__________#######
#___________*######
#____________*#####*
#______________*####*
#________________*####
#__________________*##*
#____________________*##
#_____________________*##.
#____________________.#####.
#_________________.##########
#________________.####*__*####






