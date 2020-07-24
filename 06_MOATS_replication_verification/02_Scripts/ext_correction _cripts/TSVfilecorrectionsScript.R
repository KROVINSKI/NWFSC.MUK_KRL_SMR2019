##Hello World

##R script created to record and sampling of the bits of code I think'll need to read the MOATS TSVs and use them to plot activity inside each MOATs
## Estimate 3 steps (Step 1 thru Step 3.6)

#Likely Libraries
library(shiny)
library(ggplot2)
library(stringr)
library(readxl)
library(readr)
install.packages("tidyverse")
library(tidyr)

# 1.) Set working directory
setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_MOATS_SMR2019/LabViewLogs.AllMOATS/")

# 2.) Create list of files that will be modified
files_list = list.files(
  path = "/Users/katherinerovinski/GIT/NWFSC.MUK_MOATs_SMR2019/LabViewLogs.AllMOATS/",
  recursive = TRUE,
  pattern = "^.*\\.tsv$"
)

print(files_list)

# 3.) Create loop of those files to correct rows, columns, date format
# c <- those files being corrected
for (c in files_list) {
  #action3.1,
  #action3.2,
  #action3.3,
  #action3.4,
  #action3.5,
  #action3.6,
}

# 3.1) Delete first 15 rows, settinga dataframe, DF1 and establishing the number of rows and columns
DF1 = data.frame([d= (1-14423), (1-16)])
DF1 = data.frame
for (c in files_list) {
  
}


# 3.2) create a column, fill in the value as a date that reads it from the file name

# 3.3) Deleting rows about Xdimension

# 3.4) Correct format for the date time group

# 3.5) Changes the headings of the channels from "Untitled thru Untitled 14" to real names

# 3.6) Create column or an expression that will identify Day & Night
