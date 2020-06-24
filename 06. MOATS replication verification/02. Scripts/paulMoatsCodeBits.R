library(lubridate)
library(dplyr)

setwd("/Users/paul.mcelhany/Downloads")

dml <- read.csv(file = "M01thruM13moatslog.csv", stringsAsFactors = FALSE)

#subsample every 17th row (because prime numbers are cool)
dml <- dml %>% arrange(moats, dateTime) %>% filter(row_number() %% 17 == 0)

#make dateTime posixct
dml$dateTime <- as.POSIXct(dml$dateTime, format="%Y-%m-%d %H:%M:%OS")

#make new coumn with same date (Darwin's Bday) for all rows, but keeping the original time
#copy dateTime to new column
dml$time1809 <- dml$dateTime 
#now change the date with the date() lubridate function. This fucntion can be used to get or set the date
date(dml$time1809) <- "1809-02-12"

#define day and night start stop times on darwin's birthday
#I don't remember the real times so these are made up
dayStart <- as.POSIXct("1809-02-12 08:00:00")
dayStop <- as.POSIXct("1809-02-12 19:00:00")
nightStart <- as.POSIXct("1809-02-12 20:00:00")
nightStop <- as.POSIXct("1809-02-12 07:00:00")

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









