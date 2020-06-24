###Code for assessing krill survival 

#load libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(survival)
library(survminer)

#set directory, read in file
setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/01. Survival")

# 1 pound sign Paul McElhany comments
## 2 pound signs Kate Rovinski comments


d <- read_excel("Krill Data.xlsx")
View(d)


#mess with date time format?
##2020.03.26 POSIX tool used to correct around time zones. 
##The POSIXlt class stores date/time values as a list of components (hour, min, sec, mon, etc.) 
##making it easy to extract these parts.
## The default format is a four digit year, followed by a month, then a day, separated by either dashes or slashes. The following example shows some examples of dates which as.Date will accept by default:
## as.Date('1915-6-16')

d$DateOfPresPOSIX <- as.POSIXlt.character("2019-9-11")

##Day variable is created. The is to better survival per day along the course of the 49day study.
d$Day <- difftime(d$Date ,d$DateOfPresPOSIX , units = c("days"))
##Whole, discrete days are needed
d$Day <- round(d$Day)


##creating an empty dataframe with no/zero values attached the variables.
##the variable type are defined, the variable Event is being created
df <- data.frame(Moats = character(0), Day  = numeric(0),
                 Event = numeric(0))

#run through MOATS?
## Loop created to create Event per the "Day" variable across MOATs
##Events are mortalities, a loop is created to cycle through the list of observation made over the 49day treatment
##Using a loop through the dataset to create the variable "Morts"
##is.na, not applicable- conditional statement if missing data, no MORT give value of "0"
## if there is a mort value of "1" is given
## ! tool means opposite
for(i in 1:length(d$Date)){
  if(!is.na(d$Morts[i]) && d$Morts[i] > 0){
    for (j in 1:d$Morts[i]){
      dTemp <- data.frame(Moats = d$Moats[i], Day = d$Day[i], Event = 1)
      df <- rbind(df,dTemp)
    }
  }
}

totalDead <- sum(d$Morts, na.rm = TRUE)


#MOATs were stocked with 85-80 krill
##MOATs 09 was not used during any course of the study
moatStart <- data.frame(Moats = c(1:8,10:13), nStart = c(85,85,80,80,80,80,80,80,80,80,80,80))

## Loop created to create a dataframe with observed and expected krill per Moats per the "Day" variable
##Moat Start alive has reduced number of krill per MOATs following the acclimination period
##alive oberserved decribes krill counted at the end of the study.
##Loss between the two numbers attributed 
moatStart$aliveExpected <- c(44,33,23,37,49,24,38,26,52,27,28,46)
moatStart$aliveObserved <- c(28,21,11,11,28,17,26,16,31,8,16,15)
dx <- read.csv("pointsdon'tmatter.csv")

for(i in 1:length(dx$Day)){
      dTemp <- data.frame(Moats = dx$Moats[i], Day = d$Day[i], Event = 1)
      df <- rbind(df,dTemp)
}


write.csv(df,"df.csv", 
          row.names=FALSE)

##Writing a loop to row bind Moat Start Numbers with the Day variable 
##length of of the index is the length of "moatStart", 12 observations per the 12 Moats
##New variable created "Moats" 
##The loop is going through the list of observations per Moats to create variables
##moatID created by running the loop - same number of rows as moatStart
##nAlive created by running the loop - same number of rows as moatStart
## Replicate Tool -"rep" replicates the values in "x". It is a generic function
##dataframe function "na.rm" refers to the 'logical parameter', tells the function remove/ no.remove NA values from the calculation. It literally means NA remove. Any day that has a na, null value remove that "day"
## df will have the same number of rows due to Moats- row bind will work
for(i in 1:length(moatStart$Moats)){
  moatID <- moatStart$Moats[i]
  nAlive <- moatStart$aliveObserved[i]
  dTemp <- data.frame(Moats = rep(moatID, nAlive), Day = rep(max(d$Day, na.rm = TRUE),nAlive),
                      Event = rep(0, nAlive))
  df <- rbind(df,dTemp)
}

##the "surv" tool, tool used to create a "survival object" 
##surv tool -- is comprised of the following elements/agruments: time, time2, event, type=(_), origin=0
##time -- for right censored data, this is the follow up time- the time we are allowing the study to continue- we didn't allow the study to go until each krill died. For interval data, the first argument is the starting time for the interval.
##time2 -- ending time of the interval for interval censored or counting process data only. Intervals are assumed to be open on the left and closed on the right - (start, end) but not applicable to how the tool was used in this investigation
##event -- The status indicator, 'normally' 0=alive, 1=dead. Other choices are T/F (TRUE = death) or 1/2 (2=death). For interval censored data, the status indicator is 0=right censored, 1= event at time, 2=left censored, 3=interval censored. 
##type -- character string specifying the type of censoring. Possible values are "right", "left", "counting", "interval", or "interval2". The default is "right" or "counting"

surv <- Surv(time = df$Day, event = df$Event, type = "right")
sf <- survfit(surv ~ Moats, df)
summary(sf)
print(ggsurvplot(sf, data = df))

#survforacclimationperiod
afteraccl <- df[df$Day>11,]
#Add treatment column, assigning treatments to corresponding MOATS
afteraccl$treatment <- ""
afteraccl$treatment[afteraccl$Moats==4| afteraccl$Moats==5] <- "Ambient"
afteraccl$treatment[afteraccl$Moats==1 | afteraccl$Moats==6|afteraccl$Moats==11] <- "HighTemp"
afteraccl$treatment[afteraccl$Moats==3 | afteraccl$Moats==7| afteraccl$Moats==10|afteraccl$Moats==12] <- "Current"
afteraccl$treatment[afteraccl$Moats==2 | afteraccl$Moats==8|afteraccl$Moats==13] <- "AllChange"
#survival according to MOATS and treatment
surv <- Surv(time = afteraccl$Day, event = afteraccl$Event, type = "right")
sf2 <- survfit(surv ~ Moats, afteraccl)
sf3 <- survfit(surv ~ treatment, afteraccl)
summary(sf2)
#Plot survival according to MOATS and treatment, either with (TRUE) or without (FALSE) confidence intervals
(ggsurvplot(sf2, data = afteraccl, conf.int = FALSE, xlim=c(10,40)))
(ggsurvplot(sf3, data = afteraccl, conf.int = FALSE, xlim=c(10,40)))

#Subsetting funtional MOATS from problematic (M11, M3, due to large die-off)). Probably prettier way to do this
afteraccl2 <- subset(afteraccl, Moats==1 | Moats==2 |Moats==6 | Moats==8 | Moats==10 | Moats==12 | Moats==13)
#survival according to MOATS and treatment
surv <- Surv(time = afteraccl2$Day, event = afteraccl2$Event, type = "right")
sf4 <- survfit(surv ~ Moats, afteraccl2)
sf5 <- survfit(surv ~ treatment, afteraccl2)
(ggsurvplot(sf4, data = afteraccl2, conf.int = TRUE, xlim=c(10,40)))
(ggsurvplot(sf5, data = afteraccl2, conf.int = TRUE, xlim=c(10,40)))



