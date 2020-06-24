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


# 7.) Cleaning up dml "Cdml"
# 8.) Framing the filters - Cdml
# 9.) Temperature Jump flag logic
# 10.) Calculating averages by treatment & day/night
# 11.) Summary & Group by Cdml
# 12.) Plots
# 13.) Duration between conditions- time above/below mean
#     *this is the histogram section
# X.X) Share-able tibbles... for putting on stackoverflow :( 


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


dml <- read.csv(file = "M01thruM13_n17_dml.csv)



#*********************************
## 7.) Cleaning up dml  
#*********************************
                
                
                
# Creating a new dataframe cleaned with the various filters below
# checking on the variables inside dml
                
 ## Cleaned-Up dml = Cdml
                
names(dml)
                
# # Changes to be made the dataframe by variable
# #[1] "moats" "M03", "M04", "M05", "M09", "M11" to be filtered out- all these MOATs were dropped from the study
# #[2] "dateTime" - no changes
# #[3] "aTemperature" - no changes 
# #[4] "sTemperature" - no changes    
# #[5] "pH" - no changes
# #[6] "DO"- no changes
# #[7] "salinity" - no changes 
# #[8] "treatment" - dropping the listed MOATs will eliminate the "broken_and_ambientbroken" treatment
# #[9] "period" - filtering out "other"
# #[10] "ObservationDate" - no changes
# #[11] "ObservationTime" - no changes, note that each observation could be spaced 6.8minutes apart
#                 
                
Cdml <- dml %>% filter(!moats %in% c("M03", "M04", "M05", "M11")) %>%
                filter(aTemperature>= 5 & aTemperature<=30) %>%
                filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
                filter(period != "other")
                
write.csv(dml, file = "2020.05.07_Cdml.csv", row.names = FALSE)
                
                
                
#*********************************
## 8.) Framing the filters - Cdml
#*********************************
                
                
#want to hide the names of MOATs "cleaned out" of Cdml 
#Can't use filter to truely hide/remove the elements you want gone
#filterFrame will truely removed- use & and include the drop levels portion  
                
filteredFrame = filter(Cdml,
                !moats %in% c('M03', "M04", "M05", "M11") & 
                (aTemperature>= 5 & aTemperature<=30) &
                treatment %in% c("current", "allchange", "hightemperature") &
                period != "other")
                
#this is from when I just used filter
#filter(aTemperature>= 5 & aTemperature<=30) %>%
#filter(ValidationFlag<= .250) %>%
#filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
#filter(period != "other")
# Error: Result must have length 1105183, not 456364
                
filteredFrame$moats <- droplevels(filteredFrame$moats)
                
filteredFrame$moats <- droplevels(filteredFrame$treatment)
                
                write.csv(filteredFrame, file = "2020.05.07_Cdml_filteredFrame.csv", row.names = FALSE)
                
                
                
                
                #*********************************
                ## 9.) Temperature Jump flag logic
                #*********************************
                #this make a new column with the different between 2 adjacent times
                #first sort the data so you are comparing adjacent times
                #then add new column
                # the diff() function computes x[i+lag] - x[i], the default value is lag = 1
                
                #the result of the diff() function has a length that is the length of the original vector - lag
                #therefore need to fill the new variable, deltaTempLag1,
                
                # with c(0,abs(diff(aTemperature))) it is right length and first value (with no valid diff) is zero
                
                
                
                
                # Writing flag logic to determine if an observation is valid or not
                # creating a new object to be a validation flag under name deltaTempLag1
                
                # will create a numerical value that will represent those observations to be filtered out
                
                
                Cdml <- filteredFrame %>% arrange(moats, dateTime)%>% 
                mutate(deltaTempLag1 = c(0,abs(diff(aTemperature))))
                
                dim(Cdml)
                # 12th variable is created
                
                #the diff value comparing the last time in M01 to the first time in M02 (etc.) is not valid, 
                #so set those to zero
                Cdml$deltaTempLag1[lag(Cdml$moats) != cdml$moats] <- 0
                #this shows all the rows that jump more that 1 degree from previous row (ingoring rows that are transition from one moats to another)
                Cdml %>% filter(deltaTempLag1 >1)
                
                ##ggplot check
                ggplot(subset(Cdml[Cdml$treatment == "current", ], period %in% ("night")), 
                aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=))
                
                #Creating another column of variables to be able to graph the temperature jumps 
                Cdml <- Cdml %>% mutate(tDeltaThreshold = if_else(deltaTempLag1 > 0.3, TRUE, FALSE))
                
                dim(Cdml)
                
                
                write.csv(filteredFrame, file = "2020.05.07_Cdml_FlaggedforTempjumps.3C.csv", row.names = FALSE)
                
                
                #*********************************
                ## 9.1) Determining outliers
                #*********************************
                
                
                #ggplot(subset(Cdml[Cdml$treatment == "allchange", ], period %in% ("night")), 
                #       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=))
                
                ggplot(subset(Cdml[Cdml$treatment == "current", ], period %in% ("night")), 
                aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=))
                
                #ggplot(subset(Cdml[Cdml$treatment == "hightemperature", ], period %in% ("night")), 
                #       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=))
                
                
                #Moats 12 "M12" appears to have suspected disconnected time periods with the server
                
                
                
                
                #*********************************
                ## 10.) Calculating averages by treatment & day/night
                #*********************************
                
                
                
                # These averages will be the yintercepts in plots
                # example of what to put into with ggplot " + geom_hline(yintercept = dtemperatur$`mean(aTemperature)`)   " 
                
                
                # Night Period
                subsetNightaTemp <- subset(filteredFrame, period == "night" & aTemperature >0,
                select = c( moats, dateTime, treatment, aTemperature ))
                
                # Day Period
                subsetDayaTemp <- subset(filteredFrame, period == "day" & aTemperature >0,
                select = c( moats, dateTime, treatment, aTemperature ))
                
                
                
                #Current Treatment Averages--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--CUR--
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
                
                
                #All Change Treatment Averages --All_Chg--All_Chg--All_Chg--All_Chg--All_Chg--All_Chg--All_Chg--
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
                
                
                
                #High Temperature Treatment Averages -- HiTemp -- -- HiTemp -- HiTemp -- HiTemp -- HiTemp -- HiTemp 
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
                
                # Review of 6 new variables - 6 New Variables - - 6 New Variables - 6 New Variables - 6 New Variables 
                
                avg_allchgDayaTemp
                avg_allchgNightaTemp
                avg_curDayaTemp
                avg_curNightaTemp
                avg_hitempDayaTemp
                avg_hitempNightaTemp
                
                
                #Table of Averages -- Table of Averages -- Table of Averages -- Table of Averages -- Table of Averages --
                
                
                meanCdmldatemp <- filteredFrame %>% group_by(treatment, period) %>% summarise(mean(aTemperature))
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
                
                
                #*********************************
                ## 11.) Summary & Group by Cdml
                #*********************************
                
                
                #  * * * * * * * * * * * * * * * * 
                # Group By Treatment
                #  * * * * * * * * * * * * * * * * 
                
                Cdml.summary <- Cdml %>% group_by(treatment) %>% 
                summarize(sd = sd(aTemperature, na.rm = TRUE), mean(aTemperature))
                Cdml.summary
                # Results
                # # A tibble: 3 x 3
                # treatment          sd     `mean(aTemperature)`
                # <chr>           <dbl>                <dbl>
                # 1 allchange       0.556                 13.3
                # 2 current         0.528                 11.4
                # 3 hightemperature 0.547                 13.2
                
                
                #  * * * * * * * * * * * * * * * * 
                # Group By Period (Night)
                #  * * * * * * * * * * * * * * * * 
                #Cdml.night.summary <- Cdml %>% group_by(treatment) %>% 
                #  filter(period == "night") %>%
                #  summarize(sd = sd(aTemperature, na.rm = TRUE), mean(aTemperature))
                #Cdml.night.summary
                # A tibble: 3 x 3
                # treatment          sd     `mean(aTemperature)`
                #<chr>           <dbl>                <dbl>
                # 1 allchange       0.244                 13.9
                # 2 current         0.303                 12.0
                # 3 hightemperature 0.312                 13.8
                
                
                #  * * * * * * * * * * * * * * * * 
                # Group By Period (Day)
                #  * * * * * * * * * * * * * * * * 
                #Cdml.day.summary <- Cdml %>% group_by(treatment) %>% 
                #  filter(period == "day") %>%
                #  summarize(sd = sd(aTemperature, na.rm = TRUE), mean(aTemperature))
                #Cdml.day.summary
                # A tibble: 3 x 3
                # treatment          sd       `mean(aTemperature)`
                # <chr>           <dbl>                <dbl>
                # 1 allchange       0.375                 13.0
                # 2 current         0.349                 11.1
                # 3 hightemperature 0.364                 12.9
                
                
                
                #  * * * * * * * * * * * * * * * * 
                # Cdml Day Summary, group by, mutate
                #  * * * * * * * * * * * * * * * * 
                Cdml.daynight.summary <- Cdml %>% group_by(treatment, period) %>%
                summarize(sd = sd(aTemperature, na.rm = TRUE), 
                mean = mean(aTemperature, na.rm = TRUE), 
                median = median(aTemperature, na.rm = TRUE),
                IQR = IQR(aTemperature, na.rm = TRUE),
                n = n()) %>%
                mutate(se = sd/sqrt(n)) %>%
                mutate(ci = se*1.96)
                
                
                write.csv(Cdml.daynight.summary, "2020.05.07_Cdml_daynight_summary.csv")
                
                
                Cdml.day.summary <- Cdml %>% group_by (treatment) %>%
                filter(period == "day") %>%
                summarize(sd = sd(aTemperature, na.rm = TRUE), 
                mean = mean(aTemperature, na.rm = TRUE), 
                n = n()) %>%
                mutate(se = sd/sqrt(n)) %>%
                mutate(ci = se*1.96)
                
                
                write.csv(Cdml.day.summary, file = "2020.05.07_Cdml.day.summary")
                
                #  * * * * * * * * * * * * * * * * 
                # Cdml Night Summary, group by, mutate
                #  * * * * * * * * * * * * * * * * 
                Cdml.night.summary <- Cdml %>% group_by(treatment) %>%
                filter(period == "night") %>%
                summarize(sd = sd(aTemperature, na.rm = TRUE), 
                mean = mean(aTemperature, na.rm = TRUE), 
                n = n()) %>%
                mutate(se = sd/sqrt(n)) %>%
                mutate(ci = se*1.96)
                
                
                write.csv(Cdml.day.summary, file = "2020.05.07_Cdml.night.summary")
                
                
                
                
                
                #  / / / / / / / /
                # Simple Plots boxplots followed by violin plots
                # / / / / / / / /
                
                
                
                #*********************************
                ## 12.) Plots
                #*********************************
                
                #  / / / / / / / /
                # Boxplots 
                # / / / / / / / /
                
                
                
                #1. Simple Boxplot aTemp by moats
                #boxplot(aTemperature~moats, Cdml)
                
                ggplot(Cdml, aes(treatment, aTemperature)) +
                geom_jitter(color = "grey") +
                geom_jitter(data = d.insitu, aes(treatment, aTemperature, color = "yellow")) +
                geom_jitter(data = n.insitu, aes(treatment, aTemperature, color = "orange")) +
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
                ggtitle("All Treatments") +
                theme_bw() 
                
                
                
                
                
                #2. Simple plot boxplot (treatment ~ aTemp ) with intercepts
                p_allchg_d_yinter <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ], 
                period %in% ("day")), 
                aes(x=moats, y=aTemperature, colour=treatment)) + 
                stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                geom_hline(yintercept = avg_allchgDayaTemp) +
                geom_boxplot() +
                ggtitle("All Change Day Period")
                
                
                p_allchg_d_yinter
                
                #3.  Boxplot (treatment ~ aTemp ) with intercepts & Facets
                p_allchg_d_facet <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ]),
                aes(x=moats, y=aTemperature, colour=moats)) +
                geom_hline(yintercept = avg_allchgDayaTemp) +
                geom_hline(yintercept = avg_allchgNightaTemp) +
                geom_boxplot() +
                stat_summary(fun=mean, geom="point", size=2, color="red")  +
                facet_grid(period~moats) +
                ggtitle("All Change Day Period")
                
                
                p_allchg_d_facet
                
                
                # Histogram plot 
                Cdml %>% ggplot(data=.)+geom_histogram(aes(x=aTemperature), binwidth = .05) + 
                xlim(5,30) +
                ylim(0, 10000)
                
                
                
                # 
                #  # this plots several summary stats on one chromatically disturbing graph
                #  # the box plot shows median and quantiles
                #  # the point and error bars show mean and stats relative to mean
                #  # confidence intervals are so small partly because there are so many data points
                #  # the notch in the box plot essential 95% ci on median and is so small you can hardly see it
                #  # as in all things R, there are multiple ways to make this including the stat_summary()
                #  # using stat_summary() would not even require making the Cdml.day.summary data frame
                #  #however we need the table anyway to to provde info for the paper
                #  ggplot(Cdml, aes(treatment, aTemperature)) +
                #    geom_jitter(color = "grey") +
                #    geom_jitter(data = d.insitu_aTemp, aes(Treatment, Temp)) +
                #    geom_jitter(data = n.insitu_aTemp, aes(Treatment, Temp)) +
                #    geom_boxplot(notch = TRUE, outlier.shape = NA, colour = "green") +
                #    geom_point(data = Cdml.daynight.summary, aes(x=treatment, y=mean), size=5, color = "purple") + 
                #    geom_errorbar(data = Cdml.daynight.summary, 
                #                  aes(x=treatment, y=mean, ymin = mean-sd, ymax = mean+sd), 
                #                  color = "blue") +
                #    geom_errorbar(data = Cdml.daynight.summary,
                #                  aes(x=treatment, y=mean, ymin = mean-ci, ymax = mean+ci),
                #                  colour = "red") +
                #    facet_wrap(~period) +
                #    ggtitle("All Treatments") +
                #    theme_bw() 
                #  
                #  
                #  geom_boxplot() + 
                #    facet_grid(period~moats)
                #  
                # 
                #  #2. Simple plot boxplot filtering for period and plotting against treatment ~ aTemp
                #  p_allchg_d_jitter_errorbar <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ], 
                #                                              period %in% ("day")), 
                #                                       aes(x=moats, y=aTemperature, colour=treatment)) + 
                #    stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                #    geom_jitter(aes(colour = tDeltaThreshold)) +
                #    geom_hline(yintercept = avg_allchgDayaTemp) +
                #    geom_boxplot() +
                #    geom_errorbar(aes(ymin=aTemperature-(Cdml.day.summary[1,2])), 
                #                  (ymax=(aTemperature+(Cdml.day.summary[1,2]))) +
                #                    ggtitle("title")
                #                  
                #                  
                # p_allchg_d_jitter_errorbar
                
                
                
                # Sample for error bars geom_errorbar(aes(ymin=len-sd, ymax=len+sd)
                
                #display the plot
                p_allchg_d_jitter
                
                
                
                
                #X.  Boxplot (treatment ~ aTemp ) with intercepts & Facets
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
                
                
                
                #simple violin plot with ggplot, note that day and night are not filtered
                p <- ggplot(filteredFrame, aes(x=filteredFrame$aTemperature, y=filteredFrame$treatment)) + 
                geom_violin()
                
                
                #display the plot
                p
                
                # Rotate the violin plot
                p + coord_flip()
                
                # saved as  "Rplot.boxplot_atemp_by_treatment_period_notfiltered"
                
                
                
                # / / / / / / / /
                # Plot with y intercept
                # / / / / / / / /
                
                
                
                #simple violin plot with ggplot, note that day and night are not filtered
                # pu <- ggplot(filteredFrame, aes(x=filteredFrame$aTemperature, y=filteredFrame$treatment)) + 
                #   geom_violin() + geom_hline(yintercept = meanCdmldatemp[which(treatment == "current" & period == "day",)]$`mean(aTemperature)`)
                
                
                #display the plot
                # pu
                
                # Rotate the violin plot
                # pu + coord_flip()
                
                # well that stinks
                
                
                # / / / / / / / 
                # simple plots with stats 
                # / / / / / / /
                
                sps <- ggplot(filteredFrame, aes(x=filteredFrame$aTemperature, y=filteredFrame$treatment)) +
                geom_violin() + 
                geom_boxplot(width= 0.1) +
                stat_summary(fun=median, geom="point", size=2, color="red") +
                coord_flip()
                
                sps
                
                
                
                #simple violin plot with ggplot, to filter day and night
                
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
                p_allchg_n_jitter
                
                
                p_allchg_d_jitter <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ], 
                period %in% ("day")), 
                aes(x=moats, y=aTemperature, colour=treatment)) + 
                stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                geom_jitter(aes(colour = tDeltaThreshold)) +
                geom_hline(yintercept = avg_allchgDayaTemp) +
                geom_boxplot() +
                ggtitle("Day Period - All Change Treatment- Temp Jumps (.3C)")
                
                
                #display the plot
                p_allchg_d_jitter
                
                
                p_allchg_rclub  <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ]),  
                aes(x=moats, y=aTemperature, colour=treatment)) + 
                #                                  stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                #                                 geom_jitter(aes(colour = tDeltaThreshold)) +
                #                                  geom_hline(yintercept = avg_allchgDayaTemp) +
                geom_boxplot(aes(fill = period)) 
                #                                  ggtitle("Day Period - All Change Treatment- Temp Jumps (.3C)")
                
                
                #display the plot
                p_allchg_rclub
                
                
                
                p_allchg_rclub2  <- ggplot(subset(Cdml[Cdml$treatment == "allchange", ]),  
                aes(x=moats, y=aTemperature, colour=treatment)) + 
                #                                  stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                #                                 geom_jitter(aes(colour = tDeltaThreshold)) +
                #                                  geom_hline(yintercept = avg_allchgDayaTemp) +
                geom_boxplot() + 
                facet_grid(period~moats)
                #                                  ggtitle("Day Period - All Change Treatment- Temp Jumps (.3C)")
                
                
                #display the plot
                p_allchg_rclub2
                
                
                
                #plotting with jitters set to color threshold
                
                
                p_jitter1 <- ggplot(Cdml, aes(moats, aTemperature, colour=treatment), 
                period %in% ("day"))  +
                geom_jitter(aes(colour = tDeltaThreshold))+
                geom_boxplot()
                p_jitter1
                
                
                
                
                p_jitter1 <- Cdml %>% 
                filter(period == "night") %>%
                ggplot(aes(moats, 
                aTemperature, 
                colour=treatment))  +
                geom_jitter(aes(colour = tDeltaThreshold, alpha=0.1)) +
                geom_boxplot(outlier.color = "dark green")  +
                #stat_summary(fun.data = mean_se, geom = "errorbar") +
                ggtitle("Night Period Across Treatments")
                p_jitter1
                
                
                
                
                pu_jitter2 <- Cdml %>% 
                filter(period == "night") %>%
                group_by(treatment) %>% 
                summarize(sd = sd(aTemperature, na.rm = TRUE),) %>% 
                ggplot(aes(moats, 
                aTemperature, 
                colour=treatment))  +
                geom_jitter() +
                geom_boxplot()  +
                geom_errorbar(
                aes(x=treatment,
                ymin=aTemperature-sd,
                ymax=aTemperature+sd),
                width=0.4,
                colour="orange",
                alpha=0.9, size=1.3) +
                ggtitle("Night Period Across Treatments")
                pu_jitter2
                
                
                pu_jitter2  
                
                
                
                
                
                
                #creating plots to show the odd points in M12
                p_M12cur_d <- ggplot(subset(Cdml[Cdml$moats == "M12", ], 
                period %in% ("day")), 
                aes(x=moats, y=aTemperature, colour=treatment)) + 
                stat_summary(fun=mean, geom="point", size=2, color="red")  + 
                geom_jitter(shape=16, position=position_jitter(0.2), 
                color="blue") + 
                #geom_jitter(shape=16, position=position_jitter(0.2), 
                #   (color= (Cdml, subset(Cdml$deltaTempLag1 >.3))="yellow")) + 
                geom_violin() +
                geom_boxplot(width= 0.1) +
                geom_hline(yintercept = avg_curDayaTemp)
                
                
                #display the plot
                p_M12cur_d
                
                
                
                
                
                
                #Creating plots to present
                
                p_M12_day <- ggplot(subset(Cdml[Cdml$moats == "M12", ], 
                period %in% ("day"), 
                aes(x=moats, y=aTemperature)) + 
                geom_jitter(shape=16, position=position_jitter(0.2), 
                (colour=filter(deltaTempLag1 >.3)) +
                #geom_hline(yintercept = (avg_allchgNightaTemp)) +
                geom_hline(yintercept = (avg_curDayaTemp)) +
                #geom_hline(yintercept = (avg_hitempNightaTemp)) +
                #geom_violin() +
                #geom_boxplot(width= 0.1))
                
                
                
                #display the plot
                p_M12_day
                
                
                
                
                #saved as Rplot.boxplot_atemp_by_night_violin.boxplot.colorbytreatment 
                
                
                
                
                # Messing around with vioplot 
                # Creating data for the violin plot
                # Ctreatment <- c(filteredFrame$treatment)
                # Cvalue <- c(filteredFrame$aTemperature )
                # Cdmldata <- data.frame(Ctreatment,Cvalue)
                
                # Drawing the plot
                # with(data , vioplot( 
                #  value[treatment=="allchange"] , value[treatment=="current"], value[treatment=="hightemperature"],  
                #  names=c("allchange","current","hightemperature") 
                # ))
                
                
                
                #Paul's Example
                # dml %>% filter(aTemperature > 0 & aTemperature < 25) %>% 
                # ggplot(aes(treatment, aTemperature)) +
                #    geom_jitter() +
                #    geom_boxplot(aes(colour = period), outlier.shape = NA) +
                #    theme_bw()
                #    geom_violin(aes(colour = period)) +
                #    geom_hline(yintercept = dtemperatur$`mean(aTemperature)`) 
                
                # 
                # #*********************************
                # ## XXXX.) Old Plos  
                # #*********************************
                # ## 8.0 Simple plot
                # ## 8.1 Expression for the plot, when selecting one Moats
                # 
                # 
                # 
                # ggplot(ldf, aes(x=dateTime, y=aTemperature)) +
                #   geom_point(aes(colour=moats))
                # 
                # ggplot(dTreat, aes(x=dateTime, y=aTemperature)) +
                #   geom_point(aes(colour=moats))
                # 
                # ggplot(dml, aes(x=dateTime, y=aTemperature)) +
                #   geom_point(aes(colour=moats))
                # 
                # 
                # #how I am writing this to where it's recognizing dml as a function dml
                # 
                # #example name : Rplot.TimeSeriesplt_M01_atemp_subsample_history_bothperiods
                # 
                # ggplot(dml[dml$moats == "M13",], aes(x = dateTime, y=aTemperature)) + geom_point(aes(colour=moats))
                # 
                # ##ggplot(dTreat$moats, aes(dml$dateTime), fill= dml$aTemperature, colour= moats))
                # 
                # #treatment names
                # factor(dTreat$treatment)
                # #Levels:  allchange ambient current hightemperature
                # # "allchange"
                # # "ambient"
                # # "current"
                # # "hightemperature"
                # 
                # # commands for each treatment type are listed below for ease of operator
                # 
                # 
                # 
                # 
                # 
                # 
                # # O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0
                # #                         Night Period Analysis
                # # O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0
                # 
                # # all MOATs per treatment
                # 
                # # all change 
                # # Standard Treatment Plot (with standardize Y axis 10,15)
                # # Saved as Rplot.TimeSeries_allchg_atemp_subsample_history_night_ylim
                # 
                # ggplot(subset(dTreat[dTreat$treatment == "allchange", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=)) + ylim (10, 16) 
                # 
                # # Saved as Rplot.TimeSeries_allchg_atemp_subsample_history_night_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "allchange", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = 0.95)
                # 
                # # ambient
                # #   Standard Treatment Plot (with standardize Y axis 10,15)
                # #   Saved as Rplot.TimeSeries_ambient_subsample_history_night_ylim
                # ggplot(subset(dTreat[dTreat$treatment == "ambient", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10, 16) 
                # 
                # # Saved as Rplot.TimeSeries_ambient_subsample_history_night_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "ambient", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # # Current
                # #   Standard Treatment Plot (with standardize Y axis 10,15)
                # #   Saved as Rplot.TimeSeries_current_subsample_history_night_ylim
                # ggplot(subset(dTreat[dTreat$treatment == "current", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10, 16) 
                # 
                # 
                # # Saved as Rplot.TimeSeries_current_subsample_history_night_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "current", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # # hightemperature
                # #   Standard Treatment Plot (with standardize Y axis 10,15)
                # #   Saved as Rplot.TimeSeries_hightemperature_subsample_history_night_ylim
                # ggplot(subset(dTreat[dTreat$treatment == "hightemperature", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10, 16) 
                # 
                # # Saved as Rplot.TimeSeries_hightemperature_subsample_history_night_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "hightemperature", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # #plots for just those moats's animals we plan to send off for lipids
                # 
                # 
                # #All Change for lipids
                # #   Saved as Rplot.TimeSeries_allchange_lipids_subsample_history_night_ylim
                # ggplot(subset(ldf[ldf$treatment == "allchange", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10.5, 15.5) 
                # 
                # #   Saved as Rplot.TimeSeries_allchange_lipids_subsample_history_night_geom
                # ggplot(subset(ldf[ldf$treatment == "allchange", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # #ggplot(subset(ldf[ldf$treatment == "ambient", ], period %in% ("night")), 
                # #       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats))
                # 
                # 
                # #Current for lipids
                # #   Saved as Rplot.TimeSeries_current_lipids_subsample_history_night_ylim
                # ggplot(subset(ldf[ldf$treatment == "current", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10.5, 15.5) 
                # 
                # #   Saved as Rplot.TimeSeries_current_lipids_subsample_history_night_geom
                # ggplot(subset(ldf[ldf$treatment == "current", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # #highTemperature
                # # Saved as Rplot.TimeSeries_hightemperature_lipids_subsample_history_night_ylim
                # ggplot(subset(ldf[ldf$treatment == "hightemperature", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats))+ ylim (10.5, 15.5) 
                # 
                # # Saved as Rplot.TimeSeries_hightemperature_lipids_subsample_history_night_geom
                # ggplot(subset(ldf[ldf$treatment == "hightemperature", ], period %in% ("night")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # 
                # 
                # 
                # 
                # # O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0
                # #                         Day Period Analysis
                # # O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0O0
                # 
                # 
                # # all MOATs per treatment
                # 
                # # all change 
                # # Standard Treatment Plot (with standardize Y axis 10,15)
                # # Saved as Rplot.TimeSeries_allchg_atemp_subsample_history_day_ylim
                # 
                # ggplot(subset(dTreat[dTreat$treatment == "allchange", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=)) + ylim (10, 16) 
                # 
                # # Saved as Rplot.TimeSeries_allchg_atemp_subsample_history_day_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "allchange", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = 0.95)
                # 
                # # ambient
                # #   Standard Treatment Plot (with standardize Y axis 10,15)
                # #   Saved as Rplot.TimeSeries_ambient_subsample_history_day_ylim
                # ggplot(subset(dTreat[dTreat$treatment == "ambient", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10, 16) 
                # 
                # # Saved as Rplot.TimeSeries_ambient_subsample_history_day_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "ambient", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # # Current
                # #   Standard Treatment Plot (with standardize Y axis 10,15)
                # #   Saved as Rplot.TimeSeries_current_subsample_history_day_ylim
                # ggplot(subset(dTreat[dTreat$treatment == "current", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10, 16) 
                # 
                # 
                # # Saved as Rplot.TimeSeries_current_subsample_history_day_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "current", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # # hightemperature
                # #   Standard Treatment Plot (with standardize Y axis 10,15)
                # #   Saved as Rplot.TimeSeries_hightemperature_subsample_history_day_ylim
                # ggplot(subset(dTreat[dTreat$treatment == "hightemperature", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10, 16) 
                # 
                # # Saved as Rplot.TimeSeries_hightemperature_subsample_history_day_geosmooth
                # ggplot(subset(dTreat[dTreat$treatment == "hightemperature", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # #plots for just those moats's animals we plan to send off for lipids
                # 
                # 
                # #All Change for lipids
                # #   Saved as Rplot.TimeSeries_allchange_lipids_subsample_history_day_ylim
                # ggplot(subset(ldf[ldf$treatment == "allchange", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10.5, 15.5) 
                # 
                # #   Saved as Rplot.TimeSeries_allchange_lipids_subsample_history_day_geom
                # ggplot(subset(ldf[ldf$treatment == "allchange", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # #ggplot(subset(ldf[ldf$treatment == "ambient", ], period %in% ("day")), 
                # #       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats))
                # 
                # 
                # #Current for lipids
                # #   Saved as Rplot.TimeSeries_current_lipids_subsample_history_day_ylim
                # ggplot(subset(ldf[ldf$treatment == "current", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (9.5, 13.0) 
                # 
                # #   Saved as Rplot.TimeSeries_current_lipids_subsample_history_day_geom
                # ggplot(subset(ldf[ldf$treatment == "current", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # #highTemperature
                # # Saved as Rplot.TimeSeries_hightemperature_lipids_subsample_history_day_ylim
                # ggplot(subset(ldf[ldf$treatment == "hightemperature", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats))+ ylim (10.5, 15.5) 
                # 
                # # Saved as Rplot.TimeSeries_hightemperature_lipids_subsample_history_day_geom
                # ggplot(subset(ldf[ldf$treatment == "hightemperature", ], period %in% ("day")), 
                #        aes(x=dateTime, y=aTemperature)) + geom_smooth(aes(colour=moats)) + stat_smooth(level = .95)
                # 
                # 
                # 
                # ## 8.2) expression for the plot, when selecting all Moats
                # 
                # ## Selecting all MOATs used to send off for lipids analysis            
                # ggplot(ldf, aes(x=dateTime, y=aTemperature)) +
                #   geom_point(aes(colour=moats))
                # 
                # 
                # 
                # 
                #   
                # )
                
                
                
                #*********************************
                ## 13 .) Duration between conditions- time above/below mean   
                #*********************************
                
                
                #shown below is the naming convention for adding up observations around treatment mean 
                
                #                                           2C_abv_avg
                # -  -  -  -  -  -  -  -  -  -x < +2 degree difference 
                #                                           1C_abv_avg                                            
                # -  -  -  -  -  -  -  -  -  -x < +1 degree difference 
                #                                           abv_within_avg
                # ---------------------------------- Treatment Average
                #                                           blw_within_avg
                # -  -  -  -  -  -  -  -  -  -x < -1 degree difference 
                #                                           1C_blw_avg
                # -  -  -  -  -  -  -  -  -  -x < +2 degree difference 
                #                                           2C_blw_avg
                
                # Trying to represent these relationships to the mean with a histogram
                # 4 Histogram Attempts
                
                hstogrm_fig1 <- ggplot(Cdml, 
                aes( x=aTemperature, y= ObservationTime)) + 
                geom_histogram() +
                facet_grid(~treatment ~period)
                
                hstogrm_fig1
                
                
                hstogrm_fig2 <- ggplot(data= Cdml, mapping = aes(x= Cdml$aTemperature, y= Cdml$ObservationTime)) +
                geom_histogram() +
                facet_grid(~treatment ~period)
                
                hstogrm_fig2 + geom_col(aes(color=moats)
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                # creating a new column, new variable "avg_Delta_Condition"
                Cdml <- Cdml$avg_Delta_Condition
                
                
                dim(Cdml)
                
                # Create new split date and time columns
                
                #how to avoid writing six different expressions?
                
                Cdml <- Cdml %>% mutate(avg_Delta_Condtion=case_when(
                
                #all change
                (aTemperature >= avg_allchgDayaTemp+ 2) 
                & (aTemperature >= avg_allchgNightaTemp+ 2) ~"allchg_2C_abv_avg",
                
                (aTemperature >= avg_allchgDayaTemp+ 1) 
                & (aTemperature >= avg_allchgNightaTemp+ 1)  
                & (aTemperature <= avg_allchgDayaTemp+ 2) 
                & (aTemperature <=avg_allchgNightaTemp+ 2) ~"allchg_1C_abv_avg",
                
                & (aTemperature <= avg_allchgDayaTemp+ .999) 
                & (aTemperature >=avg_allchgNightaTemp+ .999) ~"allchg_within_abv_avg",
                
                (aTemperature <= avg_allchgDayaTemp- 1) 
                & (aTemperature <= avg_allchgNightaTemp- 1) 
                & (aTemperature >= avg_allchgDayaTemp+ 2) 
                & (aTemperature >=avg_allchgNightaTemp+ 2) ~"allchg_2C_blw_avg",
                
                (aTemperature <= avg_allchgDayaTemp- 2) 
                & (aTemperature <= avg_allchgNightaTemp- 2) ~"allchg_2C_blw_avg",
                # current conditions                          
                
                (aTemperature >= avg_curDayaTemp+ 2) 
                & (aTemperature >= avg_curNightaTemp+ 2) ~"cur_2C_abv_avg",
                
                (aTemperature >= avg_curDayaTemp+ 1) 
                & (aTemperature >= avg_curNightaTemp+ 1)  
                & (aTemperature <= avg_curDayaTemp+ 2) 
                & (aTemperature <= avg_curNightaTemp+ 2) ~"cur_1C_abv_avg",
                
                & (aTemperature <= avg_curDayaTemp+ .999) 
                & (aTemperature >=avg_curNightaTemp+ .999) ~"cur_within_abv_avg",
                
                (aTemperature <= avg_allchgDayaTemp- 1) 
                & (aTemperature <= avg_allchgNightaTemp- 1) 
                & (aTemperature >= avg_allchgDayaTemp+ 2) 
                & (aTemperature >=avg_allchgNightaTemp+ 2) ~"cur_2C_blw_avg",
                
                (aTemperature <= avg_allchgDayaTemp- 2) 
                & (aTemperature <= avg_allchgNightaTemp- 2) ~"cur_2C_blw_avg",
                
                # High Temperature conditions                          
                
                (aTemperature >= avg_hitempDayaTemp+ 2) 
                & (aTemperature >= avg_hitempNightaTemp+ 2) ~"hitemp_2C_abv_avg",
                
                (aTemperature >= avg_hitempDayaTemp+ 1) 
                & (aTemperature >= avg_hitempNightaTemp+ 1)  
                & (aTemperature <= avg_hitempDayaTemp+ 2) 
                & (aTemperature <= avg_hitempNightaTemp+ 2) ~"hitemp_1C_abv_avg",
                
                & (aTemperature <= avg_hitempDayaTemp+ .999) 
                & (aTemperature >=avg_hitempNightaTemp+ .999) ~"hitemp_within_abv_avg",
                
                (aTemperature <= avg_allchgDayaTemp- 1) 
                & (aTemperature <= avg_allchgNightaTemp- 1) 
                & (aTemperature >= avg_allchgDayaTemp+ 2) 
                & (aTemperature >=avg_allchgNightaTemp+ 2) ~"hitemp_2C_blw_avg",
                
                (aTemperature <= avg_allchgDayaTemp- 2) 
                & (aTemperature <= avg_allchgNightaTemp- 2) ~"hitmep_2C_blw_avg",                          
                TRUE ~"other"
                )
                
                
                #*********************************
                ## 13.1) Establishing Setpoints  
                #*********************************
                
                # Current Conditions "current", Day setpoint temperature = 11.00
                current_Day_setpt_aTemp <- 11.0
                # Current Conditions "current", Night setpoint temperature = 12.00
                current_Night_setpt_aTemp <- 12.0
                
                # High Temperature Conditions "hightemperature", Day setpoint temperature = 13.00
                hightemperature_Day_setpt_aTemp <- 13.0
                # High Temperature "hightemperature", Night setpoint temperature = 14.00
                hightemperature_Night_setpt_aTemp <- 14.0
                
                # All Change Conditions "allchange", Day setpoint temperature = 13.00
                allchange_Day_setpt_aTemp <- 13.0
                # All Change Conditions "allchange", Night setpoint temperature = 14.00
                allchange_Night_setpt_aTemp <- 14.0
                
                
                #*********************************
                ## X.X) Calculating Time from Setpoints  
                #*********************************
                
                
                #*********************************
                # X.X Share-able tibbles
                #*********************************
                library(datapasta)
                library(lubridate)
                library(tidyr)
                library(reprex)
                
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
                
                
                
                
                
                
                
                # #*********************************
                # ## X.X) Quick Stats  
                # #*********************************
                # 
                # 
                # 
                # ANOVA 
                # 
                # 
                # night.aov <- aov(formula = aTemperature ~ treatment + moats,
                #               data = subsetNightaTemperature)
                # summary(night.aov)
                # 
                # 
                # night.aov <- glm(dateTime ~ aTemperature + treatment,
                #                  data = subsetNightaTemperature)
                # summary(night.aov)
                # 
                # day.aov <- aov(formula = treatment ~ aTemperature,
                #                  data = subsetNightaTemperature)
                # summary(day.aov)
                
                
                
                
                
                
                #**********E*N*D*****************# 
                #*********************************
                ## the rest is junk code  
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
                
                
                
                
                
                
                #bit bobs and notions of code:
                ## Making a basic bubble plot
                #Q is the placeholder dataframe
                #Q <- ggplot(data, aes(x=day, y=value)) +
                #  geom_line() + 
                #  xlab("")
                # Q
                
                






