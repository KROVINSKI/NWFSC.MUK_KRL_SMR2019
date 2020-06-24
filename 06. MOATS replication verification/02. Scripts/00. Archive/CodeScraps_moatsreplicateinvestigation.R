##Hello World

#*********************************
## Version Check
#********************************* 
R.version


## Author: OA Lab, NWFSC
## Title: Aquarium Temperature Investigation: Temperature SubSelectScript.R 
## Date: May 2020

# R script below is a collection of items from the temperature subselect investigation






#*********************************
## X.) Sections
#*********************************

# trying to filter moats
# this is from when I just used filter
filter(aTemperature>= 5 & aTemperature<=30) %>%
filter(ValidationFlag<= .250) %>%
filter(treatment %in% c("current", "allchange", "hightemperature")) %>%
filter(period != "other")
# Error: Result must have length 1105183, not 456364



#filteredFrame$treatment <- droplevels(filteredFrame$treatment)

# * * * * * * * * * * * * * * * *
## duplicates observed in "dml"  
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
# # [1] 1673352      11
# dim(dml) 
# uniqueDml <- unique(dml)
# dim(uniqueDml) 
# # [1] 1137880      11
# dml <- uniqueDml
# 
# 


#*********************************
## X.) Summary - group by
#*********************************


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





#Plots 


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




#simple violin plot with ggplot, note that day and night are not filtered
# pu <- ggplot(filteredFrame, aes(x=filteredFrame$aTemperature, y=filteredFrame$treatment)) + 
#   geom_violin() + geom_hline(yintercept = meanCdmldatemp[which(treatment == "current" & period == "day",)]$`mean(aTemperature)`)


#display the plot
# pu

# Rotate the violin plot
# pu + coord_flip()

# well that stinks











