# #*********************************
# ## X.X) Boxplots 
# #*********************************

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




# Cdml.day.summary <- Cdml %>% group_by (treatment) %>%
#   filter(period == "day") %>%
#   summarize(sd = sd(aTemperature, na.rm = TRUE), 
#             mean = mean(aTemperature, na.rm = TRUE), 
#             n = n()) %>%
#   mutate(se = sd/sqrt(n)) %>%
#   mutate(ci = se*1.96)
# 
# 
# write.csv(Cdml.day.summary, file = "2020.05.07_Cdml.day.summary")
# 
# #  * * * * * * * * * * * * * * * * 
# # Cdml Night Summary, group by, mutate
# #  * * * * * * * * * * * * * * * * 
# Cdml.night.summary <- Cdml %>% group_by(treatment) %>%
#   filter(period == "night") %>%
#   summarize(sd = sd(aTemperature, na.rm = TRUE), 
#             mean = mean(aTemperature, na.rm = TRUE), 
#             n = n()) %>%
#   mutate(se = sd/sqrt(n)) %>%
#   mutate(ci = se*1.96)
# 
# 
# write.csv(Cdml.day.summary, file = "2020.05.07_Cdml.night.summary")











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
ggplot(subset(Cdml[Cdml$treatment == "allchange", ], period %in% ("night")), 
       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats, point=)) + ylim (10, 16) 
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
ggplot(subset(Cdml[Cdml$treatment == "hightemperature", ], period %in% ("day")), 
       aes(x=dateTime, y=aTemperature)) + geom_point(aes(colour=moats)) + ylim (10, 16) 
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









# #*********************************
# ## X.X) Histogram Plots 
# #*********************************
# #*********************************
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



# 
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
                        
                        
                        
                        
# #*********************************
# ## X.X) Quick Stats  
# #*********************************


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
