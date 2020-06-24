#there are no duplicates
#if no dups, then dup2 has 0 rows
dup2 <- Cdml[duplicated(Cdml),]
#if no dups, Cdml2 has same number of rows as Cdml
Cdml2 <- Cdml %>% distinct() 

#histogram scaled by time
#find duration of each observations in hours
durObsHours <- 6.8/60
# the count (number of  observations in each bin) is scale by time duration of each obs
# option to also include polygon
# could fix the vline to only show the value relevant for each plot
#vline could show acutal mean temperature, not just target
# each bin shows 0.2 degrees
p <- ggplot(Cdml, aes(aTemperature)) +
  geom_histogram(aes(y=..count..*durObsHours), binwidth = 0.2) +
  #   geom_freqpoly(aes(y=..count..*durObs)) +
  geom_vline(xintercept = c(11,12,13,14), colour = "red") +
  facet_wrap(vars(treatment, period), ncol = 2, scales = "fixed") +
  theme_bw()