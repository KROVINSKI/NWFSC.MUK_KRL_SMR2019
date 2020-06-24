#this make a new column with the different between 2 adjacent times
#first sort the data so you are comparing adjacent times
#then add new column
# the diff() function computes x[i+lag] - x[i], the default value is lag = 1
#the result of the diff() function has a length that is the length of the original vector - lag
#therefore need to fill the new variable, deltaTempLag1,
# with c(0,abs(diff(aTemperature))) it is right length and first value (with no valid diff) is zero
cdml <- cdml %>% arrange(moats, dateTime)%>% 
  mutate(deltaTempLag1 = c(0,abs(diff(aTemperature))))
#the diff value comparing the last time in M01 to the first time in M02 (etc.) is not valid, so set those to zero
cdml$deltaTempLag1[lag(cdml$moats) != cdml$moats] <- 0
#this shows all the rows that jump more that 1 degree from previous row (ingoring rows that are transition from one moats to another)
cdml %>% filter(deltaTempLag1 >1)