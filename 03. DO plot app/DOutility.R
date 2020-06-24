##Kate's first attempt at a DO shiny app. 
##Utility Document for various DO stats (cannibalized code from 2019 November SPEC app)

#input to this utility (needs (an)) output download file from the soon to be DO shiny app
library(rcompanion)
install.packages("rcompanion")

setwd("/Users/katherinerovinski/GIT/NWFSC.MUK_KRL_SMR2019/03. DO plot app")
dday <- read.csv("DO-PreSens Krill Night.csv", stringsAsFactors = FALSE)
dnight <- read.csv("DO-PreSens Krill Day.csv", stringsAsFactors = FALSE)

#How to Get the mean and CI for tank DO by treatment
#tankCI <- groupwiseMean(DOinsitu ~ treatName, data = subset(d, unit == "Moats"), conf  = 0.95, digits = 12)
#tankCI
