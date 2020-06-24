#libraries
library(ggplot2)
#random comment

#Read in CSV file
##U:\01. Experiments\02. MUK_MOATS_Krill\DO_data\DO-PreSens Krill Day
Moats_PreSens_day <- read.csv("DO-PreSens Krill Day.csv")
Moats_PreSens_day$Period <- "day"
Moats_PreSens_night <- read.csv("DO-PreSens Krill Night.csv")
Moats_PreSens_night$Period <- "night"
d <- rbind(Moats_PreSens_day, Moats_PreSens_night)
 
dTreat <- read.csv("DOTreatmentFile.csv")
colnames(dTreat)[2]<-c("MOATS")
colnames(d)[7]<-c("DO")
d <- merge(d, dTreat, by = "MOATS")

d$Date <- as.POSIXct(strptime(d$Date, "%m/%e/%Y"))

ggplot(d, aes(Date, DO)) +
         geom_point(aes(color = Period)) +
        geom_hline(yintercept = 8) +
        facet_wrap(~TreatmentName)


p <- ggplot(d, aes(Date, DO))
p + ggtitle("Krill Day/Flow Period")  + ylab("Dissolved Oxygen (mg/L)") + 
  scale_y_continuous(breaks=seq(3,10,1)) + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20)) 

#Rename columns
#colnames(Moats_PreSens)[7]<-c("DO")
#Make MOATS a factor
Moats_PreSens$MOATS<-factor(Moats_PreSens$MOATS)
#Change DateTime format so R is happy
str(Moats_PreSens)
Moats_PreSens$NewDate <- as.POSIXct(strptime(Moats_PreSens$Date, "%m/%e/%Y"))
#View(Moats_PreSens)

#Scatter plot of MOATS with DO levelsR
Moats_PreSens$MOATS <- factor(Moats_PreSens$MOATS, 
                  levels = 
                    c("3", "7", "10", "12", "1", "6", "11","4","5", "2","8","13"))
p <- ggplot(Moats_PreSens, aes(MOATS, DO))
p + ggtitle("Krill Day/Flow Period")  + ylab("Dissolved Oxygen (mg/L)") + 
   scale_y_continuous(breaks=seq(3,10,1)) + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20)) 


#Subsetting MOATS based on treatment
Moats_PreSens_Current<- subset(Moats_PreSens, MOATS==3 | MOATS==7 | MOATS==10 | MOATS==12)
p <- ggplot(Moats_PreSens_Current, aes(MOATS, DO))
p + ggtitle("Krill Day/Flow Period - 'Current' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))


Moats_PreSens_HighTemp<- subset(Moats_PreSens, MOATS==1 | MOATS==6)
p <- ggplot(Moats_PreSens_HighTemp, aes(MOATS, DO))
p + ggtitle("Krill Day/Flow Period - 'High Temp' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))

Moats_PreSens_AllChange<- subset(Moats_PreSens, MOATS==2 | MOATS==8 | MOATS==13)
p <- ggplot(Moats_PreSens_AllChange, aes(MOATS, DO))
p + ggtitle("Krill Day/Flow Period - 'All Change' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))


Moats_PreSens_Ambient<- subset(Moats_PreSens, MOATS==4 | MOATS==5)
p <- ggplot(Moats_PreSens_Ambient, aes(MOATS, DO))
p + ggtitle("Krill Day/Flow Period - 'Ambient' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))

#Scatter plot based on treatment
p <- ggplot(Moats_PreSens, aes(Treatment, DO))
p + ggtitle("Krill Day/Flow Period")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))

################Do the same for "nightime"
#Read in CSV file
Moats_PreSens <- read.csv("C:/Users/Danielle.Perez/Documents/Krill 2019/DO-PreSens Krill Night.csv")
#Rename columns
colnames(Moats_PreSens)[7]<-c("DO")
#Make MOATS a factor
Moats_PreSens$MOATS<-factor(Moats_PreSens$MOATS)
#Change DateTime format so R is happy
str(Moats_PreSens)
Moats_PreSens$NewDate <- as.POSIXct(strptime(Moats_PreSens$Date, "%m/%e/%Y"))
#View(Moats_PreSens)

#Scatter plot of MOATS with DO levels
Moats_PreSens$MOATS <- factor(Moats_PreSens$MOATS, 
                              levels = 
                                c("3", "7", "10", "12", "1", "6", "11","4","5", "2","8","13"))
p <- ggplot(Moats_PreSens, aes(MOATS, DO))
p + ggtitle("Krill Night/ No Flow Period")  + ylab("Dissolved Oxygen (mg/L)") + 
  scale_y_continuous(breaks=seq(3,10,1)) + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20)) 

#Subsetting MOATS based on treatment
Moats_PreSens_Current<- subset(Moats_PreSens, MOATS==3 | MOATS==7 | MOATS==10 | MOATS==12)
p <- ggplot(Moats_PreSens_Current, aes(MOATS, DO))
p + ggtitle("Krill Night/ No Flow Period - 'Current' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))


Moats_PreSens_HighTemp<- subset(Moats_PreSens, MOATS==1 | MOATS==6)
p <- ggplot(Moats_PreSens_HighTemp, aes(MOATS, DO))
p + ggtitle("Krill Night/ No Flow Period - 'High Temp' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))

Moats_PreSens_AllChange<- subset(Moats_PreSens, MOATS==2 | MOATS==8 | MOATS==13)
p <- ggplot(Moats_PreSens_AllChange, aes(MOATS, DO))
p + ggtitle("Krill Night/ No Flow Period- 'All Change' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))


Moats_PreSens_Ambient<- subset(Moats_PreSens, MOATS==4 | MOATS==5)
p <- ggplot(Moats_PreSens_Ambient, aes(MOATS, DO))
p + ggtitle("Krill Night/ No Flow Period - 'Ambient' Treatment")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))

#Scatter plot based on treatment
p <- ggplot(Moats_PreSens, aes(Treatment, DO))
p + ggtitle("Krill Night/ No Flow Period")  + ylab("Dissolved Oxygen (mg/L)") + 
  geom_point(size=2) + (aes(colour =Sensor))  +  theme(text = element_text(size=20))
