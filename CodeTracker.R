#Seizure Tracker script for Brian LaGrant
NAMES <- read.table("~/Desktop/Weill Documents/SeizureTrackerProject/STFullExportCornell_20180626Seizures.csv", nrow = 1, stringsAsFactors = FALSE, sep = ",")
seizuredata <- read.table("~/Desktop/Weill Documents/SeizureTrackerProject/STFullExportCornell_20180626Seizures.csv", skip = 1, stringsAsFactors = FALSE, sep = ",")
seizuredata <- seizuredata[, 1:38]
names(seizuredata) <- NAMES 

profiledata = read.csv("~/Desktop/Weill Documents/SeizureTrackerProject/STFullExportCornell_20180626Profiles.csv", header=TRUE)
medicationdata = read.csv("~/Desktop/Weill Documents/SeizureTrackerProject/STFullExportCornell_20180626Medications.csv", header=TRUE)

#separate out infantile spasms
attach(seizuredata)
seizure_IS <- seizuredata[type=="Infantile Spasms (cluster)",]
detach(seizuredata)

#Time of year histogram

head(seizure_IS$Date)
seizure_IS$ModifiedDate <- as.Date(seizure_IS$Date, "%m/%d/%Y")
head(seizure_IS$ModifiedDate)
seizure_IS$Month <- strftime(seizure_IS$ModifiedDate, "%m")
head(seizure_IS$Month)
table(seizure_IS$Month)
seizure_IS$Month <- as.numeric(seizure_IS$Month)
table(seizure_IS$Month)
hist(seizure_IS$Month, xlim=c(1,12), ylim=c(0,3000))

#By year histogram
head(seizure_IS$Date)
seizure_IS$ModifiedYear <- as.Date(seizure_IS$Date, "%m/%d/%Y")
head(seizure_IS$ModifiedYear)
seizure_IS$Year <- strftime(seizure_IS$ModifiedYear, "%Y")
head(seizure_IS$Year)
table(seizure_IS$Year)
seizure_IS$Year <- as.numeric(seizure_IS$Year)
table(seizure_IS$Year)
hist(seizure_IS$Year, ylim=c(0,10000))

#measuring total time of episodes
seizure_IS$totaltime=((seizure_IS$length_hr*3600)+(seizure_IS$length_min*60)+(seizure_IS$length_sec))
table(seizure_IS$totaltime)
head(seizure_IS$totaltime)
seizure_IS$logtime=(log10(seizure_IS$totaltime))
head(seizure_IS$logtime)

unique(seizure_IS$length_min)
mean(seizure_IS$totaltime)
sd(seizure_IS$totaltime)
median(seizure_IS$totaltime)
quantile(seizure_IS$totaltime, c(0.25, 0.5, 0.75))
table <- table(seizure_IS$totaltime)
sort(table, descending=TRUE)
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#excluding0totalspasms
seizure_ISexclude0spasms=subset(seizure_IS, type_ISSpasmNum > 0)
mean(seizure_ISexclude0spasms$type_ISSpasmNum, na.rm = TRUE)
median(seizure_ISexclude0spasms$type_ISSpasmNum, na.rm = TRUE)
quantile(seizure_ISexclude0spasms$type_ISSpasmNum, c(0.25, 0.5, 0.75), na.rm=TRUE)
Modes(seizure_ISexclude0spasms$type_ISSpasmNum)
table(seizure_IS$type_ISSpasmNum)
table(seizure_ISexclude0spasms$type_ISSpasmNum)

mean(seizure_IS$type_ISSeverity, na.rm = TRUE)
median(seizure_IS$type_ISSeverity, na.rm = TRUE)

seizure_IS$zerotimedummy <- ifelse(as.numeric(seizure_IS$totaltime) == 0, 1,0)
table(seizure_IS$zerotimedummy)

hist(seizure_IS$totaltime, xlim=c(0,2000), ylim=c(0,12000), breaks=5000)
hist(seizure_IS$logtime, xlim=c(1,4), breaks=8)

#excluding 0 and 1 spasms
seizure_ISexclude1spasms=subset(seizure_IS, type_ISSpasmNum > 1)
mean(seizure_ISexclude1spasms$type_ISSpasmNum, na.rm = TRUE)
sd(seizure_ISexclude1spasms$type_ISSpasmNum, na.rm = TRUE)
median(seizure_ISexclude1spasms$type_ISSpasmNum, na.rm = TRUE)
quantile(seizure_ISexclude1spasms$type_ISSpasmNum, c(0.25, 0.5, 0.75), na.rm=TRUE)
Modes(seizure_ISexclude1spasms$type_ISSpasmNum)
table(seizure_IS$type_ISSpasmNum)
table(seizure_ISexclude1spasms$type_ISSpasmNum)

#excluding0totaltime among clusters
seizure_ISexclude0time=subset(seizure_ISexclude1spasms, totaltime > 0)
mean(seizure_ISexclude0time$totaltime)
sd(seizure_ISexclude0time$totaltime)
mean(seizure_ISexclude0time$logtime)
median(seizure_ISexclude0time$totaltime)
quantile(seizure_ISexclude0time$totaltime, c(0.25, 0.5, 0.75))
Modes(seizure_ISexclude0time$totaltime)

head(seizure_IS$Date_Time)

seizure_IS_dt <- data.frame(do.call('rbind', strsplit(as.character(seizure_IS$Date_Time),' ',fixed=TRUE)))
dt_names=c("Date","Time")
names(seizure_IS_dt)=dt_names
seizure_IS=cbind(seizure_IS,seizure_IS_dt)

mean(seizure_IS$totaltime)
mean(seizure_IS$type_ISSpasmNum, na.rm = TRUE)
median(seizure_IS$type_ISSpasmNum, na.rm = TRUE)
quantile(seizure_IS$type_ISSpasmNum, c(0.25, 0.5, 0.75), na.rm=TRUE)
table2 <- table(seizure_IS$type_ISSpasmNum)
sort(table2, descending=TRUE)

#looking at 0s for cluster duration
zerosecondcluster=subset(seizure_IS, totaltime==0)
table(zerosecondcluster$Hours)
zerosecondcluster <- zerosecondcluster[order(as.Date(zerosecondcluster$Date_Time, format = "%m/%d/%Y %H:%M", descending = FALSE)),]
zerosecondcluster$DateTimeConverted <- as.POSIXct(zerosecondcluster$Date_Time, format="%m/%d/%Y %H:%M")
library(data.table)
zerodatatable<-data.table(zerosecondcluster)

zerodatatable[ , diff := DateTimeConverted - shift(DateTimeConverted), by = Unlinked_ID] 
unique(zerodatatable$Unlinked_ID)
table(zerodatatable$diff)
colSums(!is.na(zerodatatable))
mean(zerodatatable$diff, na.rm = TRUE)
quantile(zerodatatable$diff, c(0.25, 0.5, 0.75), na.rm = TRUE)

zerodatatable2min=subset(zerodatatable, diff < 121 & diff > -121)
table(zerodatatable2min$diff)
unique(zerodatatable2min$Unlinked_ID)
table(zerodatatable2min$Unlinked_ID)


#ggplot of duration
library(ggplot2)
table(seizure_ISexclude0time$logtime)
ggtime <- ggplot(data = seizure_ISexclude0time, aes(seizure_ISexclude0time$logtime)) + geom_histogram(breaks=seq(0,4,by=0.1), col="red", fill="blue", alpha = 0.2) + labs(title="Duration of Cluster", y="Frequency", x="Time") + theme_bw()
ggtime <- ggtime + scale_x_continuous(breaks = c(0, 0.30, 0.48, 0.60, 0.70, 0.78, 0.85, 0.90, 0.95, 1, 1.3, 1.48, 1.60, 1.70, 1.78, 1.85, 1.90, 1.95, 2, 2.3, 2.48, 2.60, 2.70, 2.78, 2.85, 2.90, 2.95, 3, 3.3, 3.48, 3.60, 3.70, 3.78, 3.85, 3.90, 3.95), labels=c("1s", " ", " ", " ", " ", " ", " ", " ", " ", "10s", " ", " ", " ", " ", " ", " ", " ", " ", "100s"," ", " ", " ", " ", " ", " ", " ", " ", "1000s", " ", " ", " ", " ", " ", " ", " ", " "))
ggtime <- ggtime + theme(text = element_text(size=40))
ggtime 

bluecolor <- rgb(0,.46,.75)

ggtimepercent <- ggplot(data = seizure_ISexclude0time, aes(logtime)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(0,4,by=0.1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Duration of Cluster", y="Relative frequency", x="Time") + theme_bw()
ggtimepercent <- ggtimepercent + scale_x_continuous(breaks = c(0, 0.30, 0.48, 0.60, 0.70, 0.78, 0.85, 0.90, 0.95, 1, 1.3, 1.48, 1.60, 1.70, 1.78, 1.85, 1.90, 1.95, 2, 2.3, 2.48, 2.60, 2.70, 2.78, 2.85, 2.90, 2.95, 3, 3.3, 3.48, 3.60, 3.70, 3.78, 3.85, 3.90, 3.95), labels=c("1s", " ", " ", " ", " ", " ", " ", " ", " ", "10s", " ", " ", " ", " ", " ", " ", " ", " ", "100s"," ", " ", " ", " ", " ", " ", " ", " ", "1000s", " ", " ", " ", " ", " ", " ", " ", " "))
ggtimepercent <- ggtimepercent + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.15), labels = scales::percent)
ggtimepercent

table(seizure_IS$logtime)
ggtime2 <- ggplot(data = seizure_IS, aes(seizure_IS$logtime)) + geom_histogram(breaks=seq(0,4,by=0.1), col="red", fill="blue", alpha = 0.2) + labs(title="Duration of Cluster", y="Frequency", x="Time") + theme_bw()
ggtime2 <- ggtime2 + scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4), labels=c("1s", "1.6s", "2.5s", "4.0s", "6.3s", "10s","15.8s", "25.1s", "39.8s", "1.1m", "1.7m", "2.6m", "4.2m", "6.6m", "10.5m", "16.7m", "26.4m", "41.9m", "66.4m", "105.2m", "167m"))
ggtime2 

table(seizure_IS$logtime)
ggtime3 <- ggplot(data = seizure_IS, aes(seizure_IS$logtime)) + geom_histogram(breaks=seq(0,4,by=0.1), col="red", fill="blue", alpha = 0.2) + labs(title="Duration of Cluster", y="Frequency", x="Time") + theme_bw()
ggtime3 <- ggtime3 + scale_x_continuous(breaks = c(0, 1, 1.48, 1.78, 2.08, 2.38, 2.68, 2.98, 3.28), labels=c("1s", "10s", "30s", "1m", "2m", "4m", "8m", "16m", "32m"))
ggtime3

ggdurationtracker <- ggplot(data = seizure_ISexclude0time, aes(seizure_ISexclude0time$totaltime)) + geom_histogram(breaks=seq(-1,720,by=60), col="red", fill="blue", alpha = 0.2) + labs(title="Duration of Cluster", y="Frequency", x="Time") + theme_bw()
ggdurationtracker <- ggdurationtracker + scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 360, 420, 480, 540, 600, 660, 720), labels=c("0min", "1min", "2min", "3min", "4min", "5min", "6min", "7min", "8min", "9min", "10min", "11min", "12min"))
ggdurationtracker <- ggdurationtracker + theme(text = element_text(size=30)) 
ggdurationtracker 

#ggplot of spasms
seizure_ISnonzerooronespasms <- subset(seizure_IS, type_ISSpasmNum > 1)
table(seizure_ISnonzerooronespasms$type_ISSpasmNum)
ggspasms <- ggplot(data = seizure_ISnonzerooronespasms, aes(seizure_ISnonzerooronespasms$type_ISSpasmNum)) + geom_histogram(breaks=seq(1,50, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Spasms per Cluster", y="Frequency", x = "Number of Spasms") + theme_bw()
ggspasms <- ggspasms + scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"))
ggspasms <- ggspasms + theme(text = element_text(size=40))
ggspasms

ggspasmspercent <- ggplot(data = seizure_ISnonzerooronespasms, aes(seizure_ISnonzerooronespasms$type_ISSpasmNum)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(1,50, by=1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Spasms per Cluster", y="Relative frequency", x = "Number of spasms") + theme_bw()
ggspasmspercent <- ggspasmspercent + scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"))
ggspasmspercent <- ggspasmspercent + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.1), labels = scales::percent)
ggspasmspercent

ggspasms2 <- ggplot(data = seizure_ISexclude0spasms, aes(seizure_ISexclude0spasms$type_ISSpasmNum)) + geom_histogram(breaks=seq(0,50, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Number of Spasms per Cluster", y="Frequency", x = "Number of Spasms") + theme_bw()
ggspasms2 <- ggspasms2 + scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"))
ggspasms2

#bigger font  for posters
ggspasms <- ggplot(data = seizure_ISnonzerooronespasms, aes(seizure_ISnonzerooronespasms$type_ISSpasmNum)) + geom_histogram(breaks=seq(0,50, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Number of Spasms per Cluster", y="Frequency", x = "Number of Spasms") + theme_bw() + theme(text = element_text(size=30))

seizure_IS$logspasms=(log10(seizure_IS$type_ISSpasmNum))
hist(seizure_IS$type_ISSpasmNum, xlim=c(0,60), ylim=c(0,7000), breaks=180)
table(seizure_IS$type_ISSpasmNum)
hist(seizure_IS$type_ISSpasmNum, xlim=c(10,45), ylim=c(0,1000), breaks=180)


hist(seizure_IS$type_ISSeverity, xlim=c(0,5), ylim=c(0,12000), breaks=6)

head(seizure_IS$Time)

seizure_IS$Time2 <- as.POSIXct(seizure_IS$Time, format="%H:%M")
head(seizure_IS$Time2)
seizure_IS$Hours <- as.numeric(format(strptime(seizure_IS$Time, format = "%H:%M" ) , "%H" ) )
seizure_ISexclude0spasms=subset(seizure_IS, type_ISSpasmNum > 0)
hist(seizure_ISexclude0spasms$Hours, xlim=c(0,24), ylim=c(0,2000), breaks=24, main = "Histogram of Seizure Reports by Hour of Day", xlab = "Hour of Day", col = "gray", cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
gghour <- ggplot(data = seizure_ISexclude0spasms, aes(seizure_ISexclude0spasms$Hours)) + geom_histogram(breaks=seq(-1,24, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Spasms by Hour of Day", y="Frequency", x = "Hour of day") + theme_bw()
gghour <- gghour + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("0", "6",  "12", "18", "24"))
gghour <- gghour + theme(text = element_text(size=40))
gghour
table(seizure_ISexclude0spasms$Hours)
sum(table(seizure_ISexclude0spasms$Hours))
timestable  <- data.frame(table(seizure_ISexclude0spasms$Time))

library(lubridate)
library(scales)

hourdensity <- ggplot(seizure_IS, aes(Time2)) + 
  geom_density(alpha = 0.5) + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%H:%M"))

hourdensity

hourdensity2 <- ggplot(seizure_IS) + 
  geom_density(aes(x = Time2, y = ..scaled..), alpha = 0.5) +
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%H:%M"))

hourdensity2

#using night time as 11-7
seizure_IS$Night
seizure_IS$Night <- 0
table(seizure_IS$Night)
seizure_IS$Night[seizure_IS$Hours < 7] <- 1
seizure_IS$Night[seizure_IS$Hours==23] <- 1
table(seizure_IS$Night)

seizure_ISexclude0spasms=subset(seizure_IS, type_ISSpasmNum > 0)
seizure_ISNighttime=subset(seizure_ISexclude0spasms, Night==1)
seizure_ISDaytime=subset(seizure_ISexclude0spasms, Night==0)
sum(seizure_ISNighttime$type_ISSpasmNum)
sum(seizure_ISDaytime$type_ISSpasmNum)


table(seizure_IS$Hours)
table(seizure_ISexclude0spasms$Hours)
sum(seizure_ISexclude0spasms$type_ISSpasmNum)
binom.test(53978, 226932, p=0.3333, alternative = c("two.sided"), conf.level = 0.95)

#using night time as 9-9
seizure_IS$Night2
seizure_IS$Night2 <- 0
table(seizure_IS$Night2)
seizure_IS$Night2[seizure_IS$Hours < 9] <- 1
seizure_IS$Night2[seizure_IS$Hours > 20] <- 1
table(seizure_IS$Night2)

seizure_ISexclude0spasms=subset(seizure_IS, type_ISSpasmNum > 0)
seizure_ISNighttime2=subset(seizure_ISexclude0spasms, Night2==1)
seizure_ISDaytime2=subset(seizure_ISexclude0spasms, Night2==0)
sum(seizure_ISNighttime2$type_ISSpasmNum)
sum(seizure_ISDaytime2$type_ISSpasmNum)


table(seizure_IS$Hours)
table(seizure_ISexclude0spasms$Hours)
sum(seizure_ISexclude0spasms$type_ISSpasmNum)
binom.test(99035, 226932, p=0.552, alternative = c("two.sided"), conf.level = 0.95)

seizure_ISexclude0time2=subset(seizure_IS, totaltime > 0)
table(seizure_ISexclude0time2$Hours)
table(seizure_ISexclude0time2$type_ISSpasmNum)


library(plyr)
countbyid <- count(seizure_IS,c('Unlinked_ID'))
countbyid
countbyid$group[countbyid$freq < 5] <- 1
countbyid$group[countbyid$freq <= 100 & countbyid$freq >= 5] <- 2
countbyid$group[countbyid$freq > 100] <- 3
countbyid
table(countbyid$group)



#correlation between spasm number and spasm time

table(seizure_ISexclude1spasms$type_ISSpasmNum)
cor.test(seizure_ISexclude1spasms$type_ISSpasmNum, seizure_ISexclude1spasms$totaltime)

#correlation between spasm number and spasm severity
cor.test(seizure_ISexclude1spasms$type_ISSeverity, seizure_ISexclude1spasms$type_ISSpasmNum)

#variability of reported cluster length by individual
tapply(seizure_IS$totaltime, seizure_IS$Unlinked_ID, mean)
tapply(seizure_IS$totaltime, seizure_IS$Unlinked_ID, sd)
median(tapply(seizure_IS$totaltime, seizure_IS$Unlinked_ID, sd), na.rm = TRUE)


#looking at gender in combined data set
completedata <- merge(seizure_IS, profiledata, by = "Unlinked_ID", all.x = TRUE)
unique(completedata$Unlinked_ID)
detach("package:plyr", unload=TRUE)
library(tidyverse)
library(dplyr)
completedata %>% distinct(Unlinked_ID, Gender) %>% count(Gender) 


#looking at comorbidities in combined data
unique(completedata$Congenital_Condition)
completedata %>% distinct(Unlinked_ID, Congenital_Condition) %>% count(Congenital_Condition) 


unique(completedata$Stroke)
completedata %>% distinct(Unlinked_ID, Stroke) %>% count(Stroke) 

unique(completedata$Metabolic_Disorder)
completedata %>% distinct(Unlinked_ID, Metabolic_Disorder) %>% count(Metabolic_Disorder) 

unique(completedata$Genetic_Abnormalities)
completedata %>% distinct(Unlinked_ID, Genetic_Abnormalities) %>% count(Genetic_Abnormalities) 

unique(completedata$Infectious_Diseases)
completedata %>% distinct(Unlinked_ID, Infectious_Diseases) %>% count(Infectious_Diseases) 

unique(completedata$Lack_Of_Oxygen_During_Birth)
completedata %>% distinct(Unlinked_ID, Lack_Of_Oxygen_During_Birth) %>% count(Lack_Of_Oxygen_During_Birth) 

unique(completedata$Maternal_Drug_Or_Alcohol_Abuse)
completedata %>% distinct(Unlinked_ID, Maternal_Drug_Or_Alcohol_Abuse) %>% count(Maternal_Drug_Or_Alcohol_Abuse) 

unique(completedata$Brain_Injury_During_Fetal_Development)
completedata %>% distinct(Unlinked_ID, Brain_Injury_During_Fetal_Development) %>% count(Brain_Injury_During_Fetal_Development) 

unique(completedata$Electrolyte_Disturbances)
completedata %>% distinct(Unlinked_ID, Electrolyte_Disturbances) %>% count(Electrolyte_Disturbances) 

unique(completedata$Brain_Malformations)
completedata %>% distinct(Unlinked_ID, Brain_Malformations) %>% count(Brain_Malformations) 

unique(completedata$Brain_Trauma)
completedata %>% distinct(Unlinked_ID, Brain_Trauma) %>% count(Brain_Trauma) 

#looking at medication usage and number of unique meds used by each  individual/by group

fulldatausedmeds <- merge(completedata, medicationdata, by = "Unlinked_ID", all.x = TRUE)

fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Mg1XmW", "sq9hc8" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="DmQ2Gz", "qxkUOM" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="ONFvdf", "dMgSlh" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="w3AwGp", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="ro0okj", "wVErXI" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="fESlXO", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Hfx5Y8", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="03iBnP", "CEJIvF" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="IGygL5", "AGUGZk" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="U5XBzz", "ingak5" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="ZnVIdI", "nRa02Y" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="rLei4x", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="KUvWvh", "nRa02Y" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="m0DJo7", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="rEj38x", "TSiBRm" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="jdDBEL", "Lc4bz4" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="FlRk1h", "WHl7H1" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="w60MjQ", "qxkUOM" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="h8vJ05", "iDvQ2I" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="tgSko9", "ezGQ66" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="gyFbXW", "0thkIW" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="pv71YJ", "pSqhPn" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="bcZS6o", "1bp5qU" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="vG2vUu", "LrYCrT" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="GgCKdi", "xZWHPO" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="6xR13g", "nRa02Y" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="A9H9vb", "UfQgrX" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="BxbNDo", "ezGQ66" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="o8H7P0", "85dkRd" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="2RV2rl", "dMgSlh" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="VdgvYc", "LdHc8G" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="1GHb9k", "mJZ5pY" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="EKn9Cb", "ingak5" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="SH86Fk", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="7fLYPj", "qdhnwF" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="K213vn", "0JPQph" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="d7d9tS", "h5HHem" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="vC76Ls", "s9pYCP" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="d2Yig2", "GxK8gv" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="AQmFRg", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="KoqRnf", "iDvQ2I" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="2sGEH7", "V2bV02" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="uEiGhC", "V2bV02" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="dU8zP5", "Pfgorr" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="wNhjE5", "Lc4bz4" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Q30rnt", "h5HHem" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="O6jxnT", "VdzyW2" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="iWXhNY", "LdHc8G" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="fbjBBY", "itvElF" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="D6HhVt", "LdHc8G" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Z2l2XJ", "0thkIW" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="cqvIXz", "0thkIW" )

detach("package:plyr", unload=TRUE)
randomtable <- unique(fulldatausedmeds$Med_Name_Code)
unique(fulldatausedmeds$Unlinked_ID)
medicationlist <- fulldatausedmeds %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Med_Name_Code) 
medicationlist
sum(medicationlist$n)

fulldatausedmeds2=subset(fulldatausedmeds, Daily_Dose > 0)

medicationlistedited <- fulldatausedmeds2 %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Med_Name_Code) 

medicationlist2usedmeds <- fulldatausedmeds2 %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Unlinked_ID) 
medicationlist2usedmeds

fulldatausedmeds$n
fulldatausedmeds$n <- 0
fulldata2 <- merge(fulldatausedmeds, medicationlist2usedmeds, by = "Unlinked_ID", all.x = TRUE)
fulldata2$n.x <- ifelse(is.na(fulldata2$n.y), fulldata2$n.x, fulldata2$n.y)
fulldata2$n.y <- NULL
#so n.x represents number of unique meds tried per person

medicationlistfinal <- fulldata2 %>% distinct(Unlinked_ID, n.x) %>% count(n.x) 
sum(medicationlistfinal$n)

medprofileseizuregroup <- merge(fulldata2, countbyid, by = "Unlinked_ID", all.x = TRUE)
unique(medprofileseizuregroup$group)
medprofileseizuregroup %>% distinct(Unlinked_ID, group) %>% count(group) 
medicationlist2 <- medprofileseizuregroup %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Med_Name_Code) 
medicationlist2

tablenumbermeds <- medprofileseizuregroup %>%
  group_by(Unlinked_ID) %>%
  slice(1L)

mean(tablenumbermeds$n.x)
sd(tablenumbermeds$n.x)
median(tablenumbermeds$n.x)
quantile(tablenumbermeds$n.x, c(0.25, 0.75))

tapply(tablenumbermeds$n.x, tablenumbermeds$group, mean, na.rm=TRUE)
tapply(tablenumbermeds$n.x, tablenumbermeds$group, sd, na.rm=TRUE)
anovameds <- aov(n.x ~ group, data=tablenumbermeds)
summary(anovameds)

tapply(tablenumbermeds$n.x, tablenumbermeds$group, median, na.rm=TRUE)
kruskal.test(n.x ~ group, data=tablenumbermeds)

library(plyr)
groupstatmeds<-ddply(tablenumbermeds, .(group),summarise,
                    meansc= mean(n.x), sd = sd(n.x), mediansc=(median(n.x)), TwentyFifth= quantile(n.x, c(0.25)), SeventyFifth= quantile(n.x, c(0.75)))

excludegroup1meds=subset(tablenumbermeds, group > 1)
excludegroup3meds=subset(tablenumbermeds, group < 3)
excludegroup2meds=subset(tablenumbermeds, group==1 | group==3)
wilcox.test(n.x ~ as.factor(group), data=excludegroup1meds)
wilcox.test(n.x ~ as.factor(group), data=excludegroup3meds)
wilcox.test(n.x ~ as.factor(group), data=excludegroup2meds)

#age of each child
library(dplyr)
birthdatelist <- completedata %>% distinct(Unlinked_ID, Birth_Date) %>% count(Birth_Date) 
birthdatelist

tablefirstepisode <- seizure_IS %>%
  group_by(Unlinked_ID) %>%
  arrange(ModifiedDate) %>%
  slice(1L)

agedata <- merge(tablefirstepisode, profiledata, by = "Unlinked_ID", all.x = TRUE)
agedata$ModifiedBirthYear <- as.Date(agedata$Birth_Date, "%m/%d/%y")
head(agedata$ModifiedBirthYear)
agedata$ModifiedDateCorrected <- as.Date(agedata$Date, "%m/%d/%y")
head(agedata$ModifiedDateCorrected)
agedata$BirthYear <- strftime(agedata$ModifiedBirthYear, "%Y")
head(agedata$BirthYear)
table(agedata$BirthYear)

agedata$age <- as.Date(as.character(agedata$ModifiedDateCorrected), format="%Y-%m-%d")-
  as.Date(as.character(agedata$ModifiedBirthYear), format="%Y-%m-%d")

#anova for age

dataage <- merge(countbyid, agedata, by = "Unlinked_ID", all.x = TRUE)
dataageexclude=subset(dataage, age > 0)
dataageexclude$ageless1
dataageexclude$ageless1[dataageexclude$age < 366] <- 1
dataageexclude$ageless1[dataageexclude$age > 365] <- 0
library(plyr)
groupstatage<-ddply(dataageexclude, .(group),summarise,
                     meansc= mean(age), sd = sd(age), mediansc=(median(age)), TwentyFifth= quantile(totaltime, c(0.25)), SeventyFifth= quantile(totaltime, c(0.75)),  nless1= length(ageless1[ageless1==1]), nmore1= length(ageless1[ageless1==0]))
median(dataageexclude$age)
anovaage <- aov(age ~ group, data=dataageexclude)
summary(anovaage)

obsfreqage <- matrix(c(25,111, 42,77, 19,28),nrow=2,ncol=3)
chisq.test(obsfreqage)

obsfreqage <- matrix(c(25,111, 42,77),nrow=2,ncol=2)
chisq.test(obsfreqage)

obsfreqage <- matrix(c(42,77, 19,28),nrow=2,ncol=2)
chisq.test(obsfreqage)

obsfreqage <- matrix(c(25,111, 19,28),nrow=2,ncol=2)
chisq.test(obsfreqage)

#excluding negative ages
agedataexcludeoutliers=subset(agedata, age > 0)
agedata18=subset(agedata, age > 6569)

hist(as.numeric(agedataexcludeoutliers$age), breaks = c(0, 60, 120, 180, 240, 300, 360, 420, 480, 540, 600, 660, 720, 780, 840, 900, 960, 1020, 1080, 1140, 1200, 1260, 1320, 1380, 1440, 17000), xlim = c(0,1500))
hist(as.numeric(agedataexcludeoutliers$age), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360, 390, 420, 450, 480, 510, 540, 570, 600, 630, 660, 690, 720, 750, 780, 810, 840, 870, 900, 930, 960, 990, 1020, 1050, 1080, 1110, 1140, 1179, 1200, 1230, 1260, 1290, 1320, 1350, 1380, 1410, 1440, 1470, 1500, 17000), xlim = c(0,1500), xlab = "Age in days (bins of 30 days)", main = "Histogram of age")
table(agedataexcludeoutliers$age)
median(agedataexcludeoutliers$age)
quantile(agedataexcludeoutliers$age, c(0.25, 0.75))
table(agedataexcludeoutliers$age)
agedataless2=subset(agedataexcludeoutliers, as.numeric(age) < 731)


table(agedataexcludeoutliers$ModifiedBirthYear)
agedata$BirthMonthDay <- strftime(agedata$ModifiedBirthYear, "%m-%d")
table(agedata$BirthMonthDay)

#only children less than one and looking at stats
agedatalessone=subset(agedataexcludeoutliers, age < 366)
table(agedatalessone$Unlinked_ID)
median(agedatalessone$age)
quantile(agedatalessone$age, c(0.25, 0.75))
unique(agedatalessone$Unlinked_ID)

library(dplyr)

agedatalessone %>% distinct(Unlinked_ID, Gender) %>% count(Gender) 

unique(agedatalessone$Congenital_Condition)
agedatalessone %>% distinct(Unlinked_ID, Congenital_Condition) %>% count(Congenital_Condition) 


unique(agedatalessone$Stroke)
agedatalessone %>% distinct(Unlinked_ID, Stroke) %>% count(Stroke) 

unique(agedatalessone$Metabolic_Disorder)
agedatalessone %>% distinct(Unlinked_ID, Metabolic_Disorder) %>% count(Metabolic_Disorder) 

unique(agedatalessone$Genetic_Abnormalities)
agedatalessone %>% distinct(Unlinked_ID, Genetic_Abnormalities) %>% count(Genetic_Abnormalities) 

unique(agedatalessone$Infectious_Diseases)
agedatalessone %>% distinct(Unlinked_ID, Infectious_Diseases) %>% count(Infectious_Diseases) 

unique(agedatalessone$Lack_Of_Oxygen_During_Birth)
agedatalessone %>% distinct(Unlinked_ID, Lack_Of_Oxygen_During_Birth) %>% count(Lack_Of_Oxygen_During_Birth) 

unique(agedatalessone$Maternal_Drug_Or_Alcohol_Abuse)
agedatalessone %>% distinct(Unlinked_ID, Maternal_Drug_Or_Alcohol_Abuse) %>% count(Maternal_Drug_Or_Alcohol_Abuse) 

unique(agedatalessone$Brain_Injury_During_Fetal_Development)
agedatalessone %>% distinct(Unlinked_ID, Brain_Injury_During_Fetal_Development) %>% count(Brain_Injury_During_Fetal_Development) 

unique(agedatalessone$Electrolyte_Disturbances)
agedatalessone %>% distinct(Unlinked_ID, Electrolyte_Disturbances) %>% count(Electrolyte_Disturbances) 

unique(agedatalessone$Brain_Malformations)
agedatalessone %>% distinct(Unlinked_ID, Brain_Malformations) %>% count(Brain_Malformations) 

unique(agedatalessone$Brain_Trauma)
agedatalessone %>% distinct(Unlinked_ID, Brain_Trauma) %>% count(Brain_Trauma) 


fulldatausedmeds=subset(fulldatausedmeds, Unlinked_ID %in% agedatalessone$Unlinked_ID)

randomtable <- unique(fulldatausedmeds$Med_Name_Code)
unique(fulldatausedmeds$Unlinked_ID)
medicationlist <- fulldatausedmeds %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Med_Name_Code) 
medicationlist
sum(medicationlist$n)

fulldatausedmeds2=subset(fulldatausedmeds, Daily_Dose > 0)

medicationlistedited <- fulldatausedmeds2 %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Med_Name_Code) 

medicationlist2usedmeds <- fulldatausedmeds2 %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Unlinked_ID) 
medicationlist2usedmeds

fulldatausedmeds$n
fulldatausedmeds$n <- 0
fulldata2 <- merge(fulldatausedmeds, medicationlist2usedmeds, by = "Unlinked_ID", all.x = TRUE)
fulldata2$n.x <- ifelse(is.na(fulldata2$n.y), fulldata2$n.x, fulldata2$n.y)
fulldata2$n.y <- NULL
#so n.x represents number of unique meds tried per person

medicationlistfinal <- fulldata2 %>% distinct(Unlinked_ID, n.x) %>% count(n.x) 
sum(medicationlistfinal$n)

medprofileseizuregroup <- merge(fulldata2, countbyid, by = "Unlinked_ID", all.x = TRUE)
unique(medprofileseizuregroup$group)
medprofileseizuregroup %>% distinct(Unlinked_ID, group) %>% count(group) 
medicationlist2 <- medprofileseizuregroup %>% distinct(Unlinked_ID, Med_Name_Code) %>% count(Med_Name_Code) 
medicationlist2

tablenumbermeds <- medprofileseizuregroup %>%
  group_by(Unlinked_ID) %>%
  slice(1L)

mean(tablenumbermeds$n.x)
sd(tablenumbermeds$n.x)
median(tablenumbermeds$n.x)
quantile(tablenumbermeds$n.x, c(0.25, 0.75))



agelessonefulldata=subset(seizure_IS, Unlinked_ID %in% agedatalessone$Unlinked_ID)
unique(agelessonefulldata$Unlinked_ID)
agelessonefulldata <- merge(agelessonefulldata, profiledata, by = "Unlinked_ID", all.x = TRUE)
unique(agelessonefulldata$Unlinked_ID)

agelessoneexclude1spasm=subset(agelessonefulldata, type_ISSpasmNum > 1)
mean(agelessoneexclude1spasm$type_ISSpasmNum, na.rm = TRUE)
median(agelessoneexclude1spasm$type_ISSpasmNum, na.rm = TRUE)
quantile(agelessoneexclude1spasm$type_ISSpasmNum, c(0.25, 0.5, 0.75), na.rm=TRUE)
table(agelessoneexclude1spasm$type_ISSpasmNum)
sd(agelessoneexclude1spasm$type_ISSpasmNum, na.rm = TRUE)
class(agelessoneexclude1spasm$type_ISSpasmNum)
Modes(agelessoneexclude1spasm$type_ISSpasmNum)
table(agelessoneexclude1spasm$type_ISSpasmNum)
table(agelessoneexclude1spasm$type_ISSpasmNum)

agelessoneexclude0time=subset(agelessoneexclude1spasm, totaltime > 0)
mean(agelessoneexclude0time$totaltime)
sd(agelessoneexclude0time$totaltime)
mean(agelessoneexclude0time$logtime)
median(agelessoneexclude0time$totaltime)
quantile(agelessoneexclude0time$totaltime, c(0.25, 0.5, 0.75))
Modes(agelessoneexclude0time$totaltime)

agelessoneexclude0spasms=subset(agelessonefulldata, type_ISSpasmNum > 0)
sum(agelessoneexclude0spasms$type_ISSpasmNum)
agelessone1spasm=subset(agelessoneexclude0spasms, type_ISSpasmNum==1)
#so 820 out of 82,167 are individual spasms for this group

ggtimepercent1year <- ggplot(data = agelessoneexclude0time, aes(logtime)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(0,4,by=0.1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Duration of Cluster", y="Relative frequency", x="Time") + theme_bw()
ggtimepercent1year <- ggtimepercent1year + scale_x_continuous(breaks = c(0, 0.30, 0.48, 0.60, 0.70, 0.78, 0.85, 0.90, 0.95, 1, 1.3, 1.48, 1.60, 1.70, 1.78, 1.85, 1.90, 1.95, 2, 2.3, 2.48, 2.60, 2.70, 2.78, 2.85, 2.90, 2.95, 3, 3.3, 3.48, 3.60, 3.70, 3.78, 3.85, 3.90, 3.95), labels=c("1s", " ", " ", " ", " ", " ", " ", " ", " ", "10s", " ", " ", " ", " ", " ", " ", " ", " ", "100s"," ", " ", " ", " ", " ", " ", " ", " ", "1000s", " ", " ", " ", " ", " ", " ", " ", " "))
ggtimepercent1year <- ggtimepercent1year + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.15), labels = scales::percent)
ggtimepercent1year

ggspasmspercent1year <- ggplot(data = agelessoneexclude1spasm, aes(agelessoneexclude1spasm$type_ISSpasmNum)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(1,50, by=1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Spasms per Cluster", y="Relative frequency", x = "Number of spasms") + theme_bw()
ggspasmspercent1year <- ggspasmspercent1year + scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"))
ggspasmspercent1year <- ggspasmspercent1year + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.1), labels = scales::percent)
ggspasmspercent1year

spasmsbyhour1year <- aggregate(agelessoneexclude0spasms$type_ISSpasmNum, by=list(Category=agelessoneexclude0spasms$Hours), FUN=sum)
sum(spasmsbyhour1year$x)

#spasms by individual aggregate

detach("package:plyr", unload=TRUE)

spasmsbyhour1yearbyuser <- aggregate(agelessoneexclude0spasms$type_ISSpasmNum, by=list(Category=agelessoneexclude0spasms$Unlinked_ID), FUN=sum)
spasmsbyhour1yearbyuser <- spasmsbyhour1yearbyuser %>% 
  rename(Unlinked_ID = Category)

spasmsbyhour1yearbyuser2 <- agelessoneexclude0spasms %>% group_by(agelessoneexclude0spasms$Unlinked_ID, Hours) %>% summarise(type_ISSpasmNum = sum(type_ISSpasmNum))
spasmsbyhour1yearbyuser2 <- spasmsbyhour1yearbyuser2 %>% 
  rename(
    Unlinked_ID = "agelessoneexclude0spasms$Unlinked_ID"
  )

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==0)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==1)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==2)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==3)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==4)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==5)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==6)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==7)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==8)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==9)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==10)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==11)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==12)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==13)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==14)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==15)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==16)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==17)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==18)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==19)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==20)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==21)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==22)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

spasmsbyhour1yeartime0=subset(spasmsbyhour1yearbyuser2, Hours==23)
length(unique(spasmsbyhour1yeartime0$Unlinked_ID))

under1percentatleast1spasmbyhour <- data.table(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), c(13,18,18,21,21,18,28,29,34,34,34,31,25,29,33,32,32,33,30,29,29,22,19,14))
under1percentatleast1spasmbyhour$Percentage=((under1percentatleast1spasmbyhour$V2)/86)

under1percentatleast1spasmbyhour$HourChanged
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==12] <- 0
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==13] <- 1
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==14] <- 2
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==15] <- 3
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==16] <- 4
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==17] <- 5
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==18] <- 6
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==19] <- 7
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==20] <- 8
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==21] <- 9
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==22] <- 10
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==23] <- 11
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==0] <- 12
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==1] <- 13
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==2] <- 14
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==3] <- 15
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==4] <- 16
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==5] <- 17
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==6] <- 18
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==7] <- 19
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==8] <- 20
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==9] <- 21
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==10] <- 22
under1percentatleast1spasmbyhour$HourChanged[under1percentatleast1spasmbyhour$V1==11] <- 23

ggspasmshourpercent1yearbyuser <- ggplot(data = under1percentatleast1spasmbyhour, aes(x=HourChanged, y=Percentage)) + geom_bar(stat="identity", fill = rgb(0,.46,.75), alpha = 0.8, color="black") + labs(title="Proportion with at Least One Spasm", y="Proportion", x = "Hour of day") + theme_bw() + scale_y_continuous(lim = c(0,0.8)) + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("12", "18",  "0", "6", "12"))
ggspasmshourpercent1yearbyuser<- ggspasmshourpercent1yearbyuser + theme(text = element_text(size=42))
ggspasmshourpercent1yearbyuser

mergedusercounts1year <- merge(spasmsbyhour1yearbyuser2, spasmsbyhour1yearbyuser, by = "Unlinked_ID", all.x = TRUE)
mergedusercounts1year$PercentByHour=((mergedusercounts1year$type_ISSpasmNum)/(mergedusercounts1year$x))

mergedusercounts1yearfinal <- aggregate(as.numeric(mergedusercounts1year$PercentByHour), list(mergedusercounts1year$Hours), mean)
mergedusercounts1yearfinal <- mergedusercounts1yearfinal %>% 
  rename(
    mean = x
  )

ggspasmshourpercent1yearbyuser <- ggplot(data = mergedusercounts1yearfinal, aes(x=Group.1, y=mean)) + geom_bar(stat="identity", fill = rgb(0,.46,.75), alpha = 0.8) + labs(title="Relative Abundance of Spasms for Each User by Hour of Day", y="Relative Abundance", x = "Hour of day") + theme_bw()
ggspasmshourpercent1yearbyuser

#for whole group
spasmsbyhourbyuser <- aggregate(seizure_ISexclude0spasms$type_ISSpasmNum, by=list(Category=seizure_ISexclude0spasms$Unlinked_ID), FUN=sum)
spasmsbyhourbyuser <- spasmsbyhourbyuser %>% 
  rename(
    Unlinked_ID = Category
  )

spasmsbyhourbyuser2 <- seizure_ISexclude0spasms %>% group_by(seizure_ISexclude0spasms$Unlinked_ID, Hours) %>% summarise(type_ISSpasmNum = sum(type_ISSpasmNum))
spasmsbyhourbyuser2 <- spasmsbyhourbyuser2 %>% 
  rename(
    Unlinked_ID = "seizure_ISexclude0spasms$Unlinked_ID"
  )

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==0)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==1)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==2)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==3)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==4)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==5)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==6)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==7)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==8)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==9)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==10)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==11)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==12)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==13)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==14)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==15)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==16)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==17)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==18)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==19)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==20)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==21)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==22)
length(unique(spasmsbyhourtime0$Unlinked_ID))

spasmsbyhourtime0=subset(spasmsbyhourbyuser2, Hours==23)
length(unique(spasmsbyhourtime0$Unlinked_ID))

percentatleast1spasmbyhour <- data.table(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23), c(26,45,37,40,38,39,55,67,77,71,70,71,58,69,67,66,68,67,66,65,59,55,45,32))
percentatleast1spasmbyhour$Percentage=((percentatleast1spasmbyhour$V2)/314)

percentatleast1spasmbyhour$HourChanged
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==12] <- 0
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==13] <- 1
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==14] <- 2
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==15] <- 3
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==16] <- 4
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==17] <- 5
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==18] <- 6
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==19] <- 7
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==20] <- 8
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==21] <- 9
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==22] <- 10
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==23] <- 11
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==0] <- 12
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==1] <- 13
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==2] <- 14
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==3] <- 15
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==4] <- 16
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==5] <- 17
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==6] <- 18
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==7] <- 19
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==8] <- 20
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==9] <- 21
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==10] <- 22
percentatleast1spasmbyhour$HourChanged[percentatleast1spasmbyhour$V1==11] <- 23

ggspasmshourpercentbyuser <- ggplot(data = percentatleast1spasmbyhour, aes(x=HourChanged, y=Percentage)) + geom_bar(stat="identity", fill = rgb(0,.46,.75), alpha = 0.8, color="black") + labs(title="Proportion with at Least One Spasm", y="Proportion", x = "Hour of day") + theme_bw() + scale_y_continuous(lim = c(0,0.8)) + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("12", "18",  "0", "6", "12"))
ggspasmshourpercentbyuser<- ggspasmshourpercentbyuser + theme(text = element_text(size=42))
ggspasmshourpercentbyuser

mergedusercounts <- merge(spasmsbyhourbyuser2, spasmsbyhourbyuser, by = "Unlinked_ID", all.x = TRUE)
mergedusercounts$PercentByHour=((mergedusercounts$type_ISSpasmNum)/(mergedusercounts$x))

mergedusercountsfinal <- aggregate(as.numeric(mergedusercounts$PercentByHour), list(mergedusercounts$Hours), mean)
mergedusercountsfinal <- mergedusercountsfinal %>% 
  rename(
    mean = x
  )

ggspasmshourpercentbyuser <- ggplot(data = mergedusercountsfinal, aes(x=Group.1, y=mean)) + geom_bar(stat="identity", fill = rgb(0,.46,.75), alpha = 0.8) + labs(title="Relative Abundance of Spasms for Each User by Hour of Day", y="Relative Abundance", x = "Hour of day") + theme_bw()
ggspasmshourpercentbyuser

library(vcdExtra)
spasmsbyhourexpanded1year <- expand.dft(spasmsbyhour1year, freq="x")
spasmsbyhourexpanded1year$Category2
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==12] <- 0
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==13] <- 1
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==14] <- 2
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==15] <- 3
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==16] <- 4
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==17] <- 5
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==18] <- 6
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==19] <- 7
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==20] <- 8
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==21] <- 9
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==22] <- 10
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==23] <- 11
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==0] <- 12
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==1] <- 13
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==2] <- 14
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==3] <- 15
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==4] <- 16
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==5] <- 17
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==6] <- 18
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==7] <- 19
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==8] <- 20
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==9] <- 21
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==10] <- 22
spasmsbyhourexpanded1year$Category2[spasmsbyhourexpanded1year$Category==11] <- 23

table(spasmsbyhourexpanded1year$Category)
table(spasmsbyhourexpanded1year$Category2)

ggspasmshourpercent1year <- ggplot(data = spasmsbyhourexpanded1year, aes(Category2)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(-1,24, by=1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Spasms by Hour of Day", y="Relative Frequency", x = "Hour of day") + theme_bw()
ggspasmshourpercent1year <- ggspasmshourpercent1year + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("12", "18",  "0", "6", "12"))
ggspasmshourpercent1year <- ggspasmshourpercent1year + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.15), labels = scales::percent)
ggspasmshourpercent1year

detach("package:plyr", unload=TRUE)
library(tidyverse)
library(dplyr)
agelessonefulldata %>% distinct(Unlinked_ID, Gender) %>% count(Gender) 

unique(agelessonefulldata$Congenital_Condition)
agelessonefulldata %>% distinct(Unlinked_ID, Congenital_Condition) %>% count(Congenital_Condition) 

#making cut sample for under 1

agelessoneexclude0spasms$HourOnset <- as.numeric(format(strptime(agelessoneexclude0spasms$Time, format = "%H:%M" ) , "%H" ) )
agelessoneexclude0spasms$MinuteOnset <- as.numeric(format(strptime(agelessoneexclude0spasms$Time, format = "%H:%M" ) , "%M" ) )
agelessoneexclude0spasms$TimeFromMidnight=((agelessoneexclude0spasms$HourOnset*3600)+(agelessoneexclude0spasms$MinuteOnset*60))

agelessonespasmscut=subset(agelessoneexclude0spasms, TimeFromMidnight > 76754 | TimeFromMidnight < 32400)
spasmsbyhour1yearcut <- aggregate(agelessonespasmscut$type_ISSpasmNum, by=list(Category=agelessonespasmscut$Hours), FUN=sum)
table(spasmsbyhour1yearcut$x)
sum(spasmsbyhour1yearcut$x)

table(agelessonespasmscut$Night)
agelessonespasmscut$minsover30
agelessonespasmscut$minsover30 <- 0
agelessonespasmscut$minsover30[agelessonespasmscut$MinuteOnset > 29] <- 1
table(agelessonespasmscut$HourOnset)

agelessonespasmscut$editedhour
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==21] <- 6
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==22] <- 7
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==23] <- 8
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==0] <- 9
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==1] <- 10
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==2] <- 11
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==3] <- 12
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==4] <- 13
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==5] <- 14
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==6] <- 15
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==7] <- 16
agelessonespasmscut$editedhour[agelessonespasmscut$HourOnset==8] <- 17
table(agelessonespasmscut$editedhour)

spasmsbyhourexpanded1year <- expand.dft(spasmsbyhour1year, freq="x")
table(spasmsbyhourexpanded1year$Category)

#first need to make file with 82167 spasms, 11432 are nighttime

agelessoneDayNight <- data.frame("Number" = 1:70735, "Night" = c(0))
agelessoneDayNight2 <- data.frame("Number" = 70736:82167, "Night" = c(1))
agelessoneNightFinal <- rbind(agelessoneDayNight, agelessoneDayNight2)
table(agelessoneNightFinal$Night)

binom.test(11432, 82067, p=0.3333, alternative = c("two.sided"), conf.level = 0.95)
binom.test(24671, 82067, p=0.552, alternative = c("two.sided"), conf.level = 0.95)


#doing night for cut sample

agelessoneNighttimeCut=subset(agelessonespasmscut, Night==1)
agelessoneDaytimeCut=subset(agelessonespasmscut, Night==0)
sum(agelessoneNighttimeCut$type_ISSpasmNum)
sum(agelessoneDaytimeCut$type_ISSpasmNum)

#first need to make file with 23361 spasms, 11432 are nighttime

agelessoneDayNightCut <- data.frame("Number" = 1:11929, "Night" = c(0))
agelessoneDayNightCut2 <- data.frame("Number" = 11930:23361, "Night" = c(1))
agelessoneDayNightFinalCut <- rbind(agelessoneDayNightCut, agelessoneDayNightCut2)
table(agelessoneDayNightFinalCut$Night)

#percentage of individual spasms comparison, 820 are individual out of  82167 spasms
agelessoneIndividual <- data.frame("Number" = 1:81347, "Individual" = c(0))
agelessoneIndividual2 <- data.frame("Number" = 81348:82167, "Individual" = c(1))
agelessoneIndividualFinal <- rbind(agelessoneIndividual, agelessoneIndividual2)
table(agelessoneIndividualFinal$Individual)



#only children more than one and looking at stats
agedatamoreone=subset(agedataexcludeoutliers, age > 365)
table(agedatamoreone$Unlinked_ID)
median(agedatamoreone$age)
quantile(agedatamoreone$age, c(0.25, 0.75))
unique(agedatamoreone$Unlinked_ID)

agemoreonefulldata=subset(seizure_IS, Unlinked_ID %in% agedatamoreone$Unlinked_ID)
unique(agemoreonefulldata$Unlinked_ID)

agemoreoneexclude1spasm=subset(agemoreonefulldata, type_ISSpasmNum > 1)
mean(agemoreoneexclude1spasm$type_ISSpasmNum, na.rm = TRUE)
median(agemoreoneexclude1spasm$type_ISSpasmNum, na.rm = TRUE)
quantile(agemoreoneexclude1spasm$type_ISSpasmNum, c(0.25, 0.5, 0.75), na.rm=TRUE)
sd(agemoreoneexclude1spasm$typeISSpasmNum, na.rm = TRUE)
class(agemoreoneexclude1spasm$type_ISSpasmNum)
Modes(agemoreoneexclude1spasm$type_ISSpasmNum)
table(agemoreoneexclude1spasm$type_ISSpasmNum)

agemoreoneexclude0time=subset(agemoreoneexclude1spasm, totaltime > 0)
mean(agemoreoneexclude0time$totaltime)
sd(agemoreoneexclude0time$totaltime)
mean(agemoreoneexclude0time$logtime)
median(agemoreoneexclude0time$totaltime)
quantile(agemoreoneexclude0time$totaltime, c(0.25, 0.5, 0.75))
Modes(agemoreoneexclude0time$totaltime)

agemoreoneexclude0spasms=subset(agemoreonefulldata, type_ISSpasmNum > 0)
sum(agemoreoneexclude0spasms$type_ISSpasmNum)
agemoreone1spasm=subset(agemoreoneexclude0spasms, type_ISSpasmNum==1)
#so 778 out of 144,439 are individual spasms for this group

#using night time as 11-7
agelessonefulldata$Night2
agelessonefulldata$Night2 <- 0
table(agelessonefulldata$Night2)
agelessonefulldata$Night2[agelessonefulldata$Hours < 7] <- 1
agelessonefulldata$Night2[agelessonefulldata$Hours==23] <- 1
table(agelessonefulldata$Night2)

agelessoneexclude0spasms=subset(agelessonefulldata, type_ISSpasmNum > 0)
agelessoneNighttime=subset(agelessoneexclude0spasms, Night2==1)
agelessoneDaytime=subset(agelessoneexclude0spasms, Night2==0)
sum(agelessoneNighttime$type_ISSpasmNum)
sum(agelessoneDaytime$type_ISSpasmNum)


#number of unique users, formation of three groups, and demographic differences between groups
unique(seizure_IS$Unlinked_ID)
unique(completedata$Unlinked_ID)

table(seizure_IS$Unlinked_ID)
table(completedata$Unlinked_ID)
table <- table(profiledata$Unlinked_ID)
sort(table, descending=TRUE)

mergedprofilecounts <- merge(countbyid, profiledata, by = "Unlinked_ID", all.x = TRUE)
mergedprofilecounts$congenital <- 1
mergedprofilecounts$congenital[mergedprofilecounts$Congenital_Condition==""] <- 0
table(mergedprofilecounts$congenital)
mergedprofilecounts$trauma <- 1
mergedprofilecounts$trauma[mergedprofilecounts$Brain_Trauma==""] <- 0
table(mergedprofilecounts$congenital)
table(mergedprofilecounts$trauma)
mergedprofilecounts$female2[mergedprofilecounts$Gender=="F"] <- 1
mergedprofilecounts$female2[mergedprofilecounts$Gender=="M"] <- 0
table(mergedprofilecounts$female2)
mergedprofilecounts$stroke <- 1
mergedprofilecounts$stroke[mergedprofilecounts$Stroke==""] <- 0
mergedprofilecounts$metabolic <- 1
mergedprofilecounts$metabolic[mergedprofilecounts$Metabolic_Disorder==""] <- 0
mergedprofilecounts$infection <- 1
mergedprofilecounts$infection[mergedprofilecounts$Infectious_Diseases==""] <- 0
mergedprofilecounts$hypoxia <- 1
mergedprofilecounts$hypoxia[mergedprofilecounts$Lack_Of_Oxygen_During_Birth==""] <- 0
mergedprofilecounts$maternaldrug <- 1
mergedprofilecounts$maternaldrug[mergedprofilecounts$Maternal_Drug_Or_Alcohol_Abuse==""] <- 0
mergedprofilecounts$malform <- 1
mergedprofilecounts$malform[mergedprofilecounts$Brain_Malformations==""] <- 0
mergedprofilecounts$electrolyte <- 1
mergedprofilecounts$electrolyte[mergedprofilecounts$Electrolyte_Disturbances==""] <- 0

median(mergedprofilecounts$freq)
quantile(mergedprofilecounts$freq, c(0.25, 0.75))
mean(mergedprofilecounts$freq)
range(mergedprofilecounts$freq)

groupstat<-ddply(mergedprofilecounts, .(group),summarise,
                 meansc= mean(freq), sd = sd(freq), mediansc=(median(freq)),
                 nfem= length(Gender[Gender=="F"]), 
                 nmale= length(Gender[Gender=="M"]),
                 percentfem= nfem/(nfem+nmale), nnotcongenital= length(Congenital_Condition[Congenital_Condition==""]),
                 nnottrauma= length(Brain_Trauma[Brain_Trauma==""]), nnotstroke= length(Stroke[Stroke==""]), nnotmetabolic= length(Metabolic_Disorder[Metabolic_Disorder==""]),
                 nnotinfection= length(Infectious_Diseases[Infectious_Diseases==""]), nnothypoxia= length(Lack_Of_Oxygen_During_Birth[Lack_Of_Oxygen_During_Birth==""]),
                 nnotdrug= length(Maternal_Drug_Or_Alcohol_Abuse[Maternal_Drug_Or_Alcohol_Abuse==""]),  nnotmalform= length(Brain_Malformations[Brain_Malformations==""]),
                 nnotelectrolyte= length(Electrolyte_Disturbances[Electrolyte_Disturbances==""]))

obsfreqstroke <- matrix(c(4,140, 5,118, 0,47),nrow=2,ncol=3)
chisq.test(obsfreqstroke)

obsfreqmet <- matrix(c(4,140, 1,122, 0,47),nrow=2,ncol=3)
chisq.test(obsfreqmet)

obsfreqinfec <- matrix(c(6,138, 5,118, 1,46),nrow=2,ncol=3)
chisq.test(obsfreqinfec)

obsfreqhypox <- matrix(c(2,142, 5,118, 0,47),nrow=2,ncol=3)
chisq.test(obsfreqhypox)

obsfreqdrugabuse <- matrix(c(1,143, 3,120, 0,47),nrow=2,ncol=3)
chisq.test(obsfreqdrugabuse)

obsfreqmalform <- matrix(c(16,128, 17,106, 6,41),nrow=2,ncol=3)
chisq.test(obsfreqmalform)

obsfreqtrauma <- matrix(c(11,133, 14,109, 0,47),nrow=2,ncol=3)
chisq.test(obsfreqtrauma)

obsfreqelectro <- matrix(c(0,144, 1,122, 0,47),nrow=2,ncol=3)
chisq.test(obsfreqelectro)

groupquantitative <- merge(countbyid, seizure_IS, by = "Unlinked_ID", all.x = TRUE)
groupquantitativeexclude1spasms=subset(groupquantitative, type_ISSpasmNum > 1)
groupquantitativeexclude0time=subset(groupquantitativeexclude1spasms, totaltime > 0)
groupstattime<-ddply(groupquantitativeexclude0time, .(group),summarise,
                  meansc= mean(totaltime), sd = sd(totaltime), mediansc=(median(totaltime)), meanlog= mean(logtime), sdlog= sd(logtime), TwentyFifth= quantile(totaltime, c(0.25)), SeventyFifth= quantile(totaltime, c(0.75)))
anovatime <- aov(logtime ~ group, data=groupquantitativeexclude0time)
summary(anovatime)

groupquantitative2 <- merge(countbyid, seizure_IS, by = "Unlinked_ID", all.x = TRUE)
groupquantitativeexclude1spasms2=subset(groupquantitative2, type_ISSpasmNum > 1)
groupstatspasms<-ddply(groupquantitativeexclude1spasms2, .(group),summarise,
                     meansc= mean(type_ISSpasmNum), sd = sd(type_ISSpasmNum), mediansc=(median(type_ISSpasmNum)), sdlog= sd(logtime), TwentyFifth= quantile(type_ISSpasmNum, c(0.25)), SeventyFifth= quantile(type_ISSpasmNum, c(0.75)))
anovaspasms <- aov(type_ISSpasmNum ~ group, data=groupquantitativeexclude0spasms)
summary(anovaspasms)

groupstatsspasmseverity<-ddply(groupquantitative, .(group),summarise,
                               meanseverity= mean(type_ISSeverity), sd = sd(type_ISSeverity))
tapply(groupquantitative$type_ISSeverity, groupquantitative$group, mean, na.rm=TRUE)
tapply(groupquantitative$type_ISSeverity, groupquantitative$group, sd, na.rm=TRUE)
anovaseverity <- aov(type_ISSeverity ~  group, data=groupquantitative)
summary(anovaseverity)

fit <- aov(freq ~ group, data=mergedprofilecounts)
summary(fit)

fit2 <- aov(congenital ~ group, data=mergedprofilecounts)
summary(fit2)

kruskal.test(congenital ~ group, data=mergedprofilecounts)
kruskal.test(trauma ~ group, data=mergedprofilecounts)
kruskal.test(female2~group, data=mergedprofilecounts)

kruskal.test(logtime~group, data=groupquantitativeexclude0time)
kruskal.test(type_ISSpasmNum~group, data=groupquantitativeexclude1spasms2)
kruskal.test(type_ISSeverity~group, data=groupquantitative)

TukeyHSD(aov(type_ISSpasmNum ~ as.factor(group), data=groupquantitativeexclude0spasms))
TukeyHSD(aov(logtime ~ as.factor(group), data=groupquantitativeexclude0time))
TukeyHSD(aov(type_ISSeverity ~ as.factor(group), data=groupquantitative))
TukeyHSD(aov(freq ~ as.factor(group), data=mergedprofilecounts))
TukeyHSD(aov(n ~ as.factor(group), data=medicationlistmerged))
TukeyHSD(aov(n.x ~ as.factor(group), data=tablenumbermeds))

excludegroup1exclude0time=subset(groupquantitativeexclude0time, group > 1)
excludegroup3exclude0time=subset(groupquantitativeexclude0time, group < 3)
excludegroup2exclude0time=subset(groupquantitativeexclude0time, group==1 | group==3)
wilcox.test(logtime ~ as.factor(group), data=excludegroup1exclude0time)
wilcox.test(logtime ~ as.factor(group), data=excludegroup2exclude0time)
wilcox.test(logtime ~ as.factor(group), data=excludegroup3exclude0time)

excludegroup1exclude0spasms=subset(groupquantitativeexclude1spasms2, group > 1)
excludegroup3exclude0spasms=subset(groupquantitativeexclude1spasms2, group < 3)
excludegroup2exclude0spasms=subset(groupquantitativeexclude1spasms2, group==1 | group==3)
wilcox.test(type_ISSpasmNum ~ as.factor(group), data=excludegroup1exclude0spasms)
wilcox.test(type_ISSpasmNum ~ as.factor(group), data=excludegroup2exclude0spasms)
wilcox.test(type_ISSpasmNum ~ as.factor(group), data=excludegroup3exclude0spasms)

excludegroup1meds=subset(tablenumbermeds, group > 1)
excludegroup3meds=subset(tablenumbermeds, group < 3)
excludegroup2meds=subset(tablenumbermeds, group==1 | group==3)
wilcox.test(n.x ~ as.factor(group), data=excludegroup1meds)
wilcox.test(n.x ~ as.factor(group), data=excludegroup2meds)
wilcox.test(n.x ~ as.factor(group), data=excludegroup3meds)



#first medication date
#am changing firstdatausedmeds variable here
fulldatausedmeds=subset(medicationdata, Daily_Dose > 0)
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Mg1XmW", "sq9hc8" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="DmQ2Gz", "qxkUOM" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="ONFvdf", "dMgSlh" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="w3AwGp", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="ro0okj", "wVErXI" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="fESlXO", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Hfx5Y8", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="03iBnP", "CEJIvF" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="IGygL5", "AGUGZk" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="U5XBzz", "ingak5" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="ZnVIdI", "nRa02Y" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="rLei4x", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="KUvWvh", "nRa02Y" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="m0DJo7", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="rEj38x", "TSiBRm" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="jdDBEL", "Lc4bz4" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="FlRk1h", "WHl7H1" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="w60MjQ", "qxkUOM" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="h8vJ05", "iDvQ2I" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="tgSko9", "ezGQ66" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="gyFbXW", "0thkIW" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="pv71YJ", "pSqhPn" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="bcZS6o", "1bp5qU" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="vG2vUu", "LrYCrT" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="GgCKdi", "xZWHPO" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="6xR13g", "nRa02Y" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="A9H9vb", "UfQgrX" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="BxbNDo", "ezGQ66" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="o8H7P0", "85dkRd" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="2RV2rl", "dMgSlh" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="VdgvYc", "LdHc8G" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="1GHb9k", "mJZ5pY" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="EKn9Cb", "ingak5" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="SH86Fk", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="7fLYPj", "qdhnwF" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="K213vn", "0JPQph" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="d7d9tS", "h5HHem" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="vC76Ls", "s9pYCP" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="d2Yig2", "GxK8gv" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="AQmFRg", "bQ1vCD" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="KoqRnf", "iDvQ2I" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="2sGEH7", "V2bV02" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="uEiGhC", "V2bV02" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="dU8zP5", "Pfgorr" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="wNhjE5", "Lc4bz4" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Q30rnt", "h5HHem" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="O6jxnT", "VdzyW2" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="iWXhNY", "LdHc8G" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="fbjBBY", "itvElF" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="D6HhVt", "LdHc8G" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="Z2l2XJ", "0thkIW" )
fulldatausedmeds$Med_Name_Code<-replace(fulldatausedmeds$Med_Name_Code, fulldatausedmeds$Med_Name_Code=="cqvIXz", "0thkIW" )

fulldatausedmeds$ModifiedMedDate <- as.Date(fulldatausedmeds$Start_Date, "%m/%d/%Y")

tablefirstmedication <- fulldatausedmeds %>%
  group_by(Unlinked_ID) %>%
  arrange(ModifiedMedDate) %>%
  slice(1L)

mergedmedtiming <- merge(tablefirstepisode, tablefirstmedication, by = "Unlinked_ID", all.x = TRUE)
mergedmedtiming$CorrectedMedDate <- as.Date(mergedmedtiming$ModifiedMedDate, "%y/%m/%d")

mergedmedtiming$timespasmtofirstmed <- as.Date(as.character(mergedmedtiming$ModifiedMedDate), format="%Y-%m-%d")-
  as.Date(as.character(mergedmedtiming$ModifiedDate), format="%Y-%m-%d")

randomtable2 <- table(mergedmedtiming$timespasmtofirstmed)
table(mergedmedtiming$timespasmtofirstmed)
sum(randomtable2)
mean(mergedmedtiming$timespasmtofirstmed, na.rm=TRUE)

mostcommonfirstmed <- sort(table(mergedmedtiming$Med_Name_Code), descending = TRUE)
mostcommonfirstmed
sum(mostcommonfirstmed)

#large comparisons Mann whitney

t.test(seizure_ISexclude1spasms$type_ISSpasmNum,clinicalsample$TotalNumSpasmsWithout1s, na.rm=TRUE)
wilcox.test(seizure_ISexclude1spasms$type_ISSpasmNum,clinicalsample$TotalNumSpasmsWithout1s, na.rm=TRUE)

t.test(seizure_ISexclude0time$totaltime,clinicalsample$Duration.Seizure, na.rm=TRUE)
wilcox.test(seizure_ISexclude0time$totaltime,clinicalsample$Duration.Seizure, na.rm=TRUE)


#day night comparison to clinical
#makingcutsample

seizure_ISexclude0spasms$HourOnset <- as.numeric(format(strptime(seizure_ISexclude0spasms$Time, format = "%H:%M" ) , "%H" ) )
seizure_ISexclude0spasms$MinuteOnset <- as.numeric(format(strptime(seizure_ISexclude0spasms$Time, format = "%H:%M" ) , "%M" ) )
seizure_ISexclude0spasms$TimeFromMidnight=((seizure_ISexclude0spasms$HourOnset*3600)+(seizure_ISexclude0spasms$MinuteOnset*60))

STspasmscut=subset(seizure_ISexclude0spasms, TimeFromMidnight > 76754 | TimeFromMidnight < 32400)
table(STspasmscut$Night)
STspasmscut$minsover30
STspasmscut$minsover30 <- 0
STspasmscut$minsover30[STspasmscut$MinuteOnset > 29] <- 1
table(STspasmscut$HourOnset)

STspasmscut$editedhour
STspasmscut$editedhour[STspasmscut$HourOnset==21] <- 6
STspasmscut$editedhour[STspasmscut$HourOnset==22] <- 7
STspasmscut$editedhour[STspasmscut$HourOnset==23] <- 8
STspasmscut$editedhour[STspasmscut$HourOnset==0] <- 9
STspasmscut$editedhour[STspasmscut$HourOnset==1] <- 10
STspasmscut$editedhour[STspasmscut$HourOnset==2] <- 11
STspasmscut$editedhour[STspasmscut$HourOnset==3] <- 12
STspasmscut$editedhour[STspasmscut$HourOnset==4] <- 13
STspasmscut$editedhour[STspasmscut$HourOnset==5] <- 14
STspasmscut$editedhour[STspasmscut$HourOnset==6] <- 15
STspasmscut$editedhour[STspasmscut$HourOnset==7] <- 16
STspasmscut$editedhour[STspasmscut$HourOnset==8] <- 17
table(STspasmscut$editedhour)


#first need to make file with 226932 spasms, 53978 are nighttime

STSpasmsDayNight <- data.frame("Number" = 1:172954, "Night" = c(0))
STSpasmsDayNight2 <- data.frame("Number" = 172955:226932, "Night" = c(1))
STSpasmsDayNightFinal <- rbind(STSpasmsDayNight, STSpasmsDayNight2)
table(STSpasmsDayNightFinal$Night)

#doing night for cut sample

seizure_ISNighttimeCut=subset(STspasmscut, Night==1)
seizure_ISDaytimeCut=subset(STspasmscut, Night==0)
sum(seizure_ISNighttimeCut$type_ISSpasmNum)
sum(seizure_ISDaytimeCut$type_ISSpasmNum)

#first need to make file with 96783 spasms, 53978 are nighttime

STSpasmsDayNightCut <- data.frame("Number" = 1:42805, "Night" = c(0))
STSpasmsDayNightCut2 <- data.frame("Number" = 42806:96783, "Night" = c(1))
STSpasmsDayNightFinalCut <- rbind(STSpasmsDayNightCut, STSpasmsDayNightCut2)
table(STSpasmsDayNightFinalCut$Night)

#percentage of individual spasms comparison, 1603 are individual out of  225329 spasms
STSpasmsIndividual <- data.frame("Number" = 1:225329, "Individual" = c(0))
STSpasmsIndividual2 <- data.frame("Number" = 225330:226932, "Individual" = c(1))
STSpasmsIndividualFinal <- rbind(STSpasmsIndividual, STSpasmsIndividual2)
table(STSpasmsIndividualFinal$Individual)



#spasmsbyhour
spasmsbyhour <- aggregate(seizure_ISexclude0spasms$type_ISSpasmNum, by=list(Category=seizure_ISexclude0spasms$Hours), FUN=sum)
sum(spasmsbyhour$x)

#cut sample spasmsbyhour
spasmsbyhourcut <- aggregate(STspasmscut$type_ISSpasmNum, by=list(Category=STspasmscut$editedhour), FUN=sum)
sum(spasmsbyhourcut$x)

STspasmscut$EditedTimeFromMidnight=((STspasmscut$editedhour*3600)+(STspasmscut$MinuteOnset*60))
spasmsbytimecut <- aggregate(STspasmscut$type_ISSpasmNum, by=list(Category=STspasmscut$EditedTimeFromMidnight), FUN=sum)
sum(spasmsbytimecut$x)

library(vcdExtra)
spasmsbyhourcut <- expand.dft(spasmsbyhourcut, freq="x")
spasmsbytimecut <- expand.dft(spasmsbytimecut, freq="x")
spasmsbyhour21to9=subset(spasmsbyhour, Category > 20 | Category < 9)
spasmsbyhour21to9$editedhour
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==21] <- 6
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==22] <- 7
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==23] <- 8
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==0] <- 9
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==1] <- 10
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==2] <- 11
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==3] <- 12
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==4] <- 13
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==5] <- 14
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==6] <- 15
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==7] <- 16
spasmsbyhour21to9$editedhour[spasmsbyhour21to9$Category==8] <- 17

timestable=subset(spasmsbytimecut, Category==43200)

library(vcdExtra)
spasmsbyhourexpanded <- expand.dft(spasmsbyhour, freq="x")
ggspasmshour <- ggplot(data = spasmsbyhourexpanded, aes(Category)) + geom_histogram(breaks=seq(-1,24, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Spasms by Hour of Day", y="Frequency", x = "Hour of day") + theme_bw()
ggspasmshour <- ggspasmshour + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("0", "6",  "12", "18", "24"))
ggspasmshour <- ggspasmshour + theme(text = element_text(size=40)) 
ggspasmshour

ggspasmshourpercent <- ggplot(data = spasmsbyhourexpanded, aes(Category)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(-1,24, by=1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Spasms by Hour of Day", y="Relative Frequency", x = "Hour of day") + theme_bw()
ggspasmshourpercent <- ggspasmshourpercent + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("0", "6",  "12", "18", "24"))
ggspasmshourpercent <- ggspasmshourpercent + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.15), labels = scales::percent)
ggspasmshourpercent

table(spasmsbyhourexpanded$Category)

#changing x-axis to noon-noon

spasmsbyhourexpanded$Category2
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==12] <- 0
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==13] <- 1
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==14] <- 2
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==15] <- 3
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==16] <- 4
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==17] <- 5
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==18] <- 6
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==19] <- 7
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==20] <- 8
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==21] <- 9
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==22] <- 10
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==23] <- 11
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==0] <- 12
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==1] <- 13
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==2] <- 14
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==3] <- 15
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==4] <- 16
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==5] <- 17
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==6] <- 18
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==7] <- 19
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==8] <- 20
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==9] <- 21
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==10] <- 22
spasmsbyhourexpanded$Category2[spasmsbyhourexpanded$Category==11] <- 23

table(spasmsbyhourexpanded$Category)
table(spasmsbyhourexpanded$Category2)


ggspasmshourpercent <- ggplot(data = spasmsbyhourexpanded, aes(Category2)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(-1,24, by=1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Spasms by Hour of Day", y="Relative Frequency", x = "Hour of day") + theme_bw()
ggspasmshourpercent <- ggspasmshourpercent + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("12", "18",  "0", "6", "12"))
ggspasmshourpercent <- ggspasmshourpercent + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.15), labels = scales::percent)
ggspasmshourpercent

#differentiate bar colors

spasmsbyhourexpanded$outsideclinicalrange
spasmsbyhourexpanded$outsideclinicalrange <- 0
spasmsbyhourexpanded$outsideclinicalrange[spasmsbyhourexpanded$Category > 8 & spasmsbyhourexpanded$Category < 16] <- 1

ggspasmshour <- ggplot(data = spasmsbyhourexpanded, aes(Category)) + geom_histogram(data=subset(spasmsbyhourexpanded, outsideclinicalrange==0), breaks=seq(-1,24, by=1), col="red", fill="blue", alpha=0.2) + geom_histogram(data=subset(spasmsbyhourexpanded, outsideclinicalrange==1), breaks=seq(-1,24, by=1), col="red", fill="blue4", alpha=0.2) + labs(title="Spasms by Hour of Day", y="Frequency", x = "Hour of day") + theme_bw()
ggspasmshour <- ggspasmshour + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("0", "6",  "12", "18", "24"))
ggspasmshour <- ggspasmshour + theme(text = element_text(size=40)) 
ggspasmshour

ggspasmsdensity <- ggplot(spasmsbyhour21to9) + geom_density( aes(x=editedhour)) + ggtitle("Density Plot of Spasms by Time of Day")
ggspasmsdensity <- ggspasmsdensity + scale_x_continuous(labels=c("21","22","23","0","1","2","3","4","5","6","7","8"), breaks = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
ggspasmsdensity <- ggspasmsdensity + labs(title="Density Plot of Spasms by Time of Day", y="Density", x = "Hour of day") + theme_bw()
ggspasmsdensity <- ggspasmsdensity + theme(text = element_text(size=30))
ggspasmsdensity

#chi-square seizure comparisons
obsfreqindividualspasms <- matrix(c(1603,223726, 820,81247, 44,491),nrow=2,ncol=3)
chisq.test(obsfreqindividualspasms)

obsfreqindividualspasms <- matrix(c(820,81247, 44,491),nrow=2,ncol=2)
chisq.test(obsfreqindividualspasms)

obsfreqindividualspasms <- matrix(c(1603,223726, 44,491),nrow=2,ncol=2)
chisq.test(obsfreqindividualspasms)

obsfreqindividualspasms <- matrix(c(1603,223726, 820,81247),nrow=2,ncol=2)
chisq.test(obsfreqindividualspasms)


obsfreqnightspasmscut <- matrix(c(53978,42805, 11432,11929, 289,144),nrow=2,ncol=3)
chisq.test(obsfreqnightspasmscut)

obsfreqnightspasmscut <- matrix(c(53978,42805, 289,144),nrow=2,ncol=2)
chisq.test(obsfreqnightspasmscut)

obsfreqnightspasmscut <- matrix(c(11432,11929, 289,144),nrow=2,ncol=2)
chisq.test(obsfreqnightspasmscut)

obsfreqnightspasmscut <- matrix(c(53978,42805, 11432,11929),nrow=2,ncol=2)
chisq.test(obsfreqnightspasmscut)

#making groups for ANOVA seizure data

seizure_ISexclude1spasms=subset(seizure_IS, type_ISSpasmNum > 1)

STanovaspasms <- data.frame("Spasms" = seizure_ISexclude1spasms$type_ISSpasmNum, "Group" = c(1))
STunderoneanovaspasms <- data.frame("Spasms" = agelessoneexclude1spasm$type_ISSpasmNum, "Group" = c(2))
Clinicalanovaspasms <- data.frame("Spasms" = importspasmsdata$TotalNumSpasmsWithout1s, "Group" = c(3))

STanovaspasmscomparison <- rbind(STanovaspasms, STunderoneanovaspasms)
fullanovaspasmscomparison <- rbind(STanovaspasmscomparison, Clinicalanovaspasms)
table(fullanovaspasmscomparison$Group)

spasmscomparison3groups <- aov(Spasms ~ Group, data = fullanovaspasmscomparison)
summary(spasmscomparison3groups)
kruskal.test(Spasms ~ Group, data = fullanovaspasmscomparison)

Clinicaltounderonespasms <- rbind(Clinicalanovaspasms, STunderoneanovaspasms)
spasmscomparison2groups <- aov(Spasms ~ Group, data = Clinicaltounderonespasms)
summary(spasmscomparison2groups)
t.test(importspasmsdata$TotalNumSpasmsWithout1s, agelessoneexclude1spasm$type_ISSpasmNum, na.rm=TRUE)
kruskal.test(Spasms ~ Group, data = Clinicaltounderonespasms)

Clinicaltowholespasms <- rbind(Clinicalanovaspasms, STanovaspasms)
spasmscomparisongroups <- aov(Spasms ~ Group, data = Clinicaltowholespasms)
summary(spasmscomparisongroups)
t.test(importspasmsdata$TotalNumSpasmsWithout1s, seizure_ISexclude1spasms$type_ISSpasmNum, na.rm=TRUE)
kruskal.test(Spasms ~ Group, data = Clinicaltowholespasms)

t.test(seizure_ISexclude1spasms$type_ISSpasmNum, agelessoneexclude1spasm$type_ISSpasmNum, na.rm=TRUE)



STanovaduration <- data.frame("Duration" = seizure_ISexclude0time$totaltime, "Group" = c(1))
STunderoneanovaduration <- data.frame("Duration" = agelessoneexclude0time$totaltime, "Group" = c(2))
Clinicalanovaduration <- data.frame("Duration" = importdurationdata$Duration.Seizure, "Group" = c(3))

STanovadurationcomparison <- rbind(STanovaduration, STunderoneanovaduration)
fullanovadurationcomparison <- rbind(STanovadurationcomparison, Clinicalanovaduration)
table(fullanovadurationcomparison$Group)

durationcomparison3groups <- aov(Duration ~ Group, data = fullanovadurationcomparison)
summary(durationcomparison3groups)
kruskal.test(Duration ~ Group, data = fullanovadurationcomparison)

Clinicaltounderoneduration <- rbind(Clinicalanovaduration, STunderoneanovaduration)
t.test(importdurationdata$Duration.Seizure, agelessoneexclude0time$totaltime, na.rm=TRUE)
kruskal.test(Duration ~ Group, data = Clinicaltounderoneduration)

Clinicaltowholeduration <- rbind(Clinicalanovaduration, STanovaduration)
t.test(importdurationdata$Duration.Seizure, seizure_ISexclude1spasms$totaltime, na.rm=TRUE)
kruskal.test(Duration ~ Group, data = Clinicaltowholeduration)
