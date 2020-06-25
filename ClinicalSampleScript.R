#Clinical Sample script for Brian LaGrant
library(foreign)
clinicalsample = read.csv("~/Desktop/Weill Documents/SeizureTrackerProject/FinalDatasetISSeizures2mins.csv", header=TRUE)
library(car)
profiles = read.csv("~/Desktop/Weill Documents/SeizureTrackerProject/FinalDatasetISProfiles.csv", header=TRUE)
profiles <- profiles[-c(10, 11, 12, 13, 14, 15), ]
spasms = read.csv("~/Desktop/Weill Documents/SeizureTrackerProject/AllSpasmTimes.csv", header=TRUE)

#descriptive data
table(clinicalsample$TotalNumSpasmsWithout1s)
sum(clinicalsample$X11pm.7am)
sum(clinicalsample$TotalNumSpasmsWithout1s, na.rm=TRUE)
sum(clinicalsample$TotalNumSpasms)

mean(clinicalsample$TotalNumSpasmsWithout1s, na.rm=TRUE)
sd(clinicalsample$TotalNumSpasmsWithout1s, na.rm=TRUE)
median(clinicalsample$TotalNumSpasmsWithout1s, na.rm=TRUE)
quantile(clinicalsample$TotalNumSpasmsWithout1s, 0.25, na.rm=TRUE)
quantile(clinicalsample$TotalNumSpasmsWithout1s, 0.75, na.rm=TRUE)

mean(clinicalsample$Duration.Seizure, na.rm=TRUE)
sd(clinicalsample$Duration.Seizure, na.rm=TRUE)
median(clinicalsample$Duration.Seizure, na.rm=TRUE)
quantile(clinicalsample$Duration.Seizure, 0.25, na.rm=TRUE)
quantile(clinicalsample$Duration.Seizure, 0.75, na.rm=TRUE)

(sum(profiles$TotalTimeNight))/(sum(profiles$TotalTimeRecordedSec))
mean(profiles$AgeDaysVEEG)
sd(profiles$AgeDaysVEEG)
median(profiles$AgeDaysVEEG)
quantile(profiles$AgeDaysVEEG, 0.25)
quantile(profiles$AgeDaysVEEG, 0.75)

sum(profiles$TxNaiveVEEG)

#ggplots

ggdurationclinical <- ggplot(data = clinicalsample, aes(clinicalsample$Duration.Seizure)) + geom_histogram(breaks=seq(-1,540,by=30), col="red", fill="blue", alpha = 0.2) + labs(title="Duration of Cluster", y="Frequency", x="Time") + theme_bw()
ggdurationclinical <- ggdurationclinical + scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 360, 420, 480, 540), labels=c("0m", "1min", "2min", "3min", "4min", "5min", "6min", "7min", "8min", "9min"))
ggdurationclinical <- ggdurationclinical + theme(text = element_text(size=30)) + scale_y_continuous(limits=c(0,12), breaks=c(0,3,6,9,12), labels=c("0", "3", "6", "9", "12"))
ggdurationclinical 

ggdurationclinical2 <- ggplot(data = clinicalsample, aes(clinicalsample$Duration.Seizure)) + geom_histogram(breaks=seq(-1,720,by=60), col="red", fill="blue", alpha = 0.2) + labs(title="Duration of Cluster", y="Frequency", x="Time") + theme_bw()
ggdurationclinical2 <- ggdurationclinical2 + scale_x_continuous(breaks = c(0, 60, 120, 180, 240, 300, 360, 420, 480, 540, 600, 660, 720), labels=c("0min", "1min", "2min", "3min", "4min", "5min", "6min", "7min", "8min", "9min", "10min", "11min", "12min"))
ggdurationclinical2 <- ggdurationclinical2 + theme(text = element_text(size=30)) 
ggdurationclinical2 

clinicalsample$logtime=(log10(clinicalsample$Duration.Seizure))
head(seizure_IS$logtime)
ggtimelog <- ggplot(data = clinicalsample, aes(clinicalsample$logtime)) + geom_histogram(breaks=seq(0,4,by=0.1), col="red", fill="blue", alpha = 0.2) + labs(title="Duration of Cluster", y="Frequency", x="Time") + theme_bw()
ggtimelog <- ggtimelog + scale_x_continuous(breaks = c(0, 0.30, 0.48, 0.60, 0.70, 0.78, 0.85, 0.90, 0.95, 1, 1.3, 1.48, 1.60, 1.70, 1.78, 1.85, 1.90, 1.95, 2, 2.3, 2.48, 2.60, 2.70, 2.78, 2.85, 2.90, 2.95, 3, 3.3, 3.48, 3.60, 3.70, 3.78, 3.85, 3.90, 3.95), labels=c("1s", " ", " ", " ", " ", " ", " ", " ", " ", "10s", " ", " ", " ", " ", " ", " ", " ", " ", "100s"," ", " ", " ", " ", " ", " ", " ", " ", "1000s", " ", " ", " ", " ", " ", " ", " ", " "))
ggtimelog <- ggtimelog + theme(text = element_text(size=40))
ggtimelog 

ggtimelogpercent <- ggplot(data = clinicalsample, aes(clinicalsample$logtime)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(0,4,by=0.1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Duration of Cluster", y="Relative frequency", x="Time") + theme_bw()
ggtimelogpercent <- ggtimelogpercent + scale_x_continuous(breaks = c(0, 0.30, 0.48, 0.60, 0.70, 0.78, 0.85, 0.90, 0.95, 1, 1.3, 1.48, 1.60, 1.70, 1.78, 1.85, 1.90, 1.95, 2, 2.3, 2.48, 2.60, 2.70, 2.78, 2.85, 2.90, 2.95, 3, 3.3, 3.48, 3.60, 3.70, 3.78, 3.85, 3.90, 3.95), labels=c("1s", " ", " ", " ", " ", " ", " ", " ", " ", "10s", " ", " ", " ", " ", " ", " ", " ", " ", "100s"," ", " ", " ", " ", " ", " ", " ", " ", "1000s", " ", " ", " ", " ", " ", " ", " ", " "))
ggtimelogpercent <- ggtimelogpercent + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.15), labels = scales::percent)
ggtimelogpercent 

ggspasmsclinical <- ggplot(data = clinicalsample, aes(clinicalsample$TotalNumSpasmsWithout1s)) + geom_histogram(breaks=seq(1,50, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Spasms per Cluster", y="Frequency", x = "Number of spasms") + theme_bw()
ggspasmsclinical <- ggspasmsclinical + scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"))
ggspasmsclinical <- ggspasmsclinical + theme(text = element_text(size=40)) 
ggspasmsclinical

ggspasmsclinicalpercent <- ggplot(data = clinicalsample, aes(clinicalsample$TotalNumSpasmsWithout1s)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(1,50, by=1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Spasms per Cluster", y="Relative frequency", x = "Number of spasms") + theme_bw()
ggspasmsclinicalpercent <- ggspasmsclinicalpercent + scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), labels=c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"))
ggspasmsclinicalpercent <- ggspasmsclinicalpercent + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.1), labels = scales::percent)
ggspasmsclinicalpercent

#code for comparisons to ST

importspasms=subset(clinicalsample, TotalNumSpasmsWithout1s > 1)
importspasms <- importspasms[,7]

importduration=subset(clinicalsample, Duration.Seizure > 0)
importduration <- importduration[,5]

#daynight
nightsubset=subset(clinicalsample, X11pm.7am==1)
daysubset=subset(clinicalsample, X11pm.7am==0)
sum(nightsubset$TotalNumSpasms)
sum(daysubset$TotalNumSpasms)

sum(clinicalsample$X11pm.7am)

#making time variables
library(ggplot2)
clinicalsample$PatientNumber
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 1
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 2
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 3
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 4
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 5
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 6
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 7
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 8
clinicalsample$PatientNumber[clinicalsample$ID==XX] <- 9

clinicalsample$TimeDayOnset <- as.POSIXct(clinicalsample$TimeSeizureOnset, format="%H:%M:%S")
table(clinicalsample$TimeDayOnset)
library(chron) 
clinicalsample$TimeOnset <- times(strftime(clinicalsample$TimeDayOnset,"%H:%M:%S"))
clinicalsample$HourOnset <- as.numeric(format(strptime(clinicalsample$TimeOnset, format = "%H:%M:%S" ) , "%H" ) )
clinicalsample$MinuteOnset <- as.numeric(format(strptime(clinicalsample$TimeOnset, format = "%H:%M:%S" ) , "%M" ) )
clinicalsample$SecondOnset <- as.numeric(format(strptime(clinicalsample$TimeOnset, format = "%H:%M:%S" ) , "%S" ) )
clinicalsample$TimeFromMidnight=((clinicalsample$HourOnset*3600)+(clinicalsample$MinuteOnset*60)+(clinicalsample$SecondOnset))

table(clinicalsample$HourOnset)

ggplottimes <- ggplot(data=clinicalsample, aes(x=TimeFromMidnight,y=as.character(PatientNumber))) + geom_point() + labs(title="Seizures by Time of Day", y="Patient Number", x="Time") + theme_bw()
ggplottimes <- ggplottimes + scale_x_continuous(labels=c("0:00", "1:00", "2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"), breaks = c(0, 3600, 7200, 10800, 14400, 18000, 21600, 25200, 28800, 32400, 36000, 39600, 43200, 46800, 50400, 54000, 57600, 61200, 64800, 68400, 72000, 75600, 79200, 82800))
ggplottimes <- ggplottimes + geom_rect(xmin=0, xmax = 25200, ymin = 0, ymax =  Inf, fill='gray', alpha = 0.01) + geom_rect(xmin=82800, xmax=86400, ymin = 0, ymax  = Inf, fill = 'gray', alpha =0.01) + geom_point(size  = 1.5)
ggplottimes

spasms$PatientNumber
spasms$PatientNumber[spasms$ID==XX] <- 1
spasms$PatientNumber[spasms$ID==XX] <- 2
spasms$PatientNumber[spasms$ID==XX] <- 3
spasms$PatientNumber[spasms$ID==XX] <- 4
spasms$PatientNumber[spasms$ID==XX] <- 5
spasms$PatientNumber[spasms$ID==XX] <- 6
spasms$PatientNumber[spasms$ID==XX] <- 7
spasms$PatientNumber[spasms$ID==XX] <- 8
spasms$PatientNumber[spasms$ID==XX] <- 9

spasms$TimeDayOnset <- as.POSIXct(spasms$Time, format="%H:%M:%S")
table(spasms$TimeDayOnset)
library(chron) 
spasms$TimeOnset <- times(strftime(spasms$TimeDayOnset,"%H:%M:%S"))
spasms$HourOnset <- as.numeric(format(strptime(spasms$TimeOnset, format = "%H:%M:%S" ) , "%H" ) )
spasms$MinuteOnset <- as.numeric(format(strptime(spasms$TimeOnset, format = "%H:%M:%S" ) , "%M" ) )
spasms$SecondOnset <- as.numeric(format(strptime(spasms$TimeOnset, format = "%H:%M:%S" ) , "%S" ) )
spasms$TimeFromMidnight=((spasms$HourOnset*3600)+(spasms$MinuteOnset*60)+(spasms$SecondOnset))
spasms$night
spasms$night <- 0
spasms$night[spasms$TimeFromMidnight < 25200 | spasms$TimeFromMidnight > 82800] <- 1
table(spasms$night)
#cut sample in case use this
clinicalspasmscut=subset(spasms, TimeFromMidnight > 76754 | TimeFromMidnight < 32400)
table(clinicalspasmscut$HourOnset)

importnight <- spasms[,10]
importnightcut <- spasms[,10]

#making individual spasms dataset
ClinicalSpasmsIndividual <- data.frame("Number" = 1:465, "Individual" = c(0))
ClinicalSpasmsIndividual2 <- data.frame("Number" = 466:535, "Individual" = c(1))
ClinicalSpasmsIndividualFinal <- rbind(ClinicalSpasmsIndividual, ClinicalSpasmsIndividual2)
table(ClinicalSpasmsIndividualFinal$Individual)
importindividual <- ClinicalSpasmsIndividualFinal[,2]

ggplotspasms <- ggplot(data=spasms, aes(x=TimeFromMidnight,y=as.character(PatientNumber))) + geom_point(shape=3, size=1) + labs(title="Spasms by Time of Day", y="Patient Number", x="Time") + theme_bw()
ggplotspasms <- ggplotspasms + scale_x_continuous(labels=c("0:00", "1:00", "2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"), breaks = c(0, 3600, 7200, 10800, 14400, 18000, 21600, 25200, 28800, 32400, 36000, 39600, 43200, 46800, 50400, 54000, 57600, 61200, 64800, 68400, 72000, 75600, 79200, 82800))
ggplotspasms <- ggplotspasms + geom_rect(xmin=0, xmax = 25200, ymin = 0, ymax =  Inf, fill='gray', alpha = 0.001) + geom_rect(xmin=82800, xmax=86400, ymin = 0, ymax  = Inf, fill = 'gray', alpha =0.01)
ggplotspasms

gghourclinical <- ggplot(data = spasms, aes(spasms$HourOnset)) + geom_histogram(breaks=seq(-1,24, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Spasms by Hour of Day", y="Frequency", x = "Hour of day") + theme_bw()
gghourclinical <- gghourclinical + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("0", "6",  "12", "18", "24"))
gghourclinical <- gghourclinical + theme(text = element_text(size=40))
gghourclinical

gghourclinicalpercent <- ggplot(data = spasms, aes(spasms$HourOnset)) + geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(-1,24, by=1), col="black", fill=rgb(0,.46,.75), alpha = 0.8) + labs(title="Spasms by Hour of Day", y="Relative frequency", x = "Hour of day") + theme_bw()
gghourclinicalpercent <- gghourclinicalpercent + scale_x_continuous(breaks = c(-1,5,11,17,24), labels=c("0", "6",  "12", "18", "24"))
gghourclinicalpercent <- gghourclinicalpercent + theme(text = element_text(size=42)) + scale_y_continuous(lim = c(0,0.15), labels = scales::percent)
gghourclinicalpercent

table(spasms$HourOnset)

#for density plot recode 16-23 1-8 and 0-9 as 9-17

spasms$editedhour
spasms$editedhour[spasms$HourOnset==16] <- 1
spasms$editedhour[spasms$HourOnset==17] <- 2
spasms$editedhour[spasms$HourOnset==18] <- 3
spasms$editedhour[spasms$HourOnset==19] <- 4
spasms$editedhour[spasms$HourOnset==20] <- 5
spasms$editedhour[spasms$HourOnset==21] <- 6
spasms$editedhour[spasms$HourOnset==22] <- 7
spasms$editedhour[spasms$HourOnset==23] <- 8
spasms$editedhour[spasms$HourOnset==0] <- 9
spasms$editedhour[spasms$HourOnset==1] <- 10
spasms$editedhour[spasms$HourOnset==2] <- 11
spasms$editedhour[spasms$HourOnset==3] <- 12
spasms$editedhour[spasms$HourOnset==4] <- 13
spasms$editedhour[spasms$HourOnset==5] <- 14
spasms$editedhour[spasms$HourOnset==6] <- 15
spasms$editedhour[spasms$HourOnset==7] <- 16
spasms$editedhour[spasms$HourOnset==8] <- 17
table(spasms$editedhour)

table(clinicalspasmscut$HourOnset)

clinicalspasmscut$editedhour
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==21] <- 6
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==22] <- 7
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==23] <- 8
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==0] <- 9
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==1] <- 10
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==2] <- 11
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==3] <- 12
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==4] <- 13
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==5] <- 14
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==6] <- 15
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==7] <- 16
clinicalspasmscut$editedhour[clinicalspasmscut$HourOnset==8] <- 17
table(clinicalspasmscut$editedhour)

clinicalspasmscut$EditedTimeFromMidnight=((clinicalspasmscut$editedhour*3600)+(clinicalspasmscut$MinuteOnset*60)+(clinicalspasmscut$SecondOnset))


spasmsonly21to8 <- subset(spasms, editedhour > 5)
table(spasmsonly21to8$editedhour)

ggplotspasmsdensity <- ggplot(spasmsonly21to8) + geom_density( aes(x=editedhour)) + ggtitle("Density Plot of Spasms by Time of Day")
ggplotspasmsdensity <- ggplotspasmsdensity + scale_x_continuous(labels=c("21","22","23","0","1","2","3","4","5","6","7","8"), breaks = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
ggplotspasmsdensity <- ggplotspasmsdensity + labs(title="Density Plot of Spasms by Time of Day", y="Density", x = "Hour of day") + theme_bw()
ggplotspasmsdensity <- ggplotspasmsdensity + theme(text = element_text(size=30))
ggplotspasmsdensity

ggplotspasmshour <- ggplot(data = spasms, aes(editedhour)) + geom_histogram(breaks=seq(0,18, by=1), col="red", fill="blue", alpha=0.2) + labs(title="Spasms by Hour of Day", y="Frequency", x = "Hour of day") + theme_bw()
ggplotspasmshour <- ggplotspasmshour + scale_x_continuous(breaks = c(0,4,8,12,16), labels=c("16", "20",  "0", "4", "8"))
ggplotspasmshour <- ggplotspasmshour + theme(text = element_text(size=40))
ggplotspasmshour
table(spasms$editedhour)

binom.test(289, 535, p=0.541, alternative = c("two.sided"), conf.level = 0.95)

#combined density plot

clinicalspasmstocombine <- data.frame(clinicalspasmscut[,11])
clinicalspasmstocombine$clinical
clinicalspasmstocombine$clinical <- 1
colnames(clinicalspasmstocombine)[1] <- "hour"
head(clinicalspasmstocombine)
table(clinicalspasmstocombine$hour)

clinicalspasmstocombine2 <- data.frame(clinicalspasmscut[,12])
clinicalspasmstocombine2$clinical
clinicalspasmstocombine2$clinical <- 1
colnames(clinicalspasmstocombine2)[1] <- "time"
head(clinicalspasmstocombine2)
table(clinicalspasmstocombine2$time)
clinicalspasmstocombine30mins <- as.data.frame(table(cut(clinicalspasmstocombine2$time, breaks=seq(21600,64800, by=1800))))


STspasmstocombine <- data.frame(spasmsbyhourcut[,1])
STspasmstocombine$clinical
STspasmstocombine$clinical <- 0
colnames(STspasmstocombine)[1] <- "hour"
head(STspasmstocombine)
table(STspasmstocombine$hour)

STspasmstocombine2 <- data.frame(spasmsbytimecut[,1])
STspasmstocombine2$clinical
STspasmstocombine2$clinical <- 0
colnames(STspasmstocombine2)[1] <- "time"
head(STspasmstocombine2)
STspasmstocombine30mins <- as.data.frame(table(cut(STspasmstocombine2$time, breaks=seq(21600,64800, by=1800))))


combinedspasmsdensity <- rbind(clinicalspasmstocombine, STspasmstocombine)
combinedspasmsdensity$clinical[combinedspasmsdensity$clinical==0] <- "Seizure Tracker"
combinedspasmsdensity$clinical[combinedspasmsdensity$clinical==1] <- "Clinical"
table(combinedspasmsdensity$clinical)
class(combinedspasmsdensity$clinical)

combinedspasmsdensity2 <- rbind(clinicalspasmstocombine2, STspasmstocombine2)
combinedspasmsdensity2$clinical[combinedspasmsdensity2$clinical==0] <- "Seizure Tracker"
combinedspasmsdensity2$clinical[combinedspasmsdensity2$clinical==1] <- "Clinical"
table(combinedspasmsdensity2$clinical)
class(combinedspasmsdensity2$clinical)

library(reshape2)
library(plotly)
spasmsdensitycombined <- ggplot(combinedspasmsdensity, aes(x=hour, fill = clinical)) + geom_density(alpha = 0.4) + ggtitle("Density Plot of Spasms by Time of Day")
spasmsdensitycombined <- spasmsdensitycombined + scale_x_continuous(labels=c("21","22","23","0","1","2","3","4","5","6","7","8"), breaks = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
spasmsdensitycombined <- spasmsdensitycombined + labs(title="Density Plot of Spasms by Time of Day", y="Density", x = "Hour of day") + theme_bw()
spasmsdensitycombined <- spasmsdensitycombined + theme(text = element_text(size=20)) + scale_y_continuous(labels=c("0.00", "0.05", "0.10", "0.15"), breaks=c(0,0.05,0.10,0.15))
spasmsdensitycombined <- spasmsdensitycombined + scale_fill_discrete(name = "Cohort")
spasmsdensitycombined

spasmsdensitycombined <- ggplot(combinedspasmsdensity2, aes(x=time, fill = clinical)) + geom_density(alpha = 0.4) + ggtitle("Density Plot of Spasms by Time of Day")
spasmsdensitycombined <- spasmsdensitycombined + scale_x_continuous(labels=c("21","22","23","0","1","2","3","4","5","6","7","8"), breaks = c(21600, 25200, 28800, 32400, 36000, 39600, 43200, 46800, 50400, 54000, 57600, 61200))
spasmsdensitycombined <- spasmsdensitycombined + labs(title="Density Plot of Spasms by Time of Day", y="Density", x = "Hour of day") + theme_bw()
spasmsdensitycombined <- spasmsdensitycombined + scale_fill_discrete(name = "Cohort")
spasmsdensitycombined

spasmsdensitycombined30 <- ggplot(combinedspasmsdensity2, aes(x=time, fill = clinical)) + geom_density(alpha = 0.4) + ggtitle("Density Plot of Spasms by Time of Day")
spasmsdensitycombined30 <- spasmsdensitycombined30 + scale_x_continuous(labels=c("21","21.5", "22", "22.5", "23", "23.5", "0", "0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5", "5.5", "6", "6.5", "7", "7.5", "8", "8.5"), breaks = c(21600, 23400, 25200, 27000, 28800, 30600, 32400, 34200, 36000, 37800, 39600, 41400, 43200, 45000, 46800, 48600, 50400, 52200, 54000, 55800, 57600, 59400, 61200, 63000))
spasmsdensitycombined30 <- spasmsdensitycombined30 + labs(title="Density Plot of Spasms by Time of Day", y="Density", x = "Hour of day") + theme_bw()
spasmsdensitycombined30 <- spasmsdensitycombined30 + scale_fill_discrete(name = "Cohort")
spasmsdensitycombined30

bw.nrd0(combinedspasmsdensity$hour)
chisqtable = table(combinedspasmsdensity$hour, combinedspasmsdensity$clinical)
chisqtable
chisq.test(combinedspasmsdensity$hour, combinedspasmsdensity$clinical)
ks.test(STspasmstocombine2$time, clinicalspasmstocombine2$time)

spasmshistocombined <- ggplot(data = combinedspasmsdensity, aes(x=hour, fill=clinical)) + geom_histogram(breaks=seq(5,18, by=1), alpha=0.2) + labs(title="Spasms by Hour of Day", y="Frequency", x = "Hour of day") + theme_bw()
spasmshistocombined <- spasmshistocombined + scale_x_continuous(breaks = c(5,9,13,17,21), labels=c("16", "20",  "0", "4", "8"))
spasmshistocombined <- spasmshistocombined + theme(text = element_text(size=30))
spasmshistocombined

#calculating SE for error bars
#using (p(1-p)/n)^.5 then multiply by 1.96 for CI
