#Rash driving analysis.

library(dplyr)
library(randomForest)
library(ggplot2)

setwd('D:/DATA science/Assignments/DrivingStyleData')
getwd()

#Import first file
Train <- read.csv('Train.csv',header = TRUE)
View(Train)
names(Train)
names(Train) <- c("ID","VehicleLength","VehicleWeight","VehicleAxles","DrivingStyle")
nrow(Train)

#Import second file
Train_Vehicletravellingdata <- read.csv('Train_Vehicletravellingdata.csv',header = TRUE)
names(Train_Vehicletravellingdata)

nrow(Train_Vehicletravellingdata)
#This df has way more rows compared to Train df. We will have to 
#select only those rows with same IDs

View(Train_Vehicletravellingdata)
names(Train_Vehicletravellingdata) <- c("ID","Date","VehicleLane","VehicleSpeed","prID","prSpeed","prWeight","prLength","TimeGap","RoadCondition")


#Import the 3rd file

Train_WeatherData <- read.csv('Train_WeatherData.csv',header = TRUE)
nrow(Train_WeatherData) #more rows than the first file
View(Train_WeatherData)
names(Train_WeatherData)
names(Train_WeatherData) <- c("ID","Date","AirTemp","Precipitation","Precip_intensity","Humidity","WindDirec","WindSpeed","TimeofDay")



#Merge the 3 files into 1 dataframe

unique(Train$ID) #All vehicles in the data are different.(12994 diff vehicles)

unique(Train_Vehicletravellingdata$ID) #(12994 diff vehicles)
unique(Train_WeatherData$ID) #(12994 diff vehicles)

Check <- merge(Train,Train_Vehicletravellingdata, by.x = "ID")
nrow(Check)
View(Check)

checkR <- merge(Train,Train_WeatherData, by.x = "ID")
View(checkR)
names(checkR)
names(checkR) <- c("id","VehicleLength","VehicleWeight","VehicleAxles","DrivingStyle","DATE","AirTemp","Precipitation","Precip_intensity","Humidity","WindDirec","WindSpeed","TimeofDay")

#Merging all 3 files into 1 dataframe

drivedf <- cbind(Check,checkR)
View(drivedf)
dim(drivedf)
#drivedf <- drivedf[ ,C(-15,-20)]  --- Not working

#Removing duplicate coloumns
drivedf <- subset(drivedf,select = -c(id,DATE,))

#Reordering coloumns and removing more duplicate coloumns

names(drivedf)
drivedf <- drivedf[ ,c("ID","Date","VehicleLength","VehicleWeight","VehicleAxles","VehicleLane","VehicleSpeed","prID","prSpeed","prWeight","prLength","TimeGap","RoadCondition","AirTemp","Precipitation","Precip_intensity","Humidity","WindDirec","WindSpeed","TimeofDay","DrivingStyle")]

#Now we are left with 20 predictor variables.

# Analyze the data

summary(drivedf)

incomplete_rows <- nrow(drivedf[!complete.cases(drivedf), ])
count <- 0

#For loop to check na values in all coloumns of the drivedf dataframe.
for (i in names(drivedf))
{
  print(i)
  print(sum(is.na(drivedf[ ,i])))
  count <- count + 1
}
print(count) # To check if all coloumns have been accounted for.

sum(is.na(drivedf$WindSpeed))

#Check for anomalies like outliers etc.
summary(drivedf[ ,c(-1,-8)]) #excluding ID,prID

#OUTLIERS that need treatment

# Not capping VehicleSpeed prSpeed as extreme speeds increase chances of accidents

#1.VehicleLength
upper_side_outliers <- quantile(drivedf$VehicleLength,0.75) + 1.5*IQR(drivedf$VehicleLength)
drivedf[drivedf$VehicleLength > round(upper_side_outliers),"VehicleLength"] <- round(upper_side_outliers)

#2.VehicleWeight
upper_side_outliers <- quantile(drivedf$VehicleWeight,0.75) + 1.5*IQR(drivedf$VehicleWeight)
drivedf[drivedf$VehicleWeight > round(upper_side_outliers),"VehicleWeight"] <- round(upper_side_outliers)

#3.prWeight
upper_side_outliers <- quantile(drivedf$prWeight,0.75) + 1.5*IQR(drivedf$prWeight)
drivedf[drivedf$prWeight > round(upper_side_outliers),"prWeight"] <- round(upper_side_outliers)

#4.prLength
upper_side_outliers <- quantile(drivedf$prLength,0.75) + 1.5*IQR(drivedf$prLength)
drivedf[drivedf$prLength > round(upper_side_outliers),"prLength"] <- round(upper_side_outliers)


scatter.smooth(drivedf$prLength)

#5.VehicleSpeed
upper_side_outliers <- quantile(drivedf$VehicleSpeed,0.75) + 1.5*IQR(drivedf$VehicleSpeed)
drivedf[drivedf$VehicleSpeed > round(upper_side_outliers),"VehicleSpeed"] <- round(upper_side_outliers)

scatter.smooth(drivedf$VehicleSpeed)

#CHECK FOR FEATURE TYPE

str(drivedf)

drivedf$prID <- as.factor(drivedf$prID)
drivedf$DrivingStyle <- as.factor(drivedf$DrivingStyle)

#check for NA values

incompleteRows <- nrow(drivedf[!complete.cases(drivedf) , ])


#TIMEGAP has na. Fill it with mean values

#drivedf[drivedf$TimeGap == NA,"TimeGap"] <- mean(drivedf$TimeGap,na.rm = TRUE)# This wont work

#drivedf$TimeGap <- ifelse(is.na(drivedf$TimeGap),mean(drivedf$TimeGap,na.rm = TRUE),drivedf$Timegap)# This wont work

str(drivedf)

#Coloumns with na values = AirTemp,TimeGap,Humidity,WindDirec,WindSpeed
With_na <- select(drivedf,TimeGap,AirTemp,Humidity,WindDirec,WindSpeed)
View(With_na)

Avg_withna <- apply(With_na,2,mean,na.rm = TRUE)
Avg_withna <- round(Avg_withna)
Avg_withna[2]

Drivedf_clean <- drivedf %>% mutate(Replace_timegap = ifelse(is.na(TimeGap),Avg_withna[1],TimeGap),
                                    Replace_airtemp = ifelse(is.na(AirTemp),Avg_withna[2],AirTemp),
                                    Replace_humidity = ifelse(is.na(Humidity),Avg_withna[3],Humidity),
                                    Replace_winddirec = ifelse(is.na(WindDirec),Avg_withna[4],WindDirec),
                                    Replace_windspeed = ifelse(is.na(WindSpeed),Avg_withna[5],WindSpeed))

View(Drivedf_clean)
#Retaining only the required rows out of all the rows
Drivedf_clean <- select(Drivedf_clean,Replace_timegap:Replace_windspeed)

names(Drivedf_clean) <- c("TimeGap","AirTemp","Humidity","WindDirec","WindSpeed")


drivedf <- drivedf[ ,c(-12,-14,-17,-18,-19)]

drivedf <- cbind(drivedf,Drivedf_clean)
dim(drivedf)
#All NA values have been replaced with Mean.

#Reordering coloumns
drivedf <- drivedf[ ,c("ID","Date","VehicleLength","VehicleWeight","VehicleAxles","VehicleLane","VehicleSpeed","prID","prSpeed","prWeight","prLength","TimeGap","RoadCondition","AirTemp","Precipitation","Precip_intensity","Humidity","WindDirec","WindSpeed","TimeofDay","DrivingStyle")]


#Precip_intensity has both none and blank values. combine them.

levels(drivedf$Precip_intensity)
table(drivedf$Precip_intensity)
filter(drivedf,Precip_intensity == " ")

drivedf_copy <- drivedf #Copy of data for backup.
View(drivedf_copy)


#Convert blank values of Precipitation intensity to NONE.
drivedf[drivedf$Precip_intensity == " ", ]$Precip_intensity <- "None"

#Helps to drop the " " factor level which had 0 entries.
drivedf$Precip_intensity <- factor(drivedf$Precip_intensity)


str(drivedf)
drivedf$VehicleLane <- as.factor(drivedf$VehicleLane)

write.csv(drivedf,"driving_cleaned.csv") #Saving the cleaned data into an excel


#NA values and outliers have been removed.

#CONVERT DATA types as needed
drivedf$VehicleLane <- as.factor(drivedf$VehicleLane)
drivedf$prID <- as.factor(drivedf$prID)


#Copy of the original data
copy <- drivedf



# 12994 unique drivers
levels(drivedf$ID)



#Statistical test for significance of variables

#Statistical analysis of some coloumns

#ANOVA TEST - VehicleLength,VehicleWeight,VehicleAxles,VehicleSpeed,prSpeed,prWeight,prLength,TimeGap,AirTemp,Humidity,WindDirec,WindSpeed,

#1. VehicleLegth - Significant
anova <- aov(drivedf$VehicleLength ~ drivedf$DrivingStyle)
summary(anova)
TukeyHSD(anova)

#2.VehicleWeight - Significant
anova <- aov(drivedf$VehicleWeight ~ drivedf$DrivingStyle)
summary(anova)

#3.VehicleAxles - Significant
anova <- aov(drivedf$VehicleAxles ~ drivedf$DrivingStyle)
summary(anova)

#4.VehicleSpeed - Significant
anova <- aov(drivedf$VehicleSpeed ~ drivedf$DrivingStyle)
summary(anova)

#5.prSpeed - Significant
anova <- aov(drivedf$prSpeed ~ drivedf$DrivingStyle)
summary(anova)


#6.prWeight - Significant
anova <- aov(drivedf$prWeight ~ drivedf$DrivingStyle)
summary(anova)

#7.prLength - Significant
anova <- aov(drivedf$prLength ~ drivedf$DrivingStyle)
summary(anova)

#8.TimeGap - Significant
anova <- aov(drivedf$TimeGap ~ drivedf$DrivingStyle)
summary(anova)

#9.AirTemp - Significant
anova <- aov(drivedf$AirTemp ~ drivedf$DrivingStyle)
summary(anova)

#10.Humidity - Significant
anova <- aov(drivedf$Humidity ~ drivedf$DrivingStyle)
summary(anova)

#11.WindDirec - Indignificant
anova <- aov(drivedf$WindDirec ~ drivedf$DrivingStyle)
summary(anova)

#12.WindSpeed - Significant
anova <- aov(drivedf$WindSpeed ~ drivedf$DrivingStyle)
summary(anova)


#Wind direction is useless. Can be excluded from the final model

#CHI SQ TEST - VehicleLane,RoadCondition,Precipitation,Precip_intensity,TimeofDay

#1.VehicleLane - Signif
mytable <- table(drivedf$VehicleLane,drivedf$DrivingStyle)
chisq.test(mytable)

#2.RoadCondition - Signif
mytable <- table(drivedf$RoadCondition,drivedf$DrivingStyle)
chisq.test(mytable)

#3.Precipitation - Signif
mytable <- table(drivedf$Precipitation,drivedf$DrivingStyle)
chisq.test(mytable)

#4.Precip_intensity - Signif
mytable <- table(drivedf$Precip_intensity,drivedf$DrivingStyle)
chisq.test(mytable)

#5.TimeofDay - Signif
mytable <- table(drivedf$TimeofDay,drivedf$DrivingStyle)
chisq.test(mytable)

#Removing the insignificant variable "Wind direction" and date,ID,Previous vehicle ID


#I want to group by ID and calculate the mode of lane for each Vehicle ID.
?as.data.frame.table

lane_mode <- tapply(drivedf$VehicleLane,drivedf$ID,getmode)

lane_mode <- as.data.frame.table(lane_mode)

class(lane_mode)
View(lane_mode)
names(lane_mode) <- c("ID","Mostused_lane")

#TRY TO CAPTURE THE NUMBER OF TIMES A DRIVER CHANGES LANES

#Trying to manually inspect the difference between an aggresive and a good driver
#A <- filter(drivedf,ID=='DR_10013')
#B <- filter(drivedf,ID== 'DR_10024')
#dim(A)
#View(A)
#dim(B)
#View(B)

#C <- rbind(A,B)
#dim(C)
#View(C)
#Preceeding vehicle stats and time of day maybe a factor to decide.

names(drivedf)
##Taking avg or mode of variables.

#MODE - VehicleAxles,RoadCondition,Humidity,VehicleLength,VehicleWeight,Precipitation,Precip_intensity,TimeofDay,DrivingStyle  

Axes_mode <- tapply(drivedf$VehicleAxles,drivedf$ID,getmode)
Axes_mode <- as.data.frame.table(Axes_mode)
View(Axes_mode)
names(Axes_mode) <- c("ID","VehicleAxles")

RoadCondition_mode <- tapply(drivedf$RoadCondition,drivedf$ID,getmode)
RoadCondition_mode <- as.data.frame.table(RoadCondition_mode)
names(RoadCondition_mode) <- c("ID","RoadCondition")
View(RoadCondition_mode)

Humidity_mode <- tapply(drivedf$Humidity,drivedf$ID,getmode)
Humidity_mode <- as.data.frame.table(Humidity_mode)
names(Humidity_mode) <- c("ID","Humidity")
View(Humidity_mode)

VehicleLength_mode <- tapply(drivedf$VehicleLength,drivedf$ID,getmode)
VehicleLength_mode <- as.data.frame.table(VehicleLength_mode)
names(VehicleLength_mode) <- c("ID","VehicleLength")
View(VehicleLength_mode)

VehicleWeight_mode <- tapply(drivedf$VehicleWeight,drivedf$ID,getmode)
VehicleWeight_mode <- as.data.frame.table(VehicleWeight_mode)
names(VehicleWeight_mode) <- c("ID","VehicleWeight")

Precipitation_mode <- tapply(drivedf$Precipitation,drivedf$ID,getmode)
Precipitation_mode <- as.data.frame.table(Precipitation_mode)
names(Precipitation_mode) <- c("ID","Precipitation")

Precip_intensity_mode <- tapply(drivedf$Precip_intensity,drivedf$ID,getmode)
Precip_intensity_mode <- as.data.frame.table(Precip_intensity_mode)
names(Precip_intensity_mode) <- c("ID","Precip_intensity")

TimeofDay_mode <- tapply(drivedf$TimeofDay,drivedf$ID,getmode)
TimeofDay_mode <- as.data.frame.table(TimeofDay_mode)
names(TimeofDay_mode) <- c("ID","TimeofDay")

DrivingStyle_mode <- tapply(drivedf$DrivingStyle,drivedf$ID,getmode)
DrivingStyle_mode <- as.data.frame.table(DrivingStyle_mode)
names(DrivingStyle_mode) <- c("ID","DrivingStyle")



#Avg - AirTemp,VehicleSpeed,WindSpeed,TimeGap,prSpeed,prWeight,prLength

AirTemp_avg <- tapply(drivedf$AirTemp,drivedf$ID,mean)
AirTemp_avg <- as.data.frame.table(AirTemp_avg)
names(AirTemp_avg) <- c("ID","Avg_AirTemp")
View(AirTemp_avg)

VehicleSpeed_avg <- tapply(drivedf$VehicleSpeed,drivedf$ID,mean)
VehicleSpeed_avg <- as.data.frame.table(VehicleSpeed_avg)
names(VehicleSpeed_avg) <- c("ID","Avg_VehicleSpeed")

WindSpeed_avg <- tapply(drivedf$WindSpeed,drivedf$ID,mean)
WindSpeed_avg <- as.data.frame.table(WindSpeed_avg)
names(WindSpeed_avg) <- c("ID","Avg_WindSpeed")

TimeGap_avg <- tapply(drivedf$TimeGap,drivedf$ID,mean)
TimeGap_avg <- as.data.frame.table(TimeGap_avg)
names(TimeGap_avg) <- c("ID","Avg_TimeGap")

prSpeed_avg <- tapply(drivedf$prSpeed,drivedf$ID,mean)
prSpeed_avg <- as.data.frame.table(prSpeed_avg)
names(prSpeed_avg) <- c("ID","Avg_prSpeed")

prWeight_avg <- tapply(drivedf$prWeight,drivedf$ID,mean)
prWeight_avg <- as.data.frame.table(prWeight_avg)
names(prWeight_avg) <- c("ID","Avg_prWeight")

prLength_avg <- tapply(drivedf$prLength,drivedf$ID,mean)
prLength_avg <- as.data.frame.table(prLength_avg)
names(prLength_avg) <- c("ID","Avg_prLength")

#Merge these and create a new Dataframe to run Randoom forest

drivedf_fin <- cbind(lane_mode,Axes_mode,prLength_avg,prWeight_avg,prSpeed_avg,TimeGap_avg,WindSpeed_avg,VehicleSpeed_avg,AirTemp_avg,DrivingStyle_mode,TimeofDay_mode,Precip_intensity_mode,Precipitation_mode,VehicleWeight_mode,VehicleLength_mode,Humidity_mode,RoadCondition_mode)

drivedf_fin <- drivedf_fin[ ,c(-3,-5,-7,-9,-11,-13,-15,-17,-19,-21,-23,-25,-27,-29,-31,-33)]
View(drivedf_fin)
names(drivedf_fin)
drivedf_fin <- drivedf_fin[ ,c("ID","VehicleLength","VehicleWeight","VehicleAxles","Mostused_lane","Avg_VehicleSpeed","Avg_prSpeed","Avg_prWeight","Avg_prLength","Avg_TimeGap","RoadCondition","Avg_AirTemp","Precipitation","Precip_intensity","Humidity","Avg_WindSpeed","TimeofDay","DrivingStyle")]

drivedf_model <- drivedf_fin[ ,-1]
View(drivedf_model)
View(drivedf_model[ ,-17])


#Variable type got changed. Changing them back to required types.
str(drivedf_model)
drivedf_model$DrivingStyle <- as.factor(drivedf_model$DrivingStyle)
drivedf_model$VehicleAxles <- as.factor(drivedf_model$VehicleAxles)
drivedf_model$Mostused_lane <- as.factor(drivedf_model$Mostused_lane)
drivedf_model$RoadCondition <- as.factor(drivedf_model$RoadCondition)
drivedf_model$Precipitation <- as.factor(drivedf_model$Precipitation)
drivedf_model$Precip_intensity <- as.factor(drivedf_model$Precip_intensity)
drivedf_model$TimeofDay <- as.factor(drivedf_model$TimeofDay)

write.csv(drivedf_model,"Drivedf_model_RF.csv")

#RandomForest model

model1 <- randomForest(x = drivedf_model[ ,-17],y = drivedf_model[ ,17],data = drivedf_model)
summary(model1)
?rep

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model1$err.rate), times=4),
  Type=rep(c("OOB", "1", "2","3"), each=nrow(model1$err.rate)),
  Error=c(model1$err.rate[,"OOB"],
          model1$err.rate[,"1"],
          model1$err.rate[,"2"],
          model1$err.rate[,"3"]))
View(oob.error.data)

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(x = drivedf_model[ ,-17],y = drivedf_model[ ,17],data = drivedf_model, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
