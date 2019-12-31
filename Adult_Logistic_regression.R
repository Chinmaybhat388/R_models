# To predict if a person will earn more or less than 50k $. 

setwd("D:/DATA science")

adult <- read.csv(file = "adult.csv",
                      header = TRUE, sep = ",")
View(adult)
str(adult)

#Remove coloumns Fnlwgt,capital gain,capital loss, education.
adult <- adult[ ,-3]
adult <- adult[ ,-3]
adult <- adult[ ,c(-9,-10)]

#library(naniar)

#replace "?" with na

nrow(adult[adult == "?" , ])

#adult %>% replace_with_na_all(condition = ~.x =="?")
adult$Workclass <- replace(adult$Workclass, adult$Workclass == "?", NA)
adult$Occupation <- replace(adult$Occupation, adult$Occupation == "?", NA)
adult$Age <- replace(adult$Age, adult$Age == "?", NA)
adult$Race <- replace(adult$Race, adult$Race == "?", NA)
adult$Education.num <- replace(adult$Education.num, adult$Education.num == "?", NA)
adult$Marital.status <- replace(adult$Marital.status, adult$Marital.status == "?", NA)
adult$Relationship <- replace(adult$Relationship, adult$Relationship == "?", NA)
adult$Sex <- replace(adult$Sex, adult$Sex == "?", NA)
adult$Hours.per.week <- replace(adult$Hours.per.week, adult$Hours.per.week == "?", NA)
adult$Native.country <- replace(adult$Native.country, adult$Native.country == "?", NA)
adult$Class <- replace(adult$Class, adult$Class == "?", NA)




nrow(adult[is.na(adult), ])

adult <- na.omit(adult)
nrow(adult)

#NUmber of NA values

Navalues <- nrow(adult[!complete.cases(adult) , ])

View(adult)

rawdata <- adult

View(rawdata)
str(rawdata)



str(rawdata)

#Convert necessary variables to factors

#rawdata$Class <- as.factor(rawdata$Class)
#rawdata$Native.country <- as.factor(rawdata$Native.country)
#rawdata$Sex <- as.factor(rawdata$Sex)
#rawdata$Race <- as.factor(rawdata$Race)
#rawdata$Relationship <- as.factor(rawdata$Relationship)
#rawdata$Occupation <- as.factor(rawdata$Occupation)
#rawdata$Marital.status <- as.factor(rawdata$Marital.status)
#rawdata$Education.num <- as.factor(rawdata$Education.num)
#rawdata$Workclass <- as.factor(rawdata$Workclass)

####rawdata$Age <- as.factor(rawdata$Age)

#Check for number of observations in each level of the variables

table(rawdata$Class)
#enough obs for all levels

i <- table(rawdata$Native.country)
sort(i,decreasing = TRUE)

rawdata[,"Native.country"] <- as.factor(ifelse(rawdata[,"Native.country"] == "United-States","USA","Other-countries"))
levels(rawdata$Native.country)
table(rawdata$Native.country)

table(rawdata$Age)
#make a new level 80+

library(forcats)
View(fct_count(rawdata$Age)) 

rawdata$Age <- fct_collapse(rawdata$Age, "80+" = c("81","82","83","84","85","86","87","88","89","90"),group_other = FALSE)

levels(rawdata$Age)

#rawdata <- rawdata[,-12] (removed a coloumn created by mistake)

table(rawdata$Workclass)

rawdata$Workclass <- fct_collapse(rawdata$Workclass, "Other-Jobs" = c("Federal-gov","Local-gov","?","Self-emp-inc","Self-emp-not-inc","State-gov","Without-pay","Never-worked"),group_other = FALSE)


levels(rawdata$Workclass)

class(rawdata$Education.num)
table(rawdata$Education.num)

rawdata <- rawdata[ ,c(-4,-6)]

class(rawdata$Occupation)
table(rawdata$Occupation)
rawdata <- rawdata[ ,-4]

class(rawdata$Sex)
table(rawdata$Sex)

class(rawdata$Race)
levels(rawdata$Race)
table(rawdata$Race)
#Combining some races as there are very ew observations.
rawdata$Race <- fct_collapse(rawdata$Race, "others" = c("Amer-Indian-Eskimo","Asian-Pac-Islander","Other"), group_other = FALSE)

#Multivariate analysis
