# toward an Exploratory Data Analysis

## SECTION 0: Load Tools ----
library(dplyr)
library(stringr)
library(ggplot2)

## SECTION 1: Load Data ----

### the CSV file from AirTable (sensor classification and state)
MyData <- read.csv("~/Dropbox/sensing_mission/Grow/data/airtable_combi.csv")
MyData <- tbl_df(MyData)

### load("~/Dropbox/sensing_mission/data_vdl/processedData/allData.rdata") -actual sensor readings, for later

## SECTION 2: Run Descriptive Stats ----
MyData
str(MyData)  #reveals structure of the table, datatypes, a few sample records
summary(MyData)
### NB: datatypes that were char before, are now interpreted as factor (since i added "header=TRUE"?)

## SECTION 3: Details...  ----

### change datatypes
MyData$BATT <- as.integer( MyData$BATT )
MyData$WEB_ID <- as.character(MyData$WEB_ID)

MyData[which(MyData$first_soilMes == ""), "first_soilMes"] <- "N/A"   # needed to eliminate blank fields, to change datatype
MyData$first_soilMes <- as.POSIXct(MyData$first_soilMes, origin = "1970-01-01", tz ="GMT")

MyData[which(MyData$last_soilMes == ""), "last_soilMes"] <- "N/A"    # needed to eliminate blank fields, to change datatype
MyData$last_soilMes <- as.POSIXct(MyData$last_soilMes, origin = "1970-01-01", tz = "GMT")

str(MyData)

### text manipulation
MyData$WEB_ID <- toupper(MyData$WEB_ID)   #convert lowercase web_id's to uppercase

# test: to see if this toupper case change worked...
OG <- MyData[which(str_detect(MyData$WEB_ID, "ALG_VDL_OG")),]
OG  #there were a few of each; now should be all uppercase

# Create a utility function to help with title extraction
extractZone <- function(WEB_ID) {
  WEB_ID <- as.character(WEB_ID)

  if (length(grep("ALG_VDL_EF", WEB_ID)) > 0) {
    return ("EF")
  } else if (length(grep("ALG_VDL_ER", WEB_ID)) > 0) {
    return ("ER")
  } else if (length(grep("ALG_VDL_FE", WEB_ID)) > 0) {
    return ("FE")
  } else if (length(grep("ALG_VDL_FF", WEB_ID)) > 0) {
    return ("FF")
  } else if (length(grep("ALG_VDL_FR", WEB_ID)) > 0) {
    return ("FR")
  } else if (length(grep("ALG_VDL_LG", WEB_ID)) > 0) {
    return ("LG")
  } else if (length(grep("ALG_VDL_LL", WEB_ID)) > 0) {
    return ("LL")
  } else if (length(grep("ALG_VDL_MN", WEB_ID)) > 0) {
    return ("MN")
  } else if (length(grep("ALG_VDL_NF", WEB_ID)) > 0) {
    return ("NF")
  } else if (length(grep("ALG_VDL_OG", WEB_ID)) > 0) {
    return ("OG")
  } else if (length(grep("ALG_VDL_SB", WEB_ID)) > 0) {
    return ("SB")
  } else if (length(grep("ALG_VDL_SS", WEB_ID)) > 0) {
    return ("SS")
  } else if (length(grep("ALG_VDL_TN", WEB_ID)) > 0) {
    return ("TN")
  } else if (length(grep("ALG_VDL_TS", WEB_ID)) > 0) {
    return ("TS")
  } else if (length(grep("ALG_VDL_VY", WEB_ID)) > 0) {
    return ("VY")
  } else if (length(grep("ALG_VDL_WZ", WEB_ID)) > 0) {
    return ("WZ")
  } else {
    return ("Other")
  }
}

zone <- NULL
for (i in 1:nrow(MyData)) {
  zone <- c(zone, extractZone(MyData[i,"WEB_ID"]))
}
MyData$zone <- as.factor(zone)

#NB: the few zones defines as "other" are known problems

### selecting and filtering

filter(MyData, is.na(nNAs_soilMes)) #runs... But pulls no records, when i can see there's 4
MyData[which(MyData$nNAs_soilMes < 5),]  # runs OK...
is.na(MyData$nNAs_soilMes) #4 records TRUE, the rest FALSE
na_records <- MyData[MyData$nNAs_soilMes == "NA" ,] #defines a new dataframe with NA records
na_records  #prints that new dataframe, w/ 4 records... but it shows ALL variables as NA
complete <- MyData[complete.cases(MyData), ]  #defines a new dataframe without the NA records
head(complete)  #prints head of that new dataframe

### Basic scatter plot ----

ggplot(MyData) +
   geom_point(aes(x=first_soilMes, y=BATT))

