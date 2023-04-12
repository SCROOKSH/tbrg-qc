#load libraries

library(readxl)
library(stringr)
library(dplyr)
library(writexl)
library(lubridate)

## 1. Import data files

# 
#  Precipitation xls file formatted as follows should be copied into data_conversion folder:
#   
# -   "Date" column in yyyy-mm-dd
# -   "Julian Day" column
# -   "Hour" column in hmm format
# -   "mm" column meaning number of mm of precip per tip/event
# -   filename should start with a two letter station name abbreviation

setwd("C:/Users/SCROOKSH/OneDrive - Government of BC/RFiles/Projects/RainQC/tbrg-qc-main")

## import precip event xls file located in data_conversion folder

file.list <- list.files(path="C:/Users/SCROOKSH/OneDrive - Government of BC/RFiles/Projects/RainQC/tbrg-qc-main/data_conversion", 
                        pattern='*.xls', full.names=T)
df.list <- lapply(file.list, read_excel,guess_max = (4000))
df.event <- df.list[[1]]
df.event <- as.data.frame(df.event)
colnames(df.event) <- c("Date", "Julian Day", "Hour", "mm")
rm(df.list)


## 2. Date time formatting for precipitation data file

df.event$Time0 <- str_pad(df.event$Hour,4,pad = "0")
df.event$Timechar <- as.character(df.event$Time0)
df.event$Datechar <- as.character(df.event$Date)
df.event$DateTimechar <- paste(df.event$Datechar,df.event$Timechar)
df.event$DateTime <- as.POSIXct(df.event$DateTimechar, format="%Y-%m-%d %H%M", tz="Pacific/Pitcairn")

df.event$Hourchar <- substr(df.event$Timechar,1,nchar(df.event$Timechar)-2)
df.event$DateHourchar <- paste(df.event$Datechar,df.event$Hourchar)
df.event$DateHour <- as.POSIXct(df.event$DateHourchar,format="%Y-%m-%d %H", tz="UTC")

df.event$Date <- as.Date(df.event$Date)

df.event$Time0 <- NULL
df.event$Timechar <- NULL
df.event$Datechar <- NULL
df.event$DateTimechar <- NULL
df.event$Hour <- NULL
df.event$Hourchar <- NULL
df.event$DateHourchar <- NULL

basename <- basename(file.list)
StationName <- substr(basename[1], 1,2)
Year <- lubridate::year(df.event[1,1])

df.event$"Julian Day" <- NULL
df.event$DateHour <- NULL
df.event$Date <- NULL



# reorder columns


df.event <- df.event %>%
  dplyr::relocate(DateTime, .before=NULL)
  


## 3. Export as dat file in CR1000/CR6 format

Sys.setenv(TZ="")    
df.event$DateTime = force_tz(df.event$DateTime,tzone="GMT")

write_xlsx(df.event %>% mutate(DateTime = as.character(DateTime))
             , paste0("./data_conversion/ExportData/", StationName, Year, ".xlsx"))