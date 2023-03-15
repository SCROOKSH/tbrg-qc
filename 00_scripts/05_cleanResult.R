# Clean output
# "00_scripts/05_cleanResult.R"

# This script reads in the after-review excels in folder "02_test4review"
#   and generates a new table for each location-datatype. Outputs are put in the "05_cleanResult" folder
# Each output excel has 3 sheets: Day, Hour, and 5 minutes. Each sheet has 3 columns: timestamp, Rain, Grade

# Version: 0.1
# Last update: 2023-03-13





# start here -------------------------------------------------------------------
library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)


# 0. Users settings - please check every year ----------------------------------
examineYear <- "2022"
reviewfolder <- "02_test4review"

location_keyword <- c("Burn", "Cabin", "Seed")                                  # location_keyword and data_type should match.
data_type <- c("Campbell", "HoBo")        
# if Seed has 2 Campbell, modify line 26 to 2 locations, run. Then rerun this script with below setting
# location_keyword <- c("Seed")  
# data_type <- c("Campbell1", "Campbell2")  

threshold_examine <- read_excel("01_input/threshold_value.xlsx", sheet = "examine") %>% 
  column_to_rownames(var = "Data") %>%
  mutate(
    examinestart = parse_date_time(examinestart, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"),
    examineend = parse_date_time(examineend, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"))


# 1. Generate seconds and timestamp column for entire year ----------------------
yearStart <- as.POSIXct(paste0(as.numeric(examineYear) - 1, "-12-31 00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")
yearEnd <- as.POSIXct(paste0(as.numeric(examineYear) + 1, "-01-01 23:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")
year_hour <- data.frame(TimestampH = seq(yearStart, yearEnd, by = "1 hour")) %>% filter(year(TimestampH) == examineYear)
year_day <- data.frame(Date = seq(yearStart, yearEnd, by = "1 day")) %>% filter(year(Date) == examineYear)
year_minu5 <- data.frame(Minu5 = seq(yearStart, yearEnd, by = "5 mins")) %>% filter(year(Minu5) == examineYear)

# list all excel files in folder "02_test4review", exclude any opening excel start with ~ (please )
filelist <- list.files(path = reviewfolder, pattern = "^[^~].*\\.xlsx$", full.names = TRUE)


# 2. Loop locations and each data_type -----------------------------------------

for (location_keyword_i in location_keyword) { 
  for (data_type_i in data_type) { 

    pattern <- sprintf(".*%s.*%s.*\\.xlsx", location_keyword_i, data_type_i)
    file_i <- grep(pattern, filelist, value = TRUE)
    cleanName <- basename(file_i) %>% str_remove("forReview_")
    examinestart <- threshold_examine[paste0(location_keyword_i, "_", data_type_i), "examinestart"]
    examineend <- threshold_examine[paste0(location_keyword_i, "_", data_type_i), "examineend"]

    ## 2.1 read in the excel table ---------------------------------------------
    
    df <- read_excel(file_i, sheet = "data") %>%
      mutate(
        Rain = ifelse(!is.na(Rain_C), Rain_C, Rain_mm),
        Rain = as.numeric(Rain),
        Timestamp = parse_date_time(Timestamp1, "%Y-%m-%d %H:%M:%s", tz = "Pacific/Pitcairn")
      ) %>%
      select(Timestamp, Rain, flag_missing) %>%
      drop_na(Timestamp)

    # if it's HoBo data, need to add random seconds to the timestamp
    if (data_type_i == "HoBo") {
      # find replica rows
      not_re_df <- df %>%
        group_by(Timestamp) %>%
        mutate(n = n()) %>%
        filter(n == 1) %>%
        select(-n)

      replica_rows <- df %>%
        group_by(Timestamp) %>%
        mutate(n = n()) %>%
        filter(n > 1) %>%
        select(-n)

      # add random seconds to replica rows
      replica_rows_new <- replica_rows %>%
        rowwise() %>%
        mutate(
          Timestamp = ifelse(format(Timestamp, "%S") == "00",
            Timestamp + runif(1, 1, 59),
            Timestamp
          ),
          Timestamp = as.POSIXct(Timestamp, origin = "1970-01-01")
        ) %>%
        ungroup()

      # merge replica rows with original df
      df_new <- rbind(replica_rows_new, not_re_df) %>%
        arrange(Timestamp)

      # check if all rows are still there. TRUE means to go
      if (nrow(df) == nrow(df_new)) {
        message("All rows in HoBo data are successfully recorded.
                Random seconds values are added to the timestamps.")
      }
      df <- df_new
    } # end of if HoBo


    ## 2.2 Fill the data to all seconds ----------------------------------------

    S_seq <- data.frame(Timestamp = seq(examinestart, examineend, by = "sec"))
    df_seq <- S_seq %>%
      left_join(df, by = "Timestamp") %>%
      arrange(Timestamp)

    # add missing flags from start to end of all missing periods
    missing_start <- which(df_seq$`flag_missing` == "m_start")
    missing_end <- which(df_seq$`flag_missing` == "m_end")
    df_seq$`flag_missing`[unlist(mapply(function(x, y) x:y, missing_start + 1, missing_end - 1))] <- "m"
    df_seq$`flag_missing`[missing_start] <- "m_start"
    df_seq$`flag_missing`[missing_end] <- "m_end"

    # time gap new rows: value to 0. when missing flag is not na, rain =NA
    df_seq <- df_seq %>%
      mutate(Rain = ifelse(is.na(Rain), 0, Rain)) %>%
      mutate(Rain = ifelse(is.na(flag_missing), Rain, NA))

    ## 2.3 make data table for each timestep -----------------------------------
    
    df_hour <- df_seq %>%
      group_by(TimestampH = floor_date(Timestamp, "hour")) %>%
      summarise(Rain_hour = sum(Rain)) %>%
      mutate(Grade = case_when(
        is.na(Rain_hour) ~ "Missing",
        !is.na(Rain_hour) ~ "Record"
      )) %>%
      right_join(year_hour, by = "TimestampH") %>%
      mutate(Grade = ifelse(is.na(Grade), "Winter", Grade)) %>%
      arrange(TimestampH)

    df_day <- df_seq %>%
      group_by(Date = as.Date(Timestamp)) %>%
      summarise(Rain_day = sum(Rain)) %>%
      mutate(Grade = case_when(
        is.na(Rain_day) ~ "Missing",
        !is.na(Rain_day) ~ "Record"
      )) %>%
      right_join(year_day, by = "Date") %>%
      mutate(Grade = ifelse(is.na(Grade), "Winter", Grade)) %>%
      arrange(Date)

    df_minu5 <- df_seq %>%
      group_by(Minu5 = floor_date(Timestamp, "5 minutes")) %>%
      summarise(Rain_minu5 = sum(Rain)) %>%
      mutate(Grade = case_when(
        is.na(Rain_minu5) ~ "Missing",
        !is.na(Rain_minu5) ~ "Record"
      )) %>%
      right_join(year_minu5, by = "Minu5") %>%
      mutate(Grade = ifelse(is.na(Grade), "Winter", Grade)) %>%
      arrange(Minu5)

    ## 2.4 print result --------------------------------------------------------
    
    write_xlsx(
      list(
        Hour = df_hour %>% mutate(TimestampH = as.character(TimestampH)),
        Day = df_day %>% mutate(Date = as.character(Date)),
        Minute5 = df_minu5 %>% mutate(Minu5 = as.character(Minu5))
      ),
      col_names = TRUE,
      paste0("05_cleanResult/clean_", cleanName)
    )
  } # end of data_type
} # end of locations

