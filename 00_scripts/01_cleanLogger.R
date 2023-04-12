# Clean logger data
# "00_scripts/01_cleanLogger.R"

# This script will:
#   - read in the logger data files that are in the 01_input folder. Each subfolder in "01_input" will be cleaned as one output.
#   - run tests on the cleaned event data
#   - export the tipping bucket event data with QA test results to the 02_test4review folder


# Version: 0.2
# Last update: 2023-03-24

# SC update: 2023-04-03
#     - Section 2.4
#         changed group_by date formatting to account for tz as.Date(format(as.POSIXct(Timestamp1), "%Y-%m-%d"))
#     - Section 2.2
#         changed mutate date formatting before left join(Air Temperature) to account for tz as.Date(format(as.POSIXct(Timestamp1), "%Y-%m-%d"))
#     - Section 2.1
#          changed mutate(m_end = m_end + days(1) - seconds(1)) to  mutate(m_end = m_end - seconds(1))


# start here -------------------------------------------------------------------
setwd("C:/Users/SCROOKSH/OneDrive - Government of BC/RFiles/Projects/RainQC/tbrg-qc-main")

library(readr)
library(readxl)
library(lubridate)
library(tidyverse)
library(stringr)
library(writexl)
library(zoo)
source("00_scripts/functions.R")                                                # important, must run

# 0. Users settings - please check threshold.xlsx every year -------------------

threshold_value <- read_excel("01_input/threshold.xlsx", sheet = "extreme") %>%   # each row in col "Test" must have unique values
  column_to_rownames(var = "Test")

examineYear <- threshold_value["examineYear", "Value"]                          # examine year read from threshold table now

threshold_examine <- read_excel("01_input/threshold.xlsx", sheet = "examine") %>% # col Data is unique rownames
  column_to_rownames(var = "Data") %>%
  mutate(
    examinestart = parse_date_time(examinestart, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"),
    examineend = parse_date_time(examineend, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")
  )
threshold_missing <- read_excel("01_input/threshold.xlsx", sheet = "missing") %>%
  mutate(
    m_start = parse_date_time(m_start, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"),
    m_end = parse_date_time(m_end, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")
  )

location_dir <- "01_input" %>%
  list.dirs(recursive = FALSE)


# 1. loop location -------------------------------------------------------------

for (location_dir_i in location_dir) {                                          # e.g.   location_dir_i  <- "01_input/Burn"
  location_i <- basename(location_dir_i)                                        # e.g.   location_i      <- "Burn"
  data_type_dir <- location_dir_i %>% list.dirs(recursive = FALSE)              # e.g.   data_type_dir   <- "01_input/Burn/Campbell" all suborders


  ## 1.1 hourly air temperature process ----------------------------------------

  filelist <- list.files(path = location_dir_i, full.names = TRUE, recursive = FALSE) %>%
    keep(~ grepl(".xlsx$", .))
  AirTemperature <- map_dfr(.x = filelist, .f = read_excel) %>%
    mutate(Timestamp1 = parse_date_time(DateTime, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")) %>%
    arrange(Timestamp1) %>%
    mutate(TimestampH = round(Timestamp1, units = "hours")) %>% # round not floor to hour
    select(TimestampH, Tair_Avg_C) %>%
    distinct(TimestampH, .keep_all = TRUE)
  AirTemperature_sm4 <- AirTemperature %>%
    mutate(
      Hour = hour(TimestampH),
      date = as.Date(TimestampH)
    ) %>%
    group_by(date) %>%
    mutate(
      amT = any(Hour >= 0 & Hour <= 15 & Tair_Avg_C <= 0), # 0 -15 am
      pmT = any(Hour >= 12 & Hour <= 15 & Tair_Avg_C > 0), # 12-15 pm
      flag_SM4 = ifelse(amT & pmT, "SM4", NA)
    ) %>%
    ungroup() %>%
    select(date, flag_SM4) %>%
    filter(!is.na(flag_SM4)) %>%
    distinct(date, .keep_all = TRUE)


  ## 1.2 loop data type folders ------------------------------------------------

  for (data_type_dir_i in data_type_dir) {                                      # data_type_dir_i  <-  "01_input/Burn/Campbell"

    data_type_nicename <- basename(data_type_dir_i)                             # data_type_nicename <- "Campbell"
    filelist <- list.files(path = data_type_dir_i, full.names = TRUE)           # all file from one bucket

    ### 1.2.1  Campbell data process ------------------------------------------

    if (str_detect(data_type_nicename, regex("Campbell", ignore_case = TRUE))) {
      data_mytest <- map_dfr(.x = filelist, .f = readin_campbell) %>%
        rename_with(~"Timestamp", 1) %>%
        rename(Rain_mm = Tot) %>%
        select(Timestamp, Rain_mm) %>%
        mutate(Timestamp1 = parse_date_time(Timestamp, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")) %>%
        arrange(Timestamp1) %>%
        distinct(Timestamp1, .keep_all = TRUE) %>%                              # Campbell. delete duplicated rows by Timestamp1
        filter(year(Timestamp1) == examineYear) %>%
        select(Timestamp1, Rain_mm) %>%
        mutate(flag_tip = ifelse(Rain_mm >= 0.6, "Y", NA))                      # flag_tip: value can not >= 0.6mm for Campbell data

      ins_gap <- 0.2 * 3600 / threshold_value["ins", "Value"]
    } else if (str_detect(data_type_nicename, regex("HoBo", ignore_case = TRUE))) {
      
    ### 1.2.2 HoBo data process -------------------------------------------------

      data_mytest <- map_dfr(.x = filelist, .f = readin_hobo) %>%
        mutate(Timestamp1 = parse_date_time(Timestamp, "%m/%d/%Y %H:%M", tz = "Pacific/Pitcairn")) %>% # hobo doesn't have seconds in timestamp
        arrange(Timestamp1) %>%
        filter(year(Timestamp1) == examineYear) %>%
        fill(Temperature_C) %>%
        rename(Temperature_hobo = Temperature_C) %>%
        mutate(`Rain_mm` = ifelse(is.na(`Event`), NA, 0.254)) %>%               # add value 0.254 to each event row
        drop_na(Rain_mm) %>%
        select(Timestamp1, Rain_mm, Temperature_hobo) %>%
        mutate(flag_tip = ifelse(Rain_mm == 0.254, NA, "Y"))

      ins_gap <- 0.254 * 3600 / threshold_value["ins", "Value"]
   
       } else {
      warning("Datafolder name must have the keyword 'Campbell' or 'HoBo'.")
    }


    # 2. run test --------------------------------------------------------------

    ## 2.1 delete winter readings ----------------------------------------------

    examinestart <- threshold_examine[paste0(location_i, "_", data_type_nicename), "examinestart"]
    examineend <- threshold_examine[paste0(location_i, "_", data_type_nicename), "examineend"]

    data_mytest <- data_mytest %>%
      filter(between(Timestamp1, examinestart, examineend))

    # read missing table for this location 
    missingTable <- threshold_missing %>%
      filter(Data == paste0(location_i, "_", data_type_nicename)) %>%
      select(`m_start`, `m_end`) %>%
      mutate(m_end = m_end - seconds(1)) %>%
      pivot_longer(
        cols = c("m_start", "m_end"),
        names_to = "flag_missing",
        values_to = "Timestamp1"
      ) %>%
      select(Timestamp1, flag_missing) %>%
      mutate(Timestamp1 = parse_date_time(Timestamp1, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"))
    
    ## 2.2 other tests ---------------------------------------------------------

    data_mytest1 <- data_mytest %>%
      mutate(
        TimestampH = floor_date(Timestamp1, "hour"),
        Hour = hour(Timestamp1)
      ) %>%
      left_join(AirTemperature, by = c("TimestampH" = "TimestampH")) %>%
      mutate(
        gap = as.numeric(Timestamp1 - lag(Timestamp1), units = "secs"),
        event2h = cumsum(c(1, diff(Timestamp1) > 2 * 60 * 60))
      ) %>%                                                                     # time gaps more than 2 hours will be a new event number, start with '1'
      group_by(event2h) %>%
      mutate(
        flag_instantaneous = ifelse(gap < ins_gap, "instantaneous", NA),        # used to be "QC1a"
        gap_increase = ifelse(gap > lag(gap), 1, 0),
        gap_ins_ct = rollapply(gap_increase, 12, sum, fill = 0, align = "right"),
        flag_prolong = ifelse(gap_ins_ct >= 12, "prolong", NA)
      ) %>% # 12 continuous increasing gaps
      ungroup() %>%
      group_by(floor_hour = floor_date(Timestamp1, "hour")) %>%
      mutate(
        i_hrmax = sum(Rain_mm),
        flag_hrmax = ifelse(i_hrmax > threshold_value["hrmax", "Value"], "hrmax", NA)
      ) %>%
      ungroup() %>%
      group_by(floor_day = floor_date(Timestamp1, "day")) %>%
      mutate(
        i_daymax = sum(Rain_mm),
        flag_daymax = ifelse(i_daymax > threshold_value["daymax", "Value"], "daymax", NA)
      ) %>%
      ungroup() %>%
      group_by(floor_day) %>%
      mutate(flag_below0 = ifelse(Tair_Avg_C <= 0, "below0", NA)) %>%
      ungroup() %>%
      mutate(Rain_C = "") %>%
      select(-c(Hour, floor_hour, floor_day)) %>%
      mutate(date = as.Date(format(as.POSIXct(Timestamp1), "%Y-%m-%d"))) %>%
      left_join(AirTemperature_sm4, by = "date") %>%
      select(-`date`) %>%
      full_join(missingTable, by = "Timestamp1") %>%
      arrange(Timestamp1)
    

    ## 2.3 add all missing flags -----------------------------------------------
    missing_start <- which(data_mytest1$`flag_missing` == "m_start")
    missing_end <- which(data_mytest1$`flag_missing` == "m_end")
    data_mytest1$`flag_missing`[unlist(mapply(function(x, y) x:y, missing_start + 1, missing_end - 1))] <- "m"
    data_mytest1$`flag_missing`[missing_start] <- "m_start"
    data_mytest1$`flag_missing`[missing_end] <- "m_end"

    data_mytest1 <- data_mytest1 %>%
      mutate(Rain_C = ifelse(!is.na(flag_missing), -99, "")) %>%                # new end missing flag
      unite(flags, starts_with("flag"), na.rm = T, sep = ",", remove = F) %>%
      relocate(Rain_C, .after = Rain_mm) %>%
      relocate(event2h, .after = Tair_Avg_C) %>%
      relocate(flag_tip, .before = flag_instantaneous) %>%
      relocate(flags, .after = Rain_C) %>%
      relocate(`flag_missing`, .after = Rain_C)


    ## 2.4 summarise table by day ----------------------------------------------
    data_daytable <- data_mytest1 %>%
      group_by(date = as.Date(format(as.POSIXct(Timestamp1), "%Y-%m-%d"))) %>%
      summarize(
        Rain_day = sum(Rain_mm),
        flag_missing = ifelse(any(is.na(flag_missing)), NA, "Missing"),
        flag_tip = ifelse(any(flag_tip == "Y"), "Y", NA),
        flag_instantaneous = ifelse(any(flag_instantaneous == "instantaneous"), "instantaneous", NA),
        flag_prolong = ifelse(any(flag_prolong == "prolong"), "prolong", NA),
        flag_hrmax = ifelse(any(flag_hrmax == "hrmax"), "hrmax", NA),
        flag_daymax = ifelse(any(flag_daymax == "daymax"), "daymax", NA),
        flag_SM4 = ifelse(any(flag_SM4 == "SM4"), "SM4", NA),
        flag_below0 = ifelse(any(flag_below0 == "below0"), "below0", NA)
      ) %>%
      arrange(date) %>%
      mutate(days_dry = as.numeric(date - lag(date, default = first(date))) + 1) %>%
      unite(flags, starts_with("flag"), na.rm = T, sep = ",", remove = F) %>%
      relocate(flags, .after = Rain_day)


    ## 2.5 print excel for data review -----------------------------------------
    write_xlsx(
      list(
        data = data_mytest1 %>%
          mutate(Timestamp1 = as.character(Timestamp1), TimestampH = as.character(TimestampH)),
        daytable = data_daytable
      ),
      col_names = TRUE,
      paste0("02_test4review/forReview_", location_i, examineYear, data_type_nicename, ".xlsx")
    )
  } # end of loop data_type Campbell or HoBo
} # end of loop locations

