# Clean logger data
# "00_scripts/01_cleanLogger.R"

# This script will:
#   - read in the logger data files that are in the 01_input folder. Each subfolder in "01_input" will be cleaned as one output.
#   - run tests on the cleaned event data
#   - export the tipping bucket event data with QA test results to the 02_test4review folder

# Version: 0.1
# Last update: 2023-03-13




# start here -------------------------------------------------------------------
library(readr)
library(readxl)
library(lubridate)
library(tidyverse)
library(stringr)
library(writexl)
library(zoo)
library(anytime)
source("00_scripts/functions.R") # important, must run

# 0. Users settings - please check every year ----------------------------------
examineYear <- "2022"

datafolder <- c(
  "01_input/Raw Campbell Scientific Files/", # one folder must have keyword "Campbell" (or same as data_type[1])
  "01_input/Raw HoBo Files/", # one folder must have keyword "HoBo" (or same as data_type[2])
  "01_input/Hourly Air Temperature/"
) # last one folder must have keyword "Temperature"

threshold_value <- read_excel("01_input/threshold_value.xlsx", sheet = "extreme") %>% # each row in col "Test" must have unique values
  column_to_rownames(var = "Test")
threshold_examine <- read_excel("01_input/threshold_value.xlsx", sheet = "examine") %>% # col Data is unique rownames
  column_to_rownames(var = "Data") %>%
  mutate(
    examinestart = parse_date_time(examinestart, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"),
    examineend = parse_date_time(examineend, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")
  )
threshold_missing <- read_excel("01_input/threshold_value.xlsx", sheet = "missing") %>%
  mutate(
    m_start = parse_date_time(m_start, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"),
    m_end = parse_date_time(m_end, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")
  )

location_keyword <- c("Burn", "Cabin", "Seed") # in each folder, data file name must have location keyword "Burn" or "Cabin" or "Seed"
data_type <- c("Campbell", "HoBo") # subfolders under 01_input should have these data_type keywords. All data in each subfolder will be cleaned to one excel



# 1. loop location -------------------------------------------------------------
for (location_keyword_i in location_keyword) {
  for (data_type_i in data_type) {
    data_folder_i <- datafolder[grep(data_type_i, datafolder)]

    filelist <- data.frame(File = list.files(path = data_folder_i, full.names = TRUE)) %>%
      filter(str_detect(File, regex(location_keyword_i, ignore_case = TRUE)))

    ## 1.2  Campbell data process ----------------------------------------------

    if (str_detect(data_folder_i, regex("Campbell", ignore_case = TRUE))) {
      data_mytest <- map_dfr(.x = filelist$File, .f = readin_campbell) %>%
        rename_with(~"Timestamp", 1) %>%
        rename(Rain_mm = Tot) %>%
        select(Timestamp, Rain_mm) %>%
        mutate(Timestamp1 = anytime(Timestamp)) %>%
        arrange(Timestamp1) %>%
        distinct(Timestamp1, .keep_all = TRUE) %>% # Campbell. delete duplicated rows by Timestamp1
        filter(year(Timestamp1) == examineYear) %>%
        select(Timestamp1, Rain_mm) %>%
        mutate(flag_tip = ifelse(Rain_mm >= 0.6, "Y", NA)) # flag_tip: value can not >= 0.6mm for Campbell data

      ins_gap <- 0.2 * 3600 / threshold_value["ins", "Value"]
    } else if (str_detect(data_folder_i, regex("HoBo", ignore_case = TRUE))) {
      ## 1.3 HoBo data process -------------------------------------------------

      data_mytest <- map_dfr(.x = filelist$File, .f = readin_hobo) %>%
        mutate(Timestamp1 = parse_date_time(Timestamp, "%m/%d/%Y %H:%M", tz = "Pacific/Pitcairn")) %>% # hobo doesn't have seconds in timestamp
        arrange(Timestamp1) %>%
        filter(year(Timestamp1) == examineYear) %>%
        fill(Temperature_C) %>%
        rename(Temperature_hobo = Temperature_C) %>%
        mutate(`Rain_mm` = ifelse(is.na(`Event`), NA, 0.254)) %>% # add value 0.254 to each event row
        drop_na(Rain_mm) %>%
        select(Timestamp1, Rain_mm, Temperature_hobo) %>%
        mutate(flag_tip = ifelse(Rain_mm == 0.254, NA, "Y"))

      ins_gap <- 0.254 * 3600 / threshold_value["ins", "Value"]
    } else {
      warning("Please check is datafolder names have keywords have 'Campbell' or 'HoBo'.")
    }


    ## 1.4 hourly air temperature process --------------------------------------

    # only read columns: DateTime, Tair_Avg_C. Then filter to days fits SM4 test
    filelist <- data.frame(File = list.files(path = datafolder[3], full.names = TRUE)) %>%
      filter(str_detect(File, regex(location_keyword_i, ignore_case = TRUE)))

    AirTemperature <- map_dfr(.x = filelist$File, .f = read_excel) %>%
      mutate(Timestamp1 = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")) %>%
      arrange(Timestamp1) %>%
      mutate(TimestampH = round(Timestamp1, units = "hours")) %>% # round not floor to hour
      select(TimestampH, Tair_Avg_C)

    AirTemperature_sm4 <- AirTemperature %>%
      mutate(
        Hour = as.numeric(format(TimestampH, "%H")),
        date = as.Date(TimestampH)
      ) %>%
      group_by(date = as.Date(TimestampH)) %>%
      mutate(
        amT = ifelse(Hour >= 0 & Hour <= 15 & Tair_Avg_C <= 0, TRUE, FALSE), # 0 -15 am
        pmT = ifelse(Hour >= 12 & Hour <= 15 & Tair_Avg_C > 0, TRUE, FALSE), # 12-15 pm
        flag_SM4 = ifelse(amT & pmT, "SM4", NA)
      ) %>%
      ungroup() %>%
      select(date, flag_SM4) %>%
      filter(!is.na(flag_SM4))


    # 2. run test --------------------------------------------------------------

    ## 2.1 delete winter readings ----------------------------------------------

    examinestart <- threshold_examine[paste0(location_keyword_i, "_", data_type_i), "examinestart"]
    examineend <- threshold_examine[paste0(location_keyword_i, "_", data_type_i), "examineend"]

    data_mytest <- data_mytest %>%
      filter(between(Timestamp1, examinestart, examineend))

    # read missing table for this location
    missingTable <- threshold_missing %>%
      filter(Data == paste0(location_keyword_i, "_", data_type_i)) %>%
      select(m_start, m_end) %>%
      pivot_longer(
        cols = c("m_start", "m_end"),
        names_to = "flag_missing",
        values_to = "Timestamp1"
      ) %>%
      select(Timestamp1, flag_missing)


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
      ) %>% # time gaps more than 2 hours will be a new event number, start 1
      group_by(event2h) %>%
      mutate(
        flag_instantaneous = ifelse(gap < ins_gap, "instantaneous", NA), # used to be "QC1a"
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
      mutate(flag_SM4a = ifelse(Tair_Avg_C <= 0, "SM4a", NA)) %>%
      ungroup() %>%
      mutate(Rain_C = "") %>%
      select(-c(Hour, floor_hour, floor_day)) %>%
      mutate(date = as.Date(Timestamp1)) %>%
      left_join(AirTemperature_sm4, by = "date") %>%
      select(-`date`) %>%
      full_join(missingTable, by = "Timestamp1") %>% # new
      arrange(Timestamp1)

    ## 2.3 add all missing flags -----------------------------------------------
    missing_start <- which(data_mytest1$`flag_missing` == "m_start")
    missing_end <- which(data_mytest1$`flag_missing` == "m_end")
    data_mytest1$`flag_missing`[unlist(mapply(function(x, y) x:y, missing_start + 1, missing_end - 1))] <- "m"
    data_mytest1$`flag_missing`[missing_start] <- "m_start"
    data_mytest1$`flag_missing`[missing_end] <- "m_end"

    data_mytest1 <- data_mytest1 %>%
      mutate(Rain_C = ifelse(!is.na(flag_missing), -99, "")) %>% # new end migging flag
      unite(flags, starts_with("flag"), na.rm = T, sep = ",", remove = F) %>%
      relocate(Rain_C, .after = Rain_mm) %>%
      relocate(event2h, .after = Tair_Avg_C) %>%
      relocate(flag_tip, .before = flag_instantaneous) %>%
      relocate(flags, .after = Rain_C)


    ## 2.4 summarise table by day ----------------------------------------------
    data_daytable <- data_mytest1 %>%
      group_by(date = as.Date(Timestamp1)) %>%
      summarize(
        Rain_day = sum(Rain_mm),
        flag_tip = ifelse(any(flag_tip == "Y"), "Y", NA),
        flag_instantaneous = ifelse(any(flag_instantaneous == "instantaneous"), "instantaneous", NA),
        flag_prolong = ifelse(any(flag_prolong == "prolong"), "prolong", NA),
        flag_hrmax = ifelse(any(flag_hrmax == "hrmax"), "hrmax", NA),
        flag_daymax = ifelse(any(flag_daymax == "daymax"), "daymax", NA),
        flag_SM4 = ifelse(any(flag_SM4 == "SM4"), "SM4", NA),
        flag_SM4a = ifelse(any(flag_SM4a == "SM4a"), "SM4a", NA)
      ) %>%
      arrange(date) %>%
      mutate(days_dry = as.numeric(date - lag(date, default = first(date))) + 1)


    ## 2.5 print excel for data review -----------------------------------------
    write_xlsx(
      list(
        data = data_mytest1 %>%
          mutate(Timestamp1 = as.character(Timestamp1), TimestampH = as.character(TimestampH)),
        daytable = data_daytable
      ),
      col_names = TRUE,
      paste0("02_test4review/forReview_", location_keyword_i, examineYear, data_type_i, ".xlsx")
    )
  } # end of loop Campbell or HoBo
} # end of loop locations

