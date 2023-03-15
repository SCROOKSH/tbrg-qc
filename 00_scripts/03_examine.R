# Examine 2 data type for each location
# R/03_examine.R
#
# This script will:
# - Read in tables from folder "02_test4review"
# - Generate (1) 3 html reports in the "03_examine" folder and (2) a dataset "03_examine/df_wide.Rda" which is needed for "04_app.R"

# Version: 0.1
# Last update: 2023-03-13






# start here -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(hydroGOF)
library(readxl)
library(zoo)
library(plotly)
library(viridis)


# 0. Users settings - please check every year ----------------------------------

examineYear <- "2022" # manually type here
reviewfolder <- "02_test4review"
location_keyword <- c("Burn", "Cabin", "Seed")
temperature_Folder <- "01_input/Hourly Air Temperature"                         # the folder of cleaned hourly air temperature
rain_scale_factor <- 5                                                          # for plotting

# data_type Will read from excel file name. "forReview_Seed2022Campbell1.xlsx" will read data_type as Campbell - after year number and before ".xlsx"

threshold_examine <- read_excel("01_input/threshold_value.xlsx", sheet = "examine") %>% # col Data is unique rownames
  column_to_rownames(var = "Data") %>%
  mutate(
    examinestart = parse_date_time(examinestart, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"),
    examineend = parse_date_time(examineend, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"))

GOF_date <- read_excel("01_input/threshold_value.xlsx", sheet = "GOF") %>%
  column_to_rownames(var = "Location") %>%
  mutate(
    GOFstart = parse_date_time(GOFstart, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"),
    GOFend = parse_date_time(GOFend, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn"))


# 1. Loop locations ------------------------------------------------------------

for (location_keyword_i in location_keyword) {
  GOFstart <- GOF_date[location_keyword_i, "GOFstart"]
  GOFend <- GOF_date[location_keyword_i, "GOFend"]

  # read the review data folder
  filelist <- data.frame(File = list.files(path = reviewfolder, pattern = "\\.xlsx$", full.names = TRUE)) %>%
    filter(str_detect(File, regex(location_keyword_i, ignore_case = TRUE)))

  ## 1.0 loop 2 files in each location -----------------------------------------

  data_df <- list()
  data_day <- list()
  plot_p <- list()
  data_df_hr <- list()
  data_type_all <- list()

  for (i in 1:nrow(filelist)) {
    file_i <- filelist$File[i]
    data_type_i <- sub(".*\\d(.*)\\.xlsx", "\\1", file_i)
    data_type_all[[i]] <- data_type_i 
    
    ## 1.1 if Campbell ---------------------------------------------------------
    if (str_detect(file_i, regex("Campbell", ignore_case = TRUE))) {
      
      ### 1.1.1 clean data -----------------------------------------------------
      dataCa <- read_excel(file_i, sheet = "data") %>%
        mutate(
          Rain = ifelse(!is.na(Rain_C), Rain_C, Rain_mm),
          Rain = as.numeric(Rain),
          Rain = ifelse(Rain == -99, NA, Rain),
          Timestamp = parse_date_time(Timestamp1, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")
        ) %>%
        select(Timestamp, Rain) %>%
        mutate(TimestampH = floor_date(Timestamp, "hour")) %>%
        drop_na(Timestamp)

      ### 1.1.2 make day data --------------------------------------------------
      dataCa_day <- dataCa %>%
        group_by(date = as.Date(Timestamp)) %>%
        summarize(!!paste0("Rain_", data_type_i) := sum(Rain))

      ### 1.1.3 plot heatmap for Campbell --------------------------------------
      data_plot_heatmap_Campbell <- dataCa %>%
        mutate(
          year = year(Timestamp),
          month = month(Timestamp, label = TRUE, abbr = TRUE),
          day = day(Timestamp),
          hour = hour(Timestamp)
        ) %>%
        group_by(year, month, day, hour) %>%
        summarize(
          Rain = sum(Rain),
          DateTimeHour = TimestampH[1]
        ) %>%
        ungroup()

      plot_heatmap_Campbell <-
        ggplot(data = data_plot_heatmap_Campbell, aes(day, hour, fill = Rain)) +
        geom_tile(color = "white", linewidth = 0.1) +
        scale_fill_viridis(option = "D", direction = -1, begin = 0.3, end = 1) + #
        facet_grid(year ~ month) +
        scale_y_continuous(trans = "reverse", breaks = unique(data_plot_heatmap_Campbell$hour)) +
        scale_x_continuous(breaks = c(1, 10, 20, 30)) +
        labs(title = "Campbell", x = "Day", y = "Hour Commencing") +
        theme_bw() +
        theme(
          legend.position = "bottom",
          strip.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(color = "grey", linewidth = 0.2),
          panel.grid.minor.x = element_line(color = "grey", linewidth = 0.1, linetype = "dotted"),
          panel.grid.minor = element_blank())

      ### 1.1.4 save ----------------------------------------------------------------

      dataCa_hr <- dataCa %>%
        group_by(TimestampH) %>%
        summarize(!!paste0("Rain_h_", data_type_i) := sum(Rain)) %>%
        mutate(!!paste0("Rain_h_", data_type_i) := ifelse(is.na(!!sym(paste0("Rain_h_", data_type_i))), -1, !!sym(paste0("Rain_h_", data_type_i))))

      data_df[[i]] <- dataCa
      data_day[[i]] <- dataCa_day
      plot_p[[i]] <- plot_heatmap_Campbell
      # recordPlot(plot_p[[i]])
      data_df_hr[[i]] <- dataCa_hr

      
      ## 1.2 if HoBo -----------------------------------------------------------
    } else if (str_detect(file_i, regex("HoBo", ignore_case = TRUE))) {
      
      ### 1.2.1 read and clean data --------------------------------------------
      dataHo <- read_excel(file_i, sheet = "data") %>%
        mutate(
          Rain = ifelse(!is.na(Rain_C), Rain_C, Rain_mm),
          Rain = as.numeric(Rain),
          Rain = ifelse(Rain == -99, NA, Rain),
          Timestamp = parse_date_time(Timestamp1, "%Y-%m-%d/ %H:%M:%s", tz = "Pacific/Pitcairn")
        ) %>%
        select(Timestamp, Rain) %>%
        mutate(TimestampH = floor_date(Timestamp, "hour")) %>%
        drop_na(Timestamp)

      ### 1.2.2 make day data --------------------------------------------------
      dataHo_day <- dataHo %>%
        group_by(date = as.Date(Timestamp)) %>%
        summarize(!!paste0("Rain_", data_type_i) := sum(Rain))

      ### 1.2.3 heatmap for HoBo-------- ---------------------------------------
      data_plot_heatmap_HoBo <- dataHo %>%
        mutate(
          year = year(Timestamp),
          month = month(Timestamp, label = TRUE, abbr = TRUE),
          day = day(Timestamp),
          hour = hour(Timestamp)
        ) %>%
        group_by(year, month, day, hour) %>%
        summarize(
          Rain = sum(Rain),
          DateTimeHour = TimestampH[1]
        ) %>%
        ungroup()

      plot_heatmap_HoBo <-
        ggplot(data = data_plot_heatmap_HoBo, aes(day, hour, fill = Rain)) +
        geom_tile(color = "white", linewidth = 0.1) +
        scale_fill_viridis(option = "D", direction = -1, begin = 0.3, end = 1) + #
        facet_grid(year ~ month) +
        scale_y_continuous(trans = "reverse", breaks = unique(data_plot_heatmap_HoBo$hour)) +
        scale_x_continuous(breaks = c(1, 10, 20, 30)) +
        labs(title = "HoBo", x = "Day", y = "Hour Commencing") +
        theme_bw() +
        theme(
          legend.position = "bottom",
          strip.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(color = "grey", linewidth = 0.2),
          panel.grid.minor.x = element_line(color = "grey", linewidth = 0.1, linetype = "dotted"),
          panel.grid.minor = element_blank()
        )

      ### 1.2.4 save ----------------------------------------------------------------

      dataHo_hr <- dataHo %>%
        group_by(TimestampH) %>%
        summarize(!!paste0("Rain_h_", data_type_i) := sum(Rain)) %>%
        mutate(!!paste0("Rain_h_", data_type_i) := ifelse(is.na(!!sym(paste0("Rain_h_", data_type_i))), -1, !!sym(paste0("Rain_h_", data_type_i))))

      data_df[[i]] <- dataHo
      data_day[[i]] <- dataHo_day
      plot_p[[i]] <- plot_heatmap_HoBo
      # recordPlot(plot_p[[i]])
      data_df_hr[[i]] <- dataHo_hr
    
      } else {
      warning("Excel file name should have a data_type keyword to indicate if 
              this is Campbell or HoBo after year number and before '.xlsx'.")
    }
  } # end of loop filelist for one location

  
  
  # 2. GOF ---------------------------------------------------------------------

  data_day_join <- full_join(data_day[[1]], data_day[[2]], by = "date") %>%     # GOF needs 2 files for each location
    arrange(date) %>% as.data.frame()

  GOFtableDay <- data_day_join %>%
    filter(date >= GOFstart & date <= GOFend)

  GOFresultDay <- gof(sim = GOFtableDay[, 2], obs = GOFtableDay[, 3]) %>%
    t() %>%
    as.data.frame() %>%
    remove_rownames()

  data_day_long <- data_day_join %>%
    pivot_longer(cols = -date, names_to = "data_type", values_to = "Rain_day")

  plot_gof_day <-
    ggplot(data = data_day_long, aes(x = date)) +
    geom_line(aes(y = Rain_day, colour = data_type), linewidth = 1) +
    scale_color_manual(values = c("steelblue", "brown2")) +
    ylab("Rain (mm)") +
    xlab("") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
    ggtitle(paste0("Daily rain ", examineYear)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
      legend.background = element_rect(fill = "transparent")
    ) +
    geom_vline(xintercept = as.numeric(as.Date(GOFstart)), colour = "orange") +
    annotate("text", x = as.Date(GOFstart), y = 15, label = paste("GOF start: \n", GOFstart), angle = 90) +
    geom_vline(xintercept = as.numeric(as.Date(GOFend)), colour = "orange") +
    annotate("text", x = as.Date(GOFend), y = 15, label = paste("GOF end: \n", GOFend), angle = 90) # end GOF



  # 3 cold plot --------------------------------------------

  data_hr_long <- full_join(data_df_hr[[1]], data_df_hr[[2]], by = "TimestampH") %>%
    pivot_longer(cols = -TimestampH, names_to = "data_type", values_to = "Rain_h") %>%
    mutate(Rain_h_scaled = Rain_h * rain_scale_factor)

  # read in temperature data for that location, should be only one file to read
  filelist <- data.frame(File = list.files(path = temperature_Folder, full.names = TRUE)) %>%
    filter(str_detect(File, regex(location_keyword_i, ignore_case = TRUE)))
  AirTemperature <- map_dfr(.x = filelist$File, .f = read_excel) %>%
    mutate(Timestamp1 = parse_date_time(DateTime, "%Y-%m-%d %H:%M:%S", tz = "Pacific/Pitcairn")) %>%
    arrange(Timestamp1) %>%
    mutate(TimestampH = round(Timestamp1, units = "hours")) %>% # round not floor to hour
    select(TimestampH, Tair_Avg_C) %>%
    filter(TimestampH >= (min(data_hr_long$TimestampH) - days(3)) & TimestampH <= (max(data_hr_long$TimestampH) + days(3)))

  data_plot_cold <- full_join(data_hr_long, AirTemperature, by = "TimestampH") %>%
    group_by(TimestampH) %>%
    filter(n() == 1 | !is.na(Rain_h)) %>%
    ungroup() %>%
    arrange(TimestampH)

  # Create the temperature rain hourly data plot
  plot_temp_rain_hr <-
    ggplot(data = data_plot_cold, aes(x = TimestampH, text = Rain_h)) +
    geom_line(aes(y = Tair_Avg_C, color = "Tair_Avg_C"),
      show.legend = TRUE, linewidth = 0.5
    ) +
    scale_color_manual(values = "lightblue") +
    geom_col(aes(y = Rain_h_scaled, fill = data_type),
      position = position_dodge()
    ) +
    scale_fill_manual(values = c("steelblue", "brown2")) +
    scale_y_continuous(
      name = "Temperature (Celsius)",
      sec.axis = sec_axis(~ . / rain_scale_factor, name = "Hourly Rainfall (mm) (5x)")
    ) +
    scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d") +
    labs(x = "") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
      legend.background = element_rect(fill = "transparent")
    ) +
    geom_hline(yintercept = 0, color = "orange") +
    geom_vline(xintercept = GOFstart, colour = "orange") +
    geom_vline(xintercept = GOFend, colour = "orange")

  # clean table for html report
  data_hour_all <- full_join(data_df_hr[[1]], data_df_hr[[2]], by = "TimestampH") %>%
    mutate(
      TimestampH = as.character(TimestampH),
      across(2:length(.), ~ ifelse(. == -1, -99, .))
    )


  # 4 call markdown ------------------------------------------------------------
  
  rmarkdown::render("00_scripts/03_examine.Rmd",
    output_file = paste0(here::here(), "/03_examine/data_examine", location_keyword_i, "_", Sys.Date(), ".html"),
    envir = new.env(globalenv())
  )

} # end of loop through location keyword



# 5 Get the App data ready -----------------------------------------------------

## 5.1 read in all result ------------------------------------------------------

filelist <- list.files(path = "02_test4review/", pattern = "^[^~].*\\.xlsx$", full.names = TRUE)

df <- NULL
for (i in 1:length(filelist)) {
  nicename <- str_extract(filelist[i], "(?<=forReview_)[^/]+(?=\\..*)")

  df_i <- read_excel(filelist[i], sheet = "data") %>%
    mutate(
      Rain = ifelse(!is.na(Rain_C), Rain_C, Rain_mm),
      Timestamp = parse_date_time(Timestamp1, "%Y-%m-%d %H:%M:%s", tz = "Pacific/Pitcairn"),
      file = nicename
    ) %>%
    mutate(Rain = ifelse(Rain == -99, NA, Rain)) %>%
    filter(Rain != 0 & !is.na(Rain)) %>% # may need to modify later
    select(Timestamp, Rain, file)

  df <- rbind(df, df_i)
}

## 5.2 process HoBo data -------------------------------------------------------
# HoBo seconds data are all 00. we add a random value to it

# find replica rows
not_re_df <- df %>%
  group_by(Timestamp, file) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-n)

replica_rows <- df %>%
  group_by(Timestamp, file) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  select(-n)

# add random seconds to replica rows
replica_rows_new <- replica_rows %>%
  rowwise() %>%
  mutate(
    Timestamp = ifelse(grepl("HoBo", file) & format(Timestamp, "%S") == "00",
      Timestamp + runif(1, 1, 59),
      Timestamp
    ),
    Timestamp = as.POSIXct(Timestamp, origin = "1970-01-01")
  ) %>%
  ungroup()

# merge replica rows with original df
df_new <- rbind(replica_rows_new, not_re_df) %>%
  arrange(file, Timestamp)

# check if all rows are still there. TRUE means to go
 if (nrow(df) == nrow(df_new)){message("All HoBo data area successfully processed")
   } else {message("Please rerun '## 5.2 process HoBo data' to generate new random seconds")}

# 3 make cdf data --------------------------------------------------------------

df_wide <- df_new %>%
  pivot_wider(names_from = file, values_from = Rain, names_prefix = "Rain_") %>%
  mutate_at(vars(starts_with("Rain_")), as.numeric)


save(df_wide, file = "03_examine/df_wide.Rda")                                  # App will load this data

