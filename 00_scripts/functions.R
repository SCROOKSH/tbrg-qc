# Functions

# This file contains functions used by other scripts. Please do not run this file directly.

# Version: 0.1
# Last update: 2023-03-13











#' read in Campbell dat files
#'
#' @param file path and file name, file should be .dat Raw Campbell Scientific Files. Will skip first 4 rows and rename colnames
#'
#' @return
#' @export
#'
#' @examples readin_campbell("input/Raw Campbell Scientific Files/Burn_Table3_220530b.dat")

readin_campbell <- function(file){
  if (!str_detect(file, regex("Campbell", ignore_case = TRUE)))
    stop("readin_campbell can only be used on Campbell raw data file, please put your .dat in correct folder (e.g. 01_inputlogger/Raw Campbell Scientific Files/ )")

  mydata <- readr::read_csv(file,
                            skip = 3,
                            col_types = "c")
  return(mydata)
}




#' readin HoBo csv files
#'
#' @param file include path and file name
#'
#' @return
#' @export
#'
#' @examples readin_hobo("input/Raw Hobo Files/Cabin_2022_221101.csv")

readin_hobo <- function(file){
  if (!str_detect(file, regex("HoBo", ignore_case = TRUE)))
    stop("readin_hobo can only be used on HoBo raw data file, please put your data in correct folder (e.g. 01_inputlogger/Raw Hobo Files/)")

  mydata <- suppressWarnings(
    readr::read_csv(file,
                    skip = 1,
                    col_types = "c",
                    col_names = c("no", "Timestamp", "Temperature_C", "Event"))
  )

  mydata<- mydata[, 1:4]
  return(mydata)
}






