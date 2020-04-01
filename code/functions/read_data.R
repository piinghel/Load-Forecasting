#' Helper function for @get_data_fifth
#' Converts data to tsibble and renames the dateTimeMeasurement column to date
#'
#' @param df: dataframe with 2 columns Value and dateTimeMeasurement
#' @param vars: variables to select (default NULL)
#' @return a tsibble dataframe

fix_dates <- function(df, vars = NULL) {
  if (is.null(vars)) {
    vars = c(colnames(df))
  }
  df <-
    df %>% select(vars) %>% plyr::rename(c("DateTimeMeasurement" = "Date"))
  # lowercase columns
  colnames(df) <- tolower(colnames(df))
  df$date <- ymd_hms(df$date, tz = "Europe/Brussels")
  as_tsibble(df, index = date)
  
}

#' Read in the data from fifthdisplay
#'
#' @param dir directory of the data
#' @param vars variables to select (default NULL)
#'
#' @return a list with 5 in the format of a tsibble

get_data <- function(dir, vars = NULL) {
  files <- list.files(dir, pattern = "*.csv")
  path <- file.path(dir, paste0(files))
  data_list <- lapply(path, read.csv,
                      stringsAsFactors = FALSE)
  data_list <- lapply(data_list, fix_dates, vars = vars)
  names(data_list) <- gsub("\\.csv$", "", files)
  data_list
  
  
}
