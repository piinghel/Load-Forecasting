

#' Creates new lagged by means of lags of a given timeserie
#'
#' @param df: A tsibble
#' @param start_date_train: Date indicating the start date of the training set
#' @param frequency: A numeric indicating the frequency of the observations
#' @param remove_date: A Boolean indiciating whether to remove the date feature
#' @return return a tsibble dataframe with new calandar features



calandar_featues <- function(df,
                             start_date_train,
                             frequency = 15,
                             remove_date = FALSE) {
  start_obs = difftime(as.POSIXct(df$Date[1]), as.POSIXct(start_date_train),
                       units = "mins") / frequency
  
  end_obs = start_obs + nrow(df) - 1
  out <- df %>%
    mutate(
      #Date_simple = lubridate::date(Date),
      Trend = start_obs:end_obs,
      #Year = as.factor(lubridate::year(Date)),
      Month = as.factor(lubridate::month(Date, label = FALSE)),
      Week = as.factor(lubridate::week(Date)),
      WeekDay = as.factor(lubridate::wday(Date, label = TRUE)),
      #DayNr = as.factor(lubridate::yday(Date)),
      Hour = lubridate::hour(Date),
      #Minute = lubridate::minute(Date),
      Quarter = lubridate::quarter(Date),
      DayType = as.factor(ifelse(
        WeekDay %in% c("za", "zo"),
        "Sat-Sun", "Mon-Fri"
      )),
      WorkingHour = as.factor(ifelse(
        Hour %in%
          c(7, 8, 9, 10, 11, 12, 13,
            14, 15, 16, 17, 18, 19),
        "7H-19H",
        "20H-6H"
      )),
      Sin_Hour  = sin(2 * pi * Hour / 24),
      Cos_Hour = cos(2 * pi * Hour / 24),
      # Sin_Hour_Minute  = sin(2*pi*Hour_Minute/24),
      # Cos_Hour_Minute = cos(2*pi*Hour_Minute/24)
      Hour = as.factor(Hour),
      # lente: vrijdag 20 maart tot zaterdag 20 juni
      # zomer: zaterdag 20 juni Tot dinsdag 22 september
      # herfst: dinsdag 22 september Tot: maandag 21 december
      # winter: zondag 22 december Tot: vrijdag 20 maart 2020
      Season = ifelse(
        Month %in% c(1, 2, 3),
        "Winter",
        ifelse(
          Month %in% c(4, 5, 6),
          "Spring",
          ifelse(
            Month %in% c(7, 8, 9),
            "Summer",
            ifelse(Month %in% c(10, 11, 12), "Autumn", NA)
          )
        )
      )
      
    )
  # change weekday from dutch to english
  out$WeekDay <- plyr::mapvalues(
    out$WeekDay,
    from = c("ma", "di", "wo", "do", "vr", "za", "zo"),
    to = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  ) %>%
    factor(., ordered = FALSE)
  
  if (remove_date == TRUE) {
    return(out[, 2:ncol(out)])
  }
  out
  
}





#' Creates new lagged by means of lags of a given timeserie
#'
#' @param df: dataframe,
#' @param var: variable to create lags from, nr_lag
#' @param lags: number of lags (default 1)
#' @param remov_NA: boolean indicating whether to remove the NA values created from lags
#' @return return a tsibble dataframe with 'lags' new features

# How to efficiently create multiple lags: THX!!
# https://gist.github.com/drsimonj/2038ff9f9c67063f384f10fac95de566

create_lags <- function(df,
                        var,
                        lags = 1,
                        remove_NA = TRUE) {
  # lags should be greater than 0
  if (min(lags) <= 0) {
    return(df)
  }
  
  
  lag_names <- paste0(var, paste("_lag"),
                      formatC(lags,
                              width = nchar(max(lags)),
                              flag = "0"))
  
  lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"),
                            lag_names)
  
  df_lag <- df %>% dplyr::mutate_at(vars(var), funs_(lag_functions))
  
  # remove NA values created from lags
  if (remove_NA == TRUE) {
    return(df_lag[(max(lags) + 1):nrow(df), ])
  }
  else{
    df_lag
  }
  
}



#' Creates new lagged by means of lags of a given timeserie
#'
#' @param df: dataframe,
#' @param pred_time: variable to create lags from, nr_lag
#' @param frequency: number of lags (default 1)
#' @return return a tsibble dataframe with 'nr_lags' new features

make_lagPred_table <-
  function(df,
           pred_time = "00:00:00" ,
           frequency = 96) {
    # check whether the are any lagged variable present
    if (any(grepl('lag', colnames(df))) == FALSE) {
      return(df)
    }
    # get prediction dates
    pred_dates <- df %>%
      filter(., strftime(date, format = "%H:%M:%S") == pred_time)
    
    first_pred_date <- pred_dates$date[1]
    last_pred_date <- pred_dates$date[nrow(pred_dates)]
    
    # contains the consumption to predict and date feature
    y_matrix <- df %>% filter(date >= first_pred_date) %>%
      select(date, total_cs)
    
    # length of the predictions
    n_length <- frequency * (nrow(pred_dates) - 1) +
      (df %>% select(date) %>% filter(date >= last_pred_date) %>% nrow())
    
    # construct the lag matrix and add date column to join easily
    x_matrix <- pred_dates[, 3:ncol(pred_dates)] %>%
      mutate(freq = frequency) %>% uncount(freq)
    
    x_matrix <- x_matrix[1:n_length, ] %>% mutate(date = df %>% filter(date >= first_pred_date) %>% pull(date))
    
    # join both data frames by date
    inner_join(y_matrix, x_matrix, by = "date")
    
    
  }
