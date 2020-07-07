#' prepares and computes probablitic forecasting using a normal distribution 
#'
#' @param preds: 
#' @param actuals: 
#' @param residuals_inSample: 
#' @param alpha: 
#' @return 


prob_forecast_Un <-
  function(preds = NULL,
           actuals = NULL,
           residuals_inSample = NULL,
           alpha = 0.025) {
    if (is.null(residuals_inSample)) {
      residuals_inSample = actuals$cs - preds$.pred
      
    }
    
    tibble(
      date = actuals$date,
      week_day = lubridate::wday(date, label = TRUE),
      hour = lubridate::hour(date),
      day_type = as.factor(ifelse(
        week_day %in% c("za", "zo"),
        "Sat-Sun", "Mon-Fri"
      )),
      month = as.factor(lubridate::month(date, label = FALSE)),
      working_hour = as.factor(ifelse(
        hour %in% c(7, 8, 9, 10, 11, 12, 13,
                    14, 15, 16, 17, 18, 19),
        "7h-19h",
        "20h-6h"
      )),
      season = ifelse(
        month %in% c(1, 2, 3),
        "Winter",
        ifelse(
          month %in% c(4, 5, 6),
          "Spring",
          ifelse(
            month %in% c(7, 8, 9),
            "Summer",
            ifelse(month %in% c(10, 11, 12), "Autumn", NA)
          )
        )
      ),
      fitted =  preds$.pred,
      actual = actuals$cs,
      residual = fitted - actual,
      lower_nU = fitted - qnorm(1 - alpha) * sd(residuals_inSample),
      upper_nU = fitted + qnorm(1 - alpha) * sd(residuals_inSample),
      cover_nU = actual >= lower_nU & actual <= upper_nU
    )
    
  }



#'
#' @param pred: 
#' @param historical_resid: 
#' @param colNr_fitted: 
#' @param colNr_dayType: 
#' @param colNr_hour: 
#' @param hour_window
#' @return 


conditional_bootstap <- function(pred = NULL,
                                 historical_resid = NULL,
                                 size = 1000,
                                 colNr_fitted = 8,
                                 colNr_dayType = 2,
                                 colNr_hour = 3,
                                 hour_window = 1) {
  # find lower and upper hour to group
  lower <- (pred[[colNr_hour]] - hour_window) %% 24
  upper <- (pred[[colNr_hour]] + hour_window) %% 24
  
  
  # get interval
  if (lower > upper) {
    interval <- union(seq(lower, 23), seq(0, upper))
  } else {
    interval <- seq(lower, upper)
  }
  
  # generate bootstraps
  prob_dist <- historical_resid %>%
    filter(., week_day == pred[[colNr_dayType]] &
             hour %in% interval) %>%
    select(residual) %>% pull %>% sample(., size = size, replace = TRUE) %>%
    as.matrix(nrow = size) %>% t() + pred[[colNr_fitted]]
  return(prob_dist)
  
}
