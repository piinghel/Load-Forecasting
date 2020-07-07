


# ------------------------------------------------------------------
# POINT FORECASTING
# ------------------------------------------------------------------

# If you need to set options for certain metrics,
# do so by wrapping the metric and setting the options inside the wrapper,
# passing along truth and estimate as quoted arguments.
# Then add on the function class of the underlying wrapped function,
# and the direction of optimization.

#' computes the daily mase
#'
#' @param data: 
#' @param truth: 
#' @param estimate: 
#' @param na_rm: 
#' @return 

mase_daily <- function(data, truth, estimate, na_rm = TRUE, ...) {
  mase(
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    # set bias = TRUE
    m = 24,
    na_rm = na_rm,
    ...
  )
}

# Add on the underlying function class (here, "numeric_metric"), and the
# direction to optimize the metric
class(mase_daily) <- class(mase)
attr(mase_daily, "direction") <- attr(mase, "direction")

#' computes the weekly mase
#'
#' @param data: 
#' @param truth: 
#' @param estimate: 
#' @param na_rm: 
#' @return 

mase_weekly <- function(data, truth, estimate, na_rm = TRUE, ...) {
  mase(
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    # set bias = TRUE
    m = 24 * 7,
    na_rm = na_rm,
    ...
  )
}


# Add on the underlying function class (here, "numeric_metric"), and the
# direction to optimize the metric
class(mase_weekly) <- class(mase)
attr(mase_weekly, "direction") <- attr(mase, "direction")






# ------------------------------------------------------------------
# PROBABLISTIC FORECASTING
# ------------------------------------------------------------------



# implementation based on:
# https://robjhyndman.com/papers/forecasting_state_of_the_art.pdf


# WINKLER SCORE
winkler_score <- function(df = NULL,
                          alpha = .05) {
  lower <- df[1]
  upper <- df[2]
  actual <- df[3]
  
  if (lower < actual & actual < upper) {
    score <- upper - lower
  }
  
  else if (actual < lower) {
    score <- (upper - lower) + 2 / alpha * (lower - actual)
    
  }
  
  else{
    score <- (upper - lower) + 2 / alpha * (actual - upper)
  }
  
  return(score)
  
}


# PINBALL LOSS OR PERCENTILE SCORE
pinball_loss <- function(q_it = NULL,
                         i = NULL,
                         actual = NULL) {
  if (actual < q_it) {
    score <- (1  - i / 100) * (q_it - actual)
  }
  else{
    score <- (i / 100) * (acutal - q_it)
  }
  
  return(score)
  
  
}
