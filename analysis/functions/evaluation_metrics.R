
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

mase_daily <-
  function(data,
           truth,
           estimate,
           m_daily = 24,
           na_rm = TRUE,
           ...) {
    mase(
      data = data,
      truth = !!rlang::enquo(truth),
      estimate = !!rlang::enquo(estimate),
      # set bias = TRUE
      m = m_daily,
      na_rm = na_rm,
      ...
    )
  }

# Add on the underlying function class (here, "numeric_metric"), and the
# direction to optimize the metric
# class(mase_daily) <- class(mase)
# attr(mase_daily, "direction") <- attr(mase, "direction")



class(mase_daily) <- class(mase)
attr(mase_daily, "direction") <- attr(mase, "direction")




#' computes the weekly mase
#'
#' @param data:
#' @param truth:
#' @param estimate:
#' @param na_rm:
#' @return

mase_weekly <-
  function(data,
           truth,
           estimate,
           m_weekly = 168,
           na_rm = TRUE,
           ...) {
    mase(
      data = data,
      truth = !!rlang::enquo(truth),
      estimate = !!rlang::enquo(estimate),
      # set bias = TRUE
      m = m_weekly,
      na_rm = na_rm,
      ...
    )
  }

# Add on the underlying function class (here, "numeric_metric"), and the
# direction to optimize the metric
class(mase_weekly) <- class(mase)
attr(mase_weekly, "direction") <- attr(mase, "direction")



mase_vec <-
  function(truth,
           estimate,
           m = 24,
           metric = mae,
           na_rm = TRUE,
           ...) {
    mase_impl <- function(truth, estimate, m, metric) {
      mase_df <- cbind(truth, estimate) %>% tibble::as_tibble()
      mae = mase_df %>%  metric(truth, estimate) %>% pull(".estimate")
      naive_mae = mase_df %>% metric(truth, lag(truth, m)) %>% pull(".estimate")
      mase = mae / naive_mae
      mase
    }
    
    metric_vec_template(
      metric_impl = mase_impl,
      truth = truth,
      estimate = estimate,
      m = m,
      metric = metric,
      na_rm = na_rm,
      cls = "numeric",
      ...
    )
    
  }




mase_w <- function(data, ...) {
  UseMethod("mase_w")
}


mase_w.data.frame <-
  function(data, truth, estimate, na_rm = TRUE, ...) {
    metric_summarizer(
      metric_nm = "mase_w",
      metric_fn = mase_vec,
      data = data,
      truth = !!enquo(truth),
      estimate = !!enquo(estimate),
      na_rm = na_rm,
      ...
    )
    
  }



# ------------------------------------------------------------------
# PROBABLISTIC FORECASTING
# ------------------------------------------------------------------



# implementation based on:
# https://robjhyndman.com/papers/forecasting_state_of_the_art.pdf


# WINKLER SCORE
winkler_score <- function(df = NULL,
                          alpha = .05,
                          start = 1) {
  lower <- df[start]
  upper <- df[start + 1]
  actual <- df[start + 2]
  
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



pinball_loss <- function(q, actual, i) {
  score <- c(1:length(q))
  for (j in 1:length(q)) {
    if (actual < q[j]) {
      score[j] <- (1  - i[j] / 100) * (q[j] - actual)
    }
    else if (actual >= q[j]) {
      score[j] <- ((i[j] / 100) * (actual - q[j]))
      
    }
    else{
      print("error")
    }
  }
  return(score)
}


summary_stats <- function(data = NULL) {
  return (
    tibble(
      min = min(data),
      q_25 = quantile(data, 0.25) %>% as.numeric(),
      mean = mean(data),
      median = median(data),
      q_75 = quantile(data, 0.75) %>% as.numeric(),
      max = max(data),
      skewness = moments::skewness(data),
      kurtosis = moments::kurtosis(data)
    )
  )
}



# DIAGNOSTICS PLOTS:

# 1) probability integral transform


# helper function to get samples from a normal dist
get_samples_normal <- function(fitted, sd_resid, mu_resid, nr_samples = 5000) {
  n_obs <- length(fitted)
  output <- matrix(NA, nrow = n_obs, ncol = nr_samples)
  parameters <- tibble(mu_resid = mu_resid, sd_resid = sd_resid)
  for (i in 1:n_obs) {
    output[i, ] <- fitted[i] + rnorm(nr_samples, mean = parameters$mu_resid[i], sd = parameters$sd_resid[i])
  }
  return(output)
}


pit_plot <- function(pit_object = NULL,
                     nr_obs = NULL) {
  counts <- pit_object$hist_PIT$counts
  n <- counts %>% length()
  x <- seq(0.01, 1, 1 / n)[1:n]
  reference <- nr_obs / n
  
  df <- tibble(counts = counts,
               pit_samples = x)
  
  p <- ggplot(df) +
    geom_col(
      aes(x = pit_samples, y = counts),
      color = "black",
      fill = "white",
      orientation = "x"
    ) +
    geom_hline(
      yintercept = reference,
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    labs(x = "PIT samples", y = "Frequency")
  return(p)
}

# 2) RELIABILITY DIAGRAM

# 2.1 OVERALL DIAGRAM

# prep fucntion for normal distribution
prep_rel_diagram_und <-
  function(fitted, quantiles, mu_resid, sd_resid,
           nr_sim = 10000) {
    l_q <- quantiles %>% length
    nr_obs <- length(fitted)
    q_hat <- matrix(
      data = NA,
      nrow = nr_obs,
      ncol = l_q
    )
    
    for (i in 1:nr_obs) {
      q_hat[i, ] <-
        (fitted[i] + rnorm(nr_sim, mu_resid[i], sd_resid[i])) %>% quantile(quantiles) %>% as.numeric()
    }
    return(q_hat)
  }

# overall plot
reliability_diagram <- function(q_hat, quantiles, actuals) {
  emp_coverage <- quantiles
  for (i in 1:length(quantiles)) {
    emp_coverage[i] <- (q_hat[, i] > actuals) %>% mean
  }
  
  p <- tibble(emperical = emp_coverage,
              nominal = quantiles) %>% ggplot(aes(x = nominal, y = emperical)) +
    geom_point() +
    geom_abline(
      intercept = 0,
      color = "red",
      linetype = "dashed",
      size = 1
    ) +
    labs(x = "Nominal", y = "Emperical")
  return(p)
  
}


# 2.2 CONDITIONAL DIAGRAM


# prep function


# q_hat: quantile forecast or prediction interval forecasts
# df containing:
# - fitted values
# - actual values
# - conditional varialbe example hour


prep_rel_diagram_cnd <-
  function(q_hat,
           df,
           quantiles,
           cond_var = "hour",
           nr_cond = 24) {
    l_q <- length(quantiles)
    
    q_hat_cond <- (q_hat > df$actual) %>%
      tibble(cond_var = df %>% select(!!sym(cond_var)) %>% pull) %>%
      group_by(cond_var) %>%
      group_map(~ colMeans(.)) %>% do.call(cbind, .) %>%
      as_tibble() %>%
      gather(cond_var, emperical) %>%
      mutate(
        cond_var = rep(1:nr_cond, each = l_q),
        nominal = rep(quantiles, nr_cond)
      )
    
    return(q_hat_cond)
  }

# plot

# df containing:
# - emperical coverage
# - nominal coverage
# - conditional variable (col) example hour


reliability_diagram_cond <-
  function(df,
           col,
           nr_col = 24,
           legend_lab = "Hour") {
    p <- ggplot(df, aes(x = nominal, y = emperical)) +
      geom_point(aes(colour = !!sym(col)))  +
      scale_colour_gradientn(colours = terrain.colors(nr_col)) +
      labs(color = legend_lab, x = "Nominal", y = "Emperical") +
      geom_abline(
        intercept = 0,
        color = "red",
        linetype = "dashed",
        size = 1
      )
    return(p)
    
  }




summarize_winkler_hour <-
  function(data = NULL,
           lower = NULL,
           upper = NULL,
           name_method = "uncond. norm",
           name_model = "elastic net",
           alpha = ALPHA,
           func_winkler_score = winkler_score) {
    out <- data %>% select(hour, !!sym(lower), !!sym(upper), actual) %>%
      apply(1, start = 2, func_winkler_score,
            alpha = alpha) %>% as_tibble() %>%
      mutate(
        hour = data$hour,
        model = name_model,
        method = name_method,
        metric = "winkler score"
      ) %>%
      dplyr::group_by(hour, model, method, metric) %>% 
      dplyr::summarise(value = mean(value))
    return(out)
  }


summarize_reliability <- function(data, actual, quantiles, method, model){
  emp_coverage <- quantiles
  for (i in 1:length(quantiles)) {
    emp_coverage[i] <- (data[, i] > actual) %>% mean
  }
  return(tibble(value = emp_coverage,
                method = method,
                model = model))
}

