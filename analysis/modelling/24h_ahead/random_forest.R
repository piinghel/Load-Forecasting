# clean environment
rm(list = ls())


# --------------------------------------------------------------------
# 0) Load libraries
# --------------------------------------------------------------------


# data manipulations
library(lubridate)
library(tsibble)
library(dplyr)
library(forecast)

# visualisations
library(ggplot2)
library(plotly)
library(patchwork)
library(vip) # variable importance

# modelling
library(tidymodels)
library(ranger)

# evaluate models
library(scoringRules) # probablistic scores

# own functions
source("analysis/functions/create_features.R")
source("analysis/functions/error_analysis.R")
source("analysis/functions/save_output.R")
source("analysis/functions/evaluation_metrics.R")
source("analysis/functions/prob_forecasts.R")
# --------------------------------------------------------------------
#  1) Global parameters
# --------------------------------------------------------------------

# colors
cbPalette <- c(
  "#999999",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

# choose theme
THEME <- theme_gray() #theme_minimal()
# remove legend title
LEGEND <- theme(legend.title = element_blank())

# seed for reproduceability
set.seed(100)

# Parallel Processing
# parallel::detectCores(logical = TRUE)
# cl <- makeCluster(2)

# metric of interest
METRIC <- "rsq_trad"

# performance metrics
PERF_METRIC_SET <- metric_set(mase_daily,
                              mase_weekly,
                              rsq_trad,
                              mae,
                              rmse,
                              smape)

PERF_METRIC_SET

# --------------------------------------------------------------------
# 2) Read in Data
# --------------------------------------------------------------------

# read in training data (this is already grouped by hour)
train_data <-
  readRDS("data/cleaned_data/split_hourly/train.Rda") %>%
  rename(., cs = total_cs) %>% as_tibble()


train_data %>% head
train_data %>% dim

# verify whether the 1 hour difference is constant (just a check)
diff.Date(train_data$date) %>% as.numeric %>% summary


# --------------------------------------------------------------------
# 3) Preprocessing steps
# --------------------------------------------------------------------

# calandar effect
# "month","dow","doy",
# "week","month","decimal",
# "quarter","semester"

calandar_effects <- c("dow", "doy", "week")

holiday_effects <- c(
  "Easter",
  "EasterMonday",
  "EasterSunday",
  "ChristmasDay",
  "ChristmasEve",
  "NewYearsDay",
  "Advent1st",
  "Advent2nd",
  "Advent3rd",
  "Advent4th",
  "AllSaints",
  "AshWednesday",
  "GoodFriday"
)


# create lags
train_data_lags <-
  train_data %>%
  create_lags(var = "cs",
              lags = seq(from = 24, to = 2 * 7 * 24, by = 12))

# create recipe
recipe_steps <-
  recipe(cs ~ ., data = train_data_lags) %>%
  step_naomit(all_predictors()) %>%
  #step_holiday(date, holidays = holiday_effects) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(hour = lubridate::hour(date)) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_predictors()) %>%
  #step_corr(all_predictors(), threshold = 0.80) %>%
  step_zv(all_predictors())


recipe_steps <-
  recipe(cs ~ ., data = train_data_lags) %>%
  step_naomit(all_predictors()) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(
    hour = lubridate::hour(date),
    cos_hour = cos(2 * pi * hour / 24),
    cos_dow = cos(2 * pi * as.integer(date_dow) / 7)
  ) %>%
  step_rm(date, hour) %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ cos_hour:starts_with("cs_lag")) %>%
  step_interact(~ cos_dow:starts_with("cs_lag")) %>%
  step_interact(~ cos_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_normalize(all_predictors()) %>%
  #step_corr(all_predictors(), threshold = 0.90) %>%
  step_zv(all_predictors())



# prepare training data
train_data <- recipe_steps %>%
  prep(train_data_lags) %>%
  bake(train_data_lags)
  
# --------------------------------------------------------------------
# 5) Modelling: Training data
# --------------------------------------------------------------------



ranger_fit_train <- ranger(
  formula   = cs ~ .,
  data      = train_data,
  num.trees = 250,
  num.threads = 2,
  importance = "permutation",
  min.node.size = 50,
  max.depth = 10,
  quantreg = TRUE,
  case.weights = train_data$cs ** 3,
  regularization.usedepth = FALSE,
  seed = 1
)


# Variable importance plot
ranger_fit_train %>%
  vip(geom = "point",
      aesthetics = list(color = cbPalette[4],
                        fill = cbPalette[4])) +
  THEME +
  ggtitle("Random Forest")


# generate probalistic forecast
pred_train <-
  predict(
    ranger_fit_train,
    (train_data %>% select(-cs)),
    type = "quantiles",
    quantiles = c(0.025, 0.5, 0.975)
  )


prob_train <- pred_train$predictions %>%
  as_tibble() %>%
  rename(lower = `quantile= 0.025`,
         fitted = `quantile= 0.5`,
         upper = `quantile= 0.975`) %>%
  mutate(date = train_data_lags$date,
         actual = train_data$cs) %>%
  select(date, actual, fitted, lower, upper)


# visualize on training data
prob_train %>%
  slice(1:(24 * 30)) %>%
  visualize_pred(
    df = .,
    interactive = FALSE,
    theme_style = THEME,
    static_height = c(3, 2, 2),
    legend_justification = c("right", "top"),
    legend_position = c(.95, .95),
    legend_text_size = 9,
    legend_direction = "vertical",
    probablistic = TRUE,
    lower = "lower",
    upper = "upper"
  )

# --------------------------------------------------------------------
# 6) Validation Data
# --------------------------------------------------------------------

# load validation data
validation_data <-
  readRDS("data/cleaned_data/split_hourly/validation.Rda") %>%
  rename(., cs = total_cs) %>% as_tibble()


# create lags
validation_data_lags <-
  validation_data %>%
  create_lags(var = "cs",
              lags = seq(from = 24, to = 2 * 7 * 24, by = 12))

# prepare validation data
validation_data <- recipe_steps %>%
  prep(validation_data_lags) %>%
  bake(validation_data_lags)



# --------------------------------------------------------------------
# 6 Point Forecasts
# --------------------------------------------------------------------


predictions_train <-
  predict(ranger_fit_train,
          (train_data %>% select(-cs)))$predictions %>% as_tibble() %>%
  rename(fitted = value) %>%
  mutate(date = train_data_lags$date,
         actual = train_data_lags$cs) %>% select(date, fitted, actual)


predictions_val <-
  predict(object = ranger_fit_train,
          num.trees = ranger_fit_train$num.trees,
          (validation_data %>% select(-cs)))$predictions %>% as_tibble() %>%
  rename(fitted = value) %>%
  mutate(date = validation_data_lags$date,
         actual = validation_data_lags$cs) %>% select(date, fitted, actual)


# Evaluate

# training
pointScore_train <-
  PERF_METRIC_SET(predictions_train, truth = actual, estimate = fitted)
pointScore_train

# validation
pointScore_val <-
  PERF_METRIC_SET(predictions_val, truth = actual, estimate = fitted)
pointScore_val


# --------------------------------------------------------------------
# 7 Generate Probablistic Forecasts
# --------------------------------------------------------------------


# -----------------------------------------------------------------------
# 7.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# -----------------------------------------------------------------------


probScore_val <- matrix(nrow = 5, ncol = 4)
colnames(probScore_val) <-
  c("Nominal Coverage", "Winkler Score", "CRPS", "Pinball Loss")
rownames(probScore_val) <-
  c(
    "Unconditial NormD",
    "Conditial NormD",
    "Unconditial Boot",
    "Conditial Boot",
    "Quantile"
  )


# train
prob_train <-
  prob_forecast_Un(df = predictions_train)

# validation
prob_val <-
  prob_forecast_Un(df = predictions_val, residuals_inSample = prob_train$residual)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

# 1) Nominal coverage
probScore_val[1, 1] <- prob_val$cover_nU %>% mean


# 2) Winkler Score
probScore_val[1, 2] <-
  prob_val %>% select(lower_nU, upper_nU, actual) %>%
  apply(1,
        winkler_score,
        alpha = .05) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val[1, 3] <- crps(
  y = prob_val$actual,
  family = "cnorm",
  lower = 0,
  upper = Inf,
  location = prob_val$fitted,
  scale = sd(prob_train$residual)
) %>%
  mean



# -------------------------------------------------------------------
# 7.2) Normal Dist: compute CONDITIONAL std of the historical residuals
# -------------------------------------------------------------------

# residuals grouped by dow and hour
residuals_grouped <- prob_train %>% group_by(week_day) %>%
  summarise(sd_grouped = sd(residual)) %>%
  add_column(obs = 1:7, .before = 'week_day')

# add the grouped prediction intervals
prob_val <-
  prob_val %>% left_join(y = residuals_grouped, by = c("week_day")) %>%
  mutate(
    lower_nC = fitted - qnorm(1 - 0.025) * sd_grouped,
    upper_nC = fitted + qnorm(1 - 0.025) * sd_grouped,
    cover_nC = actual >= lower_nC & actual <= upper_nC
  )


# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

# 1) Nominal coverage
probScore_val[2, 1] <- prob_val$cover_nC %>% mean


# 2) Winkler Score
probScore_val[2, 2] <-
  prob_val %>% select(lower_nC, upper_nC, actual) %>%
  apply(1,
        winkler_score,
        alpha = .05) %>% as_tibble() %>% pull %>% mean()

# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val[2, 3] <- crps(
  y = prob_val$actual,
  family = "cnorm",
  lower = 0,
  upper = Inf,
  location = prob_val$fitted,
  scale = prob_val$sd_grouped
) %>%
  mean



# -------------------------------------------------------------------
# 7.3) Bootstrapping: UNCONDITIONAL
# -------------------------------------------------------------------

nr_boot <- 5000
prob_val_bootUc <- prob_train$residual %>%
  sample(., size = prob_val %>% nrow * nr_boot, replace = TRUE) %>%
  matrix(., ncol = nr_boot) + prob_val$fitted


# add to dataframe
prob_val <- prob_val %>% mutate(
  lower_bU = prob_val_bootUc %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = 0.025
  ),
  upper_bU = prob_val_bootUc %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = 0.975
  ),
  cover_bU = actual >= lower_bU & actual <= upper_bU
)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

# 1) Nominal coverage
probScore_val[3, 1] <- prob_val$cover_bU %>% mean


# 2) Winkler Score
probScore_val[3, 2] <-
  prob_val %>% select(lower_bU, upper_bU, actual) %>%
  apply(1,
        winkler_score,
        alpha = .05) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score

# bootstrap
probScore_val[3, 3] <-
  crps_sample(y = prob_val$actual, dat = prob_val_bootUc) %>% mean



# -------------------------------------------------------------------
# 7.4) Bootstrapping: CONDITIONAL
# -------------------------------------------------------------------

nr_boot <- 1000
for (i in 1:nrow(prob_val)) {
  if (i == 1) {
    prob_val_bootC <- conditional_bootstap(
      pred = prob_val[i, ],
      historical_resid = prob_train,
      size = nr_boot,
      colNr_fitted = 8,
      colNr_dayType = 2,
      colNr_hour = 3,
      hour_window = 1
    )
  }
  else{
    a <- conditional_bootstap(
      pred = prob_val[i, ],
      historical_resid = prob_train,
      size = nr_boot,
      colNr_fitted = 8,
      colNr_dayType = 2,
      colNr_hour = 3,
      hour_window = 1
    )
    prob_val_bootC <- rbind(prob_val_bootC, a)
  }
}


# add to dataframe
prob_val <- prob_val %>% mutate(
  lower_bC = prob_val_bootC %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = 0.025
  ),
  upper_bC = prob_val_bootC %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = 0.975
  ),
  cover_bC = actual >= lower_bC & actual <= upper_bC
)


# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

# 1) Nominal coverage
probScore_val[4, 1] <- prob_val$cover_bC %>% mean


# 2) Winkler Score
probScore_val[4, 2] <-
  prob_val %>% select(lower_bC, upper_bC, actual) %>%
  apply(1,
        winkler_score,
        alpha = .05) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score
# bootstrap
probScore_val[4, 3] <-
  crps_sample(y = prob_val$actual, dat = prob_val_bootC) %>% mean()


probScore_val


# -----------------------------------------------------------------------
# 7.5) Quantiles
# -----------------------------------------------------------------------

# validation data
prob_val <- predict(
  object = ranger_fit_train,
  num.trees = ranger_fit_train$num.trees,
  data = (validation_data %>% select(-cs)),
  type = "quantiles",
  quantiles = c(0.025, 0.975)
)$predictions %>%
  as_tibble() %>%
  mutate(
    lower_q = `quantile= 0.025`,
    upper_q = `quantile= 0.975`,
    date = validation_data_lags$date,
    actual = validation_data_lags$cs,
    cover_q = actual >= lower_q & actual <= upper_q
  ) %>%
  select(date, lower_q, upper_q, cover_q) %>%
  inner_join(prob_val, by = "date")


# 1) Nominal coverage
probScore_val[5, 1] <- prob_val$cover_q %>% mean



# 2) Winkler Score
probScore_val[5, 2] <-
  prob_val %>% select(lower_q, upper_q, actual) %>%
  apply(1,
        winkler_score,
        alpha = .05) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score: NOT APPLICABLE HERE



# 4) Pinball Loss Function

# generate quantile forecasts
quantiles <- seq(0.01, 0.99, by = 0.01)

q_pred <- predict(
  object = ranger_fit_train,
  num.trees = ranger_fit_train$num.trees,
  data = (validation_data %>% select(-cs)),
  type = "quantiles",
  quantiles = quantiles
)$predictions %>%
  as_tibble()

i <- quantiles * 100
actual <- prob_val$actual
store_scores <- actual

# compute pinball loss function
for (j in 1:nrow(q_pred)) {
  q_j <- q_pred[j, ] %>% as.numeric()
  store_scores[j] <-
    pinball_loss_a(q = q_j, actual = actual[j], i = i)
}

probScore_val[5, 4] <- store_scores %>% mean
probScore_val

# visualize
prob_val %>%
  slice(1:(24 * 10)) %>%
  visualize_pred(
    df = .,
    interactive = FALSE,
    theme_style = THEME,
    static_height = c(3, 2, 2),
    legend_justification = c("right", "top"),
    legend_position = c(.95, .95),
    legend_text_size = 9,
    legend_direction = "vertical",
    probablistic = TRUE,
    lower = "lower_q",
    upper = "upper_q"
  )










