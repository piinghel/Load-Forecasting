# clean environment
rm(list = ls())


# TODO:

# - visualize performance on folds: DONE
# - feature importance: DONE
# - predictions on validation: DONE
# - improve recipe: DONE
# - probablistic forecasts: DONE
# - plot probablistic forecasts
# - predictions on test ==> this is only for the final model
# - fix variable importance plot
# - Forecasting metrics:
#     - Point estimates: MAE, MSE, MAPE, MASE, RSQUARED: DONE
#     - Probablistic Forecast: Winkel score, Continuous Ranked Probability Score, Pinball Loss: DONE
# Good paper on metrics:
#   - https://www.groundai.com/project/probabilistic-load-forecasting-via-point-forecast-feature-integration/1
#   - https://towardsdatascience.com/a-short-tutorial-on-fuzzy-time-series-part-iii-69445dff83fb
#   - https://www.lokad.com/continuous-ranked-probability-score
#   - https://datascience.stackexchange.com/questions/63919/what-is-continuous-ranked-probability-score-crps
#   - https://cran.r-project.org/web/packages/scoringRules/vignettes/gettingstarted.html
#   - https://arxiv.org/pdf/1709.04743.pdf
#   - https://robjhyndman.com/papers/forecasting_state_of_the_art.pdf


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
library(glmnet)

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
  rename(., cs = total_cs)


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

calandar_effects <- c("dow", "doy", "decimal", "week")

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


# save mean and std of training data
mean_train <- train_data  %>% pull %>% mean
std_train <- train_data %>% pull %>% sd

# create lags
train_data_lags <-
  train_data %>%
  create_lags(var = "cs",
              lags = seq(from = 24, to = 1 * 7 * 24, by = 24))

recipe_steps <-
  recipe(cs ~ ., data = train_data_lags) %>%
  step_naomit(all_predictors()) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(
    hour = lubridate::hour(date),
    sin_hour  = sin(2 * pi * hour / 24),
    cos_hour = cos(2 * pi * hour / 24),
    sin_dow  = sin(2 * pi * as.integer(date_dow) / 7),
    cos_dow = cos(2 * pi * as.integer(date_dow) / 7)
  ) %>%
  step_rm(date, hour) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ sin_hour:starts_with("cs_lag")) %>%
  step_interact( ~ cos_hour:starts_with("cs_lag")) %>%
  step_interact( ~ sin_dow:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact( ~ cos_dow:starts_with("cs_lag")) %>%
  step_interact( ~ sin_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact( ~ cos_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_normalize(all_predictors()) %>%
  #step_corr(all_predictors(), threshold = 0.90) %>%
  step_zv(all_predictors())


recipe_steps %>%
  prep(train_data_lags) %>%
  bake(train_data_lags) %>%
  dim




# --------------------------------------------------------------------
# 4) resampling scheme and performance metrics
# --------------------------------------------------------------------


time_resamples <- rolling_origin(
  train_data_lags,
  initial = 24 * 7 * 30,
  # 30 week training
  assess = 24 * 7 * 10,
  # 10 week validation
  skip = 24 * 7 * 10,
  # 10 week skip
  cumulative = FALSE
)


time_resamples


# performance metrics
perf_metrics <-
  metric_set(rsq_trad,
             mae,
             rmse)

# --------------------------------------------------------------------
# 5) Modelling
# --------------------------------------------------------------------

# specify model
model_train <-
  linear_reg(mode    = "regression",
             penalty = tune(),
             mixture = tune()) %>%
  set_engine("glmnet")

# To simplify this process, we can use a model workflow, which pairs a model and recipe together
wkflow_train  <-
  workflow() %>%
  add_recipe(recipe_steps) %>%
  add_model(model_train)
wkflow_train


# Save the assessment set predictions
ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

# set grid for parameters to look at
hypers_param <- parameters(penalty(), mixture()) %>%
  grid_max_entropy(size = 30)

# visualize the parameters of glmnet
ggplot(hypers_param, aes(x = penalty, y = mixture)) +
  geom_point() +
  scale_x_log10()


# grid search
results_train <- wkflow_train %>%
  tune_grid(
    resamples = time_resamples,
    grid = hypers_param,
    metrics = PERF_METRIC_SET,
    control = ctrl
  )


# --------------------------------------------------------------------
# 6) Visualize Performance on validations folds
# --------------------------------------------------------------------

# have look at the tuning parameters
results_train %>% autoplot()


# have a look at the performance
results_train %>%
  collect_metrics() %>%
  filter(.metric == METRIC)


# show performance
show_best(results_train,
          n = 10,
          metric = METRIC)

# select best tuning parameters
best_params <-
  select_best(results_train,
              metric = METRIC)

# get predictions using the best parameters found
predictions_folds <- collect_predictions(results_train) %>%
  inner_join(best_params, by = c("penalty", "mixture"))


# get predictions and visualisze them against acutals
vis_plot_fitted(pred = predictions_folds,
                target = "cs",
                interactive = FALSE)

prep_visualize_pred(
  df = train_data_lags,
  target = "cs",
  pred = predictions_folds,
  fold = "Slice2"
) %>%
  slice(1:(24*30)) %>%
  # interactive plot
  visualize_pred(
    df = .,
    interactive = FALSE,
    theme_style = THEME,
    static_height = c(3, 2, 2),
    legend_justification = c("right", "top"),
    legend_position = c(.95, .95),
    legend_text_size = 9,
    legend_direction = "vertical"
  )



# --------------------------------------------------------------------
# 6) Fit on training data and check performance on validation data
# --------------------------------------------------------------------


# Automatically extract best parameters and fit to the training data
final_wkflow_train <- finalize_workflow(wkflow_train, best_params)

# train model on trainng data using best parameters
final_fit_train <-
  final_wkflow_train %>% fit(data = train_data_lags)


# variable importance
final_fit_train %>%
  pull_workflow_fit() %>%
  vip(geom = "point",
      aesthetics = list(color = cbPalette[4],
                        fill = cbPalette[4])) +
  THEME +
  ggtitle("Elastic Net")


# load validation data
validation_data <-
  readRDS("data/cleaned_data/split_hourly/validation.Rda") %>%
  rename(., cs = total_cs)


# create lags
validation_data_lags <-
  validation_data %>%
  create_lags(var = "cs",
              lags = seq(from = 24, to = 1 * 7 * 24, by = 24))

# --------------------------------------------------------------------
# 6 Point Forecasts
# --------------------------------------------------------------------

predictions_train <-
  predict(final_fit_train, train_data_lags) %>%
  mutate(.actual = train_data_lags$cs, date = train_data_lags$date) %>%
  select(date, .pred, .actual)

predictions_val <-
  predict(final_fit_train, validation_data_lags) %>%
  mutate(.actual = validation_data_lags$cs, date = validation_data_lags$date) %>%
  select(date, .pred, .actual)


# Evaluate

# training
pointScore_train <-
  PERF_METRIC_SET(predictions_train, truth = .actual, estimate = .pred)
pointScore_train

# validation
pointScore_val <-
  PERF_METRIC_SET(predictions_val, truth = .actual, estimate = .pred)
pointScore_val


# --------------------------------------------------------------------
# 7 Generate Probablistic Forecasts
# --------------------------------------------------------------------

prep_visualize_pred(df = prob_val, pred = )

prob_val
# ------------------------------------------------------------------
# 7.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------

# train
prob_train <-
  prob_forecast_Un(pred = predictions_train, actuals = train_data_lags)

# validation
prob_val <-
  prob_forecast_Un(
    pred = predictions_val,
    actuals = validation_data_lags,
    residuals_inSample = prob_train$residual
  )

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

probScore_val <- matrix(nrow = 4, ncol = 3)
colnames(probScore_val) <-
  c("Nominal Coverage", "Winkler Score", "CRPS")
rownames(probScore_val) <-
  c("Unconditial NormD",
    "Conditial NormD",
    "Unconditial Boot",
    "Conditial Boot")

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


for (i in 1:nrow(prob_val)) {
  if (i == 1) {
    prob_val_bootC <- conditional_bootstap(
      pred = prob_val[i,],
      historical_resid = prob_train,
      size = 5000,
      colNr_fitted = 8,
      colNr_dayType = 2,
      colNr_hour = 3,
      hour_window = 1
    )
  }
  else{
    a <- conditional_bootstap(
      pred = prob_val[i,],
      historical_resid = prob_train,
      size = 5000,
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


# visualize
prob_val %>%
  slice(1:(24*30)) %>%
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
    lower = "lower_nU",
    upper = "upper_nU"
  )




