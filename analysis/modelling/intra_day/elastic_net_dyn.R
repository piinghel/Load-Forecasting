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
library(prophet)
library(readr)

# visualisations
library(ggplot2)
library(plotly)
library(patchwork)
library(vip) # variable importance
library(DALEX) # variable importance

# modelling
library(tidymodels)
library(timetk)
library(glmnet)

# evaluate models
library(scoringRules) # probablistic scores

# other
library(progress)
library(tictoc)

# own functions
source("analysis/functions/create_features.R")
source("analysis/functions/error_analysis.R")
source("analysis/functions/save_output.R")
source("analysis/functions/evaluation_metrics.R")
source("analysis/functions/prob_forecasts.R")
source("analysis/functions/pred_intra_day.R")
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
set.seed(69)

# Parallel Processing
# parallel::detectCores(logical = TRUE)
# cl <- makeCluster(2)

# metric of interest
METRIC <- "mae"

DPI <- 500
DEVICE <- "pdf"

# performance metrics
PERF_METRIC_SET <- metric_set(mase_daily,
                              mase_weekly,
                              rsq,
                              mae,
                              rmse,
                              mape)

PERF_METRIC_SET



# --------------------------------------------------------------------
# 2) Read in Data
# --------------------------------------------------------------------

# read in training data (this is already grouped by hour)
train_data <-
  readRDS("data/cleaned_data/split_hourly/train.Rda") %>%
  dplyr::rename(., cs = total_cs) %>% as_tibble()


train_data %>% head
train_data %>% dim

# verify whether the 1 hour difference is constant (just a check)
diff.difftime(train_data$date) %>% as.numeric %>% summary


# --------------------------------------------------------------------
# 3) Preprocessing steps
# --------------------------------------------------------------------

# calandar effect
# "month","dow","doy",
# "week","month","decimal",
# "quarter","semester"

calandar_effects <- c("dow", "decimal", "week", "month", "doy")

holiday_effects <- prophet::generated_holidays %>%
  filter(country == "BE" & year %in% c(2017, 2018, 2019, 2020)) %>%
  select(ds) %>% pull %>% as_date()

lags <- c(1, 2, 3, 24, 48, 168)
# create lags
train_data_lags <-
  train_data %>%
  create_lags(var = "cs",
              lags = lags)


recipe_steps <-
  recipe(cs ~ ., data = train_data_lags) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(
    holiday = as.integer(as_date(date) %in% holiday_effects),
    hour = lubridate::hour(date),
    wday = lubridate::wday(date),
    week_hour = (wday - 1) * (24) + hour + 1,
    sin_week_hour = sin(2 * pi * hour / 168),
    cos_week_hour = cos(2 * pi * hour / 168),
    sin_hour  = sin(2 * pi * hour / 23),
    cos_hour = cos(2 * pi * hour / 23)
  ) %>%
  step_rm(date, hour, wday, week_hour) %>%
  step_dummy(all_nominal())  %>%
  step_interact( ~ sin_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact( ~ cos_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_normalize(all_predictors(), -date_decimal) %>%
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
  initial = 24 * 7 * 52,
  # 52 week training
  assess = 24 * 7 * 26,
  # 26 week validation
  skip = 24 * 7 * 13,
  # 13 week skip
  cumulative = FALSE
)


time_resamples

# --------------------------------------------------------------------
# 5) Modelling: Training data
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
  grid_max_entropy(size = 10)

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
  fold = "Slice1"
) %>% slice(1:1000) %>%
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
  dplyr::rename(., cs = total_cs) %>% as_tibble()



# create lags
validation_data_lags <-
  validation_data %>%
  create_lags(var = "cs",
              lags = lags)



predictions_train <-
  predict(final_fit_train, train_data_lags) %>%
  mutate(actual = train_data_lags$cs, date = train_data_lags$date) %>%
  dplyr::rename(fitted = .pred) %>%
  select(date, fitted, actual)

predictions_val <-
  predict(final_fit_train, validation_data_lags) %>%
  mutate(actual = validation_data_lags$cs, date = validation_data_lags$date) %>%
  dplyr::rename(fitted = .pred) %>%
  select(date, fitted, actual)



# Evaluate

# training
pointScore_train <-
  PERF_METRIC_SET(predictions_train, truth = actual, estimate = fitted)
pointScore_train

# validation
pointScore_val <-
  PERF_METRIC_SET(predictions_val, truth = actual, estimate = fitted)
pointScore_val


# point forecast validation
predictions_val %>% slice(1:1000) %>%
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



# Variable importance

# for the dalex package
custom_predict <- function(object,
                           newdata,
                           alpha = best_params$mixture,
                           s = best_params$mixture) {
  pred <-
    predict(object,
            newx = as.matrix(newdata),
            alpha = alpha,
            s = s) %>% as.numeric
  return(pred)
}



# data
train_data <- recipe_steps %>%
  prep(train_data_lags) %>%
  bake(train_data_lags)

# model
fit_train <- pull_workflow_fit(final_fit_train)


explainer_train <-
  DALEX::explain(
    fit_train$fit,
    data = train_data %>% select(-cs),
    y = train_data$cs,
    predict_function = custom_predict,
    colorize = TRUE,
    label = ""
  )


set.seed(69)
vi_train <- model_parts(
  explainer = explainer_train,
  loss_function = loss_sum_of_squares,
  B = 50,
  type = "difference"
)


# plot and save
plot(vi_train,
     max_vars = 10,
     title = "Elastic net",
     subtitle = "")

ggsave(
  paste0("output/figures/appendix/figure1a.", DEVICE),
  width = 17,
  height = 11,
  units = "cm",
  dpi = DPI
)


# --------------------------------------------------------------------
# 7) INTRA DAY FORECASTING
# --------------------------------------------------------------------

forecast_horizon <- 24

# define predict function
make_prediction <- function(data, model = model) {
  stats::predict(model, data) %>% pull
}

# feedback columns
feedback_cols <-
  c("cs_lag001",
    "cs_lag002",
    "cs_lag003")

make_prediction(data = validation_data_lags, model = final_fit_train)

# --------------------------------------------------------------------
# 7.1 TRAINING
# --------------------------------------------------------------------

# tic()
# ls_train <-
#   chunk_forecast_horizon(
#     data = train_data_lags,
#     model = final_fit_train,
#     func_make_pred = make_prediction,
#     forecast_horizon = forecast_horizon,
#     feedback_cols = feedback_cols,
#     add_quantiles = FALSE
#   )
#
# predictions_train <-
#   ls_train$center_estimates %>%
#   do.call("rbind", .)


# --------------------------------------------------------------------
# 7.2 VALIDATION
# --------------------------------------------------------------------

# start at 00:00:00
#data <- validation_data_lags %>% slice(2:nrow(validation_data_lags))
data <- validation_data_lags 

tic()
intra_val <- compare_forecast_h (
  data = data,
  forecast_h = c(1:24),
  model = final_fit_train,
  make_prediction = make_prediction,
  preprocess_steps = NULL,
  show_progress1 = TRUE,
  show_progress2 = FALSE,
  feedback_cols = feedback_cols,
  save_out = TRUE,
  model_name  = "elastic net",
  file = "output/dynamic/forecast_horizon/elasticNet.csv"
)
toc()



# performance metrics
PERF_METRIC_SET_FH <- metric_set(rsq,
                                 mae,
                                 mape,
                                 mase)


#intra_val <- store_results %>% do.call("rbind", .)
intra_val <-
  read.csv("output/dynamic/forecast_horizon/elasticNet.csv") %>%
  as_tibble() 

intra_val %>% group_by(forecast_horizon, model) %>%
  PERF_METRIC_SET_FH(.,
                     truth = actual,
                     estimate = fitted,
                     m = 24) %>%
  ggplot(aes(x = forecast_horizon, y = .estimate, color = model)) +
  geom_point() + geom_line() +
  facet_wrap(vars(.metric), scales = "free") +
  labs(y = "Point estimate", x = "Forecast Horizon (update horizon)")


# -----------------------------------------------------------------------
# 8 Generate Probablistic Forecasts
# -----------------------------------------------------------------------
