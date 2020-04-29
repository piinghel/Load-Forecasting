# clean environment
rm(list = ls())


# TODO:

# - visualize performance on folds
# - feature importance
# - predictions on validation
# - improve recipe
# - probablistic forecasts
# - predictions on test

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

# own functions
source("analysis/functions/create_features.R")
source("analysis/functions/error_analysis.R")
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

# --------------------------------------------------------------------
# 2) Read in Data
# --------------------------------------------------------------------

# read in training data (this is already grouped by hour)
train_data <- readRDS("data/cleaned_data/split_hourly/train.Rda") %>%
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
  step_interact(~ sin_hour:starts_with("cs_lag")) %>%
  step_interact(~ cos_hour:starts_with("cs_lag")) %>%
  step_interact(~ sin_dow:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact(~ cos_dow:starts_with("cs_lag")) %>%
  step_interact(~ sin_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact(~ cos_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
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
    metrics = perf_metrics,
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
vis_plot_fitted(pred = predictions_folds, target = "cs", interactive = FALSE)

prep_visualize_pred(
  df = train_data_lags,
  target = "cs",
  pred = predictions_folds,
  fold = "Slice2"
) %>%
  # interactive plot
  visualize_pred(
    df = .,
    interactive = FALSE,
    theme_style = THEME,
    static_height = c(3, 2, 2),
    legend_justification = c("right", "top"),
    legend_position = c(.95,.95),
    legend_text_size = 9,
    legend_direction = "vertical"
  )






# --------------------------------------------------------------------
# 6) Fit on training data and check performance on validation data
# --------------------------------------------------------------------


# Automatically extract best parameters and fit to the training data
final_wkflow_train <- finalize_workflow(wkflow_train, best_params)

# train model trainng data using best parameters
final_fit_train <- final_wkflow_train %>% fit(data = train_data_lags)


# variable importance
final_fit_train %>%
  pull_workflow_fit() %>%
  vip(geom = "point",
      aesthetics = list(color = cbPalette[4],
                        fill = cbPalette[4])) +
  THEME +
  ggtitle("Elastic Net")


# load validation data
validation_data <- readRDS("data/cleaned_data/split_hourly/validation.Rda") %>%
  rename(., cs = total_cs) 


# create lags
validation_data_lags <-
  validation_data %>%
  create_lags(var = "cs",
              lags = seq(from = 24, to = 1 * 7 * 24, by = 24))


predictions_train <- predict(final_fit_train, train_data_lags)
predictions_validation <- predict(final_fit_train, validation_data_lags)

df_predictions_train <- tibble(date = train_data_lags$date,
                               fitted =  predictions_train$.pred,
                               actual = train_data_lags$cs,
                               residual = actual -  fitted,
                               lower = fitted - 1.96 * sd(residual),
                               higher = fitted + 1.96 * sd(residual),
                               check_interval = actual >= lower & actual <= higher)

df_predictions_train$check_interval %>% mean


df_predictions_validation <- tibble(date = validation_data_lags$date,
                                    fitted =  predictions_validation$.pred,
                                    actual = validation_data_lags$cs)

rsq_trad(df_predictions_validation, truth = actual, estimate = fitted)


# visualize performance on validation data
df_predictions_validation %>%
  slice(1:(24 * 7 * 4)) %>%
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


