# clean environment
rm(list = ls())

# --------------------------------------------------------------------
# 0) Load libraries
# --------------------------------------------------------------------


# data manipulations
library(lubridate)
library(tsibble)
library(dplyr)

# visualisations
library(ggplot2)
library(patchwork)

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
THEME <- theme_minimal()
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
  rename(., cs = total_cs) %>%
  slice(., 1:10000)


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


mean_train <- train_data  %>% pull %>% mean
std_train <- train_data %>% pull %>% sd


train_data_lags <-
  train_data %>%
  create_lags(var = "cs",
              lags = seq(from = 24, to = 2 * 7 * 24, by = 24))

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
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_predictors()) %>%
  step_zv(all_predictors())


recipe_steps %>%
  prep(train_data_lags) %>%
  bake(train_data_lags) %>% dim




# --------------------------------------------------------------------
# 4) resampling scheme and performance metrics
# --------------------------------------------------------------------


time_resamples <- rolling_origin(
  train_data_lags,
  initial = 24 * 7 * 30,
  # 30 week training
  assess = 24 * 7 * 5,
  # 5 week validation
  skip = 24 * 7 * 5,
  # 5 week skip
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
model <-
  linear_reg(mode    = "regression",
             penalty = tune(),
             mixture = tune()) %>%
  set_engine("glmnet")

# To simplify this process, we can use a model workflow, which pairs a model and recipe together
wkflow  <-
  workflow() %>%
  add_recipe(recipe_steps) %>%
  add_model(model)
wkflow


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
results <- wkflow %>%
  tune_grid(
    resamples = time_resamples,
    grid = hypers_param,
    metrics = perf_metrics,
    control = ctrl
  )


# --------------------------------------------------------------------
# 6) Visualize Performance
# --------------------------------------------------------------------

# have look at the tuning parameters
results %>% autoplot()


# have a look at the performance
results %>%
  collect_metrics() %>%
  filter(.metric == METRIC)


# show performance
show_best(results,
          n = 10,
          metric = METRIC)

# get predictions using the best parameters found
predictions <- collect_predictions(results) %>%
  inner_join(hypers_param, by = c("penalty", "mixture"))


# get predictions and visualisze them against acutals
vis_plot_fitted(pred = predictions, target = "cs", interactive = FALSE)

train_data_lags %>% dim


prep_visualize_pred(df = train_data_lags,
                    pred = predictions) %>%
  # interactive plot
  visualize_pred(df = ., interactive = TRUE)


# view large residuals
see_large_resid(glmnnet_pred, list_df$train, max_n = 5)


see_large_resid(pred = p_perf_analysis_lm, train_data, max_n = 5)
