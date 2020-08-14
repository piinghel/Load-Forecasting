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
library(prophet)

# visualisations
library(ggplot2)
library(plotly)
library(patchwork)
library(vip) # variable importance
library(DALEX) # variable importance

# modelling
library(tidymodels)
library(ranger)

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
set.seed(100)

# Parallel Processing
# parallel::detectCores(logical = TRUE)
# cl <- makeCluster(2)

# metric of interest
METRIC <- "mae"

# to save figures
DPI <- 500
DEVICE <- "pdf"

# performance metrics
PERF_METRIC_SET <- metric_set(mase_daily,
                              mase_weekly,
                              rsq,
                              mae,
                              rmse,
                              mape)




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
diff.Date(train_data$date) %>% as.numeric %>% summary


# --------------------------------------------------------------------
# 3) Preprocessing steps
# --------------------------------------------------------------------

# calandar effect
# "month","dow","doy",
# "week","month","decimal",
# "quarter","semester"

calandar_effects <- c("dow", "week")


holiday_effects <- prophet::generated_holidays %>% 
  filter(country == "BE" & year %in% c(2017,2018,2019,2020)) %>%
  select(ds) %>% pull %>% as_date()


lags <- c(1,24,168)
# create lags
train_data_lags <-
  train_data %>%
  create_lags(var = "cs",
              lags = lags)

recipe_steps <-
  recipe(cs ~ ., data = train_data_lags) %>%
  step_naomit(all_predictors()) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(
    hour = lubridate::hour(date),
    holiday = as.integer(as_date(date) %in% holiday_effects)
  ) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("hour"):starts_with("cs_lag")) %>%
  step_interact(~ starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact(~ cs_lag001:cs_lag024) %>%
  step_interact(~ cs_lag001:cs_lag168) %>%
  step_interact(~ cs_lag024:cs_lag168) 

# fit preprocessing steps
rec_trained <- prep(recipe_steps, training = train_data_lags)
# prepare training data
train_data  <- bake(rec_trained, new_data = train_data_lags)


# save preprocessing steps
saveRDS(rec_trained,file = "output/dynamic/random forest/preprocessing_rf.rda")


# --------------------------------------------------------------------
# 5) Modelling: Training data
# --------------------------------------------------------------------


weights <- train_data$cs

ranger_fit_train <- ranger(
  formula   = cs ~ .,
  data      = train_data,
  num.trees = 350,
  num.threads = 3,
  importance = "permutation",
  min.node.size = 30,
  #max.depth = 14,
  quantreg = TRUE,
  case.weights = weights**2,
  regularization.usedepth = FALSE,
  seed = 1
)

# save preprocessing steps
saveRDS(ranger_fit_train,file = "output/dynamic/random forest/model_rf.rda")


tibble(
  variable = train_data %>% select(-cs) %>% colnames,
  importance =  ranger_fit_train$variable.importance
) %>%
  arrange(desc(importance)) %>% slice(1:10) %>%
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_point() + coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("VIP RF")


# Variable importance plot
ranger_fit_train %>%
  vip(geom = "point",
      aesthetics = list(color = cbPalette[4],
                        fill = cbPalette[4])) +
  THEME +
  ggtitle("RF")


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
  dplyr::rename(lower = `quantile= 0.025`,
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
  dplyr::rename(., cs = total_cs) %>% as_tibble()


# create lags
validation_data_lags <-
  validation_data %>%
  create_lags(var = "cs",
              lags = lags)

# prepare validation data
validation_data <- bake(rec_trained, new_data = validation_data_lags)


predictions_train <-
  predict(ranger_fit_train,
          (train_data %>% select(-cs)))$predictions %>% as_tibble() %>%
  dplyr::rename(fitted = value) %>%
  mutate(date = train_data_lags$date,
         actual = train_data_lags$cs) %>% select(date, fitted, actual)


predictions_val <-
  predict(object = ranger_fit_train,
          num.trees = ranger_fit_train$num.trees,
          (validation_data %>% select(-cs)))$predictions %>% as_tibble() %>%
  dplyr::rename(fitted = value) %>%
  mutate(date = validation_data_lags$date,
         actual = validation_data_lags$cs) %>% select(date, fitted, actual)


# save for comparison
predictions_val %>% mutate(model = "random forest") %>%
  write.csv("output/dynamic/random_forest.csv", row.names = FALSE)

# Evaluate

# training
pointScore_train <-
  PERF_METRIC_SET(predictions_train, truth = actual, estimate = fitted)
pointScore_train


# validation
pointScore_val <-
  PERF_METRIC_SET(predictions_val, truth = actual, estimate = fitted)
pointScore_val




# variable importance

# for the dalex package
custom_predict <- function(object,
                           newdata) {
  pred <- predict(object, newdata)$predictions
  return(pred)
}


explainer_val <-
  DALEX::explain(
    ranger_fit_train,
    data = validation_data %>% select(-cs),
    y = validation_data$cs,
    predict_function = custom_predict,
    colorize = TRUE,
    label = ""
  )

set.seed(69)
vi_val <- model_parts(
  explainer = explainer_val,
  loss_function = loss_sum_of_squares,
  B = 50,
  type = "difference"
)


# plot and save
plot(vi_val,
     max_vars = 10,
     title = "RF",
     subtitle = "")

ggsave(
  paste0("output/figures/appendix/figure1b.", DEVICE),
  width = 17,
  height = 11,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)


# --------------------------------------------------------------------
# 7) POINT FORECASTS INTRA DAY FORECASTING
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# 7) INTRA DAY FORECASTING
# --------------------------------------------------------------------

forecast_horizon <- 24

make_prediction <- function(data, model = ranger_fit_train) {
  pred <- predict(
    object = model,
    num.trees = model$num.trees,
    data = data,
    type = "quantiles",
    quantiles = c(0.5)
  )$predictions 
  return (pred %>% as.numeric)
}

# feedback columns
feedback_cols <-
  c("cs_lag001")

make_prediction(data = validation_data %>% slice(1:100), model = ranger_fit_train)


# --------------------------------------------------------------------
# 7.1 TRAINING
# --------------------------------------------------------------------

# tic()
# ls_train <-
#   chunk_forecast_horizon(
#     data = train_data
#     model = ranger_fit_train,
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
data <- validation_data_lags %>% slice(2:nrow(validation_data_lags)) 


bake(rec_trained,data) %>% slice(1:100) %>% 
  make_prediction(.,model = ranger_fit_train)

tic()
intra_val <- compare_forecast_h (
  data = data,
  forecast_h = c(1:24),
  model = ranger_fit_train,
  make_prediction = make_prediction,
  preprocess_steps = rec_trained,
  show_progress1 = TRUE,
  show_progress2 = FALSE,
  feedback_cols = feedback_cols,
  save_out = TRUE,
  model_name  = "random forest",
  file = "output/dynamic/forecast_horizon/randomForest.csv"
)
toc()


# performance metrics
PERF_METRIC_SET_FH <- metric_set(rsq,
                                 mae,
                                 mape,
                                 mase)


#intra_val <- store_results %>% do.call("rbind", .)
intra_val <-
  read.csv("output/dynamic/forecast_horizon/randomForest.csv") %>%
  as_tibble() 

intra_val %>% group_by(forecast_horizon, model) %>%
  PERF_METRIC_SET_FH(.,
                     truth = actual,
                     estimate = fitted,
                     m = 24) %>%
  ggplot(aes(x = forecast_horizon, y = .estimate, color = model)) +
  geom_point() + geom_line() + scale_x_continuous(breaks = c(seq(1,24,5),24)) +
  facet_wrap(vars(.metric), scales = "free") +
  labs(y = "Point estimate", x = "Forecast Horizon (update horizon)")


# -----------------------------------------------------------------------
# 8 Generate Probablistic Forecasts
# -----------------------------------------------------------------------






