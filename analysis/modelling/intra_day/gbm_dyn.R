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
library(gbm)

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
  step_interact(~ cs_lag024:cs_lag168) %>%
  step_corr(all_predictors(), threshold = 0.95) %>%
  step_zv(all_predictors())


# fit preprocessing steps
rec_trained <- prep(recipe_steps, training = train_data_lags)
# prepare training data
train_data  <- bake(rec_trained, new_data = train_data_lags)


# save preprocessing steps
saveRDS(rec_trained,file = "output/dynamic/boosting/preprocessing_gbm.rda")

# --------------------------------------------------------------------
# 5) Modelling: Training data
# --------------------------------------------------------------------


# train GBM model center
gbm.fit_train <- gbm(
  formula = cs ~ .,
  distribution = list(name = "quantile", alpha = .5),
  data = train_data,
  n.trees = 550,
  interaction.depth = 5,
  shrinkage = 0.05,
  cv.folds = 0,
  n.cores = NULL,
  # will use all cores by default
  verbose = FALSE
)

# save preprocessing steps
saveRDS(gbm.fit_train,file = "output/dynamic/boosting/model_gbm_center.rda")


# train GBM model lower
gbm.fit_train_lower <- gbm(
  formula = cs ~ .,
  distribution = list(name = "quantile", alpha = .025),
  data = train_data,
  n.trees = 550,
  interaction.depth = 5,
  shrinkage = 0.05,
  cv.folds = 0,
  n.cores = NULL,
  # will use all cores by default
  verbose = FALSE
)

# save model lower
saveRDS(gbm.fit_train_lower,file = "output/dynamic/boosting/model_gbm_lower.rda")


# train GBM model
gbm.fit_train_upper <- gbm(
  formula = cs ~ .,
  distribution = list(name = "quantile", alpha = .975),
  data = train_data,
  n.trees = 550,
  interaction.depth = 5,
  shrinkage = 0.05,
  cv.folds = 0,
  n.cores = NULL,
  # will use all cores by default
  verbose = FALSE
)

# save model upper
saveRDS(gbm.fit_train_upper,file = "output/dynamic/boosting/model_gbm_upper.rda")


tibble(
  variable = train_data %>% select(-cs) %>% colnames,
  importance =  gbm.fit_train$variable.importance
) %>%
  arrange(desc(importance)) %>% slice(1:10) %>%
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_point() + coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("VIP Boosting")


# Variable importance plot
gbm.fit_train %>%
  vip(geom = "point",
      aesthetics = list(color = cbPalette[4],
                        fill = cbPalette[4])) +
  THEME +
  ggtitle("Boosting")


# generate probalistic forecast
predictions_train <-
  tibble(
    date = train_data_lags$date,
    fitted = predict(gbm.fit_train,
                     (train_data %>% select(-cs))),
    actual = train_data$cs
  ) 


# visualize on training data
predictions_train %>%
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
    probablistic = FALSE
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


# generate probalistic forecast
predictions_val <-
  tibble(
    date = validation_data_lags$date,
    fitted = predict(gbm.fit_train,
                     (validation_data %>% select(-cs))),
    actual = validation_data$cs
  ) 

# save for comparison
predictions_val %>% mutate(model = "GBM") %>%
  write.csv("output/dynamic/GBM.csv", row.names = FALSE)

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
  pred <- predict(object, newdata)
  return(pred)
}


explainer_val <-
  DALEX::explain(
    gbm.fit_train,
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
     title = "GBM",
     subtitle = "")
ggsave(
  paste0("output/figures/appendix/figure1c.", DEVICE),
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

make_prediction <- function(data, model = gbm.fit_train) {
  new_pred <- predict(
    model,
    data %>% select(-cs),
    n.trees = model$n.trees
  )
  return (new_pred %>% as.numeric)
}

# feedback columns
feedback_cols <-
  c("cs_lag001")

make_prediction(model = gbm.fit_train, data = validation_data %>% slice(1:100))


# --------------------------------------------------------------------
# 7.1 TRAINING
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# 7.2 VALIDATION
# --------------------------------------------------------------------

# start at 00:00:00
data <- validation_data_lags %>% slice(2:nrow(validation_data_lags))
# data <- train_data_lags 

bake(rec_trained,data) %>% make_prediction(.,model = gbm.fit_train)

tic()
intra_val <- compare_forecast_h (
  data = data,
  forecast_h = c(1:24),
  model = gbm.fit_train,
  make_prediction = make_prediction,
  preprocess_steps = rec_trained,
  show_progress1 = TRUE,
  show_progress2 = FALSE,
  feedback_cols = feedback_cols,
  save_out = TRUE,
  model_name  = "GBM",
  file = "output/dynamic/forecast_horizon/GBM.csv"
)
toc()


# performance metrics
PERF_METRIC_SET_FH <- metric_set(rsq,
                                 mae,
                                 mape,
                                 mase)


#intra_val <- store_results %>% do.call("rbind", .)
intra_val <-
  read.csv("output/dynamic/forecast_horizon/GBM.csv") %>%
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







