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
trainVal_data <-
  readRDS("data/cleaned_data/split_hourly/train_validation.Rda") %>%
  dplyr::rename(., cs = total_cs) %>% as_tibble() 



trainVal_data %>% head
trainVal_data %>% dim

# verify whether the 1 hour difference is constant (just a check)
diff.Date(trainVal_data$date) %>% as.numeric %>% summary


# --------------------------------------------------------------------
# 3) Preprocessing steps
# --------------------------------------------------------------------

# calandar effect
# "month","dow","doy",
# "week","month","decimal",
# "quarter","semester"

calandar_effects <- c("dow", "week","decimal")


holiday_effects <- prophet::generated_holidays %>% 
  filter(country == "BE" & year %in% c(2017,2018,2019,2020)) %>%
  select(ds) %>% pull %>% as_date()


lags <- c(1, 2,seq(from = 24, to = 2 * 7 * 24, by = 24))
# create lags
trainVal_data_lags <-
  trainVal_data %>%
  create_lags(var = "cs",
              lags = lags)

recipe_steps <-
  recipe(cs ~ ., data = trainVal_data_lags) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(
    holiday = as.integer(as_date(date) %in% holiday_effects),
    hour = lubridate::hour(date),
    sin_hour  = sin(2 * pi * hour / 23),
    cos_hour = cos(2 * pi * hour / 23)
  ) %>%
  step_rm(date, hour) %>%
  step_dummy(all_nominal())  %>%
  step_interact( ~ sin_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact( ~ cos_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact( ~ starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_normalize(all_predictors(), -date_decimal)



# fit preprocessing steps
rec_trained <- prep(recipe_steps, training = trainVal_data_lags)
# prepare training data
trainVal_prep  <- bake(rec_trained, new_data = trainVal_data_lags)

trainVal_prep %>% dim

# --------------------------------------------------------------------
# 4) Train final model
# --------------------------------------------------------------------

# train model center
model_trainVal <-
  mlp(
    hidden_units = 119,
    epochs = 81,
    penalty = 1.55e-4,
    activation = "relu"
  ) %>%
  set_mode("regression") %>%
  set_engine("keras") %>%
  fit(cs ~ ., data = trainVal_prep)


# load test data
test_data <-
  readRDS("data/cleaned_data/split_hourly/test.Rda") %>%
  dplyr::rename(., cs = total_cs) %>% as_tibble() %>% filter(cs < 300)

test_data %>% head
test_data %>% dim
test_data %>% summary()


# create lags
test_data_lags <-
  test_data %>%
  create_lags(var = "cs",
              lags = lags)

test_prep <- recipe_steps %>%
  prep(test_data_lags) %>%
  bake(test_data_lags)


# define predict function
make_prediction <- function(data, model = model) {
  predict(model, data) %>% pull
}

# feedback columns
feedback_cols <-
  c("cs_lag001",
    "cs_lag002")


# forecast center quantile
fh_quantile_center <- chunk_forecast_horizon(
  data = test_data_lags,
  model = model_trainVal,
  preprocess_steps = rec_trained,
  func_make_pred = make_prediction,
  forecast_horizon = 6,
  feedback_cols = feedback_cols,
  add_quantiles = FALSE,
  show_progress = TRUE
)

pred_test <-
  fh_quantile_center$center_estimates %>% do.call(rbind, .)

# --------------------------------------------------------------------
# 5 Point Forecasts evaluate
# --------------------------------------------------------------------

# evaluations metrics for visuals
PERF_METRIC_SET_FH <- metric_set(rsq,
                                 mae,
                                 smape,
                                 mase)



pointScore_test <-
  PERF_METRIC_SET(pred_test, truth = actual, estimate = fitted)
pointScore_test

# save performance
write.csv(pointScore_test,"output/dynamic/mlp/perfPoint_test.csv")


# customs stats on residuals
pred_test %>%
  mutate(residuals = fitted - actual) %>%
  select(residuals) %>% pull %>% summary_stats



pred_test %>%
  slice(1:(24 * 30)) %>%
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
# 6 Probablistic Forecasts 
# --------------------------------------------------------------------


# some global variables (CAPITAL LETTER)
QUANTILES <- seq(0.01, 0.99, by = 0.01)
I_Q <- QUANTILES * 100
ACTUAL_TEST <- pred_test$actual
RESID_TEST <-
  pred_test %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull
N_SIM <- 10000
NR_BOOT <- 5000
PI_LOWER <- 0.025
PI_UPPER <- 0.975
ALPHA <- 0.05
# ------------------------------------------------------------------
# 7.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------


# validation
prob_test <-
  prob_forecast_Un(
    df = pred_test,
    #residuals_inSample = RESID_TRAIN
    residuals_inSample = RESID_TEST,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
  ) %>% mutate(
    step_ahead = pred_test$step_ahead
  )

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

probScore_test <- matrix(nrow = 2, ncol = 4)
colnames(probScore_test) <-
  c("Nominal Coverage", "Winkler Score", "CRPS", "Pinball Loss")
rownames(probScore_test) <-
  c("Unconditial NormD",
    "Conditial NormD")

# 1) Nominal coverage
probScore_test[1, 1] <- prob_test$cover_nU %>% mean


# 2) Winkler Score
probScore_test[1, 2] <-
  prob_test %>%
  select(lower_nU, upper_nU, actual) %>%
  apply(1,
        winkler_score,
        start  = 1,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_test[1, 3] <- scoringRules::crps(
  y = prob_test$actual,
  family = "cnorm",
  lower = 0,
  upper = Inf,
  location = prob_test$fitted,
  #scale = sd(prob_train$residual)
  scale = sd(RESID_TEST)
) %>%
  mean

# 4) Pinball loss function

# store values
store_pinball_loss_nU <-
  matrix(NA, nrow = length(ACTUAL_TEST), ncol = length(QUANTILES))

# compute pinball loss function
for (j in 1:nrow(prob_test)) {
  q_j <-
    (prob_test$fitted[j] + rnorm(N_SIM, mean = RESID_TEST %>% mean, sd = RESID_TEST %>% sd)) %>%
    quantile(., probs = QUANTILES) %>% as.numeric()
  store_pinball_loss_nU[j,] <-
    pinball_loss(q = q_j, actual = ACTUAL_TEST[j], i = I_Q)
}

probScore_test[1, 4] <- store_pinball_loss_nU %>% colMeans() %>% mean
probScore_test

# -------------------------------------------------------------------
# 7.2) Normal Dist: compute CONDITIONAL std of the historical residuals
# -------------------------------------------------------------------


# residuals grouped by dow and hour
residuals_grouped <- prob_test %>% group_by(step_ahead) %>%
  dplyr::summarise(mean_grouped = mean(residual),
            sd_grouped = sd(residual)) %>%
  add_column(obs = 1:6, .before = 'step_ahead')

# add the grouped prediction intervals
prob_test <-
  prob_test %>% left_join(y = residuals_grouped, by = c("step_ahead")) %>%
  mutate(lower_nC = 1,
         upper_nC = 1,
         cover_nC = 1)

# add grouped residuals
for (i in 1:nrow(prob_test)) {
  resid_sim <-
    rnorm(N_SIM,
          mean = prob_test$mean_grouped[i],
          sd = prob_test$sd_grouped[i])
  prob_test$lower_nC[i] <-
    (prob_test$fitted[i] + resid_sim) %>% quantile(PI_LOWER)
  prob_test$upper_nC[i] <-
    (prob_test$fitted[i] + resid_sim) %>% quantile(PI_UPPER)
  prob_test$cover_nC[i] <-
    ACTUAL_TEST[i] >= prob_test$lower_nC[i] &
    ACTUAL_TEST[i] <= prob_test$upper_nC[i]
}


# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

# 1) Nominal coverage
probScore_test[2, 1] <- prob_test$cover_nC %>% mean


# 2) Winkler Score
probScore_test[2, 2] <-
  prob_test %>% select(lower_nC, upper_nC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_test[2, 3] <- scoringRules::crps(
  y = prob_test$actual,
  family = "cnorm",
  lower = 0,
  upper = Inf,
  location = prob_test$fitted,
  scale = prob_test$sd_grouped
) %>%
  mean


# 4) Pinball loss function

# store values
store_pinball_loss_nC <-
  matrix(NA, nrow = length(ACTUAL_TEST), ncol = length(QUANTILES))

# compute pinball loss function
for (j in 1:nrow(prob_test)) {
  q_j <-
    (
      prob_test$fitted[j] + rnorm(
        N_SIM,
        mean = prob_test$mean_grouped[j],
        sd = prob_test$sd_grouped[j]
      )
    ) %>%
    quantile(., probs = QUANTILES)
  store_pinball_loss_nC[j,] <-
    pinball_loss(q = q_j, actual = ACTUAL_TEST[j], i = I_Q)
}



probScore_test[2, 4] <- store_pinball_loss_nC %>% colMeans() %>% mean
probScore_test

# save predictions
write.csv(prob_test,"output/dynamic/mlp/predProb_test.csv")
# save performance
write.csv(probScore_test,"output/dynamic/mlp/perfProb_test.csv")



prob_test %>%
  slice(1:(24 * 20)) %>%
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
    lower = "lower_nC",
    upper = "upper_nC"
  )













