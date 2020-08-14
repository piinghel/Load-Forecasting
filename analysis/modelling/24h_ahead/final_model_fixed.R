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
library(timetk)
library(tidyquant)


# visualisations
library(ggplot2)
library(plotly)
library(patchwork)
library(vip) # variable importance
library(DALEX) # variable importance

# modelling
library(tidymodels)
library(glmnet)


# evaluate models
library(scoringRules) # probablistic scores
library(scoringutils)

# other
library(progress)
library(tictoc)



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
set.seed(69)

# Parallel Processing
# parallel::detectCores(logical = TRUE)
# cl <- makeCluster(2)

# metric of interest
METRIC <- "mae"

# to save figures
DPI <- 500
DEVICE <- "pdf"


# performance metrics
PERF_METRIC_SET <- metric_set(mase_weekly,
                              mase_daily,
                              rsq,
                              mae,
                              rmse,
                              mape)

PERF_METRIC_SET


# --------------------------------------------------------------------
# 2) Read in Data
# --------------------------------------------------------------------

# read in training data (this is already grouped by hour)
trainVal_data <-
  readRDS("data/cleaned_data/split_hourly/train_validation.Rda") %>%
  dplyr::rename(., cs = total_cs) %>% as_tibble()


trainVal_data %>% head
trainVal_data %>% dim
trainVal_data %>% summary()

# verify whether the 1 hour difference is constant (just a check)
diff.Date(trainVal_data$date) %>% as.numeric %>% summary


# --------------------------------------------------------------------
# 3) Preprocessing steps
# --------------------------------------------------------------------

calandar_effects <- c("dow", "doy", "decimal", "week")


holiday_effects <- prophet::generated_holidays %>%
  dplyr::filter(country == "BE" &
                  year %in% c(2017, 2018, 2019, 2020)) %>%
  select(ds) %>% pull %>% as_date()


# create lags
lags <- c(seq(from = 24, to = 2 * 7 * 24, by = 24))

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
    month = lubridate::month(date),
    sin_hour  = sin(2 * pi * hour / 23),
    cos_hour = cos(2 * pi * hour / 23),
    sin_month = sin(2 * pi * month / 12),
    cos_month = cos(2 * pi * month/ 12)
  ) %>%
  step_rm(date, hour, month) %>%
  step_dummy(all_nominal()) %>%
  step_interact( ~ sin_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact( ~ cos_hour:starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_interact( ~ starts_with("date_dow"):starts_with("cs_lag")) %>%
  step_slidify(
    cs_lag024,
    period = 41,
    .f = ~ AVERAGE(.x),
    align = "Right",
    names = "moving_average_st"
  ) %>%
  step_slidify(
    cs_lag024,
    period = 168,
    .f = ~ AVERAGE(.x),
    align = "Right",
    names = "moving_average_lt"
  ) %>%
  step_slidify(
    cs_lag024,
    period = 41,
    .f = ~ MIN(.x),
    align = "Right",
    names = "moving_min_st"
  ) %>%
  step_slidify(
    cs_lag024,
    period = 41,
    .f = ~ MAX(.x),
    align = "Right",
    names = "moving_max_st"
  ) %>%
  step_normalize(all_predictors(),-date_decimal) %>%
  #step_corr(all_predictors(), threshold = 0.90) %>%
  step_zv(all_predictors())


trainVal_prep <- recipe_steps %>%
  prep(trainVal_data_lags) %>%
  bake(trainVal_data_lags)

trainVal_prep %>% dim


# --------------------------------------------------------------------
# 4) Train final model
# --------------------------------------------------------------------
set.seed(1)
#specify model
model_trainVal <-
  mlp(
    hidden_units = 195,
    epochs = 76,
    penalty = 4.19e-7,
    activation = "relu"
  ) %>%
  set_mode("regression") %>%
  set_engine("keras") %>%
  fit(cs ~ ., data = trainVal_prep)

#  # elastic net
# model_trainVal <-
#   linear_reg(mode = "regression",
#              penalty = 0.139673503218512,
#              mixture = 0.969143500784412) %>%
#   set_engine("glmnet") %>%
#   fit(cs ~ ., data = trainVal_prep)


# visualize predictions
pred_trainVal <- tibble(
  date = trainVal_data_lags$date,
  actual = trainVal_data_lags$cs,
  fitted = predict(model_trainVal, trainVal_prep) %>% pull
)


pred_trainVal %>%
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

# load test data
test_data <-
  readRDS("data/cleaned_data/split_hourly/test.Rda") %>%
  dplyr::rename(., cs = total_cs) %>% as_tibble() 

test_data %>% head
test_data %>% dim
test_data %>% select(cs) %>% summary()


# create lags
test_data_lags <-
  test_data %>%
  create_lags(var = "cs",
              lags = lags)

test_prep <- recipe_steps %>%
  prep(test_data_lags) %>%
  bake(test_data_lags)

# --------------------------------------------------------------------
# 5 Point Forecasts evaluate
# --------------------------------------------------------------------

# evaluations metrics for visuals
PERF_METRIC_SET_FH <- metric_set(rsq,
                                 mae,
                                 smape,
                                 mase)


# trainVal data
pointScore_trainVal <-
  PERF_METRIC_SET(pred_trainVal, truth = actual, estimate = fitted)
pointScore_trainVal

# customs stats on residuals
pred_trainVal %>%
  mutate(residuals = fitted - actual) %>%
  select(residuals) %>% pull %>% summary_stats



# test data
pred_test <- tibble(
  date = test_data_lags$date,
  actual = test_data_lags$cs,
  fitted = predict(model_trainVal, test_prep) %>% pull
)

pointScore_test <-
  PERF_METRIC_SET(pred_test, truth = actual, estimate = fitted)
pointScore_test


# save performance
write.csv(pointScore_test,"output/deterministic/mlp/perfPoint_test.csv")


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
RESID_TRAIN_VAL <-
  pred_trainVal %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull
N_SIM <- 10000
NR_BOOT <- 5000
PI_LOWER <- 0.025
PI_UPPER <- 0.975
ALPHA <- 0.05
# ------------------------------------------------------------------
# 7.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------


# test
prob_test <-
  prob_forecast_Un(
    df = pred_test,
    #residuals_inSample = RESID_TRAIN
    residuals_inSample = RESID_TEST,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
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
residuals_grouped <- prob_test %>% group_by(working_hour) %>%
  dplyr::summarise(mean_grouped = mean(residual),
            sd_grouped = sd(residual)) %>%
  add_column(obs = 1:2)

# add the grouped prediction intervals
prob_test <-
  prob_test %>% left_join(y = residuals_grouped, by = c("working_hour")) %>%
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



# visualize

prob_test %>%
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
    legend_direction = "vertical",
    probablistic = TRUE,
    lower = "lower_nU",
    upper = "upper_nU"
  )


# save predictions
write.csv(prob_test,"output/deterministic/mlp/predProb_test.csv")
# save performance
write.csv(probScore_test,"output/deterministic/mlp/perfProb_test.csv")



# -------------------------------------------------------------------
# 7.3 DIAGNOSTIC PLOTS
# -------------------------------------------------------------------



# 1) probability integral transform

# to get samples for the normal dist
samples_normal <-
  get_samples_normal(
    fitted = prob_test$fitted,
    mu_resid = prob_test$mean_grouped,
    sd_resid = prob_test$sd_grouped,
    nr_samples = 5000
  )


pit_object <-
  pit(prob_test$actual,
      samples_normal,
      num_bins = 10,
      full_output = TRUE)
nr_obs <- prob_test %>% nrow

p_pit <- pit_plot(pit_object = pit_object, nr_obs = nr_obs)
p_pit

pit_object$u[3]
(samples_normal[3,] < prob_test$actual[3]) %>% mean


# 2) reliability diagram

# 2.1 Normal Distribution


rel_nd <-
  prep_rel_diagram_und(
    fitted = prob_test$fitted,
    quantiles = QUANTILES,
    mu_resid = prob_test$mean_grouped,
    sd_resid = prob_test$sd_grouped,
    nr_sim = 10000
  )


# overall
p_reliab <- reliability_diagram(q_hat = rel_nd,
                                quantiles = QUANTILES,
                                actuals = prob_test$actual)

p_reliab


# conditional
prep_rel_diagram_cnd(
  q_hat = rel_nd,
  df = prob_test,
  quantiles = QUANTILES,
  cond_var = "hour",
  nr_cond = 24
) %>%
  reliability_diagram_cond(., col = "cond_var", legend_lab = "Hour")




p_pit + p_reliab + plot_annotation(
  title = 'MLP: fixed forecast horizon'
)

# # save
# ggsave(
#   paste0("output/figures/chapter4/final model/figure5.", DEVICE),
#   width = 20,
#   height = 10,
#   units = "cm",
#   dpi = DPI,
#   device = DEVICE
# )


p_reliab + ggtitle("MLP: fixed forecast horizon")


# ggsave(
#   paste0("output/figures/chapter4/final model/figure5_Reliability.", DEVICE),
#   width = 15,
#   height = 10,
#   units = "cm",
#   dpi = DPI,
#   device = DEVICE
# )










