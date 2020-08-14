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
library(gbm)

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
set.seed(100)

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

calandar_effects <- c("dow", "doy", "decimal", "week")


holiday_effects <- prophet::generated_holidays %>%
  dplyr::filter(country == "BE" &
                  year %in% c(2017, 2018, 2019, 2020)) %>%
  select(ds) %>% pull %>% as_date()


# create lags
lags <- seq(from = 24, to = 2 * 7 * 24, by = 24)

train_data_lags <-
  train_data %>%
  create_lags(var = "cs",
              lags = lags)

recipe_steps <-
  recipe(cs ~ ., data = train_data_lags) %>%
  step_naomit(all_predictors()) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(
    holiday = as.integer(as_date(date) %in% holiday_effects),
    hour = lubridate::hour(date),
    wday = lubridate::wday(date),
    sin_hour  = sin(2 * pi * hour / 23),
    cos_hour = cos(2 * pi * hour / 23)
  ) %>%
  step_rm(date, hour, wday) %>%
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
  step_mutate(cs_lag024_3 = cs_lag024 ** 3) %>%
  step_normalize(all_predictors(),-date_decimal) %>%
  #step_corr(all_predictors(), threshold = 0.90) %>%
  step_zv(all_predictors())


# fit preprocessing steps
rec_trained <- prep(recipe_steps, training = train_data_lags)
# prepare training data
train_data  <- bake(rec_trained, new_data = train_data_lags)
train_data %>% dim

# --------------------------------------------------------------------
# 5) Modelling: Training data
# --------------------------------------------------------------------


set.seed(69)
# train GBM model
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


# Variable importance plot
gbm.fit_train %>%
  vip(geom = "point",
      aesthetics = list(color = cbPalette[4],
                        fill = cbPalette[4])) +
  THEME +
  ggtitle("GBM")


# for the dalex package
custom_predict <- function(object,
                           newdata) {
  pred <- predict(object, newdata)
  return(pred)
}


explainer_train <-
  DALEX::explain(
    gbm.fit_train,
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
  B = 1,
  type = "difference"
)


plot(vi_train,
     max_vars = 1,
     title = "GBM",
     subtitle = "")

# compare resid
resids_train <- model_performance(explainer_train)
p1 <- plot(resids_train) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  labs(y = '')
p2 <- plot(resids_train, geom = "boxplot") +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

p1 + p2


# generate probalistic forecast
predictions_train <-
  tibble(
    fitted =
      predict(gbm.fit_train,
              train_data),
    date = train_data_lags$date,
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
validation_data <- recipe_steps %>%
  prep(validation_data_lags) %>%
  bake(validation_data_lags)


# --------------------------------------------------------------------
# 6 Point Forecasts
# --------------------------------------------------------------------

# evaluations metrics for visuals
PERF_METRIC_SET_FH <- metric_set(rsq,
                                 mae,
                                 mape,
                                 mase)



predictions_val <-
  predict(object = gbm.fit_train,
          (validation_data %>% select(-cs))) %>% as_tibble() %>%
  dplyr::rename(fitted = value) %>%
  mutate(date = validation_data_lags$date,
         actual = validation_data_lags$cs) %>% select(date, fitted, actual)

# save for comparison
predictions_val %>% mutate(model = "boosting") %>%
  write.csv("output/deterministic/val_boostingGbm.csv", row.names = FALSE)


# Evaluate

# training
pointScore_train <-
  PERF_METRIC_SET(predictions_train, truth = actual, estimate = fitted)
pointScore_train

# customs stats on residuals
predictions_train %>%
  mutate(residuals = fitted - actual) %>%
  select(residuals) %>% pull %>% summary_stats

# validation
pointScore_val <-
  PERF_METRIC_SET(predictions_val, truth = actual, estimate = fitted)
pointScore_val

# save performance
write.csv(pointScore_val,"output/deterministic/boosting/perfPoint_val.csv")

# customs stats on residuals
predictions_val %>%
  mutate(residuals = fitted - actual) %>%
  select(residuals) %>% pull %>% summary_stats


predictions_val %>% mutate(hour = lubridate::hour(date)) %>%
  group_by(hour)  %>%
  PERF_METRIC_SET_FH(.,
                     truth = actual,
                     estimate = fitted,
                     m = 1) %>%
  ggplot(aes(x = hour, y = .estimate))  + geom_point() + geom_line() +
  facet_wrap(vars(.metric), scales = "free") +
  labs(x = "Hour" , y = "Point estimate")


predictions_val %>%
  slice(1:500) %>%
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

# variable importance
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



plot(vi_val,
     max_vars = 10,
     title = "GBM",
     subtitle = "")
ggsave(
  paste0("output/figures/chapter4/deterministic/boosting/figure1c.",DEVICE),
  width = 17,
  height = 11,
  units = "cm",
  dpi = DPI,
  device = DEVICE
)

# compare resid
resids_val <- model_performance(explainer_val)
p1 <- plot(resids_val) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  labs(y = '')
p2 <- plot(resids_val, geom = "boxplot") +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

p1 + p2


# --------------------------------------------------------------------
# 7 Generate Probablistic Forecasts
# --------------------------------------------------------------------


# some global variables (CAPITAL LETTER)
QUANTILES <- seq(0.01, 0.99, by = 0.01)
I_Q <- QUANTILES * 100
ACTUAL_VAL <- predictions_val$actual
RESID_VAL <-
  predictions_val %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull
RESID_TRAIN <-
  predictions_train %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull
N_SIM <- 10000
NR_BOOT <- 5000
PI_LOWER <- 0.025
PI_UPPER <- 0.975
ALPHA <- 0.05
# ------------------------------------------------------------------
# 7.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------

# train
prob_train <-
  prob_forecast_Un(
    df = predictions_train,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
  )



# validation
prob_val <-
  prob_forecast_Un(
    df = predictions_val,
    #residuals_inSample = RESID_TRAIN
    residuals_inSample = RESID_VAL,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
  )

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

probScore_val <- matrix(nrow = 5, ncol = 4)
colnames(probScore_val) <-
  c("Nominal Coverage", "Winkler Score", "CRPS", "Pinball Loss")
rownames(probScore_val) <-
  c("Unconditial NormD",
    "Conditial NormD",
    "Unconditial Boot",
    "Conditial Boot",
    "Quantile")

# 1) Nominal coverage
probScore_val[1, 1] <- prob_val$cover_nU %>% mean


# 2) Winkler Score
probScore_val[1, 2] <-
  prob_val %>%
  select(lower_nU, upper_nU, actual) %>%
  apply(1,
        winkler_score,
        start  = 1,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val[1, 3] <- scoringRules::crps(
  y = prob_val$actual,
  family = "cnorm",
  lower = 0,
  upper = Inf,
  location = prob_val$fitted,
  #scale = sd(prob_train$residual)
  scale = sd(RESID_VAL)
) %>%
  mean

# 4) Pinball loss function

# store values
store_pinball_loss_nU <-
  matrix(NA, nrow = length(ACTUAL_VAL), ncol = length(QUANTILES))

# compute pinball loss function
for (j in 1:nrow(prob_val)) {
  q_j <-
    (prob_val$fitted[j] + rnorm(N_SIM, mean = RESID_VAL %>% mean, sd = RESID_VAL %>% sd)) %>%
    quantile(., probs = QUANTILES) %>% as.numeric()
  store_pinball_loss_nU[j,] <-
    pinball_loss(q = q_j, actual = ACTUAL_VAL[j], i = I_Q)
}

store_pinball_loss_nU %>% colMeans() %>% as_tibble() %>%
  mutate(quantile = I_Q) %>% ggplot(aes(x = quantile, y = value)) +
  geom_col(fill = cbPalette[2]) + 
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) +
  labs(x = "Quantile", y = "Pinball loss value")


probScore_val[1, 4] <- store_pinball_loss_nU %>% colMeans() %>% mean
probScore_val

# -------------------------------------------------------------------
# 7.2) Normal Dist: compute CONDITIONAL std of the historical residuals
# -------------------------------------------------------------------


# residuals grouped by dow and hour
residuals_grouped <- prob_val %>% group_by(week_day) %>%
  summarise(mean_grouped = mean(residual),
            sd_grouped = sd(residual)) %>%
  add_column(obs = 1:7, .before = 'week_day')

# add the grouped prediction intervals
prob_val <-
  prob_val %>% left_join(y = residuals_grouped, by = c("week_day")) %>%
  mutate(lower_nC = 1,
         upper_nC = 1,
         cover_nC = 1)

# add grouped residuals
for (i in 1:nrow(prob_val)) {
  resid_sim <-
    rnorm(N_SIM,
          mean = prob_val$mean_grouped[i],
          sd = prob_val$sd_grouped[i])
  prob_val$lower_nC[i] <-
    (prob_val$fitted[i] + resid_sim) %>% quantile(PI_LOWER)
  prob_val$upper_nC[i] <-
    (prob_val$fitted[i] + resid_sim) %>% quantile(PI_UPPER)
  prob_val$cover_nC[i] <-
    ACTUAL_VAL[i] >= prob_val$lower_nC[i] &
    ACTUAL_VAL[i] <= prob_val$upper_nC[i]
}

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
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val[2, 3] <- scoringRules::crps(
  y = prob_val$actual,
  family = "cnorm",
  lower = 0,
  upper = Inf,
  location = prob_val$fitted,
  scale = prob_val$sd_grouped
) %>%
  mean


# 4) Pinball loss function

# store values
store_pinball_loss_nC <-
  matrix(NA, nrow = length(ACTUAL_VAL), ncol = length(QUANTILES))

# compute pinball loss function
for (j in 1:nrow(prob_val)) {
  q_j <-
    (
      prob_val$fitted[j] + rnorm(
        N_SIM,
        mean = prob_val$mean_grouped[j],
        sd = prob_val$sd_grouped[j]
      )
    ) %>%
    quantile(., probs = QUANTILES)
  store_pinball_loss_nC[j,] <-
    pinball_loss(q = q_j, actual = ACTUAL_VAL[j], i = I_Q)
}

store_pinball_loss_nC %>% colMeans() %>% as_tibble() %>%
  mutate(quantile = I_Q) %>% ggplot(aes(x = quantile, y = value)) +
  geom_col(fill = cbPalette[2]) + 
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) +
  labs(x = "Quantile", y = "Pinball loss value")


probScore_val[2, 4] <- store_pinball_loss_nC %>% colMeans() %>% mean
probScore_val


# -------------------------------------------------------------------
# 7.3) Bootstrapping: UNCONDITIONAL
# -------------------------------------------------------------------


prob_val_bootUc <- RESID_VAL %>%
  sample(., size = prob_val %>% nrow * NR_BOOT, replace = TRUE) %>%
  matrix(., ncol = NR_BOOT) + prob_val$fitted


# add to dataframe
prob_val <- prob_val %>% mutate(
  lower_bU = prob_val_bootUc %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = PI_LOWER
  ),
  upper_bU = prob_val_bootUc %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = PI_UPPER
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
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score

# bootstrap
probScore_val[3, 3] <-
  crps_sample(y = prob_val$actual, dat = prob_val_bootUc) %>% mean



# 4) Pinball loss function
# store values
store_pinball_loss_bU <-
  matrix(NA, nrow = length(ACTUAL_VAL), ncol = length(QUANTILES))

q_hat_bU <- prob_val_bootUc %>% apply(.,
                                      MARGIN = 1 ,
                                      FUN = quantile,
                                      probs = QUANTILES) %>% t

# compute pinball loss function
for (j in 1:nrow(prob_val)) {
  store_pinball_loss_bU[j,] <-
    pinball_loss(q = q_hat_bU[j, ], actual = ACTUAL_VAL[j], i = I_Q)
}

store_pinball_loss_bU %>% colMeans() %>% as_tibble() %>%
  mutate(quantile = I_Q) %>% ggplot(aes(x = quantile, y = value)) +
  geom_col(fill = cbPalette[2]) + 
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) +
  labs(x = "Quantile", y = "Pinball loss value")


probScore_val[3, 4] <- store_pinball_loss_bU %>% colMeans() %>% mean
probScore_val


# -------------------------------------------------------------------
# 7.4) Bootstrapping: CONDITIONAL
# -------------------------------------------------------------------


prob_val_bootC <- vector(mode = "list", NR_BOOT)
pb <- progress_bar$new(total = nrow(prob_val))
for (i in 1:nrow(prob_val)) {
  prob_val_bootC[[i]] <- conditional_bootstap(
    pred = prob_val[i,],
    historical_resid = prob_val,
    size = NR_BOOT,
    colNr_fitted = 8,
    colNr_dayType = 2,
    colNr_hour = 3,
    hour_window = 5
  )
  pb$tick()
  Sys.sleep(1 / nrow(prob_val))
}

prob_val_bootC <- prob_val_bootC %>% do.call("rbind", .)


# add to dataframe
prob_val <- prob_val %>% mutate(
  lower_bC = prob_val_bootC %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = PI_LOWER
  ),
  upper_bC = prob_val_bootC %>% apply(
    .,
    MARGIN = 1 ,
    FUN = quantile,
    probs = PI_UPPER
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
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score
# bootstrap
probScore_val[4, 3] <-
  crps_sample(y = prob_val$actual, dat = prob_val_bootC) %>% mean()


# pinball loss function

# generate quantile forecasts

q_hat_bC <- prob_val_bootC %>% apply(.,
                                     MARGIN = 1 ,
                                     FUN = quantile,
                                     probs = QUANTILES) %>% t


store_pinball_loss_bC <-
  matrix(NA, nrow = length(ACTUAL_VAL), ncol = length(QUANTILES))

# compute pinball loss function
for (j in 1:nrow(q_hat_bC)) {
  store_pinball_loss_bC[j, ] <-
    pinball_loss(q = q_hat_bC[j, ], actual = ACTUAL_VAL[j], i = I_Q)
}

store_pinball_loss_bC %>% colMeans() %>% as_tibble() %>%
  mutate(quantile = I_Q) %>% ggplot(aes(x = quantile, y = value)) +
  geom_col(fill = cbPalette[2]) + 
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) +
  labs(x = "Quantile", y = "Pinball loss value")


probScore_val[4, 4] <- store_pinball_loss_bC %>% colMeans() %>% mean
probScore_val

# -------------------------------------------------------------------
# 7.5) Quantile regression
# -------------------------------------------------------------------

set.seed(69)
# train GBM model lower quantile
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

set.seed(69)
# train GBM model upper quantile
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


prob_val$lower_q <- predict(gbm.fit_train_lower, validation_data)
prob_val$upper_q <- predict(gbm.fit_train_upper, validation_data)
prob_val <- prob_val %>% mutate(cover_q = actual >= lower_q & actual <= upper_q)


# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

# 1) Nominal coverage
probScore_val[5, 1] <- prob_val$cover_q %>% mean


# 2) Winkler Score
probScore_val[5, 2] <-
  prob_val %>% select(lower_q, upper_q, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean




# save performance
write.csv(probScore_val,"output/deterministic/boosting/perfProb_val.csv")

# -------------------------------------------------------------------
# 7.6) Summarize perf  METRICS BY HOUR
# -------------------------------------------------------------------
#

# 1) reliability diagram

# 1.1)norm uncond
rel_ndU <-
  prep_rel_diagram_und(
    fitted = prob_val$fitted,
    quantiles = QUANTILES,
    mu_resid = rep(RESID_VAL %>% mean, nrow(prob_val)),
    sd_resid = rep(RESID_VAL %>% sd, nrow(prob_val))
  )

reliab_nU <- summarize_reliability(rel_ndU, ACTUAL_VAL,
                                   QUANTILES, "norm uncond.", "boosting")

# 1.2)norm cond
rel_ndC <-
  prep_rel_diagram_und(
    fitted = prob_val$fitted,
    quantiles = QUANTILES,
    mu_resid = prob_val$mean_grouped,
    sd_resid = prob_val$sd_grouped
  )

reliab_nC <- summarize_reliability(rel_ndC, ACTUAL_VAL,
                                   QUANTILES, "norm cond.", "boosting")



# 1.3) boot uncond.
rel_bUc <- prob_val_bootUc %>%
  apply(.,
        MARGIN = 1 ,
        FUN = quantile,
        probs = QUANTILES) %>% t


reliab_bU <- summarize_reliability(rel_bUc, ACTUAL_VAL,
                                   QUANTILES, "boot uncond.", "boosting")


# 1.4) boot cond.
rel_bC <- prob_val_bootC %>%
  apply(.,
        MARGIN = 1 ,
        FUN = quantile,
        probs = QUANTILES) %>% t


reliab_bC <- summarize_reliability(rel_bC, ACTUAL_VAL,
                                   QUANTILES, "boot cond.", "boosting")

# add all together
probPerf_reliab <- rbind(reliab_nU, reliab_nC, reliab_bU, reliab_bC)


# save
probPerf_reliab %>%
  write.csv("output/deterministic/boosting/reliability_val.csv",
            row.names = FALSE)

#  2) Winkler score

# 2.1)norm uncond
winkler_nU <- summarize_winkler_hour(
  data = prob_val,
  lower = "lower_nU",
  upper = "upper_nU",
  name_method = "norm uncond.",
  name_model = "boosting"
)

# 2.2)norm cond
winkler_nC <- summarize_winkler_hour(
  data = prob_val,
  lower = "lower_nC",
  upper = "upper_nC",
  name_method = "norm cond.",
  name_model = "boosting"
)

# 2.3) boot uncond.
winkler_bU <- summarize_winkler_hour(
  data = prob_val,
  lower = "lower_bU",
  upper = "upper_bU",
  name_method = "boot uncond.",
  name_model = "boosting"
)

# 2.4) boot cond.
winkler_bC <- summarize_winkler_hour(
  data = prob_val,
  lower = "lower_bC",
  upper = "upper_bC",
  name_method = "boot cond.",
  name_model = "boosting"
)

# 2.5) quantiles
winkler_qR <- summarize_winkler_hour(
  data = prob_val,
  lower = "lower_q",
  upper = "upper_q",
  name_method = "quantile reg.",
  name_model = "boosting"
)



probPerf_winkler <-
  rbind(winkler_nU, winkler_nC, winkler_bU, winkler_bC, winkler_qR)



# 3) Continious ranked probability score

# 3.1 norm uncond

crps_nU <- tibble(
  value = scoringRules::crps(
    y = prob_val$actual,
    family = "cnorm",
    lower = 0,
    upper = Inf,
    location = prob_val$fitted,
    scale = RESID_VAL %>% sd
  )
) %>% mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "norm uncond.",
         model = "boosting",
         metric = "crps")

# 3.2 norm cond

crps_nC <- tibble(
  value = scoringRules::crps(
    y = prob_val$actual,
    family = "cnorm",
    lower = 0,
    upper = Inf,
    location = prob_val$fitted,
    scale = prob_val$sd_grouped
  )
) %>% mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "norm cond.",
         model = "boosting",
         metric = "crps")

# 3.3 boot uncond.

crps_bU <-
  tibble(value = crps_sample(y = prob_val$actual, dat = prob_val_bootUc)) %>%
  mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "boot uncond.",
         model = "boosting",
         metric = "crps")


# 3.4 boot cond.

crps_bC <-
  tibble(value = crps_sample(y = prob_val$actual, dat = prob_val_bootC)) %>%
  mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "boot cond.",
         model = "boosting",
         metric = "crps")

probPerf_crsp <- rbind(crps_nU, crps_nC, crps_bU, crps_bC)

# 4) pinball loss function

# 4.1 norm uncond

p_loss_nU <-
  tibble(value = store_pinball_loss_nU %>% rowMeans()) %>%
  mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "norm uncond.",
         model = "boosting",
         metric = "pinball loss")

# 4.2 norm cond

p_loss_nC <-
  tibble(value = store_pinball_loss_nC %>% rowMeans()) %>%
  mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "norm cond.",
         model = "boosting",
         metric = "pinball loss")

# 4.3 boot uncond.

p_loss_bU <-
  tibble(value = store_pinball_loss_bU %>% rowMeans()) %>%
  mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "boot uncond.",
         model = "boosting",
         metric = "pinball loss")

# 4.4 boot cond.

p_loss_bC <-
  tibble(value = store_pinball_loss_bC %>% rowMeans()) %>%
  mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(value = mean(value)) %>%
  mutate(method = "boot cond.",
         model = "boosting",
         metric = "pinball loss")


# add everything together
probPerf_pinball <-
  rbind(p_loss_nU, p_loss_nC, p_loss_bU, p_loss_bC)

# visualize
rbind(probPerf_winkler, probPerf_crsp, probPerf_pinball) %>%
  ggplot(., aes(x = hour, y = value, color = method)) + geom_line() + geom_point() +
  facet_wrap(vars(metric), scales = "free")

# save
rbind(probPerf_winkler, probPerf_crsp, probPerf_pinball) %>%
  write.csv("output/deterministic/boosting/probPerf_val.csv",
            row.names = FALSE)


# -------------------------------------------------------------------
# 7.5 DIAGNOSTIC PLOTS
# -------------------------------------------------------------------



# 1) probability integral transform

# to get samples for the normal dist
samples_normal <-
  get_samples_normal(
    fitted = prob_val$fitted,
    sd_resid = prob_train$residual %>% sd %>% rep(nrow(prob_val)),
    mu_resid = prob_train$residual %>% mean %>% rep(nrow(prob_val)),
    nr_samples = 5000
  )


pit_object <-
  pit(prob_val$actual,
      samples_normal,
      num_bins = 10,
      full_output = TRUE)
nr_obs <- prob_val %>% nrow

p <- pit_plot(pit_object = pit_object, nr_obs = nr_obs)
p

pit_object$u[3]
(samples_normal[3,] < prob_val$actual[3]) %>% mean


# 2) reliability diagram

# 2.1 Normal Distribution


rel_nd <-
  prep_rel_diagram_und(
    fitted = prob_val$fitted,
    quantiles = QUANTILES,
    mu_resid = prob_val$mean_grouped,
    sd_resid = prob_val$sd_grouped
  )



# overall
reliability_diagram(q_hat = rel_nd,
                    quantiles = QUANTILES,
                    actuals = prob_val$actual)


# conditional
prep_rel_diagram_cnd(
  q_hat = rel_nd,
  df = prob_val,
  quantiles = QUANTILES,
  cond_var = "hour",
  nr_cond = 24
) %>%
  reliability_diagram_cond(., col = "cond_var", legend_lab = "Hour")



# 2.2 Bootstrap


# unconditional or conditional bootstrap
rel_b <- prob_val_bootUc %>%
  apply(.,
        MARGIN = 1 ,
        FUN = quantile,
        probs = QUANTILES) %>% t


# overall
reliability_diagram(q_hat = rel_b,
                    quantiles = QUANTILES,
                    actuals = prob_val$actual)

# conditional
prep_rel_diagram_cnd(
  q_hat = rel_b,
  df = prob_val,
  quantiles = QUANTILES,
  cond_var = "hour",
  nr_cond = 24
) %>%
  reliability_diagram_cond(., col = "cond_var", legend_lab = "Hour")



# visualize predictions
prob_val %>%
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
    lower = "lower_nU",
    upper = "upper_nU"
  )

# save forecasts
prob_val %>%
  write.csv("output/deterministic/boosting/pred_validation.csv",
            row.names = FALSE)

