# clean environment
rm(list = ls())

# --------------------------------------------------------------------
# 0) Load libraries
# --------------------------------------------------------------------

# data manipulations
library(dplyr)
library(tidymodels)
library(plyr)
library(prophet)

# evaluate models
library(scoringRules) # probablistic scores

# other
library(progress)
library(tictoc)

# own functions
source("analysis/functions/evaluation_metrics.R")
source("analysis/functions/prob_forecasts.R")
source("analysis/functions/pred_intra_day.R")
source("analysis/functions/create_features.R")

# --------------------------------------------------------------------
#  1) Global parameters and load data
# --------------------------------------------------------------------


set.seed(69)
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

# performance metrics
PERF_METRIC_SET1 <- metric_set(mase_daily,
                               mase_weekly,
                               rsq,
                               mae,
                               rmse,
                               mape)

PERF_METRIC_SET2 <- metric_set(mase,
                               rsq,
                               mae,
                               mape)


# Elastic net
pred_enet <-
  read.csv("output/dynamic/forecast_horizon/elasticNet.csv") %>%
  filter(forecast_horizon == 6) %>% mutate(model = "Elastic net") %>%
  select(-c(forecast_horizon))

# Random Forest
pred_rf <-
  read.csv("output/dynamic/forecast_horizon/randomForest.csv") %>%
  filter(forecast_horizon == 6) %>% mutate(model = "Random forest") %>%
  select(-c(forecast_horizon))

# GBM
pred_gbm <- read.csv("output/dynamic/forecast_horizon/GBM.csv") %>%
  filter(forecast_horizon == 6) %>% mutate(model = "GBM") %>%
  select(-c(forecast_horizon))

# MLP
pred_mlp <- read.csv("output/dynamic/forecast_horizon/MLP.csv") %>%
  filter(forecast_horizon == 6) %>% mutate(model = "MLP") %>%
  select(-c(forecast_horizon))

# BEST MODEL FIXED MODEL
pred_mlp_fixed <- read.csv("output/deterministic/val_mlp.csv") %>%
  mutate(model = "MLP Fixed H")

pred <- rbind(pred_enet, pred_rf, pred_gbm, pred_mlp)


# --------------------------------------------------------------------
# 2 Point Forecasting
# --------------------------------------------------------------------

# performance by model
pred %>% group_by(model) %>%
  PERF_METRIC_SET1(.,
                   truth = actual,
                   estimate = fitted) %>%
  arrange(model) %>%
  write.csv("output/dynamic/perf_point.csv")


# create figure
perf_pointDyn <- pred %>%
  group_by(step_ahead, model)  %>%
  PERF_METRIC_SET2(.,
                   truth = actual,
                   estimate = fitted,
                   m = 1)

perf_pointDyn <- perf_pointDyn %>% mutate(.metric =  mapvalues(
  perf_pointDyn$.metric,
  from = c("mae", "mape", "mase", "rsq"),
  to = c("MAE", "MAPE (%)", "MASE (24)", "R2")
))


perf_pointDyn %>%
  ggplot(aes(x = step_ahead, y = .estimate, color = model))  + geom_point() + geom_line() +
  facet_wrap(vars(.metric), scales = "free") +
  labs(x = "Number of steps (hours) ahead forecasted" , y = "Point estimate")


ggsave(
  "output/figures/chapter4/dynamic/figure2.png",
  width = 20,
  height = 15,
  units = "cm"
)


# --------------------------------------------------------------------
# 3 Generate Probablistic Forecasts
# --------------------------------------------------------------------

# some global variables (CAPITAL LETTER)
QUANTILES <- seq(0.01, 0.99, by = 0.01)
I_Q <- QUANTILES * 100
ACTUAL_VAL <- pred_enet$actual
RESID_VAL <-
  pred_enet %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull
N_SIM <- 10000
NR_BOOT <- 5000
PI_LOWER <- 0.025
PI_UPPER <- 0.975
ALPHA <- 0.05

# load validation data
VALIDATION_DATA <-
  readRDS("data/cleaned_data/split_hourly/validation.Rda") %>%
  dplyr::rename(., cs = total_cs) %>% as_tibble()


# --------------------------------------------------------------------
# 3.1 ELASTIC NET
# --------------------------------------------------------------------



# ------------------------------------------------------------------
# 3.1.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------


# validation
prob_val <-
  prob_forecast_Un(
    df = pred_enet,
    residuals_inSample = RESID_VAL,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
  ) %>% mutate(step_ahead = pred_enet$step_ahead)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------


probScore_val_enet <- matrix(nrow = 4, ncol = 4)
colnames(probScore_val_enet) <-
  c("Nominal Coverage", "Winkler Score", "CRPS", "Pinball Loss")
rownames(probScore_val_enet) <-
  c("Unconditial NormD",
    "Conditial NormD",
    "Unconditial Boot",
    "Conditial Boot")

# 1) Nominal coverage
probScore_val_enet[1, 1] <- prob_val$cover_nU %>% mean


# 2) Winkler Score
probScore_val_enet[1, 2] <-
  prob_val %>%
  select(lower_nU, upper_nU, actual) %>%
  apply(1,
        winkler_score,
        start  = 1,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_enet[1, 3] <- scoringRules::crps(
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


probScore_val_enet[1, 4] <-
  store_pinball_loss_nU %>% colMeans() %>% mean
probScore_val_enet


# -------------------------------------------------------------------
# 3.1.2) Normal Dist: compute CONDITIONAL std of the historical residuals
# -------------------------------------------------------------------


# residuals grouped by dow and hour
residuals_grouped <- prob_val %>% group_by(step_ahead) %>%
  dplyr::summarise(mean_grouped = mean(residual),
                   sd_grouped = sd(residual))

# add the grouped prediction intervals
prob_val <-
  prob_val %>% left_join(y = residuals_grouped, by = c("step_ahead")) %>%
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
probScore_val_enet[2, 1] <- prob_val$cover_nC %>% mean


# 2) Winkler Score
probScore_val_enet[2, 2] <-
  prob_val %>% select(lower_nC, upper_nC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_enet[2, 3] <- scoringRules::crps(
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


probScore_val_enet[2, 4] <-
  store_pinball_loss_nC %>% colMeans() %>% mean
probScore_val_enet


# -------------------------------------------------------------------
# 3.1.3) Bootstrapping: UNCONDITIONAL
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
probScore_val_enet[3, 1] <- prob_val$cover_bU %>% mean


# 2) Winkler Score
probScore_val_enet[3, 2] <-
  prob_val %>% select(lower_bU, upper_bU, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score

# bootstrap
probScore_val_enet[3, 3] <-
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


probScore_val_enet[3, 4] <-
  store_pinball_loss_bU %>% colMeans() %>% mean
probScore_val_enet


# -------------------------------------------------------------------
# 3.1.4) Bootstrapping: CONDITIONAL
# -------------------------------------------------------------------

prob_val_bootC <- vector(mode = "list", NR_BOOT)
pb <- progress_bar$new(total = nrow(prob_val))
for (i in 1:nrow(prob_val)) {
  prob_val_bootC[[i]] <- conditional_bootstap_stepAhead(
    pred = prob_val[i,],
    historical_resid = prob_val,
    size = NR_BOOT,
    group_var = "step_ahead"
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
probScore_val_enet[4, 1] <- prob_val$cover_bC %>% mean


# 2) Winkler Score
probScore_val_enet[4, 2] <-
  prob_val %>% select(lower_bC, upper_bC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score
# bootstrap
probScore_val_enet[4, 3] <-
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

probScore_val_enet[4, 4] <-
  store_pinball_loss_bC %>% colMeans() %>% mean
probScore_val_enet




# predictions
write.csv(prob_val, "output/dynamic/elastic net/predProb_val.csv")

# performance
write.csv(probScore_val_enet,
          "output/dynamic/elastic net/perfProb_val.csv")



# --------------------------------------------------------------------
# 3.2 Random Forest
# --------------------------------------------------------------------

ACTUAL_VAL <- pred_rf$actual
RESID_VAL <-
  pred_rf %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull


# ------------------------------------------------------------------
# 3.2.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------


# validation
prob_val <-
  prob_forecast_Un(
    df = pred_rf,
    residuals_inSample = RESID_VAL,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
  ) %>% mutate(step_ahead = pred_rf$step_ahead)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------


probScore_val_rf <- matrix(nrow = 5, ncol = 4)
colnames(probScore_val_rf) <-
  c("Nominal Coverage", "Winkler Score", "CRPS", "Pinball Loss")
rownames(probScore_val_rf) <-
  c(
    "Unconditial NormD",
    "Conditial NormD",
    "Unconditial Boot",
    "Conditial Boot",
    "Quantile"
  )

# 1) Nominal coverage
probScore_val_rf[1, 1] <- prob_val$cover_nU %>% mean


# 2) Winkler Score
probScore_val_rf[1, 2] <-
  prob_val %>%
  select(lower_nU, upper_nU, actual) %>%
  apply(1,
        winkler_score,
        start  = 1,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_rf[1, 3] <- scoringRules::crps(
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


probScore_val_rf[1, 4] <-
  store_pinball_loss_nU %>% colMeans(na.rm = TRUE) %>% mean
probScore_val_rf


# -------------------------------------------------------------------
# 3.2.2) Normal Dist: compute CONDITIONAL std of the historical residuals
# -------------------------------------------------------------------


# residuals grouped by dow and hour
residuals_grouped <- prob_val %>% group_by(step_ahead) %>%
  dplyr::summarise(mean_grouped = mean(residual),
                   sd_grouped = sd(residual))

# add the grouped prediction intervals
prob_val <-
  prob_val %>% left_join(y = residuals_grouped, by = c("step_ahead")) %>%
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
probScore_val_rf[2, 1] <- prob_val$cover_nC %>% mean


# 2) Winkler Score
probScore_val_rf[2, 2] <-
  prob_val %>% select(lower_nC, upper_nC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_rf[2, 3] <- scoringRules::crps(
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


probScore_val_rf[2, 4] <-
  store_pinball_loss_nC %>% colMeans() %>% mean
probScore_val_rf


# -------------------------------------------------------------------
# 3.2.3) Bootstrapping: UNCONDITIONAL
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
probScore_val_rf[3, 1] <- prob_val$cover_bU %>% mean


# 2) Winkler Score
probScore_val_rf[3, 2] <-
  prob_val %>% select(lower_bU, upper_bU, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score

# bootstrap
probScore_val_rf[3, 3] <-
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



probScore_val_rf[3, 4] <-
  store_pinball_loss_bU %>% colMeans() %>% mean
probScore_val_rf


# -------------------------------------------------------------------
# 3.2.4) Bootstrapping: CONDITIONAL
# -------------------------------------------------------------------


prob_val_bootC <- vector(mode = "list", NR_BOOT)
pb <- progress_bar$new(total = nrow(prob_val))
for (i in 1:nrow(prob_val)) {
  prob_val_bootC[[i]] <- conditional_bootstap_stepAhead(
    pred = prob_val[i,],
    historical_resid = prob_val,
    size = NR_BOOT,
    group_var = "step_ahead"
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
probScore_val_rf[4, 1] <- prob_val$cover_bC %>% mean


# 2) Winkler Score
probScore_val_rf[4, 2] <-
  prob_val %>% select(lower_bC, upper_bC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score
# bootstrap
probScore_val_rf[4, 3] <-
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


probScore_val_rf[4, 4] <-
  store_pinball_loss_bC %>% colMeans() %>% mean
probScore_val_rf


# -------------------------------------------------------------------
# 3.2.5) Quantile Regression
# -------------------------------------------------------------------

prep_rf <-
  readRDS(file = "output/dynamic/random forest/preprocessing_rf.rda")
model_rf <-
  readRDS(file = "output/dynamic/random forest/model_rf.rda")

lags <- c(1, 24, 168)


# you need to define this or you get an error
holiday_effects <- prophet::generated_holidays %>%
  filter(country == "BE" & year %in% c(2017, 2018, 2019, 2020)) %>%
  select(ds) %>% pull %>% as_date()

# create lags
validation_data_lags_rf <-
  VALIDATION_DATA %>%
  create_lags(var = "cs",
              lags = lags)


# make prediction rf
make_prediction_rf1 <-
  function(data,
           model = model_rf,
           quantiles = QUANTILES) {
    out <- list()
    pred <- predict(
      object = model,
      num.trees = model$num.trees,
      data = data,
      type = "quantiles",
      quantiles = quantiles
    )$predictions
    
    out$center_estimate <- pred[, 50]
    out$quantiles <- pred
    return (out)
  }



make_prediction_rf2 <-
  function(data,
           model = model_rf,
           quantiles = c(0.025, 0.5, 0.975)) {
    out <- list()
    pred <- predict(
      object = model,
      num.trees = model$num.trees,
      data = data,
      type = "quantiles",
      quantiles = quantiles
    )$predictions
    
    out$center_estimate <- pred[, 2]
    out$quantiles <- pred
    return (out)
  }

data <-
  validation_data_lags_rf %>% slice(2:nrow(validation_data_lags_rf))

# quantile predictions
fh_quantile_rf1 <- chunk_forecast_horizon(
  data = data,
  model = model_rf,
  preprocess_steps = prep_rf,
  func_make_pred = make_prediction_rf1,
  forecast_horizon = 6,
  feedback_cols = c("cs_lag001"),
  add_quantiles = TRUE,
  show_progress = TRUE
)

# quantile predictions
fh_quantile_rf2 <- chunk_forecast_horizon(
  data = data,
  model = model_rf,
  preprocess_steps = prep_rf,
  func_make_pred = make_prediction_rf2,
  forecast_horizon = 6,
  feedback_cols = c("cs_lag001"),
  add_quantiles = TRUE,
  show_progress = TRUE
)

# concat
q_hatQ_rf1 <-
  fh_quantile_rf1$quantile_estimates %>% do.call(rbind, .)
q_hatQ_rf2 <-
  fh_quantile_rf2$quantile_estimates %>% do.call(rbind, .)

# add lower and upper quantile
prob_val <- prob_val %>% mutate(
  lower_q  = q_hatQ_rf2[, 1],
  upper_q = q_hatQ_rf2[, 3],
  cover_q = actual >= lower_q & actual <= upper_q
)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------


# 1) Nominal coverage
probScore_val_rf[5, 1] <- prob_val$cover_q %>% mean

# 2) Winkler Score
probScore_val_rf[5, 2] <-
  prob_val %>% select(lower_q, upper_q, actual) %>%
  apply(1,
        winkler_score,
        alpha = .05) %>% as_tibble() %>% pull %>% mean


# 3) Pinball Loss Function
store_pinball_loss_qR <-
  matrix(NA, nrow = length(ACTUAL_VAL), ncol = length(QUANTILES))

# compute pinball loss function
for (j in 1:nrow(q_hatQ_rf1)) {
  q_j <- q_hatQ_rf1[j,] %>% as.numeric()
  store_pinball_loss_qR[j, ] <-
    pinball_loss(q = q_j, actual = ACTUAL_VAL[j], i = I_Q)
}


probScore_val_rf[5, 4] <-
  store_pinball_loss_qR %>% colMeans() %>% mean


# 4) Continuous Ranked Probability Score: NOT APPLICABLE HERE
probScore_val_rf[5, 3] <-
  crps_sample(y = prob_val$actual, dat = q_hatQ_rf1 %>% as.matrix()) %>% mean()
probScore_val_rf



# predictions
write.csv(prob_val, "output/dynamic/random forest/predProb_val.csv")

# save performance
write.csv(probScore_val_rf,
          "output/dynamic/random forest/perfProb_val.csv")


# --------------------------------------------------------------------
# 3.3 GBM
# --------------------------------------------------------------------

ACTUAL_VAL <- pred_gbm$actual
RESID_VAL <-
  pred_gbm %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull


# ------------------------------------------------------------------
# 3.3.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------


# validation
prob_val <-
  prob_forecast_Un(
    df = pred_gbm,
    residuals_inSample = RESID_VAL,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
  ) %>% mutate(step_ahead = pred_gbm$step_ahead)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------


probScore_val_gbm <- matrix(nrow = 5, ncol = 4)
colnames(probScore_val_gbm) <-
  c("Nominal Coverage", "Winkler Score", "CRPS", "Pinball Loss")
rownames(probScore_val_gbm) <-
  c(
    "Unconditial NormD",
    "Conditial NormD",
    "Unconditial Boot",
    "Conditial Boot",
    "Quantile"
  )

# 1) Nominal coverage
probScore_val_gbm[1, 1] <- prob_val$cover_nU %>% mean


# 2) Winkler Score
probScore_val_gbm[1, 2] <-
  prob_val %>%
  select(lower_nU, upper_nU, actual) %>%
  apply(1,
        winkler_score,
        start  = 1,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_gbm[1, 3] <- scoringRules::crps(
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


probScore_val_gbm[1, 4] <-
  store_pinball_loss_nU %>% colMeans() %>% mean
probScore_val_gbm


# -------------------------------------------------------------------
# 3.3.2) Normal Dist: compute CONDITIONAL std of the historical residuals
# -------------------------------------------------------------------


# residuals grouped by dow and hour
residuals_grouped <- prob_val %>% group_by(step_ahead) %>%
  dplyr::summarise(mean_grouped = mean(residual),
                   sd_grouped = sd(residual))

# add the grouped prediction intervals
prob_val <-
  prob_val %>% left_join(y = residuals_grouped, by = c("step_ahead")) %>%
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
probScore_val_gbm[2, 1] <- prob_val$cover_nC %>% mean


# 2) Winkler Score
probScore_val_gbm[2, 2] <-
  prob_val %>% select(lower_nC, upper_nC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_gbm[2, 3] <- scoringRules::crps(
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


probScore_val_gbm[2, 4] <-
  store_pinball_loss_nC %>% colMeans() %>% mean
probScore_val_gbm


# -------------------------------------------------------------------
# 3.3.3) Bootstrapping: UNCONDITIONAL
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
probScore_val_gbm[3, 1] <- prob_val$cover_bU %>% mean


# 2) Winkler Score
probScore_val_gbm[3, 2] <-
  prob_val %>% select(lower_bU, upper_bU, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score

# bootstrap
probScore_val_gbm[3, 3] <-
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


probScore_val_gbm[3, 4] <-
  store_pinball_loss_bU %>% colMeans() %>% mean
probScore_val_gbm


# -------------------------------------------------------------------
# 3.3.4) Bootstrapping: CONDITIONAL
# -------------------------------------------------------------------


prob_val_bootC <- vector(mode = "list", NR_BOOT)
pb <- progress_bar$new(total = nrow(prob_val))
for (i in 1:nrow(prob_val)) {
  prob_val_bootC[[i]] <- conditional_bootstap_stepAhead(
    pred = prob_val[i,],
    historical_resid = prob_val,
    size = NR_BOOT,
    group_var = "step_ahead"
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
probScore_val_gbm[4, 1] <- prob_val$cover_bC %>% mean


# 2) Winkler Score
probScore_val_gbm[4, 2] <-
  prob_val %>% select(lower_bC, upper_bC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score
# bootstrap
probScore_val_gbm[4, 3] <-
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


probScore_val_gbm[4, 4] <-
  store_pinball_loss_bC %>% colMeans() %>% mean
probScore_val_gbm



# -------------------------------------------------------------------
# 3.3.5) Quantile Regression
# -------------------------------------------------------------------

prep_gbm <-
  readRDS(file = "output/dynamic/boosting/preprocessing_gbm.rda")
model_gbm_center <-
  readRDS(file = "output/dynamic/boosting/model_gbm_center.rda")
model_gbm_lower <-
  readRDS(file = "output/dynamic/boosting/model_gbm_lower.rda")
model_gbm_upper <-
  readRDS(file = "output/dynamic/boosting/model_gbm_upper.rda")

lags <- c(1, 24, 168)

# create lags
validation_data_lags_gbm <-
  VALIDATION_DATA %>%
  create_lags(var = "cs",
              lags = lags)


# gbm model
make_prediction_gbm <- function(data, model) {
  new_pred <- predict(model,
                      data %>% select(-cs),
                      n.trees = model$n.trees)
  return (new_pred %>% as.numeric)
}


data <-
  validation_data_lags_gbm %>% slice(2:nrow(validation_data_lags_gbm))
# forecast center quantile
fh_quantile_gbm_center <- chunk_forecast_horizon(
  data = data,
  model = model_gbm_center,
  preprocess_steps = prep_gbm,
  func_make_pred = make_prediction_gbm,
  forecast_horizon = 6,
  feedback_cols = c("cs_lag001"),
  add_quantiles = FALSE,
  show_progress = TRUE
)

# lower quantile
fh_quantile_gbm_lower <- chunk_forecast_horizon(
  data = data,
  model = model_gbm_lower,
  preprocess_steps = prep_gbm,
  func_make_pred = make_prediction_gbm,
  forecast_horizon = 6,
  feedback_cols = c("cs_lag001"),
  add_quantiles = FALSE,
  show_progress = TRUE
)

# upper quantile
fh_quantile_gbm_upper <- chunk_forecast_horizon(
  data = data,
  model = model_gbm_upper,
  preprocess_steps = prep_gbm,
  func_make_pred = make_prediction_gbm,
  forecast_horizon = 6,
  feedback_cols = c("cs_lag001"),
  add_quantiles = FALSE,
  show_progress = TRUE
)

# append
centerQ_gbm <-
  fh_quantile_gbm_center$center_estimates %>% do.call(rbind, .)
lowerQ_gbm <-
  fh_quantile_gbm_lower$center_estimates %>% do.call(rbind, .)
upperQ_gbm <-
  fh_quantile_gbm_upper$center_estimates %>% do.call(rbind, .)


prob_val$lower_q <- lowerQ_gbm$fitted
prob_val$upper_q <- upperQ_gbm$fitted
prob_val <-
  prob_val %>% mutate(cover_q = actual >= lower_q &
                        actual <= upper_q)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------


# 1) Nominal coverage
probScore_val_gbm[5, 1] <- prob_val$cover_q %>% mean


# 2) Winkler Score
probScore_val_gbm[5, 2] <-
  prob_val %>% select(lower_q, upper_q, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


probScore_val_gbm



# predictions
write.csv(prob_val, "output/dynamic/boosting/predProb_val.csv")
# performance
write.csv(probScore_val_gbm,
          "output/dynamic/boosting/perfProb_val.csv")

# --------------------------------------------------------------------
# 3.4 MLP
# --------------------------------------------------------------------

ACTUAL_VAL <- pred_mlp$actual

RESID_VAL <-
  pred_mlp %>% mutate(residual = fitted - actual) %>% select(residual) %>% pull


# ------------------------------------------------------------------
# 3.4.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------

# validation
prob_val <-
  prob_forecast_Un(
    df = pred_mlp,
    residuals_inSample = RESID_VAL,
    q_lower = PI_LOWER,
    q_upper = PI_UPPER,
    nr_samples = N_SIM
  ) %>% mutate(step_ahead = pred_mlp$step_ahead)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------


probScore_val_mlp <- matrix(nrow = 4, ncol = 4)
colnames(probScore_val_mlp) <-
  c("Nominal Coverage", "Winkler Score", "CRPS", "Pinball Loss")
rownames(probScore_val_mlp) <-
  c("Unconditial NormD",
    "Conditial NormD",
    "Unconditial Boot",
    "Conditial Boot")

# 1) Nominal coverage
probScore_val_mlp[1, 1] <- prob_val$cover_nU %>% mean


# 2) Winkler Score
probScore_val_mlp[1, 2] <-
  prob_val %>%
  select(lower_nU, upper_nU, actual) %>%
  apply(1,
        winkler_score,
        start  = 1,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_mlp[1, 3] <- scoringRules::crps(
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


probScore_val_mlp[1, 4] <-
  store_pinball_loss_nU %>% colMeans() %>% mean
probScore_val_mlp


# -------------------------------------------------------------------
# 3.4.2) Normal Dist: compute CONDITIONAL std of the historical residuals
# -------------------------------------------------------------------


# residuals grouped by dow and hour
residuals_grouped <- prob_val %>% group_by(step_ahead) %>%
  dplyr::summarise(mean_grouped = mean(residual),
                   sd_grouped = sd(residual))

# add the grouped prediction intervals
prob_val <-
  prob_val %>% left_join(y = residuals_grouped, by = c("step_ahead")) %>%
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
probScore_val_mlp[2, 1] <- prob_val$cover_nC %>% mean


# 2) Winkler Score
probScore_val_mlp[2, 2] <-
  prob_val %>% select(lower_nC, upper_nC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val_mlp[2, 3] <- scoringRules::crps(
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


probScore_val_mlp[2, 4] <-
  store_pinball_loss_nC %>% colMeans() %>% mean
probScore_val_mlp


# -------------------------------------------------------------------
# 3.4.3) Bootstrapping: UNCONDITIONAL
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
probScore_val_mlp[3, 1] <- prob_val$cover_bU %>% mean


# 2) Winkler Score
probScore_val_mlp[3, 2] <-
  prob_val %>% select(lower_bU, upper_bU, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score

# bootstrap
probScore_val_mlp[3, 3] <-
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


probScore_val_mlp[3, 4] <-
  store_pinball_loss_bU %>% colMeans() %>% mean
probScore_val_mlp


# -------------------------------------------------------------------
# 3.4.4) Bootstrapping: CONDITIONAL
# -------------------------------------------------------------------


prob_val_bootC <- vector(mode = "list", NR_BOOT)
pb <- progress_bar$new(total = nrow(prob_val))
for (i in 1:nrow(prob_val)) {
  prob_val_bootC[[i]] <- conditional_bootstap_stepAhead(
    pred = prob_val[i,],
    historical_resid = prob_val,
    size = NR_BOOT,
    group_var = "step_ahead"
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
probScore_val_mlp[4, 1] <- prob_val$cover_bC %>% mean


# 2) Winkler Score
probScore_val_mlp[4, 2] <-
  prob_val %>% select(lower_bC, upper_bC, actual) %>%
  apply(1,
        winkler_score,
        alpha = ALPHA) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score
# bootstrap
probScore_val_mlp[4, 3] <-
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


probScore_val_mlp[4, 4] <-
  store_pinball_loss_bC %>% colMeans() %>% mean
probScore_val_mlp


# predictions
write.csv(prob_val, "output/dynamic/mlp/predProb_val.csv")
# performance
write.csv(probScore_val_mlp, "output/dynamic/mlp/perfProb_val.csv")
