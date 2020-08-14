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


# visualisations
library(ggplot2)
library(plotly)
library(patchwork)
library(vip) # variable importance
library(DALEX) # variable importance

# modelling
library(tidymodels)
library(gbm)
library(h2o)


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



# performance metrics
PERF_METRIC_SET <- metric_set(mase_daily,
                              mase_weekly,
                              rsq,
                              mae,
                              rmse,
                              mape)

PERF_METRIC_SET


# create H2O environment
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-13.0.2")
h2o.init()

# --------------------------------------------------------------------
# 2) Read in Data
# --------------------------------------------------------------------

# read in training data (this is already grouped by hour)
train_data <-
  readRDS("data/cleaned_data/split_hourly/train.Rda") %>%
  rename(cs = total_cs) %>% as_tibble


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

holiday_effects <- prophet::generated_holidays %>% 
  filter(country == "BE" & year %in% c(2017,2018,2019,2020)) %>%
  select(ds) %>% pull %>% as_date()



# create lags
lags <- seq(from = 24, to = 2 * 7 * 24, by = 12)
train_data_lags <-
  train_data %>%
  create_lags(var = "cs",
              lags = lags)

# create recipe
recipe_steps <-
  recipe(cs ~ ., data = train_data_lags) %>%
  step_naomit(all_predictors()) %>%
  step_date(date, features = calandar_effects) %>%
  step_mutate(hour = lubridate::hour(date),
              holiday = as.integer(as_date(date) %in% holiday_effects)) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  #step_normalize(all_numeric(), -c(date_decimal, cs)) %>%
  step_zv(all_numeric())


# prepare training data
train_data <- recipe_steps %>%
  prep(train_data_lags) %>%
  bake(train_data_lags) 


# prepare validation data

# load validation data
validation_data <-
  readRDS("data/cleaned_data/split_hourly/validation.Rda") %>%
  rename(., cs = total_cs) %>% as_tibble()


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
# 5) Fit model
# --------------------------------------------------------------------

y <- "cs"
x <- setdiff(names(train_data), y)

# turn training set into h2o object
train.h2o <- as.h2o(train_data)
validaton.h2o <- as.h2o(validation_data)




# training basic GBM model with defaults
h2o.fit_boost <- h2o.gbm(
  y = y,
  ntrees = 400,
  distribution = "quantile",
  quantile_alpha = 0.5,
  training_frame = train.h2o,
  validation_frame =  validaton.h2o,
  stopping_rounds = 5,
  stopping_tolerance = 0.005,
  learn_rate = 0.025,
  learn_rate_annealing = 1,
  max_depth = 6,
  min_rows = 20,
  seed = 123,
  col_sample_rate = 1,
  categorical_encoding = "EnumLimited",
  verbose = FALSE
)


# convert feature variables to a data frame - tibble is also a data frame 
x_train <- train_data %>% select(-cs)

# change response variable to a numeric binary vector
y_train <- as.vector(as.numeric(as.character(train_data$cs)))

# create custom predict function
custom_pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, newdata %>% as.h2o()))
  return(results[["predict"]])
}


# gradient boosting machine explainer
explainer_train <- DALEX::explain(
  model            = h2o.fit_boost, 
  type             = "regression",
  data             = x_train,
  y                = y_train,
  predict_function = custom_pred,
  colorize = TRUE,
  label = ""
)



vi_train <- model_parts(
  explainer = explainer_train,
  loss_function = loss_sum_of_squares,
  B = 1,
  type = "difference"
)


plot(vi_train, max_vars = 10, title = "Boosting", subtitle = "")

# --------------------------------------------------------------------
# 6 Point Forecasts
# --------------------------------------------------------------------


# evaluations metrics for visuals
PERF_METRIC_SET_FH <- metric_set(
  rsq,
  mae,
  mape,
  mase)

predictions_train <- h2o.predict(h2o.fit_boost, newdata = train.h2o) %>%
  as_tibble() %>%
  mutate(date = train_data_lags$date,
         actual = train_data$cs) %>%
  rename(fitted = predict) %>% 
  select(date, fitted, actual)


predictions_val <- h2o.predict(h2o.fit_boost, newdata = validaton.h2o) %>%
  as_tibble() %>%
  mutate(date = validation_data_lags$date,
         actual = validation_data$cs) %>%
  rename(fitted = predict) %>% 
  select(date, fitted, actual)


# save for comparison
predictions_val %>% mutate(model = "boosting") %>%
  write.csv("output/deterministic/val_boostingH20.csv", row.names = FALSE)

# Evaluate

# training
pointScore_train <-
  PERF_METRIC_SET(predictions_train, truth = actual, estimate = fitted)
pointScore_train

# customs stats on residuals
predictions_train %>% 
  mutate(residuals = fitted-actual) %>% 
  select(residuals) %>% pull %>% summary_stats

# validation
pointScore_val <-
  PERF_METRIC_SET(predictions_val, truth = actual, estimate = fitted)
pointScore_val



# customs stats on residuals
predictions_val %>% 
  mutate(residuals = fitted-actual) %>% 
  select(residuals) %>% pull %>% summary_stats


predictions_val %>% mutate(hour = lubridate::hour(date)) %>%
  group_by(hour)  %>%
  PERF_METRIC_SET_FH(., truth = actual, estimate = fitted) %>%
  ggplot(aes(x = hour, y = .estimate))  + geom_point() + geom_line() + 
  facet_wrap(vars(.metric), scales = "free") +
  labs(x = "Hour" , y ="Point estimate")


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




# convert feature variables to a data frame - tibble is also a data frame 
x_val <- validation_data %>% select(-cs)

# change response variable to a numeric 
y_val <- as.vector(as.numeric(as.character(validation_data$cs)))

# gradient boosting machine explainer
explainer_val <- explain(
  model            = h2o.fit_boost, 
  type             = "regression",
  data             = x_val,
  y                = y_val,
  predict_function = custom_pred,
  colorize = TRUE,
  label = ""
)



vi_val  <- model_parts(
  explainer = explainer_val,
  loss_function = loss_sum_of_squares,
  B = 10,
  type = "difference"
)

plot(vi_val, max_vars = 10,title = "Boosting", subtitle = "")
ggsave("output/figures/chapter4/deterministic/boosting/figure1c.png",
       width = 17, height = 11, units = "cm")

# --------------------------------------------------------------------
# 7 Generate Probablistic Forecasts
# --------------------------------------------------------------------

# can be used for the prediction intervals
# note this is computed on the validation data
sd_normal_dist <- predictions_val %>% mutate(
  residual = fitted - actual
) %>% summarise(sd = sd(residual)) %>% pull


# ------------------------------------------------------------------
# 7.1) Normal Dist: compute UNCONDITIONAL std of the historical residuals
# ------------------------------------------------------------------

# train
prob_train <-
  prob_forecast_Un(df = predictions_train,
                   q_lower = 0.025,
                   q_upper = 0.975,
                   nr_samples = 100000)


residual_val <- predictions_val %>% 
  mutate(residual = fitted - actual) %>% 
  select(residual) %>% pull

# validation
prob_val <-
  prob_forecast_Un(df = predictions_val,
                   residuals_inSample = residual_val,
                   q_lower = 0.025,
                   q_upper = 0.975,
                   nr_samples = 100000)

# ---------------------------------------------------------
# evaluate
# ---------------------------------------------------------

probScore_val <- matrix(nrow = 5, ncol = 3)
colnames(probScore_val) <-
  c("Nominal Coverage", "Winkler Score", "CRPS")
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
        alpha = .05) %>% as_tibble() %>% pull %>% mean()


# 3) Continuous Ranked Probability Score

# assume normal distribution
probScore_val[1, 3] <- scoringRules::crps(
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
residuals_grouped_train <- prob_train %>% group_by(week_day) %>%
  summarise(
    mean_grouped = mean(residual),
    sd_grouped = sd(residual)) %>%
  add_column(obs = 1:7, .before = 'week_day')

residuals_grouped_val <- prob_val %>% group_by(week_day) %>%
  summarise(
    mean_grouped = mean(residual),
    sd_grouped = sd(residual)) %>%
  add_column(obs = 1:7, .before = 'week_day')

# add the grouped prediction intervals
prob_val <-
  prob_val %>% left_join(y = residuals_grouped_val, by = c("week_day")) %>%
  mutate(
    lower_nC = fitted + rnorm(100000, mean = mean_grouped, sd = sd_grouped) %>% quantile(0.025),
    upper_nC = fitted + rnorm(100000, mean = mean_grouped, sd = sd_grouped) %>% quantile(0.975),
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
probScore_val[2, 3] <- scoringRules::crps(
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
prob_val_bootUc <- prob_val$residual %>%
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


prob_val_bootC <- vector(mode = "list", nr_boot)
nr_boot <- 5000
pb <- progress_bar$new(total = nrow(prob_val))
for (i in 1:nrow(prob_val)) {
  
  prob_val_bootC[[i]] <- conditional_bootstap(
    pred = prob_val[i,],
    historical_resid = prob_val,
    size = nr_boot,
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

# -----------------------------------------------------------------------
# 7.5) Quantiles
# -----------------------------------------------------------------------


# lower quantile
h2o.fit_boost_lower <- h2o.gbm(
  x = x,
  y = y,
  ntrees = 400,
  distribution = "quantile",
  quantile_alpha = 0.025,
  training_frame = train.h2o,
  validation_frame =  validaton.h2o,
  stopping_rounds = 5,
  stopping_tolerance = 0.005,
  learn_rate = 0.025,
  learn_rate_annealing = 1,
  max_depth = 6,
  min_rows = 20,
  seed = 123,
  col_sample_rate = 1,
  categorical_encoding = "EnumLimited",
  verbose = FALSE
)

# upper quantile
h2o.fit_boost_upper <- h2o.gbm(
  x = x,
  y = y,
  ntrees = 400,
  distribution = "quantile",
  quantile_alpha = 0.975,
  training_frame = train.h2o,
  validation_frame =  validaton.h2o,
  stopping_rounds = 5,
  stopping_tolerance = 0.005,
  learn_rate = 0.025,
  learn_rate_annealing = 1,
  max_depth = 6,
  min_rows = 20,
  seed = 123,
  col_sample_rate = 1,
  categorical_encoding = "EnumLimited",
  verbose = FALSE
)

lower_q <- h2o.predict(h2o.fit_boost_lower, newdata = validaton.h2o) %>% 
  as_tibble() %>% pull

upper_q <- h2o.predict(h2o.fit_boost_upper, newdata = validaton.h2o) %>% 
  as_tibble() %>% pull


# validation data
prob_val <- prob_val %>%
  as_tibble() %>%
  mutate(
    lower_q = lower_q,
    upper_q = upper_q,
    date = validation_data_lags$date,
    actual = validation_data_lags$cs,
    cover_q = actual >= lower_q & actual <= upper_q
)




# 1) Nominal coverage
probScore_val[5, 1] <- prob_val$cover_q %>% mean
probScore_val


# 2) Winkler Score
probScore_val[5, 2] <-
  prob_val %>% select(lower_q, upper_q, actual) %>%
  apply(1,
        winkler_score,
        alpha = .05) %>% as_tibble() %>% pull %>% mean


# 3) Continuous Ranked Probability Score: NOT APPLICABLE HERE



# 4) Pinball Loss Function: TOO MUCH COMPUTATIONS



# -------------------------------------------------------------------
# 7.6) VISUALIZE PERFORMANCE METRICS BY HOUR
# -------------------------------------------------------------------

# Summarise 

p_winkler <- prob_val %>% select(hour, lower_nC, upper_nC, actual) %>%
  apply(1,start = 2, winkler_score,
        alpha = .05) %>% as_tibble() %>%
  mutate(hour = prob_val$hour) %>%
  group_by(hour) %>% summarise(winkler_score = mean(value)) %>%
  ggplot(aes(x = hour , y  = winkler_score)) +
  geom_point() + geom_line() + 
  labs(x = "hour", y = "Winkler score")

p_emperical_cov <- prob_val %>% select(hour, cover_nC) %>%
  group_by(hour) %>% summarise(emp_cov = mean(cover_nC)) %>%
  ggplot(aes(x = hour, y = emp_cov)) + 
  geom_point() + geom_line() +
  geom_hline(yintercept   = 0.95, color = "red", linetype = "dashed", size = 1) +
  labs(x = "Hour" , y ="Emperical Coverage (95%)")


p_crsp <- crps_sample(y = prob_val$actual, dat = prob_val_bootC) %>% as_tibble() %>%
  mutate(hour = prob_val$hour) %>% group_by(hour) %>%
  summarise(crps = mean(value)) %>% ggplot(aes(x = hour, y = crps)) +
  geom_point() + geom_line() + labs(x = "Hour", y = "CRSP")

p_emperical_cov + p_winkler  + p_crsp


# -------------------------------------------------------------------
# 7.5 DIAGNOSTIC PLOTS
# -------------------------------------------------------------------

# 1) probability integral transform


# to get samples for the normal dist
samples_normal <-
  get_samples_normal(
    fitted = prob_val$fitted,
    sd_resid = prob_val$residual %>% sd %>% rep(nrow(prob_val)),
    mu_resid = prob_val$residual %>% mean %>% rep(nrow(prob_val)),
    nr_samples = 5000
  )



pit_object <- pit(prob_val$actual, samples_normal, num_bins = 10)
nr_obs <- prob_val %>% nrow()

p <- pit_plot(pit_object = pit_object, nr_obs = nr_obs)
p


# 2) reliability diagram
quantiles <- seq(0.01,0.99,0.01)

# 2.1 Normal Distribution


rel_nd <-
  prep_rel_diagram_und(
    fitted = prob_val$fitted,
    quantiles = quantiles,
    mu_resid = prob_val$mean_grouped,
    sd_resid = prob_val$sd_grouped
  )



# overall
reliability_diagram(
  q_hat = rel_nd,
  quantiles = quantiles,
  actuals = prob_val$actual
)


# conditional
prep_rel_diagram_cnd(
  q_hat = rel_nd,
  df = prob_val,
  quantiles = quantiles,
  cond_var = "hour",
  nr_cond = 24
) %>%
  reliability_diagram_cond(., col = "cond_var", legend_lab = "Hour")



# 2.2 Bootstrap

# unconditional or conditional bootstrap
rel_b <- prob_val_bootUc %>% apply(
  .,
  MARGIN = 1 ,
  FUN = quantile,
  probs = quantiles
) %>% t


# overall
reliability_diagram(
  q_hat = rel_b,
  quantiles = quantiles,
  actuals = prob_val$actual
)

# conditional
prep_rel_diagram_cnd(
  q_hat = rel_b,
  df = prob_val,
  quantiles = quantiles,
  cond_var = "hour",
  nr_cond = 24
) %>%
  reliability_diagram_cond(., col = "cond_var", legend_lab = "Hour")




# visualize
prob_val %>%
  slice(20:(24 * 20)) %>%
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









