
# clean environment
rm(list=ls())

# load packages

# building blocks
library(tidyverse)
library(lubridate)
library(tsibble)
#library(plyr)
library(dplyr)
library(lubridate)

#model building packages
library(recipes)
library(workflows)
library(tidymodels)
library(tune)    # tuning
library(glmnet)

# visuals
library(ggplot2)
library(gridExtra)
library(plotly)
library(vip)
library(GGally)
library(patchwork)

# others
library(tictoc)
library(doParallel)
library(reshape2)
library(forecast) # (partial) autocorrelation plots

# custom functions
source("analysis/functions/split_data.R")
source("analysis/functions/create_features.R")
source("analysis/functions/error_analysis.R")
source("analysis/functions/tidy_model_helper.R")
source("analysis/functions/save_output.R")


# library(doParallel)
# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoParallel(cores = all_cores)


# Global parameters ---------------------------------------

# colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# choose theme
THEME <- theme_minimal()
# remove legend title
LEGEND <- theme(legend.title=element_blank())


# seed for reproduceability
set.seed(100)


# Parallel Processing
# parallel::detectCores(logical = TRUE)
cl <- makeCluster(2)

# metric of interest
METRIC <- "rsq_trad"
MAXIMIZE <- TRUE
NUM_VIP <- 10


#TODO  
#      1) make code smaller, make functions for iterations
#      2) reverse engineer predictions
#      3) Interpretation of variable importance plots
#      4) add step for lags (custom implementation)
#      5) combo: How will I implement this
#      6) Make a more flexible framework: so not use the 
#         tidymodel infrastructure
#      7) Remove trend in data and work on remainder
#      8) Add holidays
#      9) Varying forecast horizon from 1h ahead till 24h ahead



#=====================================================#
#  import data and split data
#=====================================================#

list_df <- readRDS('data/cleaned_data/final_tot_c.Rda') %>% 
  # sample a certain fraction to try out some things (work faster)
  slice(., 1:20000) %>%
  as_tsibble(index=date) %>%
  index_by(
    # there is a difference between floor_date and ceiling_date
    date_h = lubridate::floor_date(date, "1 hour")
    ) %>% 
    dplyr::summarise(total_cs = sum(total_cs)) %>%
    # again rename
    dplyr::rename(
      date = date_h) %>%
    # split data
    split_data(df = ., 
             train_perc = .6,
             validation_perc = .2, embargo=0) %>% 
    # # create lags
    lapply(.,create_lags,var = "total_cs",
           lags = seq(from = 24, to = 2*7*24, by = 24))


# function to check wether there is still 
# a constant time resolution (15 min or 1h)
time_diff <- function(df) {
  df$date %>% diff.Date() %>% as.integer() %>% summary()
}

# time difference should still be 1 hour
lapply(list_df, time_diff)
lapply(list_df, head)
lapply(list_df, head)
lapply(list_df,dim)



# calandar effect
# "month","dow","doy",
# "week","month","decimal",
# "quarter","semester"

calandar_effects <- c("dow","doy","decimal","week")

# holiday_effects <- c("Easter","EasterMonday","EasterSunday",
#                      "ChristmasDay","ChristmasEve","NewYearsDay",
#                      "Advent1st","Advent2nd","Advent3rd",
#                      "Advent4th","AllSaints","AshWednesday","GoodFriday")


# simple recipe
recipe_steps1 <- 
  recipe(total_cs ~., data = list_df$train) %>%
  #step_YeoJohnson(all_outcomes()) %>%
  step_sqrt(all_outcomes()) %>%
  step_sqrt(starts_with("total_cs_lag")) %>%
  step_mutate(hour = lubridate::hour(date)) %>%
  step_date(date,features = calandar_effects) %>%
  step_novel(all_nominal()) %>%
  step_unknown(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_numeric()) %>%
  step_zv(all_predictors()) %>%
  step_rm(date) 



# cosinus and sinus transformations of time
recipe_steps2 <- 
  recipe(total_cs ~., data = list_df$train) %>%
  step_sqrt(all_outcomes()) %>%
  step_sqrt(starts_with("total_cs_lag")) %>%
  step_mutate(hour = lubridate::hour(date)) %>%
  step_date(date,features = calandar_effects) %>%
  step_novel(all_nominal()) %>%
  step_unknown(all_nominal()) %>%
  step_mutate(
    sin_hour  = sin(2*pi*hour/24),
    cos_hour = cos(2*pi*hour/24),
    sin_dow  = sin(2*pi*as.integer(date_dow)/7),
    cos_dow = cos(2*pi*as.integer(date_dow)/7),
    hour = as.factor(hour)
  ) %>%
  #step_rm(hour) %>%
  step_dummy(all_nominal()) %>%
  step_normalize(all_numeric()) %>%
  step_zv(all_predictors()) %>%
  step_rm(date) 


# cosinus and sinus of time and also with interactions
recipe_steps3 <- 
  recipe(total_cs ~., data = list_df$train) %>%
  #step_YeoJohnson(all_outcomes()) %>%
  step_mutate(hour = lubridate::hour(date)) %>%
  step_date(date,features = calandar_effects) %>%
  step_novel(all_nominal()) %>%
  step_unknown(all_nominal()) %>%
  step_mutate(
  sin_hour  = sin(2*pi*hour/24),
  cos_hour = cos(2*pi*hour/24),
  sin_dow  = sin(2*pi*as.integer(date_dow)/7),
  cos_dow = cos(2*pi*as.integer(date_dow)/7)
  ) %>%
  step_rm(hour) %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ sin_hour:starts_with("total_cs_lag")) %>%
  step_interact(~ sin_dow:starts_with("total_cs_lag")) %>%
  step_interact(~ sin_hour:starts_with("date_dow"):starts_with("total_cs_lag")) %>%
  step_interact(~ cos_hour:starts_with("total_cs_lag")) %>%
  step_interact(~ cos_hour:starts_with("date_dow"):starts_with("total_cs_lag")) %>%
  step_normalize(all_numeric()) %>%
  step_zv(all_predictors()) %>%
  step_rm(date) 

recipe_steps3




# check data prep
check_prep  <- prep(recipe_steps3, training = list_df$train, verbose = TRUE)
check_juice <- juice(check_prep) 

colnames(check_juice)
ncol(check_juice)
nrow(check_juice)


# mean zero and std 1
sapply(check_juice,mean)
sapply(check_juice,sd)


# resampling scheme
time_resamples <- rolling_origin(list_df$train, 
                                 initial = 24*7*6, # 4 week training
                                 assess = 24*7*2, # 1 week validation
                                 skip = 24*7*2,  # 1 week skip
                                 cumulative = FALSE)
time_resamples



# If you need to set options for certain metrics,
# do so by wrapping the metric and setting the options inside the wrapper,
# passing along truth and estimate as quoted arguments.
# Then add on the function class of the underlying wrapped function,
# and the direction of optimization.

# Add on the underlying function class (here, "numeric_metric"), and the
# direction to optimize the metric


# If you need to set options for certain metrics,
# do so by wrapping the metric and setting the options inside the wrapper,
# passing along truth and estimate as quoted arguments.
# Then add on the function class of the underlying wrapped function,
# and the direction of optimization.
mase_daily <- function(data, truth, estimate, na_rm = TRUE, ...) {
  mase(
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    # set bias = TRUE
    m = 24,
    na_rm = na_rm,
    ...
  )
}

# Add on the underlying function class (here, "numeric_metric"), and the
# direction to optimize the metric
class(mase_daily) <- class(mase)
attr(mase_daily, "direction") <- attr(mase, "direction")


mase_weekly <- function(data, truth, estimate, na_rm = TRUE, ...) {
  mase(
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    # set bias = TRUE
    m = 24*7,
    na_rm = na_rm,
    ...
  )
}


# Add on the underlying function class (here, "numeric_metric"), and the
# direction to optimize the metric
class(mase_weekly) <- class(mase)
attr(mase_weekly, "direction") <- attr(mase, "direction")


# performance metrics
perf_metrics <- metric_set(mase_daily, 
                           mase_weekly,
                           rsq_trad,
                           mae,
                           rmse,
                           smape)

# Save the assessment set predictions
ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)


#=====================================================#
#  Model Building
#=====================================================#

#-------------------------------
# glmnet
#-------------------------------


# specify model
glmnet_model <- linear_reg(
  mode    = "regression", 
  penalty = tune(), 
  mixture = tune()
) %>%
  set_engine("glmnet")


# create workflow (add recipe and model)
glmnet_wf <-
  workflow() %>% 
  add_recipe(recipe_steps3) %>% 
  add_model(glmnet_model)
glmnet_wf

# Save the assessment set predictions
ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

# set grid for parameters to look at
glmnet_hypers <- parameters(penalty(), mixture()) %>%
  grid_max_entropy(size = 10)

# visualize the parameters of glmnet
ggplot(glmnet_hypers,aes(x = penalty, y = mixture))+geom_point()+
  scale_x_log10()

tic("glmnet")
# grid search
#registerDoParallel(cl)
glmnnet_results <- glmnet_wf %>%
                 tune_grid( 
                           resamples = time_resamples,
                           grid = glmnet_hypers,
                           metrics = perf_metrics,
                           control = ctrl
                           )
#stopCluster(cl)
toc()

# have look at the tuning parameters
glmnnet_results %>% autoplot()


# have a look at the performance
glmnnet_results %>%
  collect_metrics() %>%
  filter(.metric == METRIC)


# show performance
show_best(glmnnet_results, 
          n = 10,
          metric = METRIC, 
          maximize = MAXIMIZE)


glmnet_params <-
  select_best(glmnnet_results, 
              metric = METRIC, 
              maximize = MAXIMIZE)

# get predictions using the best parameters found 
glmnnet_pred <- collect_predictions(glmnnet_results) %>%
  inner_join(glmnet_params, by = c("penalty", "mixture"))

# get predictions and visualisze them against acutals
vis_plot_fitted(pred = glmnnet_pred, interactive = FALSE)

p_perf_analysis_lm <- prep_visualize_pred(df = list_df$train, 
                    pred = glmnnet_pred, fold = "Slice1") %>%
  # interactive plot
  visualize_pred(df = ., interactive = TRUE)

p_perf_analysis_lm

# view large residuals
see_large_resid(glmnnet_pred, list_df$train, max_n = 5)


# fit on training data
glmnnet_fit_train <- get_final_model(recipe_steps = recipe_steps3,
                data = list_df$train,
                model = glmnet_model,
                params = glmnet_params)

# Get the set of coefficients across penalty values
tidy_coefs <-
  broom::tidy(glmnnet_fit_train) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::select(-step,-dev.ratio) 

# get the lambda closest to tune's optimal choice
delta <- abs(tidy_coefs$lambda - glmnet_params$penalty)
lambda_opt <- tidy_coefs$lambda[which.min(delta)]


# Keep the large values
label_coefs <-
  tidy_coefs %>%
  mutate(abs_estimate = abs(estimate)) %>%
  dplyr::filter(abs_estimate >= .25) %>%
  distinct(term) %>%
  inner_join(tidy_coefs, by = "term") %>%
  dplyr::filter(lambda == lambda_opt)


# plot the paths and hightlist the large values
tidy_coefs %>%
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) +
  geom_vline(xintercept = lambda_opt, lty = 2) +
  geom_line(alpha = 1) +
  theme(legend.position = "none") +
  scale_x_log10() +
  ggrepel::geom_text_repel(data = label_coefs, aes(label = term, x = .0005)) 

# variable importance plot
p_vip_glmnet <- vip(glmnnet_fit_train, num_features = NUM_VIP,
                    lambda = glmnet_params$penalty, geom = "point",
                    aesthetics = list(color = cbPalette[4], 
                    fill = cbPalette[4])) + THEME +
                    ggtitle("Linear model (glmnet)")

p_vip_glmnet %>% ggplotly()


# Look at performance on validation data
#---------------------------------------

# 'mae_train	
# A numeric value which allows the user to provide the in-sample seasonal 
# naive mean absolute error. If this value is not provided, 
# then the out-of-sample seasonal naive mean absolute error will be calculated 
# from truth and will be used instead.

# # take the value of the previous day
# mase(glmnet_pred_val, truth = actual, estimate = fitted, m = 24, mae_train = NULL)
# # take the value of the previous week
# mase(glmnet_pred_val, truth = actual, estimate = fitted, m = 24*7, mae_train = NULL)
# 

glmnet_pred_val <- prediction_new_data(recipe_steps = recipe_steps3,
                    data = list_df$validation,
                    model = glmnnet_fit_train)

visualize_pred(df = glmnet_pred_val)


perf_metrics(glmnet_pred_val,
             truth = actual, estimate = fitted)



#-------------------------------
# GAM
#-------------------------------


# specify model
mars_model <- mars(
  mode    = "regression", 
  num_terms = tune(), 
  prune_method = tune(),
  prod_degree = tune()
) %>%
  set_engine("earth")


# create workflow (add recipe and model)
mars_wf <-
  workflow() %>% 
  add_recipe(recipe_steps1) %>% 
  add_model(mars_model)
mars_wf


# set grid for parameters to look at
mars_hypers <- parameters(num_terms(c(1,20)), 
                          prune_method(),
                          prod_degree(c(1,3))) %>%
  grid_max_entropy(size = 10)


tic("mars")
# grid search
#registerDoParallel(cl)
mars_results <- mars_wf %>%
  tune_grid( 
    resamples = time_resamples,
    grid = mars_hypers,
    metrics = perf_metrics,
    control = ctrl
  )
#stopCluster(cl)
toc()

# have look at the tuning parameters
mars_results %>% autoplot()


# have a look at the performance
mars_results %>%
  collect_metrics() %>%
  filter(.metric == METRIC)


# show performance
show_best(mars_results, 
          n = 10,
          metric = METRIC, 
          maximize = MAXIMIZE)


mars_params <-
  select_best(mars_results, 
              metric = METRIC, 
              maximize = MAXIMIZE)

# get predictions using the best parameters found 
mars_pred <- collect_predictions(mars_results) %>%
  inner_join(mars_params, by = c("num_terms", "prune_method","prod_degree"))


# get predictions and visualisze them against acutals
vis_plot_fitted(pred = mars_pred, interactive = FALSE)

prep_visualize_pred(df = list_df$train, pred = mars_pred, fold = NULL) %>%
  # interactive plot
  visualize_pred(df = .)


# view large residuals
see_large_resid(mars_pred, list_df$train, max_n = 5)


# fit on training data
mars_fit_train <- get_final_model(recipe_steps = recipe_steps1,
                                     data = list_df$train,
                                     model = mars_model,
                                     params = mars_params)


# variable importance plot
p_vip_mars <- vip(mars_fit_train, num_features = NUM_VIP,
                    geom = "point",
                    aesthetics = list(color = cbPalette[4], 
                    fill = cbPalette[4])) + THEME +
                    ggtitle("Multivariate Adaptive Splines")

p_vip_mars %>% ggplotly()

# Look at performance on validation data
#---------------------------------------

# 'mae_train	
# A numeric value which allows the user to provide the in-sample seasonal 
# naive mean absolute error. If this value is not provided, 
# then the out-of-sample seasonal naive mean absolute error will be calculated 
# from truth and will be used instead.

# # take the value of the previous day
# mase(glmnet_pred_val, truth = actual, estimate = fitted, m = 24, mae_train = NULL)
# # take the value of the previous week
# mase(glmnet_pred_val, truth = actual, estimate = fitted, m = 24*7, mae_train = NULL)
# 

mars_pred_val <- prediction_new_data(recipe_steps = recipe_steps1,
                                       data = list_df$validation,
                                       model = mars_fit_train)

visualize_pred(df = mars_pred_val)


perf_metrics(mars_pred_val,
             truth = actual, estimate = fitted)


#-------------------------------
# Random forest
#-------------------------------

rf_model <-
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger",importance = "impurity") %>%
  set_mode("regression")

# specify grid 
# Two relevant descriptors for what we are about to do are:
#   .preds(): the number of predictor variables in the data set that are associated with the predictors prior to dummy variable creation.
# .cols(): the number of predictor columns after dummy variables (or other encodings) are created.



# create workflow (add recipe and model)
rf_wf <-
  workflow() %>% 
  add_recipe(recipe_steps2) %>% 
  add_model(rf_model)
rf_wf

rf_hypers <- parameters(mtry(c(5,20)), min_n(),
                        trees(c(500,2500))) %>% 
                        grid_max_entropy(size = 10)


#registerDoParallel(cl)
tic("Random Forest")

rf_results <- rf_wf %>% 
  tune_grid( 
    resamples = time_resamples,
    metrics = perf_metrics,
    grid = rf_hypers,
    control = ctrl
  )

#stopCluster(cl)
toc()

perf_vals_rf <- 
  collect_metrics(rf_results) %>% 
  filter(.metric == METRIC)

show_best(rf_results, 
          metric = METRIC,
          maximize = MAXIMIZE)

rf_pred <-
  rf_results %>%
  collect_predictions() %>%
  inner_join(
    select_best(rf_results, metric = METRIC, maximize = MAXIMIZE),
    by = c("mtry", "trees", "min_n")
  )



# get predictions and visualisze them against acutals
vis_plot_fitted(pred = rf_pred, interactive = TRUE)

prep_visualize_pred(df = list_df$train, pred = rf_pred, fold = "Slice4") %>%
  # interactive plot
  visualize_pred(df = .)

# view large residuals
see_large_resid(rf_pred, list_df$train, max_n = 5)


# get best parameter values
rf_params <-
  select_best(rf_results, metric = METRIC, maximize = MAXIMIZE)
rf_params

# fit on training data
rf_fit_train <- get_final_model(recipe_steps = recipe_steps2,
                                data = list_df$train,
                                model = rf_model,
                                params = rf_params)

# visualize preditors
p_vip_rf <- vip(rf_fit_train, num_features = NUM_VIP, geom = "point",
                  aesthetics = list(color = cbPalette[4], 
                  fill = cbPalette[4])) + THEME + ggtitle("Random Forest")

p_vip_rf %>% ggplotly()

# Look at performance on validation data
#---------------------------------------

rf_pred_val <- prediction_new_data(recipe_steps = recipe_steps2,
                                   data = list_df$validation,
                                   model = rf_fit_train)

# visualize performance
visualize_pred(df = rf_pred_val)

# summary statistics
perf_metrics(rf_pred_val,
             truth = actual, estimate = fitted)


#-------------------------------
# Boosting
#-------------------------------

boost_model <-
  boost_tree(mtry = tune(), 
             trees = tune(), 
             min_n = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# create workflow (add recipe and model)
boost_wf <-
  workflow() %>% 
  add_recipe(recipe_steps2) %>% 
  add_model(boost_model)
boost_wf


boost_hypers <- parameters(mtry(c(5,20)), 
                           min_n(),
                           trees(c(750,2500))) %>% 
                grid_max_entropy(size = 10)


#registerDoParallel(cl)
tic("Boosting")

boost_results <- boost_wf %>% 
  tune_grid( 
    resamples = time_resamples,
    metrics = perf_metrics,
    grid = boost_hypers,
    control = ctrl
  )

#stopCluster(cl)
toc()

perf_vals_boost <- 
  collect_metrics(boost_results) %>% 
  filter(.metric == METRIC)

show_best(boost_results, 
          metric = METRIC,
          maximize = MAXIMIZE)

boost_pred <-
  boost_results %>%
  collect_predictions() %>%
  inner_join(
    select_best(boost_results, metric = METRIC, maximize = MAXIMIZE),
    by = c("mtry", "trees", "min_n")
  )


# get predictions and visualisze them against acutals
vis_plot_fitted(pred = boost_pred, interactive = FALSE)

prep_visualize_pred(df = list_df$train, pred = boost_pred, fold = "Slice1") %>%
  # interactive plot
  visualize_pred(df = .)

# view large residuals
see_large_resid(boost_pred, list_df$train, max_n = 5)

# get best parameter values
boost_params <-
  select_best(boost_results, metric = METRIC, maximize = MAXIMIZE)
boost_params

# fit on training data
boost_fit_train <- get_final_model(recipe_steps = recipe_steps2,
                                data = list_df$train,
                                model = boost_model,
                                params = boost_params)

# visualize preditors
p_vip_boost <- vip(boost_fit_train, num_features = NUM_VIP, geom = "point",
    aesthetics = list(color = cbPalette[4], fill = cbPalette[4])) +
    THEME + ggtitle("Boosting")

p_vip_boost %>% ggplotly()


# Look at performance on validation data
#---------------------------------------

boost_pred_val <- prediction_new_data(recipe_steps = recipe_steps2,
                                   data = list_df$validation,
                                   model = boost_fit_train)

# visualize performance
visualize_pred(df = boost_pred_val)

# summary statistics
perf_metrics(boost_pred_val,
             truth = actual, estimate = fitted)


#-------------------------------
# Compare variable importance
#-------------------------------


# feature importance
figure6_pres <- ((p_vip_glmnet + p_vip_mars)/ (p_vip_rf + p_vip_boost))

figure6_pres 

#-------------------------------
# compare performance
#-------------------------------


# get performance on time series cross validation
glmnnet_perf_tcv <- collect_metrics(glmnnet_results) %>%
  inner_join(glmnet_params, by = c("penalty", "mixture"))


# get performance on time series cross validation
mars_perf_tcv <- collect_metrics(mars_results) %>%
  inner_join(mars_params, by = c("num_terms", 
                                 "prune_method","prod_degree"))

# get performance on time series cross validation
rf_perf_tcv <- collect_metrics(rf_results) %>%
  inner_join(rf_params, by = c("mtry", "trees", "min_n"))

# get performance on time series cross validation
boost_perf_tcv <- collect_metrics(boost_results) %>%
  inner_join(boost_params, by = c("mtry", "trees", "min_n"))


# add everything to one list
model_perf_list <- list(glmnet = glmnnet_perf_tcv, 
     mars = mars_perf_tcv, random_forest = rf_perf_tcv, 
     boosting = boost_perf_tcv)


# function I can pass on to the list
select_perf_cols <- function(df){
  df %>% select(.metric, .estimator, mean, std_err)
}
  
# apply to the list   
lapply(model_perf_list, select_perf_cols) %>%
  dplyr::bind_rows(., .id = "model") %>% 
  plot_perf_metric()


# function to selection columns for each tibble
select_cols <- function(df){
  df %>% select(date, fitted, actual, id) %>% 
    mutate(residual = fitted - actual) %>% 
    as_tibble()
}


# make a lit of all predictions
list_pred_models <- list(glmnet = glmnnet_pred, 
                      random_forest = rf_pred, 
                      boosting = boost_pred,
                      mars = mars_pred)

# update the list
list_pred_models <- lapply(list_pred_models, prep_visualize_pred, 
                           df = list_df$train) %>%
                           lapply(.,select_cols)

# transform list to long dataframe
pred_long <- dplyr::bind_rows(list_pred_models, .id = "model")



# filter 
p_compare_modelpred_vis <- pred_long %>% filter(date < "2017-07-29 13:00:00") %>%
  compare_pred_models(., interactive = TRUE)

p_compare_modelpred_vis


# fitted values against acutal values
p_fit_truth <- ggplot(pred_long,aes(x = fitted, y = actual)) +
  geom_point(alpha = .3) +
  ggrepel::geom_label_repel(aes(label = as.character(date)),size = 3,
  data = filter(pred_long, abs(residual) > 4)) +
  geom_abline(col = "red",linetype = "dashed") + 
  facet_wrap(~ model, scales = "free") +
  theme_minimal()

p_fit_truth
ggplotly(p_fit_truth)


# pairplot

# compare predictions across models
pairplot_fitted <- pred_long %>% select(date,fitted,model) %>% 
            spread(.,model,fitted) %>%
            dplyr::mutate(actual = pred_long %>% 
            filter(model == "glmnet") %>% select(actual) %>% pull()) 
# plot predictions
ggpairs(pairplot_fitted[,2:ncol(pairplot_fitted)]) + THEME


# compare residuals across models
pairplot_resid <- pred_long %>% select(date,residual,model) %>% 
  spread(.,model,residual)   
# plot residuals
ggpairs(pairplot_resid[,2:ncol(pairplot_resid)]) + THEME


 




