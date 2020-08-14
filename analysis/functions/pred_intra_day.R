




chunk_forecast_horizon <-
  function(data = NULL,
           forecast_horizon = 24,
           model = NULL,
           func_make_pred = NULL,
           feedback_cols = c("cs_lag001"),
           preprocess_steps = NULL,
           add_quantiles = FALSE,
           show_progress = FALSE) {
    chunks <-
      ggplot2::cut_interval(1:nrow(data), length = forecast_horizon, labels = FALSE)
    
    
    save_output <-
      list(center_estimates = NULL,
           quantile_estimates = NULL)
    center_estimates <- vector("list", length(unique(chunks)))
    if (add_quantiles) {
      quantile_estimates <- vector("list", length(unique(chunks)))
    }
    
    if (show_progress) {
      pb <- progress_bar$new(total = length(unique(chunks)))
    }
    for (i in unique(chunks)) {
      data_chunk <- data[which(chunks == i), ]
      #loop over forecast horizon and make prediction
      out <- save_pred_chunks <-
        loop_forecast_horizon(
          data = data_chunk,
          model = model,
          func_make_pred = func_make_pred,
          feedback_cols = feedback_cols,
          preprocess_steps = preprocess_steps,
          add_quantiles = add_quantiles
        )
      center_estimates[[i]] <- out$center_estimates
      if (add_quantiles) {
        quantile_estimates[[i]] <- out$quantiles_estimates
      }
      if (show_progress) {
        pb$tick()
        Sys.sleep(1 / length(unique(chunks)))
      }
    }
    if (add_quantiles) {
      save_output$quantile_estimates <- quantile_estimates
    }
    save_output$center_estimates <- center_estimates
    return(save_output)
  }



loop_forecast_horizon <-
  function(data = NULL,
           model = NULL,
           func_make_pred = NULL,
           feedback_cols = NULL,
           preprocess_steps = NULL,
           add_quantiles = FALSE) {
    forecast_horizon <- nrow(data)
    center_estimates <- c(1:forecast_horizon)
    
    if (add_quantiles) {
      quantiles_estimates <- vector("list", forecast_horizon)
    }
    save_output <-
      list(center_estimates = NULL,
           quantiles_estimates = NULL)
    if (is.null(feedback_cols)) {
      nr_feedback_cols <- 0
    }
    else{
      nr_feedback_cols <- length(feedback_cols)
    }
    for (i in 1:forecast_horizon) {
      # first prediction
      if (i == 1) {
        # define update columns: apply before the recipe steps
        if (nr_feedback_cols > 0) {
          # start at column 3 (1 and 2 are date and the response variable)
          feedback <- data[i, ] %>% select(feedback_cols)
          # columns that get feedback from previous values
          no_feedback <- data[i, ] %>% select(-feedback_cols)
          example <- cbind(feedback, no_feedback)
        }
        else{
          example <- data[i, ]
        }
        # apply pre-processing steps if specified
        if (!is.null(preprocess_steps))
        {
          example_baked <- bake(preprocess_steps, new_data = example)
        }
        # preprocessing are done in the prediction model specification
        else {
          example_baked <- example
        }
        
        # make prediction
        # 1 in case you want to add quantiles (quantile regression)
        if (add_quantiles) {
          out <- func_make_pred(model = model, data = example_baked)
          center_estimates[i] <- out$center_estimate
          quantiles_estimates[[i]] <- out$quantiles
        }
        # 2) only center forecast
        else{
          center_estimates[i] <-
            func_make_pred(model = model, data = example_baked)
        }
      }
      else{
        # remove oldest lag and add newest prediction to dataframe in front
        # shift all values by 1 (so cs_lag1 = pred, cs_lag2 = cs_lag1)
        if (nr_feedback_cols > 0) {
          if (nr_feedback_cols > 1) {
            feedback[2:ncol(feedback)] <- feedback[1:(ncol(feedback) - 1)]
          }
          feedback[1] <- center_estimates[i - 1]
          no_feedback <- data[i, ] %>% select(-feedback_cols)
          example <- cbind(feedback, no_feedback)
        }
        else{
          example <- data[i, ]
        }
        
        # apply pre-processing steps if specified
        if (!is.null(preprocess_steps))
        {
          example_baked <- bake(preprocess_steps, new_data = example)
        }
        # preprocessing are done in the prediction model specification
        else {
          example_baked <- example
        }
        
        # make prediction
        # 1 in case you want to add quantiles (quantile regression)
        if (add_quantiles) {
          out <- func_make_pred(model = model, data = example_baked)
          center_estimates[i] <- out$center_estimate
          quantiles_estimates[[i]] <- out$quantiles
        }
        # 2) only center forecast
        else{
          center_estimates[i] <-
            func_make_pred(model = model, data = example_baked)
        }
        
      }
    }
    if (add_quantiles) {
      save_output$quantiles_estimates <-
        do.call(rbind, quantiles_estimates)
    }
    tbl_center_estimates <- tibble(date = data$date,
                                   fitted = center_estimates,
                                   actual = data$cs,
                                   step_ahead = 1:forecast_horizon)
    save_output$center_estimates <- tbl_center_estimates
    return(save_output)
  }


# main function
compare_forecast_h <- function(data = NULL,
                               forecast_h = c(1:24),
                               model = NULL,
                               make_prediction = NULL,
                               preprocess_steps = NULL,
                               feedback_cols = NULL,
                               model_name = NULL,
                               show_progress1 = TRUE,
                               show_progress2 = FALSE,
                               save_out = FALSE,
                               file = NULL) {
  n_forecast_h <- length(forecast_h)
  store_results <- vector(mode = "list", n_forecast_h)
  if (show_progress1) {
    pb <- progress_bar$new(total = n_forecast_h)
  }
  # forecas horizon == 1 (forecast_h contains 1)
  if (!is.null(preprocess_steps)){
    # perform preprocessing
    data_prep <- bake(preprocess_steps,data)
    fitted <- make_prediction(data = data_prep, model = model)
  }
  else{
    # preprocessing is done within the make_prediction
    fitted <- make_prediction(data = data, model = model)
  }
  if (min(forecast_h) == 1) {
    store_results[[1]] <- tibble(
      date = data$date,
      fitted = fitted,
      actual = data$cs,
      step_ahead = 1,
      forecast_horizon = 1
    )
  }
  if (show_progress1) {
    pb$tick()
    Sys.sleep(1 / n_forecast_h)
  }
  
  # forecas horizon > 1
  
  for (i in 2:(n_forecast_h)) {
    store_results[[i]] <-
      chunk_forecast_horizon(
        data = data,
        model = model,
        preprocess_steps = preprocess_steps,
        func_make_pred = make_prediction,
        forecast_horizon = forecast_h[i],
        feedback_cols = feedback_cols,
        add_quantiles = FALSE,
        show_progress = show_progress2
      )$center_estimates %>%
      do.call("rbind", .) %>%
      mutate(forecast_horizon = forecast_h[i])
    if (show_progress1) {
      pb$tick()
      Sys.sleep(1 / n_forecast_h)
    }
  }
  
  if (save_out) {
    store_results %>%
      do.call(rbind, .) %>%
      mutate(model = model_name) %>%
      write.csv(., file,row.names = FALSE)
  }
  return(store_results)
  
}







