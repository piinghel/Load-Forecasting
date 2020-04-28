
#' returns the fitted model on the data
#' 
#' @param recipe A recipe
#' @param data tibble containg date and consumption feature
#' @param model parsnip model
#' @param tibble with the parameters
#' @return returns a tsibble dataframe with new calandar features

get_final_model <- function(recipe_steps, data, model, params){
  
  # prep data
  prep_data <- prep(recipe_steps)
  # select model with chosen parameters
  mod_params <- finalize_model(model, params)
  # fit model
  mod_params %>% fit(total_cs ~.,data = bake(prep_data, new_data = data))
  
}

#' Creates new lagged by means of lags of a given timeserie
#' 
#' @param recipe_steps: A recipe
#' @param data: tibble containing date and consumption 
#' @param model parsnip model
#' @return return a tibble with the predictions and true values

prediction_new_data <- function(recipe_steps, data, model){
  
  # prep  data
  prep_data <- recipe_steps %>% 
    prep(data) %>%
    bake(data) 
  
  # get predictions
  model %>% 
    predict(new_data = prep_data) %>% 
    mutate(actual = prep_data$total_cs,
           date = data$date) %>%
    dplyr::rename(fitted = .pred) %>%
    select(date,fitted,actual)
  
}