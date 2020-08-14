
#' splits data in train, validation, train_validation and test set
#'
#' @param df: Dataframe, Tibble, Tsibble
#' @param train_perc: Numeric, representing the training percentage
#' @param validation_perc: Numeric, representing the validation percentage
#' @param embargo: Numeric, indiciating the embargo period to avoid information leakage
#' @param save: Boolean indicating to safe the train/validation/train_validation and test set
#' @param output_dir: Character, directory where the data is saved
#' @param return_split: Boolean, return the split
#' @return  print where the file was saved and returns the data when return_split is true 


split_data <-
  function(df = NULL,
           train_perc = NULL,
           validation_perc = NULL,
           embargo = 0,
           save = TRUE,
           output_dir = NULL,
           return_split = FALSE,
           ...) {
    
    nr_obs <- df %>% nrow()
    idx_endtrain <- floor(nr_obs * train_perc)
    # add one week between to remove autocorr
    idx_begin_validation <- floor(idx_endtrain + embargo)
    idx_end_validation <-
      floor(idx_begin_validation + nr_obs * validation_perc)
    
    # add one week between to remove autocorr
    idx_begin_test <- floor(idx_end_validation + embargo)
    idx_end_test <- nr_obs
    
    # split into train, validation and test with 1 week between
    # train and validation and 1 week between validation and test
    train <- df[0:idx_endtrain, ]
    validation <- df[idx_begin_validation:idx_end_validation, ]
    test <- df[idx_begin_test:idx_end_test, ]
    # both train and validation
    train_validation <- df[0:idx_end_validation, ]
    
    # return in list format
    output <- list(
      train = train,
      validation = validation,
      train_validation = train_validation,
      test = test
    )
    
    # save split both as csv file and rda
    if (save) {
      
      # 1) save training data
      
      # csv
      save_output(
        output_dir = output_dir,
        FUN = write.csv,
        x = train,
        file = "train.csv",
        row.names=FALSE
      )
      
      # Rda
      save_output(
        output_dir = output_dir,
        FUN = saveRDS,
        object = train,
        file = "train.Rda"
      )
      
      
      # 2) save validation data
      
      # csv
      save_output(
        output_dir = output_dir,
        FUN = write.csv,
        x = validation,
        file = "validation.csv",
        row.names=FALSE
      )
      
      # Rda
      save_output(
        output_dir = output_dir,
        FUN = saveRDS,
        object = validation,
        file = "validation.Rda"
      )
      
      # 3) save train_validation data
      
      # csv
      save_output(
        output_dir = output_dir,
        FUN = write.csv,
        x = train_validation,
        file = "train_validation.csv",
        row.names=FALSE
      )
      
      # Rda
      save_output(
        output_dir = output_dir,
        FUN = saveRDS,
        object = train_validation,
        file = "train_validation.Rda"
      )
      
      # 4) save test data
      
      # csv
      save_output(
        output_dir = output_dir,
        FUN = write.csv,
        x = test,
        file = "test.csv",
        row.names=FALSE
      )
      
      # Rda
      save_output(
        output_dir = output_dir,
        FUN = saveRDS,
        object = test,
        file = "test.Rda"
      )
      
    }
    
    # return split when asked
    if (return_split==TRUE){
      return(output)
    }
    
    
}