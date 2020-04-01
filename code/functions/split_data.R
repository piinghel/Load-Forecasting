
#' splits data in train, validation, train_validation and test set
#'
#' @param df: dataframe,
#' @param train_perc: double representing the training percentage
#' @param validation_perc: double representing the validation percentage
#' @param embargo: integer indiciating the embargo period to avoid information leakage
#' @param save: boolean indicating to safe the train/validation/train_validation and test set
#' @return  print where the file was saved


split_data <-
  function(df = NULL,
           train_perc = NULL,
           validation_perc = NULL,
           embargo = 0,
           save = FALSE,
           output_dir = NULL) {
    nr_obs <- dim(df)[1]
    idx_endtrain <- nr_obs * train_perc
    # add one week between to remove autocorr
    idx_begin_validation <- idx_endtrain + embargo
    idx_end_validation <-
      idx_begin_validation + nr_obs * validation_perc
    
    # add one week between to remove autocorr
    idx_begin_test <- idx_end_validation + embargo
    idx_end_test <- nr_obs
    
    # split into train, validation and test with 1 week between
    # train and validation and 1 week between validation and test
    train <- df[0:idx_endtrain, ]
    validation <- df[idx_begin_validation:idx_end_validation, ]
    test <- df[idx_begin_test:idx_end_test, ]
    # both train and validation
    train_validation <- df[0:idx_end_validation, ]
    
    if (save) {
      # save training data
      save_output(
        output_dir = "data/cleaned_data",
        FUN = write.csv,
        x = train,
        file = "train.csv",
        row.names=FALSE
      )
      # save validation data
      save_output(
        output_dir = "data/cleaned_data",
        FUN = write.csv,
        x = validation,
        file = "validation.csv",
        row.names=FALSE
      )
      # save train_validation data
      save_output(
        output_dir = "data/cleaned_data",
        FUN = write.csv,
        x = train_validation,
        file = "train_validation.csv",
        row.names=FALSE
      )
      # save test data
      save_output(
        output_dir = "data/cleaned_data",
        FUN = write.csv,
        x = test,
        file = "test.csv",
        row.names=FALSE
      )
      
      
    }
    # return in list format
    list(
      train = train,
      validation = validation,
      train_validation = train_validation,
      test = test
    )
    
    
    
    
  }