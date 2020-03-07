
#' splits data in train, validation, train_validation and test set
#' 
#' @param df: dataframe, 
#' @param train_perc: double representing the training percentage
#' @param validation_perc: double representing the validation percentage
#' @param embargo: integer indiciating the embargo period to avoid information leakage
#' @return  print where the file was saved


split_data<-function(df, train_perc,validation_perc, embargo=0){
  
  nr_obs<-dim(df)[1]
  idx_endtrain<-nr_obs*train_perc
  # add one week between to remove autocorr
  idx_begin_validation<-idx_endtrain+embargo
  idx_end_validation<-idx_begin_validation+nr_obs*validation_perc
  
  # add one week between to remove autocorr
  idx_begin_test<-idx_end_validation+embargo
  idx_end_test<-nr_obs                
  
  # split into train, validation and test with 1 week between
  # train and validation and 1 week between validation and test
  train<-df[0:idx_endtrain,]
  validation<-df[idx_begin_validation:idx_end_validation,]
  test<-df[idx_begin_test:idx_end_test,]
  # both train and validation
  train_validation<-df[0:idx_end_validation,]
  
  # return in list format
  list(train=train,
       validation=validation,
       train_validation=train_validation,
       test=test)
  
}