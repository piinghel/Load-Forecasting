

#' Creates new lagged by means of lags of a given timeserie
#' 
#' @param df: dataframe, 
#' @param output_dir: string indicating the directory to save
#' @param FUN: function 
#' @param ...: additional arguments for the function
#' @return  print where the file was saved

save_output<-function(output_dir, FUN,...){
  
  # get working directory
  working_dir<-getwd()
  # combine path and filename
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
    # set directory where to save
    setwd(output_dir)
    FUN(...)
    cat("saved in \"",output_dir,"\"","directory")
    # back to the original wd 
    setwd(working_dir)
    
  } else {
    # set directory where to save
    setwd(output_dir)
    FUN(...)
    cat("saved in \"",output_dir,"\"","directory")
    # back to the original wd 
    setwd(working_dir)
  }
  

}



