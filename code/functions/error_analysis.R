
#  shows largest errors
#' @export
#' @param pred: A tibble containg the id .pred .row 
#' @param df: A tibble containing the prepped dataset 
#' @param max_n: integer to choose the number of observations to see
#' @return tibble with the highest absolute residuals in descending order

see_large_resid <-function(pred, df, max_n = 5){
  
  
  large_resid <- 
    pred %>% 
    mutate(resid = total_cs - .pred) %>% 
    arrange(desc(abs(resid))) %>% 
    slice(1:max_n) # top max_n highest resid
  
  df %>%
    slice(sort(large_resid$.row)) %>%
    select(date) %>%
    mutate(day = wday(date, label = TRUE)) %>%
    bind_cols(large_resid) %>%
    select(date, id,.pred,total_cs,resid)
}


#  shows fitted (=predictions) against actual (=true) values
#' @export
#' @param pred: A tibble with the 2 columns: 'actuals' and 'fitted'
#' @param interactive: Boolean to convert figure to an interactive figure
#' @return  ggplot or ggplotly figure:  fitted against true values


vis_plot_fitted<-function(pred , interactive = FALSE){
  
  # static figure
  p <- ggplot(pred, aes(x = .pred, y = total_cs)) + 
    geom_abline(col = "red",linetype = "dashed") + 
    geom_point(alpha = .3) + 
    coord_equal() + labs(x = "Fitted", y = "Acutal")
  
  # make figure interactive
  if (interactive == TRUE){
    return(ggplotly(p))
  }
  p
}



#  prepare predictions for visualizations
#' @export
#' @param df: A tibble with the 2 columns: 'actuals' and 'fitted'
#' @param pred: A tibble with
#' @param fold: Numeric to indicate the "slice" you want to see. 
#'              This only useful if you have performed time_series cors validation.
#' @return Tibble containg the columns: date, fitted, actual, id (= slice)

prep_visualize_pred <- function(df, pred, fold = NULL){
  
  out <- df %>%
    select(date) %>%
    slice(pred$.row) %>%
    bind_cols(pred) %>%
    select(date,.pred,total_cs,id) %>%
    dplyr::rename(
      actual = total_cs,
      fitted = .pred)
  
  # if null return all folds
  if (is.null(fold)) {
    return(out)
  }
  # return a specific fold
  out %>% filter(id == fold)
  
}


#   Visual Analysis of the predictions
#' @export
#' @param df: A tibble with the 2 columns: 'actuals' and 'fitted'
#' @param max_lags: A numeric to define the maximum number of lags 
#' @param treshold_qq: A numertic indicating the maximum number of samples used in the qqplot
#' @param interactive: Boolean indicating to convert to plot to an interactive vizulation
#' @param theme: ggplot2 element specifying the theme 
#' @param legend: ggplot2 element specifying to remove the text above the legend
#' @param FUN: function, either ggPacF or ggAcf
#' 
#' @return 4 plots: 1) plotted and fitted values
#'                  2) residuals
#'                  3)(parital) autocorrelation of the resiudals 
#'                  4) qqplot of the residuals


visualize_pred <- function(df,max_lags = 60,
                          treshold_qq = 5000,
                          interactive = TRUE,
                          theme  = theme_minimal(),
                          legend = theme(legend.title = element_blank()),
                          FUN = forecast::ggPacf,...){
  

  # determine y label autocorrelation function
  label_y_p3 <- "PACF"
  if (identical(forecast::ggAcf, FUN))
  {
    label_y_p3 <- "ACF"
  }
  
  df <- df %>% mutate(
    residuals = actual-fitted
  )
  df_wide <- df %>%
    select(date,actual,fitted) %>%
    gather(key="variable",value="value",-date)
  
  
  # plot fitted and acutal values (static)
  p1 <- ggplot(df_wide, aes(x = date, y = value)) + 
    geom_line(aes(color = variable,linetype = variable)) +
    geom_hline(yintercept=0, size=.5,color="grey", linetype="dashed") +
    scale_color_manual(values = c("#00AFBB", "red")) +
    labs(x="",y="Consumption") + theme + legend
  p1_i<-ggplotly(p1)
  
  
  # plot residuals (static)
  p2 <- ggplot(df,aes(x=date,y=residuals))+geom_line(color="#009E73") +
    geom_hline(yintercept=0, size=.5,color="grey", linetype="dashed") + 
    labs(y="Consumption") + labs(x="Date",y="Residuals") + theme
  p2_i <- ggplotly(p2)
  
  # (Partial) autocorrelation plot (static)
  p3 <- FUN(df["residuals"], plot = TRUE, lag.max = max_lags) + 
    theme(legend.title = element_blank()) + ggtitle("")+
    labs(x = "Lag", y = label_y_p3) + theme 
  
  p3_i <- ggplotly(p3)
  
  # be careful with plotting to many points (becomes very slow)
  if (nrow(df) > treshold_qq){
    n <- treshold_qq
  }
  else{
    n <- nrow(df)
  }
  
  # qqplot (static)
  p4 <- ggplot(df[sample(1:nrow(df), n),], 
             aes(sample = residuals)) +
    stat_qq() + stat_qq_line() + labs(x = "Sample", y = "Theoretical") + 
    theme_minimal()
  
  p4_i <- ggplotly(p4)
  
  # return static plot
  if (interactive == FALSE){
  return(((p1/p2)/(p3+p4)))
  }
  # make the interactive plot
  plotly::subplot(plotly::subplot(p1_i,p2_i,nrows=2,shareX = TRUE, titleY = TRUE),
                  plotly::subplot(p3_i,p4_i,nrows=1,margin=.05, titleY = TRUE,titleX = TRUE),
                  nrows = 2,heights = c(0.6, 0.4),margin=.05) %>%
                  plotly::layout(title = "Residuals Analysis")
  
}


#   Visualization of the performance
#' @export
#' @param df: Tibble
#' @param metrics: Character vector with the performance metrics
#' @param pal: color pallette
#' @param legend_place: character determing the place of the legend
#' @param legend_names: character vector with the legend names
#' @return Ggplot or ggplotly with the performance of different metrics.
#'         This will only work if you have performed some kind of cross validation. 
#'         

plot_perf_metric <- function(df, metrics = c("mase","rsq_trad"), 
                             pal = scale_color_brewer(palette="Dark2"),
                             legend_place="top",
                             legend_names = c("mase (t-168)", "r2")){
  
  # filter metric and plot
  df %>% dplyr::filter(.metric %in% metrics) %>% 
    ggplot(., aes(x=model, y = mean, color = .metric)) + geom_point() + 
    geom_errorbar(aes(ymin=mean-std_err, ymax=mean+std_err), 
                  width=.2,position=position_dodge(0.05)) +
    facet_grid(.metric~., scales = "free_y") + 
    labs(x = "Model", y = "Mean estimate (1 std error)") +
    pal + THEME + LEGEND + theme(legend.position=legend_place) +
    scale_color_manual(labels = legend_names, values = c(cbPalette[4], cbPalette[7]))
  
  
}

#  Compare predictions and redisuals across models over time
#' @export
#' @param df: A tibble with the 2 columns: 'actuals' and 'fitted'
#' @param interactive: Boolean indicating to convert to plot to an interactive vizulation
#' @param size: Numeric to determine the size of lines in the figures
#' @return Ggplot or ggplotly object with the top figure containing 
#'         the predictions and true values over time. 
#'         The lower plot contains the residuals of different models over time.


compare_pred_models <- function(df, interactive = FALSE, size = 1){
  
  # acutal values
  truth <- tibble(model = "actual", date = df$date,
                  fitted = df$actual)
  # add acutal values
  df_long_update <- df %>% select(model, date, fitted) %>% bind_rows(truth)

  # convert to interactive figure  
  if (interactive==TRUE){
    size = .5
  }
  # compare predictions of all models
  p_compare_pred<- ggplot(df_long_update, aes(x=date, y=fitted)) +
    geom_line(aes(color=model, linetype = model),size = size) + 
    scale_color_brewer(palette="Dark2") +
    theme_minimal()+theme(legend.position="top") + 
    theme(legend.title=element_blank())+
    geom_hline(yintercept=0, size=.5,color="grey", 
               linetype="dashed")+labs(y="Consumption")
  
  # compare residuals of all models
  p_compare_resid <- ggplot(df, aes(x=date, y=residual)) +
    geom_line(aes(color=model, linetype = model),size = size) + 
    theme_minimal() + theme(legend.position = "none") +
    scale_color_brewer(palette="Dark2") +
    geom_hline(yintercept=0, size=.5,color="grey",linetype="dashed")+
    labs(x="Date", y="Residuals")
  
  # static plot
  if (interactive == FALSE){
    return((p_compare_pred/p_compare_resid))
  }
  
  # convert to interacive figure
  plotly::subplot(ggplotly(p_compare_pred), ggplotly(p_compare_resid), nrows = 2,
                  titleY=TRUE, shareX = TRUE) %>% layout(showlegend = FALSE)
}








